#' Read specific ICD, CPT, and charge codes from premier data files
#'
#' @param files Character vector of PHD files containing ICD diagnosis, ICD procedure, CPT/HCPS, or premier charge/billing codes (all of same type).
#' Assumed to be SAS files (read by \code{haven::read_sas})
#' @param icd9 named list of ICD 9 codes of interest (whether procedure or diagnosis is determined by \code{files})
#' @param icd10 named list of ICD 10 codes of interest
#' @param cpt named list of CPT codes of interest
#' @param charge named list of premier charge codes of interest (only one of icd9, icd10, cpt, charge should be provided)
#' @param pat_keys vector of PAT_KEY encounter IDs to keep. If icd9=icd10=cpt=charge=NULL and PAT_KEY is specified, the function will resturn all
# codes associated with PAT_KEYs. Otherwise, all PAT_KEYs matching specified codes are returned
# (along with columns showing matches using names from code list)
# specfic columns can then be used to filter encounters
#' @param pat_filter_only should encounters only be kept if they appear in pat_keys?
#' @return Either the original data with new columns (from \code{cols}) identifying those with a match within
#' the time frame specified by \code{num_months} (otherwise just those columns if \code{add_columns_to_data = F})
#' @import data.table
#' @importFrom haven read_sas
#' @export
read_premier_codes = function(files, icd9=NULL, icd10=NULL, cpt=NULL,
                              charge=NULL, pat_keys=NULL, pat_filter_only = F){

  if ( is.null(icd9) & is.null(icd10) & is.null(cpt) & is.null(charge) & is.null(pat_keys) ){
    stop("at least one of icd9, icd10, cpt, charge, pat_keys should be specified")
  }

  if (!is.null(cpt) & (!is.null(icd9) | !is.null(icd10))){
    stop("cpt *or* icd9/icd10 should be specified")
  }

  if (!is.null(charge) & (!is.null(cpt) | !is.null(icd9) | !is.null(icd10))){
    stop("charge *or* cpt *or* icd9/icd10 should be specified")
  }

  if ( pat_filter_only & is.null(pat_keys)){
    stop("if pat_filter_only = TRUE, pat_keys must be specified")
  }

  if (!is.null(icd9) | !is.null(icd10) ){
    if ( !is.null(icd9) && ( !is.list(icd9) || is.null(names(icd9))) ){
      stop("icd9 should be a named list")
    }
    if ( !is.null(icd10) && ( !is.list(icd10) || is.null(names(icd10))) ){
      stop("icd10 should be a named list")
    }
    new_column_prefix = "ICD_"
  }

  if (!is.null(cpt)){
    if ( !is.list(cpt) || is.null(names(cpt)) ) {
      stop("cpt should be a named list")
    }
    new_column_prefix = "CPT_"
  }

  if (!is.null(charge)){
    if ( !is.list(charge) || is.null(names(charge)) ) {
      stop("charge should be a named list")
    }
    new_column_prefix = "CHARGE_"
  }

  readf <- function(file){
    stopifnot(length(file) == 1)
    if (grepl("sas7bdat", file)){
      haven::read_sas(file)
    } else{
      data.table::fread(file)
    }
  }

  # read files
  out <-lapply(X = files, function(x) {
    readf(x)
  })
  # go from list to df
  out = data.table::rbindlist(out)

  match=NULL
  if ( !is.null(icd9) | !is.null(icd10)) {
    match = (!is.null(icd9) & grepl(paste0("^", unlist(icd9), collapse = "|"), out$ICD_CODE) & out$ICD_VERSION == 9) |
      (!is.null(icd10) & grepl(paste0("^", unlist(icd10), collapse = "|"), out$ICD_CODE) & out$ICD_VERSION == 10)

    icd_versions = unique(out$ICD_VERSION) # keep track
  }
  if (!is.null(cpt)){
    match = grepl(paste0("^", unlist(cpt), collapse = "|"), out$CPT_CODE)
  }
  if (!is.null(charge)){
    match = grepl(paste0("^", unlist(charge), collapse = "|"), out$STD_CHG_CODE)
  }
  if (!is.null(match)){
    out[, paste0(new_column_prefix, "codes_matched") := as.numeric(match)]
  }

  cols = colnames(out)

  if (pat_filter_only){
    keep_i = which(out$PAT_KEY %in% pat_keys)
  } else{
    keep_i = which(out$PAT_KEY %in% c(out$PAT_KEY[match], pat_keys))
  }

  # one row per PAT_KEY
  out = out[i = keep_i, lapply(.SD, function(x) paste0(x, collapse = " | ")), by=PAT_KEY, .SDcols=cols[cols!="PAT_KEY"]]

  # the next lines take the matched codes and use the names in the codes list
  # to create columns with 1 or 0 depending on whether a code was matched
  if (!is.null(icd9)){
    if ( 9 %in% icd_versions & any(match) ){
      v = strsplit(out$ICD_VERSION, split = " \\| ")
      code_list = strsplit(out$ICD_CODE, split = " \\| ")
      code_list = lapply(1:length(code_list), function(x){
        code_list[[x]][v[[x]] == "9"]
      })
      X = make_cols(codes = icd9, match_list = code_list)

    } else{ # if 9 wasn't available in this dataset then don't bother looking for matches
      X = as.data.frame(matrix(0, nrow = nrow(out), ncol = length(names(icd9))))
      colnames(X) = names(icd9)
    }
    colnames(X) = paste0("ICD9_", colnames(X))
    X$PAT_KEY = out$PAT_KEY
    out = data.table::merge.data.table(out, X, by = "PAT_KEY")
  }
  if (!is.null(icd10)){
    if ( 10 %in% icd_versions & any(match) ){
      v = strsplit(out$ICD_VERSION, split = " \\| ")
      code_list = strsplit(out$ICD_CODE, split = " \\| ")
      code_list = lapply(1:length(code_list), function(x){
        code_list[[x]][v[[x]] == "10"]
      })
      X = make_cols(codes = icd10, match_list = code_list)

    } else{ # if 10 wasn't available in this dataset then don't bother looking for matches
      X = as.data.frame(matrix(0, nrow = nrow(out), ncol = length(names(icd10))))
      colnames(X) = names(icd10)
    }
    colnames(X) = paste0("ICD10_", colnames(X))
    X$PAT_KEY = out$PAT_KEY
    out = data.table::merge.data.table(out, X, by = "PAT_KEY")
  }
  if (!is.null(cpt)){
    code_list = strsplit(out$CPT_CODE, split = " \\| ")
    X = make_cols(codes = cpt, match_list = code_list)
    colnames(X) = paste0("CPT_", colnames(X))
    X$PAT_KEY = out$PAT_KEY
    out = data.table::merge.data.table(out, X, by = "PAT_KEY")
  }
  if (!is.null(charge)){
    code_list = strsplit(out$STD_CHG_CODE, split = " \\| ")
    X = make_cols(codes = charge, match_list = code_list)
    colnames(X) = paste0("CHARGE_", colnames(X))
    X$PAT_KEY = out$PAT_KEY
    out = data.table::merge.data.table(out, X, by = "PAT_KEY")
  }

  # if these are procedure codes we should specify
  if ("PROC_PHY" %in% cols){
    data.table::setnames(x = out, old = colnames(out), new = gsub("ICD", "ICDP", colnames(out), ignore.case = F))
  }

  return(out)
}
