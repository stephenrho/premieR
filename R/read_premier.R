#' Read data from the premier healthcare database (PHD)
#'
#' This is a wrapper function for \code{read_premier_codes()}
#'
#' @param no_apr_file name of file containing demographic information
#' @param ICD_proc_file name of file containing ICD procedure codes
#' @param ICD9_proc_codes named list containing codes of interest
#' @param ICD10_proc_codes named list containing codes of interest
#' @param CPT_file name of file containing CPT codes
#' @param CPT_codes named list containing codes of interest
#' @param ICD_diag_file name of file containing ICD procedure codes
#' @param ICD9_diag_codes named list containing codes of interest
#' @param ICD10_diag_codes named list containing codes of interest
#' @param charge_file name of file containing premier charge codes
#' @param charge_codes named list containing codes of interest
#' @param filter_codes column names that should be used to filter data.
#' Should match the naming convention: e.g., \code{ICD9_[name used in ICD9_diag_codes]},
#' \code{ICDP10_[name used in ICD10_proc_codes]}, \code{CPT_[name used in CPT_codes]}
#' @param medrec_ids specific patient IDs to be retained
#' @return data.table containing encounters matching codes (or patients) of interest
#' with demograpic information
#' @import data.table
#' @export
read_premier = function(no_apr_file,
                        ICD_proc_file=NULL, ICD9_proc_codes=NULL, ICD10_proc_codes=NULL,
                        CPT_file=NULL, CPT_codes=NULL,
                        ICD_diag_file=NULL, ICD9_diag_codes=NULL, ICD10_diag_codes=NULL,
                        charge_file=NULL, charge_codes=NULL,
                        filter_codes=NULL,
                        medrec_ids=NULL){

  mymerge = function(x,y) data.table::merge.data.table(x, y, by="PAT_KEY", all=TRUE)

  # at least one of these should be non-null
  stopifnot( !is.null(ICD_proc_file) | !is.null(ICD_diag_file) | !is.null(CPT_file) )

  if (is.null(medrec_ids)){
    # if specific patient IDs aren't specified then find matches for codes provided
    # and keep rows based on filter_cols (useful for finding MEDREC_KEYs of eligible patients)
    dfs = list()
    # read proc codes
    if (!is.null(ICD_proc_file)){
      dfs[["ICD_proc"]] = read_premier_codes(files = ICD_proc_file,
                                             icd9 = ICD9_proc_codes,
                                             icd10 = ICD10_proc_codes)
    } else{
      cat("No ICD procedure file(s)\n")
    }
    # read cpt codes
    if (!is.null(CPT_file)){
      dfs[["CPT"]] = read_premier_codes(files = CPT_file,
                                        cpt = CPT_codes)
    } else{
      cat("No CPT file(s)\n")
    }
    # read diagnosis codes
    if (!is.null(ICD_diag_file)){
      dfs[["ICD_diag"]] = read_premier_codes(files = ICD_diag_file,
                                             icd9 = ICD9_diag_codes,
                                             icd10 = ICD10_diag_codes)
    } else{
      cat("No ICD diagnosis file(s)\n")
    }
    # read charge codes
    if (!is.null(charge_file)){
      dfs[["charge"]] = read_premier_codes(files = charge_file,
                                           charge = charge_codes)
    } else{
      cat("No ICD charge file(s)\n")
    }

    # put everything together
    # procdiag = merge(ICD_proc, ICD_diag, by = "PAT_KEY", all=T)
    # procdiag = merge(procdiag, CPT, by = "PAT_KEY", all=T)
    #procdiag = plyr::join_all(dfs, by = "PAT_KEY")
    # procdiag = Reduce(function(...) data.table::merge.data.table(..., by="PAT_KEY", all = TRUE), dfs)
    procdiag = Reduce(mymerge, dfs)
    #procdiag = Reduce(f = function(...) merge(..., by="PAT_KEY", all=T), dfs)

    if (!is.null(filter_codes)){
      # filter_codes should include the prefix used in call to read_premier_icdcpt
      if (length(filter_codes) == 1){
        procdiag = procdiag[which(procdiag[,..filter_codes] == 1),]
      } else {
        procdiag = procdiag[which(apply(procdiag[,..filter_codes] == 1, MARGIN = 1, FUN = any)),]
      }
    }

    # read in patient info and merge with procdiag/diagnoses
    proc_apr <-lapply(no_apr_file, function(x) {
      haven::read_sas(x)
      # a<-haven::read_sas(x)
      # merge(a, procdiag, by = "PAT_KEY")
    })

    proc_apr = data.table::rbindlist(proc_apr)
    # dat = bind_rows(proc_apr)
    dat = data.table::merge.data.table(proc_apr, procdiag, by = "PAT_KEY", all.y = T)

  } else{
    # match the given medreckeys
    apr <-lapply(no_apr_file, function(x) {
      haven::read_sas(x)
      # a<-read_sas(x)
      # out = subset(a, MEDREC_KEY %in% medrec_ids)
      # out
    })
    # go from list to df
    # apr = dplyr::bind_rows(apr)
    apr = data.table::rbindlist(apr)

    apr = subset(apr, MEDREC_KEY %in% medrec_ids)

    pk = apr$PAT_KEY # get encounters associated with selected medrec keys

    dfs = list()
    # read proc codes
    if (!is.null(ICD_proc_file)){
      dfs[["ICD_proc"]] = read_premier_codes(files = ICD_proc_file,
                                             icd9 = ICD9_proc_codes,
                                             icd10 = ICD10_proc_codes,
                                             pat_keys=pk,
                                             pat_filter_only = T)
    } else{
      cat("No ICD procedure file(s)\n")
    }
    # read cpt codes
    if (!is.null(CPT_file)){
      dfs[["CPT"]] = read_premier_codes(files = CPT_file,
                                        cpt = CPT_codes,
                                        pat_keys = pk,
                                        pat_filter_only = T)
    } else{
      cat("No CPT file(s)\n")
    }
    # read diagnosis codes
    if (!is.null(ICD_diag_file)){
      dfs[["ICD_diag"]] = read_premier_codes(files = ICD_diag_file,
                                             icd9 = ICD9_diag_codes,
                                             icd10 = ICD10_diag_codes,
                                             pat_keys = pk,
                                             pat_filter_only = T)
    } else{
      cat("No ICD diagnosis file(s)\n")
    }
    # charges
    if (!is.null(charge_file)){
      dfs[["charge"]] = read_premier_codes(files = charge_file,
                                           charge = charge_codes,
                                           pat_keys = pk,
                                           pat_filter_only = T)
    } else{
      cat("No charge file(s)\n")
    }

    # put everything together
    # procdiag = merge(ICD_diag, ICD_proc, by = "PAT_KEY", all=T)
    # procdiag = merge(procdiag, CPT, by = "PAT_KEY", all=T)
    #procdiag = plyr::join_all(dfs, by = "PAT_KEY")
    # procdiag = Reduce(function(...) data.table::merge.data.table(..., by="PAT_KEY", all = TRUE), dfs)
    procdiag = Reduce(mymerge, dfs)
    #procdiag = Reduce(f = function(...) merge(..., by="PAT_KEY", all=T), dfs)

    dat = data.table::merge.data.table(apr, procdiag, by = c("PAT_KEY"), all=T)

  }

  # if (!is.null(charge_codes)){
  #   # read charges and merge
  #   charges = read_premier_charge(charge_file = charge_file,
  #                                 PAT = dat$PAT_KEY,
  #                                 charge_codes = charge_codes)
  #
  #   dat = merge(dat, charges, by="PAT_KEY", all.x=T)
  # } else{
  #   cat("No charge codes given so skipping patbill file(s)\n")
  # }

  return(dat)
}
