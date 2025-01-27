

#' Read newer premier data
#'
#' @param year of data (we have 2000 - 2022)
#' @param quarter 1-4
#' @param cpt named list of CPT codes
#' @param icd9_diag named list
#' @param icd10_diag named list
#' @param icd9_proc named list
#' @param icd10_proc named list
#' @param charge named list of premier charge codes
#' @param medrec_keys vector of MEDREC_KEY (patients) to filter on
#' @param pat_keys vector of PAT_KEY (encounters) to filter on
#' @param file_prefix start of file names
#' @param dir where the files are
#' @param partial are we looking for partial code matches for matching sub-codes (e.g. 188.1 matched by 188.12) or literal matches (probably faster)
#' @param cols_to_keep list of additional columns from the various PHD tables.
#' e.g. if you edited charges = "BILL_CHARGES", the result would have a column for BILL_CHARGES with each element associated with an encounter pasted
#' together separated by "|". By default this only happens for the "_CODE" columns (and "_VERSION" for ICD)
#'
#' @returns data.table of results from PATDEMO merged with info from other tables
#' depending on the code systems specified.
#' @export
read_premier2 <- function(year, quarter,
                          cpt,
                          icd9_diag, icd10_diag,
                          icd9_proc, icd10_proc,
                          charge,
                          medrec_keys, pat_keys,
                          file_prefix="uhc_", dir=getwd(),
                          partial=FALSE,
                          cols_to_keep = list(cpt=c(),
                                              icd_diag=c(),
                                              icd_proc=c(),
                                              charge=c()
                          )){

  f <- paste0(file_prefix, year, quarter)

  # find files
  demo <- list.files(pattern = paste0(f, "_patdemo"),
                     path = dir, full.names = TRUE)
  if (length(demo) != 1) stop("Issue with the patdemo file (", length(demo), " found)")

  if (!missing(cpt)){
    cptfile <- list.files(pattern = paste0(f, "_patcpt"),
                          path = dir, full.names = TRUE)
    if (length(cptfile) != 1) stop("Issue with the patcpt file (", length(cptfile), " found)")
  } else{
    cptfile <- NULL
  }

  if (!missing(icd9_diag) | !missing(icd10_diag)){
    icd_diagfile <- list.files(pattern = paste0(f, "_paticd_diag"),
                               path = dir, full.names = TRUE)
    if (length(icd_diagfile) != 1) stop("Issue with the paticd_diag file (", length(icd_diagfile), " found)")
  } else{
    icd_diagfile <- NULL
  }

  if (!missing(icd9_proc) | !missing(icd10_proc)){
    icd_procfile <- list.files(pattern = paste0(f, "_paticd_proc"),
                               path = dir, full.names = TRUE)
    if (length(icd_procfile) != 1) stop("Issue with the paticd_proc file (", length(icd_procfile), " found)")
  } else{
    icd_procfile <- NULL
  }

  if (!missing(charge)){
    patbillfile <- list.files(pattern = paste0(f, "_patbill"),
                              path = dir, full.names = TRUE)
    if (length(patbillfile) != 1) stop("Issue with the patbill file (", length(patbillfile), " found)")
  } else{
    patbillfile <- NULL
  }

  ###
  dat <- data.table::fread(demo)
  if (!missing(medrec_keys)) {
    dat <- subset(dat, MEDREC_KEY %in% medrec_keys)
    if (nrow(dat) < 1) warning("subsetting on medrec_keys left nothing")
  }

  if (!missing(pat_keys)) {
    dat <- subset(dat, PAT_KEY %in% pat_keys)
    if (nrow(dat) < 1) warning("subsetting on pat_keys left nothing")
  }

  matchfun <- function(x, y){
    if (partial){
      grepl(paste0("^", y, collapse = "|"), x)
    } else{
      x %in% y
    }
  }

  ### CPT
  if (!is.null(cptfile)){
    cptdata <- data.table::fread(cptfile, select = c("PAT_KEY", "CPT_CODE", cols_to_keep$cpt))
    if (!missing(medrec_keys) | !missing(pat_keys)) cptdata <- subset(cptdata, PAT_KEY %in% dat$PAT_KEY)

    cptitems <- names(cpt)
    if (any(duplicated(cptitems))) stop("check cpt code names for duplicates")
    cptdata[, (paste0("cpt_", cptitems)) := lapply(cptitems, \(x) matchfun(CPT_CODE, cpt[[x]]))]

    dat <- data.table::merge.data.table(x = dat,
                             y = cptdata[, lapply(.SD, paste0, collapse="|"), by="PAT_KEY", .SDcols=c("CPT_CODE", cols_to_keep$cpt)],
                             by = "PAT_KEY", all.x = TRUE)

    dat <- data.table::merge.data.table(x = dat,
                             y = cptdata[, lapply(.SD, any), by="PAT_KEY", .SDcols=paste0("cpt_", cptitems)],
                             by = "PAT_KEY", all.x = TRUE)

    rm(cptdata)
  }

  ### diags
  if (!is.null(icd_diagfile)){
    if(missing(icd9_diag)) icd9_diag <- list()
    if(missing(icd10_diag)) icd10_diag <- list()

    icd_diagdata <- data.table::fread(icd_diagfile, select = c("PAT_KEY", "ICD_VERSION", "ICD_CODE", cols_to_keep$icd_diag))
    if (!missing(medrec_keys) | !missing(pat_keys)) icd_diagdata <- subset(icd_diagdata, PAT_KEY %in% dat$PAT_KEY)

    dat <- data.table::merge.data.table(x = dat,
                                        y = icd_diagdata[, lapply(.SD, paste0, collapse="|"), by="PAT_KEY", .SDcols=c("ICD_VERSION", "ICD_CODE", cols_to_keep$icd_diag)],
                                        by = "PAT_KEY", all.x = TRUE)

    icd9items <- names(icd9_diag)
    if (any(duplicated(icd9items))) stop("check icd 9 code names for duplicates")
    if (!is.null(icd9items))
      icd_diagdata[, (paste0("icd9_", icd9items)) := lapply(icd9items, \(x) matchfun(ICD_CODE, icd9_diag[[x]]) & ICD_VERSION == 9)]

    icd10items <- names(icd10_diag)
    if (any(duplicated(icd10items))) stop("check icd 10 code names for duplicates")
    if (!is.null(icd10items))
      icd_diagdata[, (paste0("icd10_", icd10items)) := lapply(icd10items, \(x) matchfun(ICD_CODE, icd10_diag[[x]]) & ICD_VERSION == 10)]

    # combine
    icdditems <- intersect(icd9items, icd10items)
    for (i in icdditems){
      #icd_diagdata[, (paste0("ICD_", i)) := apply(icd_diagdata[, paste0("ICD", 9:10, "_", i), with=FALSE], 1, any) ]
      icd_diagdata[, (paste0("icd_", i)) := icd_diagdata[[paste0("icd", 9, "_", i)]] | icd_diagdata[[paste0("icd", 10, "_", i)]] ]
    }

    sd <- grep("icd", colnames(icd_diagdata), value = TRUE, ignore.case = FALSE)
    dat <- data.table::merge.data.table(x = dat,
                                        y = icd_diagdata[, lapply(.SD, any), by="PAT_KEY", .SDcols=sd],
                                        by = "PAT_KEY", all.x = TRUE)

    rm(icd_diagdata)
  }

  ### procs
  if (!is.null(icd_procfile)){
    if(missing(icd9_proc)) icd9_proc <- list()
    if(missing(icd10_proc)) icd10_proc <- list()

    icd_procdata <- data.table::fread(icd_procfile, select = c("PAT_KEY", "ICD_VERSION", "ICD_CODE", cols_to_keep$icd_proc))
    if (!missing(medrec_keys) | !missing(pat_keys)) icd_procdata <- subset(icd_procdata, PAT_KEY %in% dat$PAT_KEY)

    dat <- data.table::merge.data.table(x = dat,
                                        y = icd_procdata[, lapply(.SD, paste0, collapse="|"), by="PAT_KEY", .SDcols=c("ICD_VERSION", "ICD_CODE", cols_to_keep$icd_proc)],
                                        by = "PAT_KEY", all.x = TRUE, suffixes = c("", "_PROC"))

    icd9items <- names(icd9_proc)
    if (any(duplicated(icd9items))) stop("check icd 9 proc code names for duplicates")
    if (!is.null(icd9items))
      icd_procdata[, (paste0("icdp9_", icd9items)) := lapply(icd9items, \(x) matchfun(ICD_CODE, icd9_proc[[x]]) & ICD_VERSION == 9)]

    icd10items <- names(icd10_proc)
    if (any(duplicated(icd10items))) stop("check icd 10 proc code names for duplicates")
    if (!is.null(icd10items))
      icd_procdata[, (paste0("icdp10_", icd10items)) := lapply(icd10items, \(x) matchfun(ICD_CODE, icd10_proc[[x]]) & ICD_VERSION == 10)]

    # combine
    icdditems <- intersect(icd9items, icd10items)
    for (i in icdditems){
      #icd_procdata[, (paste0("ICD_", i)) := apply(icd_procdata[, paste0("ICD", 9:10, "_", i), with=FALSE], 1, any) ]
      icd_procdata[, (paste0("icdp_", i)) := icd_procdata[[paste0("icdp", 9, "_", i)]] | icd_procdata[[paste0("icdp", 10, "_", i)]] ]
    }

    sd <- grep("icdp", colnames(icd_procdata), value = TRUE, ignore.case = FALSE)
    dat <- data.table::merge.data.table(x = dat,
                                        y = icd_procdata[, lapply(.SD, any), by="PAT_KEY", .SDcols=sd],
                                        by = "PAT_KEY", all.x = TRUE)

    rm(icd_procdata)
  }

  ## charges
  if (!is.null(patbillfile)){
    billdata <- data.table::fread(patbillfile, select = c("PAT_KEY", "STD_CHG_CODE", cols_to_keep$charge))
    if (!missing(medrec_keys) | !missing(pat_keys)) billdata <- subset(billdata, PAT_KEY %in% dat$PAT_KEY)

    billitems <- names(charge)
    billdata[, (paste0("charge_", billitems)) := lapply(billitems, \(x) matchfun(STD_CHG_CODE, charge[[x]]))]

    dat <- data.table::merge.data.table(x = dat,
                                        y = billdata[, lapply(.SD, paste0, collapse="|"), by="PAT_KEY", .SDcols=c("STD_CHG_CODE", cols_to_keep$charge)],
                                        by = "PAT_KEY", all.x = TRUE)

    dat <- data.table::merge.data.table(x = dat,
                                        y = billdata[, lapply(.SD, any), by="PAT_KEY", .SDcols=paste0("charge_", billitems)],
                                        by = "PAT_KEY", all.x = TRUE)

    rm(billdata)
  }

  return(dat)
}

# library(premieR)
# read_premier2(year = 2000, quarter = 1, cpt = list("A"="23456"),
#               icd9_diag = list("P" = "188"), icd10_diag = list("P" = "C65"),
#               icd9_proc = list("s" = "12.22"), dir = "ignore/")
#
# mrks = c(-1885091522, -1885068153, -1885090675,
#          -1885022150, -1885091483, -1885089155)
# read_premier2(year = 2000, quarter = 1, cpt = list("A"="23456"),
#               icd9_diag = list("P" = "188"), icd10_diag = list("P" = "C65"),
#               icd9_proc = list("s" = "12.22"), dir = "ignore/",
#               medrec_keys = mrks)
#
# pks = c(-1885091522, -1885091516, -1885091497, -1885091493)
#
# read_premier2(year = 2000, quarter = 1, cpt = list("A"="23456"),
#               icd9_diag = list("P" = "188"), icd10_diag = list("P" = "C65"),
#               icd9_proc = list("s" = "12.22"), dir = "ignore/",
#               pat_keys = pks) #w/o mrk
#
# read_premier2(year = 2000, quarter = 1, cpt = list("A"="23456"),
#               icd9_diag = list("P" = "188"), icd10_diag = list("P" = "C65"),
#               icd9_proc = list("s" = "12.22"), dir = "ignore/",
#               medrec_keys = mrks, pat_keys = pks) #w/ mrk
#
#
#
# x = read_premier2(year = 2000, quarter = 2,
#                   cpt = list(SCH = c("58541", "58542","58543", "58544", "58180")),
#               dir = "ignore/", partial = F)
#
# x$cpt_SCH |> table()
