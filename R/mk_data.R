
#' Wrapper for comorbidity package allowing for mix of 9 and 10
#'
#' @param data with columns MEDREC_KEY, ICD_CODE, ICD_VERSION - where
#' the latter 2 have values pasted together separated by "|"
#' @param type "charlson" or "elixhauser"
#'
#' @returns data.frame of individual comorbidities (T/F) and the index
#' @export
#'
#' @keywords internal
get_comorbidities <- function(data, type = c("charlson", "elixhauser")){
  type <- match.arg(type)

  if (type == "charlson"){
    maps <- c("charlson_icd9_quan", "charlson_icd10_quan")
    wts <- "quan"
  } else{
    maps <- c("elixhauser_icd9_quan", "elixhauser_icd10_quan")
    wts <- "swiss"
  }

  codes <- strsplit(data$ICD_CODE, split = "\\|")
  versions <- strsplit(data$ICD_VERSION, split = "\\|")
  comor <- data.frame(code = unlist(codes), version = unlist(versions),
                      MEDREC_KEY = rep(data$MEDREC_KEY, unlist(lapply(codes, length))))

  c9 <- comorbidity::comorbidity(subset(comor, version == "9"),
                                 id = "MEDREC_KEY",
                                 code = "code",
                                 map = maps[1],
                                 assign0 = FALSE)

  c10 <- comorbidity::comorbidity(subset(comor, version == "10"),
                                 id = "MEDREC_KEY",
                                 code = "code",
                                 map = maps[2],
                                 assign0 = FALSE)

  com = data.table::data.table(rbind(c9, c10))
  com = com[, lapply(.SD, function(x) as.numeric(any(x==1, na.rm = T))),
            by="MEDREC_KEY",
            .SDcols = colnames(c10)[-1]]

  # need to give class back...
  names <- attr(c10, "variable.labels")
  attr(com, "variable.labels") <- names
  attr(com, "map") <- attr(c10, "map")
  class(com) <- c("comorbidity", "data.frame")
  com$comorbidity_index <- comorbidity::score(x = com,
                                              weights = wts,
                                              assign0 = FALSE)

  colnames(com)[2:length(names)] <- janitor::make_clean_names(names[-1])
  colnames(com)[length(names)+1] <- paste0(type, "_index")
  com[, 2:length(names)] <- com[, 2:length(names)] == 1

  return(com)
}

#' Combine PHD data
#'
#' @description
#' For running on the results of calls to read_premier2
#' to calculate days between index_event and events
#' defined by ICD, CPT, charge codes (identified via column prefix)
#'
#' @param files vector of files to read in and combine
#' @param index_event character. Column containing event of interest
#' (the first instance of which will be called index). Can be created in combine
#' @param medrec_keys optional vector if IDs to subset on
#' @param combine named list of variables to create that are combinations
#' of existing variables. Best to use alist. e.g. combine = alist("A" = cpt_A | icdp_A)
#' @param comorbidity "none", "charlson", or "elixhauser". Uses the comorbidity package
#' on ICD codes within a number of days of index defined by comorbidity_time
#' @param comorbidity_time time window to use ICD codes to calculate comorbidity index from
#' @param ignore_from_index optional vector of columns to ignore when pasting time from index
#' (e.g. if interest is only presence at index)
#' @param return_all return all events or just index (default)
#'
#' @returns data.table of index events (if return_all = F) with
#' comorbidity info (if requested) and columns containing time from index
#' for events defined by ICD, CPT, or premier charge codes.
#' @export
mk_data <- function(files,
                    index_event,
                    medrec_keys,
                    combine,
                    comorbidity = c("none", "charlson", "elixhauser"),
                    comorbidity_time = c(-365, -1),
                    ignore_from_index, return_all=FALSE){

  if(missing(index_event)) stop("must specify an index_event")
  comorbidity <- match.arg(comorbidity)
  if (missing(medrec_keys)) medrec_keys <- NULL

  dat <- data.table::rbindlist(
    lapply(files, \(x) {
      tmp <- data.table::fread(x)
      if (!is.null(medrec_keys)) tmp <- subset(tmp, MEDREC_KEY %in% medrec_keys)
      tmp
    })
  )

  norig <- length(unique(dat$MEDREC_KEY))

  cols <- grep("^icd_|^icdp_|^icd9_|^icdp9_|^icd10_|^icdp10_|^cpt_|^charge_", colnames(dat), value = TRUE)

  for (col in cols){
    data.table::set(dat, i = which(is.na(dat[[col]])), j=col, value = FALSE)
  }

  # make the combination of columns requested
  if (!missing(combine)){
    if (!is.list(combine) || is.null(names(combine))) stop("combine should be a named list")
    if (any(names(combine) %in% cols)) stop("name in combine is already in data. Please change")
    #if (any(!unlist(combine) %in% cols)) stop("column(s) in combine are not in data. Please check")

    dat[, paste0(names(combine)) := lapply(combine, eval, envir=dat)]

    cols <- c(cols, names(combine)) # add to the cols
  }
  if (!index_event %in% cols) stop("index_event could not be found")

  # find index event
  dat <- data.table::merge.data.table(dat,
                               dat[which(dat[[index_event]]),
                                   .(index_date = min(ADMIT_DATE)),
                                   by=MEDREC_KEY],
                               by = "MEDREC_KEY", all.x=TRUE)

  dat[, from_index := ADMIT_DATE - index_date]

  # comorbidities
  if (comorbidity != "none"){
    if (!all(c("ICD_CODE", "ICD_VERSION") %in% colnames(dat))) stop("data doesn't contain ICD diagnosis codes")
    if (length(comorbidity_time) != 2) stop("comorbidity_time should be length 2")
    if (!is.numeric(comorbidity_time)) stop("comorbidity_time should be numeric")
    if (any(comorbidity_time >= 0)) warning("comorbidity_time contains events on or after the index event")

    co <- get_comorbidities(data = dat[between(from_index,
                                         lower = comorbidity_time[1],
                                         upper = comorbidity_time[2]),],
                      type = comorbidity)
    dat <- data.table::merge.data.table(dat, co,
                                        by = "MEDREC_KEY", all.x=TRUE)
  }

  # code days from index for all things of interest (in cols)
  if (!missing(ignore_from_index)) cols <- cols[!cols %in% ignore_from_index]

  dat[, (paste0(cols, "_from_index")) :=  lapply(.SD, \(x) paste0(from_index[which(x)], collapse = "|")),
      by="MEDREC_KEY", .SDcols = cols]

  # information on range of encounters
  dat[, `:=`(min_encounter = min(from_index),
          max_encounter = max(from_index),
          n_encounter = .N), by=MEDREC_KEY]
  if (!return_all){
    dat <- subset(dat, from_index == 0)
    if (any(duplicated(dat$MEDREC_KEY))) warning(sum(duplicated(dat$MEDREC_KEY)), " duplicated IDs in output")
  }
  nend <- length(unique(dat$MEDREC_KEY))
  if(norig != nend) warning("end number of IDs is different to start (start=", norig, ", end=", nend, ")")
  return(dat)
}


# x = mk_data(files = files, index_event = "SCH",
#             combine = alist("SCH" = cpt_SCH | cpt_SCH_lap))


#' Extract information on timing of events from pasted-together columns
#'
#' @description
#' mk_data pastes the number of units time between the 'index' event
#' and a given event (ICD/CPT/etc) to a single cell separated by "|" .
#' e.g. "-45|0|90" would be an event that occured 45 before, concurrent,
#' and 90 days after index. This allows the contruction of columns from these cells.
#'
#' @param data data from mk_data
#' @param times a list of lists. See details
#' @param split character that separates times in pasted cells (default = "\\|")
#' @param from_suffix suffix of columns containing relative times pasted. default = "from_index"
#'
#' @details
#' times should be a list of lists such that each element is an event of interest
#' and the sublist contains the times of interest. Using Inf or -Inf in the range of
#' times will return the closests event after or before the other end of the range (which must be non-Inf).
#' For example times = list(eventA = list(c(0,90), c(-Inf, -1)))
#' would result in a column containing T/F for if eventA occurs in the interval [0, 90]
#' and a column containing the closest event to the day before index(inclusive). Note
#' eventA_from_index (the column containing the pasted times) must exist in data.
#'
#' @returns modified data w/ additional columns
#' @export
addtimecols <- function(data, times, split = "\\|", from_suffix="_from_index"){

  between2 <- function(x, lower, upper, splitchar){

    if (lower >= upper) stop("lower >= upper")
    x <- strsplit(x, split = as.character(splitchar))

    if (is.infinite(lower) | is.infinite(upper)){
      # if one bound is infinite, return number of days from index (closest)
      if (is.infinite(lower) & is.infinite(upper)) stop("only one of lower/upper should be Inf")
      f <- ifelse(is.infinite(lower), max, min) # closest to 0

      sapply(x, function(xx){
        xx <- na.omit(as.numeric(xx))
        xx <- xx[between(xx, lower = lower, upper = upper)]
        if (length(xx) > 0){
          f(xx)
        }
        else {
          NA_integer_
        }
      })
    } else{
      sapply(x, function(xx){
        xx <- na.omit(as.numeric(xx))
        any(between(xx, lower = lower, upper = upper))
      })
    }
  }

  vnames <- function(ti, v){
    lab <- function(x){
      if (x == 0){
        "index"
      } else if (x < 0){
        paste0(abs(x), "_pre")
      } else if (x > 0){
        paste0(x, "_post")
      }
    }
    if (is.infinite(ti[1])){
      suf <- paste0("days_before_", lab(ti[2]))
    } else if (is.infinite(ti[2])){
      suf <- paste0("days_after_", lab(ti[1]))
    } else {
      suf <- paste0(sapply(ti, lab), collapse = "_to_")
    }
    paste(v, suf, sep ="_")
  }

  for (v in names(times)){
    pcol <- paste0(v, from_suffix) # col with pasted times
    if (!pcol %in% colnames(dat)) stop("couldn't find ", pcol)
    data[, (sapply(times[[v]], vnames, v=v)) := lapply(times[[v]], \(x){
      between2(data[[pcol]], lower = x[1], upper = x[2], splitchar=split)
    })]
  }
  return(data)
}
