#' Find encounters within a particular time-frame
#'
#' @param data from \code{read_premier}
#' @param cols columns of interest
#' @param month_col column containing number of months from event of interest
#' @param num_months a vector of length 2 with lower and upper bounds (inclusive)
#' @param by what to aggregate over (default = "MEDREC_KEY")
#' @param fun function to apply (default = function(x) any(x == 1))
#' @param add_columns_to_data add the new columns to \code{data}?
#' @param exclude_col column name to exclude when looking (used for excluding the index encounter)
#' @return Either the original data with new columns (from \code{cols}) identifying those with a match within
#' the time frame specified by \code{num_months} (otherwise just those columns if \code{add_columns_to_data = F})
#' @export
get_encounters_within_mrange = function(data, cols, month_col, num_months=c(0,3), by=c("MEDREC_KEY"), fun=function(x) any(x == 1), add_columns_to_data=T, exclude_col=NULL){
  # find encounters within num_months = c(min, max)

  stopifnot(by %in% c("MEDREC_KEY", "PAT_KEY"))

  tmp = subset(data, get(month_col) >= num_months[1] & get(month_col) <= num_months[2])

  if (!is.null(exclude_col)){
    tmp = tmp[!tmp[[exclude_col]],]
  }

  encs = tmp[, lapply(.SD, fun), .SD = cols, by = by ]

  encs$match = apply(encs[, ..cols], 1, FUN = function(x) sum(x) > 0)

  # encs = subset(encs, match)

  suff = paste0("_", abs(num_months[1]), "m_", ifelse( sign(num_months[1]) == -1, "prior", "post"),
                "_to_", abs(num_months[2]), "m_", ifelse( sign(num_months[2]) == -1, "prior", "post") )

  setnames(encs, old = cols, new = paste0(cols, suff))

  if(add_columns_to_data){
    data = merge(data, encs[, !"match"], by=by, all=T )

    return(data)
  } else{
    # encs = subset(encs, match)
    return( encs[, !"match"] )
  }
}

