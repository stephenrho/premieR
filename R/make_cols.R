#' Create columns signifying a code match
#'
#' @param codes list of codes of interest (passed through \code{sanitize_codes})
#' @param match_list codes associated with hospital encounters in list of vectors form
#' @param codes_matched vector of codes associated with hospital encounters separated by " | ".
#' This is needed if \code{match_list} is not provided, in which case \code{match_list = strsplit(codes_matched, split = " \\| ")}
#' @return a data.frame with \code{length(codes)} columns and \code{length(match_list)} rows with a
#' 0 if no codes in \code{codes} were identified for an encounter and a 1 if there was at least one match.
#' @export
make_cols = function(codes, match_list=NULL, codes_matched=NULL){
  # already separated codes in list form
  # codes matched separated by " | "
  # codes = named list
  # returns matrix with columns = names(codes) and rows 1:length(match_list)
  # with a zero or one (match/no match)
  if (is.null(match_list) & is.null(codes_matched)){
    stop("match_list *or* codes_matched must be provided")
  }

  if (is.null(match_list)){
    match_list = strsplit(codes_matched, split = " \\| ")
  }

  code_names = names(codes)

  X = matrix(0, nrow = length(match_list), ncol = length(code_names))

  colnames(X) = code_names

  for (n in code_names){
    X[,n] = sapply(match_list, FUN = function(z) {
        # any(grepl(paste0("^", codes[[n]],"$", collapse = "|"), z))
        any(grepl(paste0("^", codes[[n]], collapse = "|"), z))
      }
      )
  }

  return(as.data.frame(X))
}
