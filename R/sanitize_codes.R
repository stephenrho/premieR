#' Modify codes to allow for inclusive matching
#'
#' @param code_list list of ICD or CPT codes
#' @param x the character used to allow for inclusive matching (default = "x").
#' For example, to match codes "99.[0:10]" (and all sub-codes, like 99.01), one would
#' put "99.x" in their code list. This would be translated to "99." for inclusive matching.
#' Codes not ending in \code{char} have $ put at the end to stop matching.
#' @return modified version of code_list to allow for inclusive matching where necessary
#' @export
sanitize_codes = function(code_list, char="x"){
  if (is.null(code_list)){
    NULL
  } else{
    if (length(code_list) > 1){
      out = lapply(code_list, function(i) {
        ifelse(grepl(char, x = i), # lower case x signals inclusive matching
               gsub(char, "", i, ignore.case = F), # leave the end blank for inclusive matching
               paste0(i, "$") # add $ to end matching
        )
      })
    } else{
      out = lapply(code_list, function(i) {
        ifelse(grepl(char, x = i), # lower case x signals inclusive matching
               gsub(char, "", i, ignore.case = F), # leave the end blank for inclusive matching
               paste0(i, "$") # add $ to end matching
        )
      })
    }
    stopifnot(all(names(out) == names(code_list)))
    return(out)
  }
}
