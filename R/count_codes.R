#' Count the number of times particular codes are matched
#'
#' @param code_list list of ICD/CPT/charge codes of interest
#' @param codes vector of codes from n encounters
#' @param sep character separating individual codes in \code{codes} (default = " \\| ")
#' @return data.frame containing counts of individual codes (\code{group} = name in code_list)
#' @export
count_codes = function(codes, code_list=NULL, sep=" \\| "){

  codev = na.omit(unlist(strsplit(codes, sep)))

  if (!is.null(code_list)){
    code_df = stack(code_list)
  } else{
    code_df = data.frame(values = unique(codev),
               ind = NA)
  }

  n = lapply(sanitize_codes(code_df$values), function(x){
    sum(grepl(pattern = x, x = codev))
  })

  code_df$n = unlist(n)

  colnames(code_df) = c("code", "group", "n")

  return(code_df)
}

# x = count_codes(codes = codes, code_list = code_list)
#
# ggplot(x, aes(x = n, y = code)) +
#   geom_bar(stat="identity") +
#   facet_grid(group~., scales = "free", space="free_y") #+
#   #theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
#   #ggrepel::geom_text_repel(aes(label=code), size=2, nudge_x = .1)
