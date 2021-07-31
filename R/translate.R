#' Translate English to Chinese or the verse using trans function from translate-shell translator, https://github.com/soimort/translate-shell
#'
#' @param input The sentences to be translated
#'
#' @return The translated English or Chinese sentences
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' translate("中文")
#' translate("English")
translate <- function(input) {
  stopifnot(length(input) < 6)
  pattern <- grepl(pattern = "[\u4e00-\u9fa5]", input) # find the Chinese character
  if (pattern[1]) {
    quary <- paste0("trans -b :en ", "\"", input, "\"")
  } else {
    quary <- paste0("trans -b :zh ", "\"", input, "\"")
  }
  trans.out <- vector(mode = "character", length = length(input))
  for (i in seq_along(quary)) {
    trans.out[i] <- quary[i]  |>
      system(intern = TRUE)
  }
  return(trans.out)
}
