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
  if (class(input) == "data.frame") input <- input[[1]]
  n <- length(input) # the input vector length
  num <- nchar(input) # the number of characters in the input vector
  n.char <- cumsum(num) # accumulated sum of the number of characters
  stopifnot(n.char[n] < 5000)
  pattern <- grepl(pattern = "[\u4e00-\u9fa5]", input[1]) # find the Chinese character
  if (n > 1) input <- do.call("paste", list(input, collapse = " a_._a "))
  if (pattern) {
    quary <- paste0("trans -b :en ", "\"", input, "\"")
  } else {
    quary <- paste0("trans -b :zh ", "\"", input, "\"")
  }
  trans.out <- system(quary, intern = TRUE)
  trans.out <- strsplit(trans.out, split = "a_._a")[[1]]
  return(trans.out)
}
