#' Literature search using litsence API from NCBI
#'
#' @param keywords keyword
#'
#' @return a data frame
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' litsence("gut microbiota")
litsence <- function(keywords){
  keyword <- keywords
  keyword <- gsub("  ", " ", keyword, perl = TRUE)
  #
  options (warn = -1)
  docs <- tm::Corpus(tm::VectorSource(keyword))
  docs <- tm::tm_map(docs, tm::content_transformer(tolower))
  docs <- tm::tm_map(docs, stripWhitespace)
  docs <- tm::tm_map(docs, removeNumbers)
  docs <- tm::tm_map(docs, removeWords, c("one", "two", "three", "four", "five",
                                          "six", "seven", "eight", "nine", "ten",
                                          "eleven", "twelve"))
  docs <- tm::tm_map(docs, removeWords, stopwords("english"))
  keyword <- docs$content
  #
  keyword <- strsplit(keyword, split = " ")[[1]]
  keyword <- gsub(",", "", keyword)
  keyword <- gsub("\\.", "", keyword, perl = TRUE)
  keyword <- gsub("\\?", "", keyword, perl = TRUE)
  keyword <- gsub("\\;", "", keyword, perl = TRUE)
  keyword <- keyword[nchar(keyword) > 2]
  keyword <- unique(keyword)
  query <- paste(keyword, collapse = "+")
  print(paste("-------- query:", query, "--------", sep = " "))
  url <- paste0("https://www.ncbi.nlm.nih.gov/research/litsense-api/api/?format=api&query=",
                query, "&rerank=true")
  con <- curl::curl(url = url)
  print("-------- Downloading data --------------------------")
  temp <- readLines(con)
  temp2 <- temp[grep("&quot;text&quot;:", temp)]
  temp3 <- temp[grep("&quot;section&quot;:", temp)]
  temp2 <- gsub("        &quot;text&quot;: &quot;", "", temp2)
  temp3 <- gsub("        &quot;section&quot;:", "", temp3)
  temp3 <- gsub(" ", "", temp3)
  temp3 <- gsub("&quot;", "", temp3)
  temp3 <- gsub(",", "", temp3)
  temp3 <- tolower(temp3)
  temp3 <- gsub("abstract", "abs", temp3)
  temp3 <- gsub("discuss", "dis", temp3)
  temp3 <- gsub("results", "res", temp3)
  temp3 <- gsub("methods", "meth", temp3)
  temp3 <- gsub("concl", "ctl", temp3)
  temp2 <- gsub("&quot;,", "", temp2)
  temp2 <- gsub("\\&quot;", "", temp2)
  result <- data.frame(temp3, temp2)
  colnames(result) <- NULL
  options (warn = 1)
  print("Bingo!")
  return(result)
}


