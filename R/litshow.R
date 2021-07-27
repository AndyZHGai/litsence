#' Show the result of litsence
#'
#' @param litsence the result of litsence function
#' @param n display number
#'
#' @return a plain text
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' res <- litsence(keywords = "gut microbiota")
#' litshow(res, 1:30)
litshow <- function(litsence, n = 1:30, size = 15){
  res <- data.frame(row.names(litsence), litsence)
  res <- res[n, ]
  colnames(res) <- NULL
  m <- nrow(res)
  m <- 1:m
  res  |>  kableExtra::kable('html')  |>
    kableExtra::kable_styling(font_size = size)  |>
    kableExtra::row_spec(m[m %% 2 != 0], bold = T, color = "white",
             background = "#D7261E80")  |>
    kableExtra::row_spec(m[m %% 2 == 0], bold = T, color = "black",
             background = "#00bbff30")  |>
    kableExtra::column_spec(1, bold = T, color = "black", italic = T,
                background = "#00bbff30", underline = T,
                border_right = T, border_left = T)
}




