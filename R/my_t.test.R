#' my t.test for data
#'
#' @param x Numeric vector input
#' @param alternative String input, should be \code{"two.sided"}, \code{"less"} or \code{"greater"}.
#' @param mu Numeric input, the mean of the null hypothesis value. default to \code{60}.
#'
#' @keywords inference
#'
#' @return A list includes alternative and p_value.
#'
#' @examples
#' my_t.test(x)
#' my_t.test(x, alternative="less")
#' my_t.test(x, alternative="greater", mu=60)
#'
#' @export
my_t.test <- function(x, alternative="two.sided", mu=60){
  t1 <- t.test(x, mu=60, alternative = alternative)
  p_value = t1$p.value
  df = t1$df
  result <- list("alternative" = alternative, "p_value" = p_value)
  return(result)
}
