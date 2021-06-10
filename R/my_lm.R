#' Linear Model for data
#'
#' @param formula model formula
#' @param da data set
#'
#' @keywords prediction
#'
#' @return A list includes linear model and model summary.
#'
#' @examples
#' data(cars)
#' my_lm(speed~dist, data=cars)
#'
#' @export
my_lm <- function(formula, da){
  lm1 <- lm(formula, data=da)
  sm1 <- summary(lm1)
  #library(ggplot2)
  #ggplot(data=tmpDa, aes(x=lifeExp, y=fitted))+geom_point()

  result = list("lm"=lm1, "summary"=sm1)
  return(result)
}
