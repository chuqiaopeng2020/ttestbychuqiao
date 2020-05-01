#' Get the boxplot for the dataframe using simple function
#'
#' @param m
#'
#' @return boxplot
#' @export
#'
#' @examples
#' plot(x)
plot = function(m){
  library(ggplot2)
  ggplot(m, aes(x = cat, y = sample, fill = cat )) + geom_boxplot()
}
