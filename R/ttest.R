#' How to carry out different t-test with a simply function
#'Takes a list, return the dataframe of two samples with attributes of confident interval and test type
#'
#' @param a
#' @param b
#' @param alpha
#' @param paired
#'
#' @return A dataframe consists of two samples and attributes of the confident interval of t -test
#' @export
#'
#' @examples
#'set.seed(32)
#'x=rnorm(30,mean=10,sd=15)
#'set.seed(35)
#'y=rnorm(30,mean=8,sd=15)
#'myttest(x,y,alpha = 0.05, paired = TRUE)
#'myttest(x,y,alpha = 0.05, paired = FALSE)
#'
myttest<- function(a, b, alpha , paired){
  v= var.test(a,b)
  if (v$p.value > 0.05){
    if (paired == TRUE){
      tp = t.test (a, b, mu = 0, paired = TRUE, var.equal = TRUE, conf.level = 1-alpha)
      ci = tp$conf.int
      test_type = "T-test"
      pop=rep(c("x", "y"), c(length(a),length(b)))
      df = data.frame(cat = pop, sample = c(a,b))
      attr(df, "ci")<- ci
      attr(df, "test_type") <- test_type
      df
    }
    else {
      tp = t.test (a, b, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)
      ci = tp$conf.int
      test_type = "T-test"
      pop=rep(c("x", "y"), c(length(a),length(b)))
      df = data.frame(cat = pop, sample = c(a,b))
      attr(df, "ci")<- ci
      attr(df, "test_type") <- test_type
      df
    }
  }
    else {
      tp = t.test (a,b, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)
      ci = tp$conf.int
      test_type = "T-test"
      pop=rep(c("x", "y"), c(length(a),length(b)))
      df = data.frame(cat = pop, sample = c(a,b))
      attr(df, "ci")<- ci
      attr(df, "test_type") <- test_type
      df
    }
}
