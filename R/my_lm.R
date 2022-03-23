#' Linear Model Function
#'
#' This function performs linear regression on model
#'
#' @param formula x and y for the model
#' @param dat the data set that contains x and y
#'
#' @return A table with coefficients of the model
#'
#' @import stats
#'
#' @examples
#' my_lm(mpg ~ hp, mtcars)
#' my_lm(mpg ~ drat + wt, mtcars)
#'
#' @export

# function: my_lm, performs a linear regression model on input data with given formula that returns coefficient table similar to that from summary(lm())
# input: formula and a dataset
# output: coefficient table from lm()
my_lm <- function(formula, dat) {
  # extract X and Y matrices from input dataset
  X = model.matrix(formula, dat)
  Y = model.frame(formula, dat)
  Y = model.response(Y)
  # calculate linear regression coefficients
  p_hat = solve(t(X) %*% X) %*% t(X) %*% Y
  # calculate degree of freedom
  df = nrow(dat) - ncol(X)
  # calculate sigma squared
  sigma_2 = sum((as.matrix(Y) - X %*% p_hat)^2 / df)
  # calculate the standard error
  se = diag(sqrt(sigma_2 * solve(t(X) %*% X)))
  # calculate the t_values
  t = p_hat / se
  # calculate p_values
  p = 0
  for (i in length(t)) {
    tt = t[i]
    p_val = pt(tt, df, lower.tail = F)
    p_vall = pt(tt, df, lower.tail = T)
    p[i] = 2 * min(p_val, p_vall)
  }
  # return a table of all the calculations above
  result = data.frame(Estimate = p_hat, Standard_error = se, t_values = t, p_values = p)
  return (result)
}
