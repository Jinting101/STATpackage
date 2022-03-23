#' Student's t_test function
#'
#' This function performs statistical t.test
#'
#' @param x Numeric vector to be performed t.test
#' @param alternative Alternative hypothesis (only accepting "less", "greater", or "two.sided")
#' @param mu null hypothesis value of mean
#'
#' @return A list with test statistics, degree of freedom of \code{x} and the alternative
#'
#'@import stats
#'
#' @examples
#' my_t.test(rnorm(10, 1, 1), "greater", 0)
#' my_t.test(rnorm(20), "less", 0.5)
#'
#' @export

# function: my_t.test that performs the same as t.test
# input; x: numeric vector, alternative: alternative hypothesis, mu: null hypothesis value of mean
# output: t: test statistic, df: degree of freedom, alternative: from input, p_val: the p_value
my_t.test <- function(x, alternative, mu) {
  # throw an error if alternative isn't one of "two.sided", "less", "greater"
  if (!alternative %in% c("two.sided", "less", "greater")) {
    return("Input for alternative not valid")
  }
  # calculate the test statistics
  t = (mean(x) - mu) / sd(x) * sqrt(length(x))
  # calculate the degree if freedom
  df = length(x) - 1
  p_val = 0
  # apply a right tail t_test if alternative is greater
  if (alternative == "greater") {
    p_val = pt(t, df, lower.tail = F)
    # apply a left tail t_test if alternative is less
  } else if (alternative == "less") {
    p_val = pt(t, df, lower.tail = T)
    # apply a two sided t_test if alternative is two.sided
  } else {
    p_val = pt(t, df, lower.tail = F)
    p_vall = pt(t, df, lower.tail = T)
    p_val = 2 * min(p_val, p_vall)
  }
  return(list("t" = t, "df" = df, "alternative" = alternative, "p_value" = p_val))
}
