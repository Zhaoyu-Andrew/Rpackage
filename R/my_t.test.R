#' T-test function
#'
#' This function performs a sample t-test based on the given input.
#' @param x A numeric set of data.
#' @param alternative A character string specifying the alternative.
#' @param mu A number indicating the null hypothesis value of the mean.
#' @keywords prediction
#'
#' @return A list containing the test statistic, degree of freedom, a string
#'   represent the type of t-test, and the p-value of the t-test.
#'
#' @examples
#' my_t.test(my_gapminder[[4]], "less", 1)
#' my_t.test(my_gapminder[[4]], "greater", 1)
#' my_t.test(my_gapminder[[4]], "two.sided", 1)
#'
#' @export
#' @import class magrittr stats
my_t.test <- function(x, alternative, mu) {
  if(alternative != "two.sided" & alternative != "less" &
     alternative != "greater") {
    stop("The second function input must be \"two.sided\" or \"less\" or
         \"greater\"")
  }

  x_mean <- mean(x)
  x_sd <- sd(x)
  x_length <- length(x)
  test_stat <- (x_mean - mu) / (x_sd / sqrt(x_length))
  df <- x_length - 1

  if(alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if(alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if(alternative == "two.sided") {
    if(test_stat < 0) {
      p_val <- 2 * pt(test_stat, df, lower.tail = TRUE)
    } else {
      p_val <- 2 * pt(test_stat, df, lower.tail = FALSE)
    }
  }
  my_list <- list("test_stat" = test_stat,
                  "df" = df,
                  "alternative" = alternative,
                  "p_value" = p_val)
  return(my_list)
}
