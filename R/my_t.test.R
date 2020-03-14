#' T-test function
#'
#' This function performs a sample t-test based on the given input.
#' @param x A numeric set of data.
#' @param alternative A character string specifying the t-test type for \code{x}.
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
#' @import magrittr
my_t.test <- function(x, alternative, mu) {
  # send error message when the second input is not equal to "two.sided" or
  # "less" or "greater"
  if(alternative != "two.sided" & alternative != "less" &
     alternative != "greater") {
    stop("The second function input must be \"two.sided\" or \"less\" or
         \"greater\"")
  }

  # calculate the mean of the the matrix
  x_mean <- mean(x)
  # calculate the standard deviation of the matrix
  x_sd <- sd(x)
  # calculat the sample size
  x_length <- length(x)
  # get the test statistic of one sample t-test
  test_stat <- (x_mean - mu) / (x_sd / sqrt(x_length))
  # get degree of freedom
  df <- x_length - 1

  # calculate the p-value based on the input "alternative"
  if(alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if(alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if(alternative == "two.sided") {
    # calculate p-value based on whether or not the test statistic is bigger
    # than
    if(test_stat < 0) {
      p_val <- 2 * pt(test_stat, df, lower.tail = TRUE)
    } else {
      p_val <- 2 * pt(test_stat, df, lower.tail = FALSE)
    }
  }
  # create a list for the 4 main components of a t-test
  my_list <- list("test_stat" = test_stat,
                  "df" = df,
                  "alternative" = alternative,
                  "p_value" = p_val)
  return(my_list)
}
