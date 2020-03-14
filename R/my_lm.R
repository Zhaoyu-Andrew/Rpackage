#' Linear model function
#'
#' This function fits a linear model in r and give a summary table.
#' @param formula A formula class object specifying the response variable and explanatory variables by using columns of \code{data}.
#' @param data Input data frame.
#' @keywords prediction
#'
#' @return A table which is similar to the summary() given by the lm() function.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
#' @import magrittr
my_lm <- function(formula, data) {
  # extract the model matrix out of the given vector set
  matrix_x <- model.matrix(formula, data)
  # extract the model response
  matrix_y <- model.response(model.frame(formula, data))

  # get the transpose matrix of the model matrix
  x_T <- t(matrix_x)
  # solve for the linear coefficients beta
  beta <- solve(x_T %*% matrix_x) %*% x_T %*% matrix_y
  # get the number of rows and column of model matrix
  n_row <- nrow(matrix_x)
  n_col <- ncol(matrix_x)
  # calculate degree of freedom
  df <-n_row - n_col
  # calculate the variance and the standard error
  variance <- sum((matrix_y - matrix_x %*% beta)^2 / df)
  standard_error <- sqrt(diag(variance * solve(x_T %*% matrix_x)))
  # calcualte the test statistic for beta
  test_stat_new <- (beta - 0) / standard_error
  # get the p-value from the two sided t-test
  Pr <- 2 * pt(abs(test_stat_new), df, lower.tail = FALSE)

  # generate a table for the linear model components
  table_matrix <- as.table(cbind(beta, standard_error, test_stat_new, Pr))

  # give column names to the table
  colnames(table_matrix) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(table_matrix)
}
