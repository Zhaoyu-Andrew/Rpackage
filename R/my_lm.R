#' Linear model function
#'
#' This function fits a linear model in r and give a summary table.
#' @param formula A formula class object.
#' @param data Input data frame.
#' @keywords prediction
#'
#' @return A table which is similar to the summary() given by the lm() function.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
#' @import class magrittr stats
my_lm <- function(formula, data) {
  matrix_x <- model.matrix(formula, data)
  matrix_y <- model.response(model.frame(formula, data))

  x_T <- t(matrix_x)
  beta <- solve(x_T %*% matrix_x) %*% x_T %*% matrix_y
  n_row <- nrow(matrix_x)
  n_col <- ncol(matrix_x)
  df <-n_row - n_col
  variance <- sum((matrix_y - matrix_x %*% beta)^2 / df)
  standard_error <- sqrt(diag(variance * solve(x_T %*% matrix_x)))
  test_stat_new <- (beta - 0) / standard_error
  Pr <- 2 * pt(abs(test_stat_new), df, lower.tail = FALSE)

  table_matrix <- as.table(cbind(beta, standard_error, test_stat_new, Pr))

  colnames(table_matrix) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(table_matrix)
}
