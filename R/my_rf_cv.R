#' Random Forest Cross-Validation function
#'
#' This function performs a random forest cross-validation on the given data.
#' @param k A number of fold.
#' @keywords prediction
#'
#' @return A number representing the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
#' @import class magrittr randomForest
my_rf_cv <- function(k) {
  # get the total number of the dataset
  my_gapminder <- my_gapminder
  n <- nrow(my_gapminder)
  inds <- sample(rep(1:k, length = n))
  # randomly assigns observations to folds 1,…,k
  my_gapminder[, "split"] <- inds
  pred_mat2 <- matrix(NA, n, 2)
  i <- 1
  for(i in 1:k) {
    data_train <- my_gapminder %>%
      dplyr::filter(split != i)
    data_test <- my_gapminder %>%
      dplyr::filter(split == i)
    # create the random forest model
    MODEL <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 30)

    n <- nrow(my_gapminder)
    inds <- sample(rep(1:k, length = n))
    # randomly assigns observations to folds 1,…,k
    my_gapminder[, "split"] <- inds

    pred_mat2[inds == i, 1] = predict(MODEL, data_test[, -1])
    continent_matrix <- as.matrix(data_test)
    pred_mat2[inds == i, 2] = continent_matrix[, 2]
  }

  lifeExp_col <- pred_mat2[, 1]
  lifeExp_num <- as.numeric(lifeExp_col)
  # calculate the average CV estimate
  cv_mean <- mean(lifeExp_num)
  # calculate the standard deviation of the CV estimates
  cv_sd <- sd(lifeExp_num)
  # convert vector to matrix
  lifeExp_num_matrix <- as.matrix(lifeExp_num)
  # calculate the MSE across all k value
  MSE <- colMeans((lifeExp_num_matrix - my_gapminder$lifeExp)^2)

  return(MSE)
}
