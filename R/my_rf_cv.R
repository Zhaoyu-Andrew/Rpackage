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
my_rf_cv <- function(k) {
  # get the total number of the dataset
  n <- nrow(dataset)
  inds <- sample(rep(1:k, length = n))
  # randomly assigns observations to folds 1,…,k
  dataset[, "split"] <- inds
  pred_mat2 <- matrix(NA, n, 1)
  for(i in 1:k) {
    data_train <- dataset %>%
      filter(split != i)
    data_test <- dataset %>%
      filter(split == i)
    # create the random forest model
    MODEL <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 100)

    pred_mat2[inds == i, 1] = predict(MODEL, data_test[, -1])
  }
  # calculate the MSE across all k value
  MSE <- colMeans((pred_mat2 - dataset$lifeExp)^2)
  return(MSE)
}
