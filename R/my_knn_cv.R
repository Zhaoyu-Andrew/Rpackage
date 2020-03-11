#' k-Nearest Neighbors Cross-Validation function
#'
#' This function performs a k-nearest neighbors cross-validation on the given
#'   data.
#' @param train An input data frame.
#' @param cl A true class value of your training data.
#' @param k_nn An integer representing the number of neighbors.
#' @param k_cv An integer representing the number of folds.
#' @keywords prediction
#'
#' @return A list containing a vector set of the predicted class for all the
#'   observatoin and a number representing the cross-validation
#'   misclassification error.
#'
#' @examples
#' my_knn_cv(iris, iris$Species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  measure <- train[,-5]
  set.seed(302)
  n <- nrow(data)
  inds <- sample(rep(1:k_cv, length = n))
  data_combine <- data.frame("x" = measure, "y" = cl, "split" = inds)
  error <- matrix(NA, k_cv, 1)
  for(i in 1:k_cv) {
    data_train <- data_combine %>% filter(split != i)
    data_train_knn <- data_train %>%
      select(x.Sepal.Length, x.Sepal.Width, x.Petal.Length, x.Petal.Width)
    data_test_knn <- data_test %>%
      select(x.Sepal.Length, x.Sepal.Width, x.Petal.Length, x.Petal.Width)

    cl_train_column <- data_train[ ,5]
    cl_test_column <- data_test[ ,5]

    knn_predict <- knn(train = data_train_knn, cl = cl_train_column,
                       test = data_test_knn, k = k_nn)

    tab <- table(knn_predict, cl_test_column)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    false_rate <- (100 - accuracy(tab)) / 100
    error[i, 1] <- false_rate
  }
  knn_full <- knn(train = measure, cl = cl, test = measure, k = k_nn)
  cv_error <- colMeans(error)
  mylist <- list("class" = knn_full,
                 "cv_error" = cv_error)
  return(mylist)
}
