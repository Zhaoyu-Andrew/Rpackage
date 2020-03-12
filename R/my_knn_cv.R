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
#' @import class magrittr dplyr
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # select only the numeric column
  measure <- train %>%
    select(lifeExp, gdpPercap)
  set.seed(302)
  # get the total number of the data
  n <- nrow(train)
  # Randomly splicl at data into k parts
  inds <- sample(rep(1:k_cv, length = n))
  data_combine <- data.frame("x" = measure, "y" = cl, "split" = inds)
  # create empty matrix for storing misclassification rate
  error <- matrix(NA, k_cv, 1)
  for(i in 1:k_cv) {
    data_train <- data_combine %>% filter(split != i)
    data_test <- data_combine %>% filter(split == i)
    # select only the numeric column
    data_train_knn <- data_train %>%
      select(x.lifeExp, x.gdpPercap)
    data_test_knn <- data_test %>%
      select(x.lifeExp, x.gdpPercap)
    # extract 5th column of train data set and use it for knn later
    cl_train_column <- data_train[ ,3]
    # extract 5th column of test data set to measure accuracy
    cl_test_column <- data_test[ ,3]
    # Train our models
    knn_predict <- knn(train = data_train_knn, cl = cl_train_column,
                       test = data_test_knn, k = k_nn)
    # create confusion matrix
    tab <- table(knn_predict, cl_test_column)
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    # calculate the misclassification rate
    false_rate <- (100 - accuracy(tab)) / 100
    # store the misclassification rate in the empty matrix
    error[i, 1] <- false_rate
  }
  # build model with full data
  knn_full <- knn(train = measure, cl = cl, test = measure, k = k_nn)
  # calculate the average misclassification rate
  cv_error <- colMeans(error)
  # create a list to the model as well as the average misclassification rate
  mylist <- list("class" = knn_full,
                 "cv_error" = cv_error)
  mylist
  return(mylist)
}
