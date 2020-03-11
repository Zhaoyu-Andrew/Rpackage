library(tidyverse)
library(ggplot2)
library(class)
library(randomForest)
data(iris)
data <- iris

test_that("my_knn works", {
  expect_is(my_knn_cv(iris, iris$Species, 1, 5), "list")
})
