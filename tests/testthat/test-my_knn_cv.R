@import dataset

test_that("my_knn works", {
  expect_is(my_knn_cv(my_knn_cv(iris, iris$Species, 1, 5)), "list")
})

