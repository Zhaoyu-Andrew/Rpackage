#' @import class magrittr randomForest

dataset <-  my_gapminder
test_that("my_rf works", {
  expect_is(my_rf_cv(5), "numeric")
})
