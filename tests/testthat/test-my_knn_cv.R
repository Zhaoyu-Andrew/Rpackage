test_that("my_knn works", {
  expect_is(my_knn_cv(my_gapminder, my_gapminder[[2]], 1, 5), "list")
})

