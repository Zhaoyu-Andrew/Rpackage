data(mtcars)

test_that("my_lm works", {
  expect_is(my_lm(mpg ~ hp + wt, data = mtcars), summary(lm(mpg~hp +wt, data = mtcars)))
})
