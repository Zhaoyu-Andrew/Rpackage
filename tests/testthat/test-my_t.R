library(stringr)
sample <- c(1,2,3,4,5)

test_that("my_t.test less properly", {
  expect_is(my_t.test(sample, "less", 1), t.test(sample, mu = 1, alternative = "less"))
})

test_that("my_t.test greater properly", {
  expect_is(my_t.test(sample, "greater", 1), t.test(sample, mu = 1, alternative = "greater"))
})

test_that("my_t.test two.sided properly", {
  expect_is(my_t.test(sample, "two.sided", 1), t.test(sample, mu = 1, alternative = "two.sided"))
})
