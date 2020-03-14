test_that("my_t.test less properly", {
  expect_is(my_t.test(my_gapminder[[4]], "less", 60), "list")
})

test_that("my_t.test greater properly", {
  expect_is(my_t.test(my_gapminder[[4]], "greater", 60), "list")
})

test_that("my_t.test two.sided properly", {
  expect_is(my_t.test(my_gapminder[[4]], "two.sided", 60), "list")
})

test_that("my_t.test two.sided properly", {
  expect_is(my_t.test(my_gapminder[[6]], "two.sided", 60), "list")
})

test_that("String input throws error", {
  expect_error(my_t.test(my_gapminder[[4]], 1, 1))
})

