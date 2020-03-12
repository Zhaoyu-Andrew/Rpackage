test_that("my_t.test less properly", {
  expect_is(my_t.test(my_gapminder[[4]], "less", 1), "list")
})

test_that("my_t.test greater properly", {
  expect_is(my_t.test(my_gapminder[[4]], "greater", 1), "list")
})

test_that("my_t.test two.sided properly", {
  expect_is(my_t.test(my_gapminder[[4]], "two.sided", 1), "list")
})
