test_that("my_lm works", {
  expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder), "table")
})
