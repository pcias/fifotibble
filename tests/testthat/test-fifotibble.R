library(tibble)

test_that("fifotibble works on basic data", {
  q <- c(10,-5,20,-15)
  p <- c(10,12,12,10)
  e <- fifotibble(q[1:3], p[1:3])
  expectede <- new_tibble(list(qty=c(10,20), price=c(10,12), openstock=c(5,20), value=c(50,240)), nrow=2)

  expect_identical(e, expectede)
})

test_that("basic gainOnSell works", {
    q <- c(10,-5,20,-15)
    p <- c(10,12,12,10)
    e <- fifotibble(q[1:3], p[1:3])
    expect_equal(gainOnSell(e,30,11.7), 2.5)
})

test_that("gains calculator works", {
  q <- c(10,-5,20,-15)
  p <- c(10,12,12,10)
  e <- gains(q,p)

  expectede <- c(0,10,0,-20)
  expect_equal(e, expectede)
})
