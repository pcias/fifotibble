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

test_that("gains works on better sample data", {

    q<-c(10, 6, 15, 6, 40, 25, 6, -17, -1, 20, 2, 23, 2, 75, 20, 50,
         20, 10, 80, 20, 5, 10, -27, -60, 10, 4, 7, 1, 2, 8, 5, 2, 1,
         11, 3, -50, 5, 5, 2, 4, 4, 5, 5, 5, -29, 8, 5, 7, 3, 1, 1, -100,
         -100, -50, -18, -7, -100, 100, 100, -20, -30, -50, -50, -50,
         30, 10, 20, -1, 20, -29, -20, -10, -20, 20, -20)

    p<-c(144.13, 144.14, 143.52, 143.5, 147.35, 147.1, 153.93, 154.43,
         154.44, 164.08, 164.5, 163.32, 163.31, 163.31, 154.44, 154.37,
         153.9, 153.9, 151.21, 152.59, 151.31, 147.48, 146.55, 152.16,
         151.52, 155.28, 156.12, 156.11, 156.07, 155.22, 154.9, 155.32,
         161.12, 161.12, 161.05, 167.02, 157.59, 158.88, 146.34, 135.17,
         136.83, 138.98, 140.69, 140.51, 149, 155.99, 159.78, 177.38,
         172.97, 179.26, 183.71, 194.94, 194.94, 194.94, 194.95, 194.96,
         194.95, 186.09, 186.18, 187.88, 187.92, 187.92, 187.9, 187.93,
         183.44, 184.2, 185.13, 189.53, 189.64, 211.61, 210.5, 208.65,
         208.28, 178.48, 167.8201)

    e<- gains(q,p)

    expectede <-c(0, 0, 0, 0, 0, 0, 0, 175.65, 10.92, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 51.29, 275.110000000001, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 195.63, 0, 0, 0, 0, 0, 0, 0, 0, -414.99, 0, 0, 0,
                  0, 0, 0, 3644.36, 4241.74, 2158.5, 822.35, 289.04, 3956.13, 0,
                  0, 35.7999999999997, 54.8999999999996, 91.5, 86, 87.5, 0, 0,
                  0, 6.09, 0, 816.93, 516.7, 235.2, 372.800000000001, 0, -213.198)

    expect_equal(e, expectede)

})


test_that("tidyfifo works on better sample data (by comparison with the way fifotibble works", {
  q<-c(10, 6, 15, 6, 40, 25, 6, -17, -1, 20, 2, 23, 2, 75, 20, 50,
       200, 10, 80, 20, 5, 10, -27, -60, 10, 4, 7, 1, 2, 8, 5, 2, 1,
       11, 3, -50, 5, 5, 2, 4, 4, 5, 5, 5, -29, 8, 5, 7, 3, 1, 1, -100,
       -100, -50, -18, -7, -100, 100, 100, -20, -30, -50, -50, -50,
       30, 10, 20, -1, 20, -29, -20, -10, -20, 20, -20)

  p<-c(144.13, 144.14, 143.52, 143.5, 147.35, 147.1, 153.93, 154.43,
       154.44, 164.08, 164.5, 163.32, 163.31, 163.31, 154.44, 154.37,
       153.9, 153.9, 151.21, 152.59, 151.31, 147.48, 146.55, 152.16,
       151.52, 155.28, 156.12, 156.11, 156.07, 155.22, 154.9, 155.32,
       161.12, 161.12, 161.05, 167.02, 157.59, 158.88, 146.34, 135.17,
       136.83, 138.98, 140.69, 140.51, 149, 155.99, 159.78, 177.38,
       172.97, 179.26, 183.71, 194.94, 194.94, 194.94, 194.95, 194.96,
       194.95, 186.09, 186.18, 187.88, 187.92, 187.92, 187.9, 187.93,
       183.44, 184.2, 185.13, 189.53, 189.64, 211.61, 210.5, 208.65,
       208.28, 178.48, 167.8201)

  fifotibble_result <- fifotibble(q,p)
  qpt <- tibble(q,p)
  tidyfifo_result <- qpt%>%tidyfifo(quantities = q, prices = p)
  names(tidyfifo_result) <- names(fifotibble_result)


  expect_equal(fifotibble_result, tidyfifo_result)

})
