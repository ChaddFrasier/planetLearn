library(testthat)
library(planetLearn)

testthat::test_that("Sigmoid function returns values between 0 and 1",
                    {
                      # test using vector to test many values at once
                      result <- sigmoid(c(54.464, 12.454, 38, 1, 0, 468734, 1.233))
                      expect_true(all(result >= 0 && result <= 1 ))
                    })

testthat::test_that("Minnaert function validates input to prevent misuse",
                    {
                      emission <- as.double(25.73810005)
                      incidence <- as.double(46.18210983)
                      k <- as.double(0.85)
                      
                      # incorrect use of k
                      testthat::expect_error( minnaert( emission, incidence, as.numeric(k) ) )
                      
                      DN <- 0.00574214
                    })