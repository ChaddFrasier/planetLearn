library(testthat)
library(planetLearn)

testthat::test_that("LinearModelL1CV function checks input",
                    {
                      # set the working directory to the home of the projects
                      # load the data from the data folder
                      load(file = "../../data/lunarData.rda")
                      
                      y.vec <- as.numeric(lunarData[,1])
                      X.mat <- as.matrix(lunarData[,-1])
                      
                      n.folds <- 10
                      set.seed(7125798)
                      fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
                      penalty.vec <- 10^seq(0, -2, by = -.125)
                      step.size <- .002
                      
                       expect_error( LinearModelL1CV(
                        X.mat, y.vec, fold.vec = fold.vec, n.folds = 11,
                        penalty.vec = penalty.vec, step.size = step.size) )
                    })

testthat::test_that("LinearModelL1CV function Should pass and run",
              {
                # set the working directory to the home of the project
                # load the data from the data folder
                load(file = "../../data/lunarData.rda")
                
                y.vec <- as.numeric(lunarData[,1])
                X.mat <- as.matrix(lunarData[,-1])
                
                n.folds <- 10
                set.seed(7125798)
                fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
                penalty.vec <- 10^seq(0, -2, by = -.125)
                step.size <- .002
                result.list <- LinearModelL1CV(
                  X.mat, y.vec, fold.vec = fold.vec, n.folds = n.folds,
                  penalty.vec = penalty.vec, step.size = step.size)
                
                expect_true ( is.list(result.list) )
              })