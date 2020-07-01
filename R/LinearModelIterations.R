#' Linear model iteration with square loss
#'
#' Training by using gradient descent on a linear model with square loss .
#' Return a matrix of weight vector for each iteration to the max iteration.
#'
#' @param X.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param max.iterations integer scalar greater than 1
#' @param step.size integer scalar
#'
#' @return W.mat matrix of weight vectors of size [(p + 1) x max.iterations].
#' A prediction can be obtained by cbind(1,X.mat) %*% W.mat.
#' @export
#'
#' @examples
#' 
#' W.mat <- LMSquareLossIterations(X.mat, y.vec, max.iterations = 5L)
LMSquareLossIterations <- function( X.mat, y.vec, max.iterations, step.size = 1 )
  {
    if ( !all(is.matrix(X.mat), is.numeric(X.mat)) )
      {
        stop("X.mat must be a numeric matrix.")
      }
    
    if ( !all(is.vector(y.vec),
           is.numeric(y.vec),
           length(y.vec) == nrow(X.mat)))
      {
        stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
      }
    
    if ( !all(is.integer(max.iterations),
           max.iterations > 1,
           length(max.iterations) == 1))
      {
        stop("Input max.iterations must be a greater than 1 integer scalar number.")
      }
    
    if ( !all(is.numeric(step.size), length(step.size) == 1) )
      {
        stop("step.size must be a numeric scalar value.")
      }
    
    # Obatin X.scaled.mat from the orginal X.mat, to make sure std = 1, u = 0
    num.train <- dim(X.mat)[1]
    num.feature <- dim(X.mat)[2]
    
    X.mean.vec <- colMeans(X.mat) 
    
    X.std.vec <-
      sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
    X.std.mat <- diag(num.feature) * (1 / X.std.vec)
    
    X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
    slope.mat <-
      matrix(c(
        rep(0, num.feature * max.iterations)),
        num.feature,
        max.iterations
      )
    
    intercept.vec <- c(rep(0, max.iterations))
    
    # for-loop to get the slope.mat matrix
    for ( iter.index in (1:max.iterations) )
      {
        if (iter.index == 1)
          {
            mean.loss.slope.vec <- 2 * t(X.scaled.mat) %*% 
              (X.scaled.mat %*% slope.mat[,1]  + rep(intercept.vec[1], num.train) - y.vec)/num.train
          
            mean.loss.intercept <- 2 * colSums(X.scaled.mat %*% slope.mat[,1] + rep(intercept.vec[1], num.train) - y.vec)/num.train
            slope.vec.temp <- 
              slope.mat[, 1] - step.size * mean.loss.slope.vec
            intercept.vec.temp <- intercept.vec[1] - step.size * mean.loss.intercept
            
          } 
        else 
          {
            mean.loss.slope.vec <- 2 * t(X.scaled.mat) %*% 
              (X.scaled.mat %*% slope.mat[,iter.index - 1]  + rep(intercept.vec[iter.index - 1], num.train) - y.vec)/num.train
            
            mean.loss.intercept <- 2 * colSums(X.scaled.mat %*% slope.mat[,iter.index - 1] + rep(intercept.vec[iter.index - 1], num.train) - y.vec)/num.train
            
            slope.vec.temp <-
              slope.mat[, iter.index - 1] - step.size * mean.loss.slope.vec
            
            intercept.vec.temp <- intercept.vec[iter.index - 1] - step.size * mean.loss.intercept
          }
        
      slope.mat[, iter.index] <- slope.vec.temp
        intercept.vec[iter.index] <- intercept.vec.temp
        
      }
    intercept.part1 <- -X.mean.vec %*% X.std.mat %*% slope.mat  # 1 x m
    intercept <- intercept.part1 + t(intercept.vec)
    slope <- X.std.mat %*% slope.mat # p x m
    W.mat <- rbind(intercept, slope) # (p+1) x m
    
    return(W.mat)
  }

#' Linear model iteration with logistic loss
#'
#' Training by using gradient descent on a linear model with logistic loss .
#' Return a matrix of weight vector for each iteration to the max iteration.
#'
#' @param X.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param max.iterations integer scalar greater than 1
#' @param step.size a numeric scalar greater than zero
#'
#' @return W.mat matrix of weight vectors of size [(p + 1) x max.iterations]
#'
#' @export
#'
#' @examples
#' 
#'
#' W.mat <- LMLogisticLossIterations(X.mat, y.vec, max.iterations = 100L, step.size = 0.5)
#' (W.mat)
LMLogisticLossIterations <- function( X.mat, y.vec, max.iterations, step.size )
  {
    # Check type and dimension
    if (!all(is.numeric(X.mat), is.matrix(X.mat)))
      {
        stop("X.mat must be a numeric matrix")                            
      }
    
    if (!all(is.numeric(y.vec),
         is.vector(y.vec),
         length(y.vec) == nrow(X.mat)))
      {
        stop("y.vec must be a numeric vector of length nrow(X.mat)")
      }
    
    if (!all(
      is.numeric(max.iterations),
      is.integer(max.iterations),
      length(max.iterations) == 1,
      max.iterations >= 1
    )) {
      stop("max.iterations must be an integer scalar greater than one")
    }
    
    if ( !all(is.numeric(step.size), length(step.size) == 1, step.size > 0))
      {
        stop("step.size must be a numeric scalar value.")
      }
    
    # get dimensions
    n.train <- nrow(X.mat)
    n.features <- ncol(X.mat)
    
    # Scale X.mat with m = 0, sd = 1
    feature.mean.vec <- colMeans(X.mat)
    feature.sd.vec <- sqrt(rowSums((t(X.mat) - feature.mean.vec) ^ 2) / n.train) # try to use sd but that gives the sd for the whole matrix
    
    # Check if there is a feature with 0 deviation
    # column with zero variance will become zero at the end
    feature.sd.vec[feature.sd.vec == 0] <- 1
    
    feature.sd.mat <- diag(1 / feature.sd.vec)
    
    X.scaled.mat <- t((t(X.mat) - feature.mean.vec) / feature.sd.vec) # Use scale to simplify.
    
    # Initialize W.mat matrix
    W.mat <- matrix(0, nrow = n.features, ncol = max.iterations)
    beta.vec <- rep(0, l = max.iterations)
    
    W.temp.vec <- W.mat[, 1]
    beta.temp <- 0
    
    # Iteration
    for (n.iterations in (1:max.iterations)) {
      # Calculate L(w)'
      # This is training without interception
      # loss.gradient.vec <-
      #   -t(X.scaled.mat) %*% y.vec / (1 + exp(y.vec * (X.scaled.mat %*% W.temp.vec)))
      
      # This is trainging with interception
      # Calculate L(w)'
      W.gradient.vec <-
        -t(X.scaled.mat) %*% (y.vec / (1 + exp(y.vec * (
          X.scaled.mat %*% W.temp.vec + rep(1,n.train) * beta.temp
        ))))/ n.train
      # Calculate L(beta)'
      beta.gradient <-
        -sum(y.vec / (1 + exp(y.vec * (
          X.scaled.mat %*% W.temp.vec + rep(1,n.train) * beta.temp
        ))))/n.train  
      
      # Take a step
      W.mat[, n.iterations] <-
        W.temp.vec - step.size * W.gradient.vec
      beta.vec[n.iterations] <-
        beta.temp - step.size * beta.gradient
      
      W.temp.vec <- W.mat[, n.iterations]
      beta.temp <- beta.vec[n.iterations]
    }
    
    # unscaling
    intercept.vec <-
      -feature.mean.vec %*% feature.sd.mat %*% W.mat + beta.vec
    W.mat <- feature.sd.mat %*% W.mat
    W.mat <- rbind(intercept.vec, W.mat)
    
    return(W.mat)
  }