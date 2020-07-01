#' Linear model with L2 penalties and square loss
#'
#' Training by using L2 regularization on a linear model with square loss .
#' Return a matrix of weight vector for the given penalty vector.
#'
#' @param X.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#' @param opt.thresh a non-negative numeric scalar
#'
#' @return W.mat a numeric weight matrix of size [ncol(X.mat) x length(penalty.vec)]
#' @export
#'
#' @examples
#'
#' W.mat <- LMSquareLossL2penalties(X.mat, y.vec,seq(5, 0.1, by = -0.1))
#' 
LMSquareLossL2penalties <- function( X.mat, y.vec, penalty.vec, opt.thresh = 0.5 ) {
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),
           is.numeric(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  is.decending <- function(vec) {
    result <- all(diff(vec) < 0)
    return(result)
  }
  
  if (!all(
    is.vector(penalty.vec),
    is.numeric(penalty.vec),
    penalty.vec >= 0,
    is.decending(penalty.vec)
  )) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  #Obatin X.scaled.mat from the orginal X.mat, to make sure std = 1, u = 0
  num.train <- dim(X.mat)[1]
  num.feature <- dim(X.mat)[2]
  
  X.mean.vec <- colMeans(X.mat)
  X.std.vec <-
    sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
  X.std.mat <- diag(num.feature) * (1 / X.std.vec)
  
  X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
  
  slope.mat <- matrix(c(
    rep(0, (num.feature + 1) * length(penalty.vec))),
    num.feature + 1,
    length(penalty.vec)
  )
  
  optimal.weight.vec <- c(rep(0, (num.feature + 1)))
  for (index in seq(length(penalty.vec))) {
    optimal.weight.vec <- LMSquareLossL2(X.scaled.mat,
                                         y.vec = y.vec,
                                         penalty = penalty.vec[index],
                                         initial.weight.vec = optimal.weight.vec)
    slope.mat[, index] <- optimal.weight.vec
  }
  
  unscaled.slope.mat <- X.std.mat %*% slope.mat[-1,]
  unscaled.intercept.vec <- -X.mean.vec %*% X.std.mat %*% slope.mat[-1,] + t(slope.mat[1,])
  
  W.mat <- rbind(unscaled.intercept.vec, unscaled.slope.mat)
  return(W.mat)
}



#' Linear model with L2 penalties and logistic loss
#'
#' Training by using L2 regularization on a linear model with logistic loss .
#' Return a matrix of weight vector for the given penalty vector.
#'
#' @param X.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#' @param opt.thresh a non-negative numeric scalar 
#'
#' @return W.mat a numeric weight matrix of size [ncol(X.mat) x length(penalty.vec)]
#' @export
#'
#' @examples
#' data(spam, package = "ElemStatLearn")
#' X.mat <- as.matrix(spam[, 1:57])
#' y.vec <- ifelse(spam$spam == "spam", 1, -1)
#' penalty.vec <- seq(5,0.1, by = -0.1)
#' W.mat <- LMLogisticLossL2penalties(X.mat, y.vec, penalty.vec, 0.5)
#' (W.mat)

LMLogisticLossL2penalties <-
  function(X.mat, y.vec, penalty.vec, opt.thresh = 0.5) {
    # Check type and dimension
    if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
      stop("X.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of length nrow(X.mat)")
    }
    
    is.decending <- function(vec) {
      result <- all(diff(vec) < 0)
      return(result)
    }
    
    if (!all(
      is.vector(penalty.vec),
      is.numeric(penalty.vec),
      penalty.vec >= 0,
      is.decending(penalty.vec)
    )) {
      stop("penalty.vec must be a non-negative decreasing numeric vector")
    }
    
    if (!all(is.numeric(opt.thresh),
             length(opt.thresh) == 1,
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar")
    }
    
    # Initializing
    # X.mat <- X.mat[,-1]
    
    n.train <- nrow(X.mat)
    n.features <- ncol(X.mat) # features is p here
    n.penalties <- length(penalty.vec)
    # opt.thresh <- 5 # Do we need to expose this?
    
    # Scale X.mat with m = 0, sd = 1
    feature.mean.vec <- colMeans(X.mat)
    feature.sd.vec <-
      sqrt(rowSums((t(X.mat) - feature.mean.vec) ^ 2) / n.train)
    
    # column with zero variance will become zero at the end
    feature.sd.vec[feature.sd.vec == 0] <- 1
    
    feature.sd.mat <- diag(1 / feature.sd.vec)
    
    X.scaled.mat <- t((t(X.mat) - feature.mean.vec) / feature.sd.vec)
    
    initial.weight.vec <- rep(0, n.features + 1)
    
    W.mat <- matrix(0, nrow = n.features + 1, ncol = n.penalties)
    # W.temp.mat <- W.mat
    
    for (i.penalty in c(1:n.penalties)) {
      W.mat[, i.penalty] <-  # W.mat is (p+1) x i
        LMLogisticLossL2(X.scaled.mat,
                         y.vec,
                         penalty.vec[i.penalty],
                         opt.thresh,
                         initial.weight.vec)
      initial.weight.vec <- W.mat[, i.penalty] # is penalty in a decreasing order?
    }
    
    intercept.vec <-   
      -feature.mean.vec %*% feature.sd.mat %*% W.mat[-1,] + W.mat[1,] # W.mat is the beta.vec
    W.mat <- rbind(intercept.vec, feature.sd.mat %*% W.mat[-1,])
    
    return(W.mat) # W.mat is (p + 1) x i
    
  }