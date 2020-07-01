#' Linear model L2 regularization with square loss
#'
#' Training by using L2 regularization on a linear model with square loss .
#' Return the optimal weight vector for the given threshold and penalty.
#'
#'
#' @param X.scaled.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric matrix of length nrow(X.scaled.mat)
#' @param penalty a non-negative numeric scalar
#' @param opt.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size ncol(X.scaled.mat)
#' @param step.size a numeric scalar, which is also greater than 0
#'
#' @return opt.weight the optimal weight vector of length ncol(X.scaled)
#' @export
#'
#' @examples
#' data(ozone, package = "ElemStatLearn")
#' y.vec <- ozone[, 1]
#' X.mat <- as.matrix(ozone[,-1])
#' num.train <- dim(X.mat)[1]
#' num.feature <- dim(X.mat)[2]
#' X.mean.vec <- colMeans(X.mat)
#' X.std.vec <- sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
#' X.std.mat <- diag(num.feature) * (1 / X.std.vec)
#' X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
#' optimal.weight.vec <- LMSquareLossL2(X.scaled.mat, y.vec, penalty = 0.5, initial.weight.vec = c(rep(0, ncol(X.mat) + 1)))
LMSquareLossL2 <-
  function(X.scaled.mat,
           y.vec,
           penalty,
           opt.thresh = 0.5,
           initial.weight.vec,
           step.size = 0.01)
    {
      if (!all(is.matrix(X.scaled.mat), is.numeric(X.scaled.mat))) {
        stop("X.mat must be a numeric matrix.")
      }
      
      if (!all(is.vector(y.vec),
               is.numeric(y.vec),
               length(y.vec) == nrow(X.scaled.mat))) {
        stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
      }
      
      if (!all(length(penalty) == 1, is.numeric(penalty), penalty >= 0)) {
        stop("penalty must be a non-negative numeric scalar.")
      }
      
      if (!all(length(opt.thresh) == 1,
               is.numeric(opt.thresh),
               opt.thresh > 0)) {
        stop("opt.thresh must be a positive numeric scalar.")
      }
      
      if (!all(is.vector(initial.weight.vec),
               is.numeric(initial.weight.vec),
               length(initial.weight.vec) == ncol(X.scaled.mat) + 1)) {
        stop("initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1")
      }
      
      weight.vec <- initial.weight.vec[-1]
      intercept.scalar <- initial.weight.vec[1]
      n.train = dim(X.scaled.mat)[1]
      # Compute the gradient
      while (TRUE) {
        grad.cost.func.slope <- 2 * t(X.scaled.mat) %*% 
          (X.scaled.mat %*% weight.vec  + rep(intercept.scalar, n.train) - y.vec)/n.train +
          2 * penalty * weight.vec
        
        
        grad.cost.func.intercept <- 2 * colSums(X.scaled.mat %*% weight.vec + rep(intercept.scalar, n.train) - y.vec)/n.train
        
        if (sum(abs(grad.cost.func.slope)) <= opt.thresh) {
          break
        } else{
          
          weight.vec <- weight.vec - step.size * grad.cost.func.slope
          intercept.scalar <- intercept.scalar - step.size * grad.cost.func.intercept
        }
        
      }
      optimal.weight <- c(intercept.scalar, weight.vec)
      return(optimal.weight)
    }


#' Linear model L2 regularization with logistic loss, including beta during the training
#'
#' Training by using L2 regularization on a linear model with logistic loss .
#' Return the optimal weight vector for the given threshold and penalty.
#'
#' @param X.scaled.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric matrix of length nrow(X.scaled.mat)
#' @param penalty a non-negative numeric scalar
#' @param opt.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size ncol(X.scaled.mat)
#' @param step.size a numeric scalar greater than zero
#' @param max.iteration a integer scalar greater than one
#'
#' @return opt.weight the optimal weight vector of length ncol(X.scaled)
#' @export
#'
#' @examples
#' 
#' data(spam, package = "ElemStatLearn")
#' X.mat <- as.matrix(spam[, 1:57])
#' X.scaled.mat <- scale(X.mat)
#' y.vec <- ifelse(spam$spam == "spam", 1, -1)
#' opt.weight.vec <- LMLogisticLossL2(X.mat, y.vec, 0.5, 0.5, rep(0,ncol(X.scaled.mat) + 1), 0.01, 100L)
#' (opt.weight.vec)


LMLogisticLossL2 <-
  function(X.scaled.mat,
           y.vec,
           penalty,
           opt.thresh,
           initial.weight.vec,
           step.size = 0.01,
           max.iteration = 10) {
    # Check type and dimension
    if (!all(is.numeric(X.scaled.mat), is.matrix(X.scaled.mat))) {
      stop("X.scaled.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.scaled.mat))) {
      stop("y.vec must be a numeric vector of lenght nrow(X.scaled.mat).")
    }
    
    if (!all(is.numeric(penalty), length(penalty) == 1, penalty >= 0)) {
      stop("penalty must be a non-negative numeric scalar")
    }
    
    if (!all(is.numeric(opt.thresh),
             length(opt.thresh) == 1,
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar")
    }
    
    if (!all(
      is.numeric(initial.weight.vec),
      is.vector(initial.weight.vec),
      length(initial.weight.vec) == ncol(X.scaled.mat) + 1
    )) {
      stop("initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1") # <- Change here
    }
    
    # Initializing
    # here n.feature is also p
    n.features <- ncol(X.scaled.mat)
    n.trains <- nrow(X.scaled.mat)
    
    opt.W.vec = initial.weight.vec[-1]
    opt.beta = initial.weight.vec[1]
    
    W.gradient.vec <-
      -t(X.scaled.mat) %*% (y.vec / (1 + exp(
        y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
      ))) +  2 * penalty * opt.W.vec
    beta.gradient <-
      -sum(y.vec / (1 + exp(
        y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
      )))
    
    # loss.gradient.vec <- -t(X.scaled.mat) %*% y.vec / (1 + exp(y.vec * (X.scaled.mat %*% opt.weight.vec)))
    # cost.gradient.vec <- loss.gradient.vec + penalty * opt.weight.vec # This is for L1 norm
    
    n.iteration <- 0
    
    while (norm(abs(W.gradient.vec)) > opt.thresh &&
           n.iteration <= max.iteration) {
      n.iteration = n.iteration + 1
      W.gradient.vec <-
        -t(X.scaled.mat) %*% (y.vec / (1 + exp(
          y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
        ))) +  2 * penalty * opt.W.vec
      beta.gradient <-
        -sum(y.vec / (1 + exp(
          y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
        )))
      
      opt.W.vec <- opt.W.vec - step.size * W.gradient.vec
      opt.beta <- opt.beta - step.size * beta.gradient
      
      # opt.weight.vec <- opt.weight.vec - step.size * cost.gradient.vec # Is this L1 norm gradient?
    }
    
    # # Iteration # L1 norm?
    # while (cost > opt.thresh){ # Change this if it is a L1 norm of gradient
    #   last.weight.vec <- opt.weight.vec
    #   loss.gradient.vec <- -t(X.scaled.mat) %*% y.vec / (1 + exp(y.vec * (X.scaled.mat %*% last.weight.vec)))
    #   cost.gradient.vec <- loss.gradient + penalty * 2 * last.weight.vec # This is for L2 norm
    #   opt.weight.vec <- opt.weight.vec - step.size * cost.gradient.vec
    #   cost <-
    #     sum(1 + exp(-y.vec * (X.scaled.mat %*% t(
    #       initial.weight.vec
    #     )))) + penalty * sum(initial.weight.vec ^ 2)
    # }
    
    opt.weight.vec <- c(opt.beta, opt.W.vec) #opt.weight.vec is p+1
    
    return(opt.weight.vec)
  }