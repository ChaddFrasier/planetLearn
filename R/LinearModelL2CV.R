#' Cross validation using linear model with L2 regularization and square loss
#'
#' Training by using cross validation on a linear model with square loss and L2 regularization.
#' Return a list which contains the best penalty, mean loss of training and validation data,
#' and a predict function which gives a prediction based on the selected penalty.
#'
#' @param X.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param fold.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#'
#' @return result.list a list with mean.validation.loss.vec,
#' mean.train.loss.vec,penalty.vec,selected.penalty,weight.vec,and predict function
#' @export
#'
#' @examples
#' result.list <- LMSquareLossL2CV(X.mat, y.vec, NULL, penalty.vec = seq(5, 0.1, by = -0.1))
#' result.list$predict(X.mat[c(3,4,11),])
LMSquareLossL2CV <- function(X.mat, y.vec, fold.vec=NULL, penalty.vec) {
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),
           is.numeric(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (is.null(fold.vec)) {
    fold.vec <- sample(rep(1:5, l = nrow(X.mat)))
  } else
    if (!all(is.numeric(fold.vec),
             is.vector(fold.vec),
             length(fold.vec) == nrow(X.mat))) {
      stop("fold.vec must be a numeric vector of length nrow(X.mat)")
    }
  
  if (!all(
    is.vector(penalty.vec),
    is.numeric(penalty.vec),
    penalty.vec >= 0,
    diff(penalty.vec) < 0
  )) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  # Find the num of K-fold
  n.folds <- length(unique(fold.vec))
  
  validation.loss.mat <-
    matrix(rep(0, n.folds * length(penalty.vec)),
           n.folds, length(penalty.vec))
  train.loss.mat <- matrix(rep(0, n.folds * length(penalty.vec)),
                           n.folds, length(penalty.vec))
  
  #Learning process for each fold
  for (fold.i in seq_len(n.folds)) {
    train.index <- which(fold.vec != fold.i)
    validation.index <- which(fold.vec == fold.i)
    
    #Calculate train.loss
    W.mat <-
      LMSquareLossL2penalties(X.mat[train.index, ], y.vec[train.index], penalty.vec)
    
    train.predit <- cbind(1, X.mat[train.index, ]) %*% W.mat
    train.loss <- (train.predit - y.vec[train.index]) ^ 2
    
    #Calculate validation.loss
    validation.predict <- cbind(1,X.mat[validation.index, ]) %*% W.mat
    validation.loss <-
      (validation.predict - y.vec[validation.index]) ^ 2
    
    mean.train.loss.vec <- colMeans(train.loss)
    mean.validation.loss.vec <- colMeans(validation.loss)
    
    train.loss.mat[fold.i, ] = mean.train.loss.vec
    validation.loss.mat[fold.i, ] = mean.validation.loss.vec
  }
  
  mean.train.loss.vec <- colMeans(train.loss.mat)
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  
  selected.penalty <-
    penalty.vec[which.min(mean.validation.loss.vec)]
  W.mat <-
    LMSquareLossL2penalties(X.mat[train.index, ], y.vec[train.index], penalty.vec)
  weight.vec <- W.mat[, which(penalty.vec == selected.penalty)]
  
  predict <- function(testX.mat) {
    if (!all(is.numeric(testX.mat),
             is.matrix(testX.mat),
             ncol(testX.mat) == ncol(X.mat))) {
      stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
    }
    prediction.vec <- cbind(1, testX.mat) %*% weight.vec
  }
  
  result.list <- list(
    mean.validation.loss.vec = mean.validation.loss.vec,
    mean.train.loss.vec = mean.train.loss.vec,
    penalty.vec = penalty.vec,
    selected.penalty = selected.penalty,
    weight.vec = weight.vec,
    predict = predict
  )
  
  return(result.list)
}


#' Cross validation using linear model with L2 regularization and logistic loss
#'
#' Training by using cross validation on a linear model with logistic loss and L2 regularization.
#' Return a list which contains the best penalty, mean loss of training and validation data,
#' and a predict function which gives a prediction based on the selected penalty.
#'
#' @param X.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param fold.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#'
#' @return result.list a list with mean.validation.loss.vec,
#' mean.train.loss.vec,penalty.vec,selected.penalty,weight.vec,and predict function
#' 
#' @export
#'
#' @examples
#' data(spam, package = "ElemStatLearn")
#' X.mat <- as.matrix(spam[, 1:57])
#' y.vec <- ifelse(spam$spam == "spam", 1, 0)
#' penalty.vec <- seq(5,0.1, by = -0.1)
#' L2.list <- LMLogisticLossL2CV(X.mat, y.vec, NULL, penalty.vec)
#' (L2.list$predict(as.matrix(X.mat[c(2,3),])))

LMLogisticLossL2CV <- function(X.mat, y.vec, fold.vec=NULL, penalty.vec) {
  # Check type and dimension
  if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
    stop("X.mat must be a numeric matrix")
  }
  
  if (!all(is.numeric(y.vec),
           is.vector(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of length nrow(X.mat)")
  }
  
  if (is.null(fold.vec)) {
    fold.vec <- sample(rep(1:5, l = nrow(X.mat)))
  } else
    if (!all(is.numeric(fold.vec),
             is.vector(fold.vec),
             length(fold.vec) == nrow(X.mat))) {
      stop("fold.vec must be a numeric vector of length nrow(X.mat)")
    }
  if (!all(
    is.vector(penalty.vec),
    is.numeric(penalty.vec),
    penalty.vec >= 0,
    diff(penalty.vec) < 0
  )) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  # If y contains 0 and 1 then match to -1, 1
  if (all(y.vec %in% c(0, 1))) {
    y.vec <- 2 * (y.vec - 0.5) # Maybe a better way?
  }
  
  # Initiallize
  n.features <- ncol(X.mat)
  n.folds <- length(unique(fold.vec))
  train.loss.mat <-
    matrix(0, nrow = n.folds, ncol = length(penalty.vec))
  validation.loss.mat <-
    matrix(0, nrow = n.folds, ncol = length(penalty.vec))
  
  # Iterating folds
  for (fold.index in (1:n.folds)) {
    train.index <- which(fold.vec != fold.index)
    
    # Iterating between train and validation splits
    for (validation.set in c("train", "validation")) {
      if (validation.set == "train") {
        validation.index <- which(fold.vec != fold.index)
      } else{
        validation.index <- which(fold.vec == fold.index)
      }
      
      W.mat <- # (p+1) * i
        LMLogisticLossL2penalties(X.mat[train.index,], y.vec[train.index], penalty.vec) # Do we need to expose step.size?
      
      prediction.vec <-
        ifelse(cbind(1, X.mat)[validation.index, ] %*% W.mat > 0.5, 1,-1)
      
      if (validation.set == "train") {
        
        train.loss.mat[fold.index, ] <-
          colMeans(prediction.vec != y.vec[validation.index])
      } else{
        validation.loss.mat[fold.index, ] <-
          colMeans(prediction.vec != y.vec[validation.index])
      }
    }
  }
  mean.train.loss.vec <- colMeans(train.loss.mat)
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  selected.penalty.index <- which.min(mean.validation.loss.vec)
  
  weight.vec <- # (p + 1) length
    LMLogisticLossL2penalties(X.mat, y.vec, penalty.vec)[, selected.penalty.index]
  
  predict <- function(testX.mat) {
    # Check type and dimension
    if (!all(is.numeric(testX.mat),
             is.matrix(testX.mat),
             ncol(testX.mat) == n.features)) {
      stop("testX.mat must be a numeric matrix with n.features columns")
    }
    
    # prediction.vec <- ifelse(cbind(1,testX.mat) %*% t(weight.vec) > 0.5, 1, -1)
    prediction.vec <- cbind(1,testX.mat) %*% weight.vec
    
    return(prediction.vec)
  }
  
  result.list <- list(
    mean.validation.loss.vec = mean.validation.loss.vec,
    mean.train.loss.vec = mean.train.loss.vec,
    penalty.vec = penalty.vec,
    selected.penalty = penalty.vec[selected.penalty.index],
    weight.vec = weight.vec,
    predict = predict
  )
  
  return(result.list)
}