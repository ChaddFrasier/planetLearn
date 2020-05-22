source("R/utils.R")

# (non-negative numeric scalar) = penalty
# (positive numeric scalar) = opt.thresh
# x.scaled has already been scaled, 
# minimizes the following cost function: 1/n ∑i=1^n L[w^T x_i + b, y_i] + penalty * ||w||_1

LinearModelL1 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size )
{
  "Hello World"
}

# Output: W.mat (n_features+1 x n_penalties), weight matrix on original scale, 
# that can be used to get predictions via cbind(1, X.mat) %*% W.mat (the first row of W.mat should be the bias/beta/intercept)
LinearModelL1penalties <- function( X.mat, y.vec, penalty.vec, step.size )
{
  if ( !is.vector(penalty.vec) ) stop("Penalty vector is not a vector, pass the data as a vector")
  
  weights <- matrix(data=0, nrow = (ncol(X.mat)+1), ncol = length(penalty.vec))
  # this function should begin by scaling X.mat to obtain X.scaled.mat with each column mean=0 and sd=1
  
  
  # loop over penalty values, calling LinearModelL1 to get the (scaled) optimal weight vector for each

  # it should then convert the optimal weight vector (tilde w, tilde beta) back to the original scale, using the mean/sd of each column/feature.
  
  # use warm restarts, i.e. instead of starting the optimization from the 0 vector each time (slow), use the optimal solution for the previous penalty value as the next initial.weight.vec (faster)
  
  return(weights)
}

LinearModelL1CV <- function( X.mat, y.vec, fold.vec=NULL, n.folds=5, penalty.vec=NULL, step.size=.02 )
{
  # TODO: validate all input
  if( !is.data.frame(X.mat) ) stop("X.mat must be a data frame")
  
  if( !is.vector(y.vec) ) stop("y.vec must be an array")

  # check y.vec dimensions
  if( length(y.vec) != nrow(X.mat) ) stop("y.vec must contain the same number of elements found in ncol(X.mat)")
  
  # check that the fold number matches the vector
  if( n.folds != unique(fold.vec) ) stop("n.folds must match the number of folds found in fold.vec")
  
  # check and correct if fold.vec not given
  if( is.null(fold.vec) || !is.vector(fold.vec) )
  {
    # get random ordered array
    set.seed(4352)
    fold.vec <- sample( rep(1:n.folds), length(y.vec), TRUE )
  }
  
  # check and correct if penalty.vec not given
  if( is.null(penalty.vec) || !is.vector(penalty.vec) ) penalty.vec <- 10^seq(0, -2, by = -.125)

  # Init return data
  # n.penalties x n.folds
  mean.validation.loss.vec <- vector(length = length(penalty.vec))
  mean.train.loss.vec <- vector(length = length(penalty.vec))
  
  # should use K-fold cross-validation based on the fold IDs provided in fold.vec 
  for ( fold in 1:n.folds )
  {
    # isolate the training data and labels
    is.train = fold.vec[which(fold.vec == fold)]
    X.train <- X.mat[is.train,]
    y.train <- y.vec[is.train]
    
    # call the penalty training function to get that scaled weight vector for predictions
    W.mat <- LinearModelL1penalties( X.mat = X.train, y.vec = y.train, penalty.vec = penalty.vec, step.size = step.size )
    
    # predict the test data
    train.pred.vec <- cbind(1, X.mat[-is.train,]) %*% W.mat
    val.pred.vec <- cbind(1, X.mat[is.train,]) %*% W.mat
    
    # compute mean.validation.loss.vec (which is a vector (with n_penalties elements) of mean validation loss over all K folds)
    mean.validation.loss.vec <- mean.sq.error(actual=y.vec[-is.train], predicted= val.pred.vec )
    
    # compute mean.train.loss.vec which is a vector (with n_penalties elements) of mean train loss over all K folds
    mean.train.loss.vec <- mean.sq.error(actual=y.train, predicted= train.pred.vec )
  }
  
  # minimize the mean validation loss to determine selected.penalty ( the optimal penalty value that gives lowest error )
  selected.penalty <- penalty.vec[which(min(mean.validation.loss.vec))]
  
  # train the optimal model passing only 1 value for penalty will yeild 1 row
  w.vec <- LinearModelL1penalties( X.mat = X.mat, y.vec=y.vec, penalty.vec=selected.penalty )
  
  return( list(
    mean.validation.loss.vec=mean.validation.loss.vec,
    mean.train.loss.vec=mean.train.loss.vec,
    penalty.vec=penalty.vec,
    selected.penalty=selected.penalty,
    weight.vec=w.vec,
    predict=function(testX.mat)
    {
      cbind(1,testX.mat) %*% W.mat
    }
  ))
}