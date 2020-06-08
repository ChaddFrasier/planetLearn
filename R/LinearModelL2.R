source("R/utils.R")

#' LinearModelL2
#' 
#' minimizes the following cost function: 1/n ∑i=1^n L[w^T x_i + b, y_i] + penalty * ||w||^2_2
#'
#' @param X.scaled.mat the scaled training matrix [ n_observations : n_features ]
#' @param y.vec the label vector for the data matrix [ n_observations : 1 ]
#' @param penalty a positive scaler penalty term
#' @param opt.thresh a positive scaler
#' @param initial.weight.vec a starting weight vector, can be random numbers or 0s [ n_features : 1 ]
#' @param step.size the incrimental step to descent with
#'
#' @return an optimal weight vector for predictings [n_features + 1 : 1] where the first entry is the intercept term
#' @export
#'
#' @examples
#' # load the default dataset
#' getwd()
#' 
#' 
LinearModelL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size )
{
  # fast initialization
  # w.vec <- vector(length = (ncol(X.scaled.mat)+1) )
  
  # ** temporary **
  w.vec <- rep(0, times= (ncol(X.scaled.mat)+1))
  
  return(w.vec)
}


#' LinearModelL2penalties
#'
#' train different models based on the different penalty values in the penalty_vec and create a matric of weights for all of the penalties
#' W.mat (n_features+1 x n_penalties), weight matrix on original scale can be used to make
#' predictions via cbind(1, X.mat) %*% W.mat (the first row of W.mat should be the bias/beta/intercept)
#'
#' @param X.mat an unscaled matrix of [ n_observations : n_features ]
#' @param y.vec labels for the observations [ n_observations : 1 ]
#' @param penalty.vec a vector of possible penelty values to test [ n_penalties : 1 ]
#' @param step.size the incrimental size to descent by
#'
#' @return a matrix of weights for each feature for each penalty used [ n_features + 1 : n_penalties ] (first row is the bias)
#' @export
#'
#' @examples
#' 
#' 
LinearModelL2penalties <- function( X.mat, y.vec, penalty.vec, step.size )
{
  if ( !is.vector(penalty.vec) ) stop("Penalty vector is not a vector, pass the data as a vector")
  
  # init weight matrix
  weight.mat <- matrix(data=0, nrow = (ncol(X.mat)+1), ncol = length(penalty.vec))
  
  w.vec <- vector(length = ncol(X.mat)+1)
  initial.weight.vec <- vector(length = ncol(X.mat)+1)
  
  
  # scale the input data
  # Note:
  # attr(X.scaled.mat, "scaled:center") returns the mean of each column
  # attr(X.scaled.mat, "scaled:scale") returns the std of each column
  X.scaled.mat <- scale(X.mat, center = TRUE, scale = TRUE)
  
  # loop over penalty values, calling LinearModelL1 to get the (scaled) optimal weight vector for each
  for ( penalty in 1:length(penalty.vec) )
  {
    # get the weigth vector for each penalty value
    w.vec <- LinearModelL2( X.scaled.mat = X.scaled.mat, y.vec = y.vec, penalty = penalty, opt.thresh = opt.thresh, initial.weight.vec = initial.weight.vec, step.size = step.size )
    
    # it should then convert the optimal weight vector (tilde w, tilde beta) back to the original scale, using the mean/sd of each column/feature.
    w.tilda <- c(w.vec[1], (w.vec[-1] - attr(X.scaled.mat, "scaled:center") / attr(X.scaled.mat, "scaled:scale")) )
    
    # use warm restarts, use the optimal solution for the previous penalty value as the next initial.weight.vec
    initial.weight.vec <- w.vec
    
    # store this penatly run in the final matrix
    weight.mat[,penalty] <- w.tilda
  }
  
  return(weight.mat)
}

#' LinearModelL2CV
#' 
#' perform a cross-validation using n.folds to select the nest training set for the data, this function trains a new model for each train/test split
#' and then selects the best weight vector for the whole data set
#'
#' @param X.mat Unscaled data matrix [ n_observations : n_features ]
#' @param y.vec a vector of labels [ n_observations : 1 ]
#' @param fold.vec a vector that indicadeted which fold each observation belongs to [ n_observatons : 1 ]
#' @param n.folds positive integer (default: 5)
#' @param penalty.vec a vector of penalties to use for the CV of each fold
#' @param step.size the incriment to step by when doing gradient descent
#'
#' @return a list of objects to evaluate how the whole model performed over all folds.
#' 1. mean.validation.loss.vec: a vector of the mean validation loss over all the folds.
#' 2. mean.train.loss.vec: a vector of training loss over all the folds.
#' 3. penalty.vec: the penalty vector used in the CV.
#' 4. selected.penalty: the best penalty to use in that vector,
#' 5. weight.vec: the optimal weight vector for predictions
#' 6. predict: a function that can be called like this `result.list$predict(testX.mat)` where result list is the whole list object
#' 
#' @export
#'
#' @examples
#' getwd()
#' 
LinearModelL2CV <- function( X.mat, y.vec, fold.vec=NULL, n.folds=5, penalty.vec=NULL, step.size=.02 )
{
  # TODO: validate all input
  if( !is.matrix(X.mat) || !all(is.numeric(X.mat)) ) stop("X.mat must be a numeric matrix")
  
  if( !is.vector(y.vec) || !all(is.numeric(y.vec)) ) stop("y.vec must be a numeric array")
  
  # check y.vec dimensions
  if( length(y.vec) != nrow(X.mat) ) stop("y.vec must contain the same number of elements found in ncol(X.mat)")
  
  # check that the fold number matches the vector
  if( n.folds != length(unique(fold.vec)) ) stop("n.folds must match the number of folds found in fold.vec")
  
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
    is.train <- which(fold.vec == fold)
    X.train <- X.mat[is.train,]
    y.train <- y.vec[is.train]
    
    # call the penalty training function to get that scaled weight vector for predictions
    W.mat <- LinearModelL2penalties( X.mat = X.train, y.vec = y.train, penalty.vec = penalty.vec, step.size = step.size )
    
    # predict the test data dims = [ nrows(X.mat[train]) x n_penalties ] so we take the mean to get the average prediction for each observation on every penalty
    train.pred <- cbind(1, X.mat[is.train,]) %*% W.mat
    val.pred <- cbind(1, X.mat[-is.train,]) %*% W.mat
    
    temp <- colMeans(val.pred - y.vec[-is.train])
    # compute mean.validation.loss (which is a vector (with n_penalties elements) of mean validation loss over all K folds)
    mean.validation.loss.vec <- (temp*temp)
    
    temp <- colMeans(train.pred - y.train)
    # compute mean.train.loss.vec which is a vector (with n_penalties elements) of mean train loss over all K folds
    mean.train.loss.vec <- (temp*temp)
  }
  
  # minimize the mean validation loss to determine selected.penalty ( the optimal penalty value that gives lowest error )
  selected.penalty <- penalty.vec[which(mean.validation.loss.vec == min(mean.validation.loss.vec))]
  
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