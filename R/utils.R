#' sigmoid
#' 
#' Calculate the sigmoid for data given by x
#'
#' @param x The object to apply sigmoid to.
#' Types can be: Numeric, Double, Integer, Vector, Data Frame
#'
#' @return returns value is of type x
#' @export
#'
#' @examples
#' # Numerical
#' # ---------
#' val <- 25.6548
#' sigmoid(val)
#' 
#' # Vector
#' # ------
#' vector <- c(9.6574, 13.52, 22.34)
#' sigmoid(vector)
#' 
#' # Data Frame
#' # ----------
#' df <- data.frame(column_1=c(9.6574, 3.52, 232.34), column_2=c(9.6574, 13.52, 22.43))
#' sigmoid(df)
#' 
sigmoid <- function( x ) {
  return( 1/(1+exp(-x)) )
}


#' minnaert
#' 
#' Uses Emission Angle and Incidence Angle to predict DN
#' 
#' @param phase the phase angle of the observation
#' @param emission the emission angle of the observation
#' @param incidence the incidence angle of the observation
#' @param k constant between 0 and 1
#'
#' @return returns minnaert prediction based on emission and incidence angles
#' @export
#'
#' @examples
#' # Out of Bounds
#' # ---------
#' k <- as.double(0.5)
#' phase <- as.double(27.51269913)
#' emission <- as.double(25.73810005)
#' incidence <- as.double(46.18210983)
#' 
#' minnaert(phase, emission, incidence, k)
#' 
#' # Success
#' # -------
#' phase <- as.double(105.79740143)
#' emission <- as.double(31.06315994)
#' incidence <- as.double(76.24420929)
#' k <- as.double(0.025)
#' 
#' minnaert(phase, emission, incidence, k)
#' 
minnaert <- function( phase, emission, incidence, k ){
  # validate input
  if( !is.double(k) || (k > 1 || k <= 0) )
  {
    stop("K must be a double between 0 and 1")
  }
  else if( !is.double(emission) || (emission > 360 || emission < 0) )
  {
    stop("Emissions Angle must be a double between 0 and 360")
  }
  else if( !is.double(incidence) || (incidence > 360 || incidence < 0) )
  {
    stop("Incidence Angle must be a double between 0 and 360")
  }
  
  # do not use for prediction if emission is out of phase bound
  if( emission > (70 + phase/9) )
  {
    stop("Out of Minnaert Bounds for prediction")
  }

  return( cos(emission)^(k-1) * cos(incidence)^(k) )
}


#' mean.sq.error
#'
#' calculates the mean squared error of the prediction vector
#'
#' @param actual the real values
#' @param predicted the values that were predicted
#'
#' @return returns an error value between 0 and 1
#' @export
#'
#' @examples
#' 
#' 
mean_sq_error <- function( actual, predicted )
{
  residual.vec <- (actual - predicted)
  return( colMeans(residual.vec * residual.vec) )
}


#' gradientDecent
#'
#' @param X.mat scaled matrix of features
#' @param y.vec labels to X.mat
#' @param step.size the step size for the gradient decent
#' @param max.iterations the max number of iterations to perform the decent
#'
#' @return a matrix of weights for each iteration [ncol(X.mat) : max.iterations]
#' @export
#'
#' @examples
#' 
#' 
gradientDecent <- function( X.mat, y.vec, step.size, max.iterations )
{
  # create a weight matrix to return
  w.vec <- rep( 0, ncol(X.mat) )
  W.mat <- matrix( 0, ncol( X.mat ), max.iterations)
  
  # init gradient
  gradient <- 0
  
  # loop over each all iterations
  for( index in 1:max.iterations )
  {
    # generated the weighted predictions
    prediction <- X.mat %*% w.vec
    gradient <- colMeans(-as.numeric(y.vec) * X.mat / as.numeric(1+exp(y*(prediction))) )
    
    # calculate the new weight vector based on the pervious iterations
    w.vec = w.vec - step.size * gradient
    # save the new weights into the return matrix at the iteration
    W.mat[,index] = w.vec
  }
  
  return (W.mat)
}

#' k.fold.cv
#' 
#' runs an algorithm with a fold vector to get get an array of errors for each fold number 1:k-folds
#'
#' @param X.mat scaled data matrix
#' @param y.vec labels for the data
#' @param compute.predictions a function to make the predictions
#' @param fold.vec a vector of length nrow(X.mat) telling which fold each observation belongs to
#'
#' @return returns a vector of errors for each fold length(return.vec) = n.folds
#' @export
#'
#' @examples
#' 
#' 
k.fold.cv <- function(X.mat, y.vec, compute.predictions, fold.vec) {
  
  # create a vector to return that has as many spots as it does folds
  error.vec <- vector( length = length( unique(fold.vec) ))
  
  # iterate over each fold; k total
  for( fold in unique(fold.vec) )
    {
      # get the new test set
      X.test <- X.mat[fold.vec == fold, ]
      y.test <- y.vec[fold.vec == fold]
      # get the new train set
      X.train <- X.mat[fold_vec != fold, ]
      y.train <- y.vec[fold_vec != fold]
      
      # call the prediction function passed to the function
      pred.vec <- compute.predictions(X.train, y.train, X.test)
      
      # calculate the loss of the prediction function based on the average of the 
      loss <- colMeans(y.test == as.vector(pred.vec))
      
      error.vec[fold] <- loss
    }
  return(error.vec)
}

# =================================================================
#                      Might Be Used

#' k
#'
#' @param alpha alpha angle of the photometric data
#'
#' @return a value between 0 and 1
#' @export
#'
#' @examples
#' 
#' 
k <- function( alpha )
{
  # some function that pushes alpha into a k value between 0 and 1
}