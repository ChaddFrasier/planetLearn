#' Cross validation algorithm using linear model with square loss
#'
#' Training by using cross validation on a linear model with square loss and early stopping method.
#' Return a list which contains the best iteration step, mean loss of training and validation data,
#' and a predict function which gives a prediction based on the selected step.
#'
#' @param X.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param fold.vec the fold vector information of size [n x 1], which is assigned in the general test
#' @param max.iteration integer scalar greater than 1
#'
#' @return result.list a list with mean.validation.loss.vec,
#' mean.train.loss.vec,selected.steps,weight.vec,and predict function
#'
#' @export
#'
#' @examples 
#' # load the data from the data folder
#'load(file = "../../data/lunarData.rda")
#' y.vec <- lunarData[, 1]
#' X.mat <- as.matrix(lunarData[,-1])
#' result.list <- LMSquareLossEarlyStoppingCV(X.mat, y.vec, NULL, max.iteration = 5L)
#' result.list$predict(X.mat[c(3,19,20,35),])
LMSquareLossEarlyStoppingCV <- function( X.mat, y.vec, fold.vec=NULL, max.iteration, step.size=.00001 )
  {
    # perform input validation
    if ( !all(is.matrix(X.mat), is.numeric(X.mat)) )
      {
        stop("X.mat must be a numeric matrix.")
      }
    
    if ( !all(is.vector(y.vec),
         is.numeric(y.vec),
         length(y.vec) == nrow(X.mat)) ) 
      {
        stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
      }
    
    if ( !all(is.integer(max.iteration),
         max.iteration > 1,
         length(max.iteration) == 1) )
    {
      stop("Input max.iteration must be a greater than 1 integer scalar number.")
    }
    
    if ( is.null(fold.vec) )
      {
        # create a default fold vec
        fold.vec <- sample(rep(1:5, l = nrow(X.mat)))
      }
    else
      if (!all(is.numeric(fold.vec),
           is.vector(fold.vec),
           length(fold.vec) == nrow(X.mat)))
      {
        stop("fold.vec must be a numeric vector of length nrow(X.mat)")
      }
    
    # get the number of folds
    n.folds <- length(unique(fold.vec))
    
    # init the return data structures
    validation.loss.mat <- matrix(rep(0, n.folds * max.iteration), n.folds, max.iteration)
    train.loss.mat <- matrix(rep(0, n.folds * max.iteration), n.folds, max.iteration)
    
    # loop over each fold
    for ( fold.i in seq_len(n.folds) )
      {
        # find the index for each set
        train.index <- which(fold.vec != fold.i)
        validation.index <- which(fold.vec == fold.i)
        
        # calculate train loss using weight vector
        W.mat <- LMSquareLossIterations(X.mat[train.index,], y.vec[train.index], max.iteration, step.size)
        # finish calculating squared loss
        train.predit <- cbind(1, X.mat[train.index,]) %*% W.mat
        train.loss <- (train.predit - y.vec[train.index]) ^ 2
        
        # calculate validation.loss
        validation.predict <- cbind(1, X.mat[validation.index,]) %*% W.mat
        validation.loss <- (validation.predict - y.vec[validation.index]) ^ 2
        
        # calculate the mean loss for all dimensions
        mean.train.loss.vec <- colMeans(train.loss)
        mean.validation.loss.vec <- colMeans(validation.loss)
        
        # store the mean loss of the fold
        train.loss.mat[fold.i,] = mean.train.loss.vec
        validation.loss.mat[fold.i,] = mean.validation.loss.vec
      }
    
    # get the mean loss over all the folds
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    
    # Train the overall optimaized model
    selected.steps <- which.min(mean.validation.loss.vec)
    W.mat.all <- LMSquareLossIterations(X.mat, y.vec, max.iteration, step.size)
    weight.vec <- W.mat.all[, selected.steps]
    
    # create the prediction function using the weight vector
    predict <- function( testX.mat )
      {
        if (!all(is.numeric(testX.mat),
             is.matrix(testX.mat),
             ncol(testX.mat) == ncol(X.mat))
          ){
            stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
          }
        prediction.vec <- cbind(1, testX.mat) %*% weight.vec
      }
    # create the return list
    result.list <-
      list(
        mean.validation.loss.vec = mean.validation.loss.vec,
        mean.train.loss.vec = mean.train.loss.vec,
        selected.steps = selected.steps,
        weight.vec = weight.vec,
        predict = predict
      )
    return(result.list)
  }



#' Cross validation algorithm of a linear model with logistic loss
#'
#' Training by using cross validation on a linear model with logistic loss and early stopping method.
#' Return a list which contains the best iteration step, mean loss of training and validation data,
#' and a predict function which gives a prediction based on the selected step.
#'
#' @param X.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param fold.vec fold index vector of size [n x 1]
#' @param max.iteration integer scalar greater than 1
#' @param step.size a numeric scaler greater than 0, default is 0.5

#'
#' @return result.list a list with mean.validation.loss.vec,
#' mean.train.loss.vec,selected.steps,weight.vec,and predict function
#'
#' @export
#'
#' @examples
#' 
#' load(file = "../../data/lunarData.rda")
#' 01i <- lunarData[which(lunarData$DN %in% c(0,1))]
#' # earlystopping.list <- LMLogisticLossEarlyStoppingCV(X.mat, y.vec, NULL, 100L, 0.5)
#' # (earlystopping.list$predict(as.matrix(X.mat[c(2,4),])))
#' 

LMLogisticLossEarlyStoppingCV <- function(X.mat, y.vec, fold.vec = NULL, max.iteration, step.size = 0.5 )
  {
    # Check type and dimension
    if ( !all(is.numeric(X.mat), is.matrix(X.mat)) )
      {
        stop("X.mat must be a numeric matrix")
      }
    
    if ( !all(is.numeric(y.vec),
         is.vector(y.vec),
         length(y.vec) == nrow(X.mat)) )
      {
        stop("y.vec must be a numeric vector of length nrow(X.mat)")
      }
    
    if ( is.null(fold.vec) )
      {
        fold.vec <- sample(rep(1:5, l = nrow(X.mat)))
      } 
    else 
      if ( !all(is.numeric(fold.vec),
           is.vector(fold.vec),
           length(fold.vec) == nrow(X.mat)))
      {
        stop("fold.vec must be a numeric vector of length nrow(X.mat)")
      }
    
    if ( !all(
      is.numeric(max.iteration),
      is.integer(max.iteration),
      length(max.iteration) == 1,
      max.iteration > 1
    ))
      {
        stop("max.iteration must be an integer scalar greater than 1")
      }
    
    if (!all(is.numeric(step.size), length(step.size) == 1, step.size > 0))
      {
        stop("step.size must be a positive scalar")
      }
    
    # Initiallize
    n.features <- ncol(X.mat)
    n.folds <- length(unique(fold.vec))
    
    # If y contains 0 and 1 then match to -1, 1
    if (all(y.vec %in% c(0, 1))) {
      y.vec <- 2 * (y.vec - 0.5) # Maybe a better way?
    }
    
    # init the train loss matrix
    train.loss.mat <- matrix(0, nrow = n.folds, ncol = max.iteration)
    validation.loss.mat <- matrix(0, nrow = n.folds, ncol = max.iteration)
    
    # Iterating folds
    for ( fold.index in (1:n.folds) )
      {
        train.index <- which(fold.vec != fold.index)
      
        # Iterating between train and validation splits
        for (validation.set in c("train", "validation")) {
          if (validation.set == "train") {
            validation.index <- which(fold.vec != fold.index)
          } else{
            validation.index <- which(fold.vec == fold.index)
          }
          
          # W.mat is [(p + 1) x max.iteration]
          W.mat <-
            LMLogisticLossIterations(X.mat[train.index, ], y.vec[train.index], max.iteration, step.size) # Do we need to expose step.size?
          
          prediction.vec <-
            ifelse(cbind(1, X.mat)[validation.index, ] %*% W.mat > 0.5, 1,-1) # Use cbind here because W.mat is (P + 1) x max.iteration
          
          # Not correct here, need recalculate, because it is a binary classification.
          if (validation.set == "train") {
            train.loss.mat[fold.index,] <-
              colMeans(ifelse(prediction.vec != y.vec[validation.index], 1, 0))
          } else{
            validation.loss.mat[fold.index,] <-
              colMeans(ifelse(prediction.vec != y.vec[validation.index], 1, 0))
          }
        }
      }
      
      # loss of the train set
      mean.train.loss.vec <- colMeans(train.loss.mat)
      mean.validation.loss.vec <- colMeans(validation.loss.mat)
      selected.steps <- which.min(mean.validation.loss.vec)
      
      # calculate the weight vector of the selected 
      weight.mat <- LMLogisticLossIterations( X.mat, y.vec, as.integer(selected.steps), step.size )
      weight.vec <- weight.mat[, selected.steps] # weight.vec is p+1 length
      
      predict <- function(testX.mat) {
        # Check type and dimension
        if (!all(is.numeric(testX.mat),
                 is.matrix(testX.mat),
                 ncol(testX.mat) == n.features)) {
          stop("testX.mat must be a numeric matrix with n.features columns")
        }
      
      # testX.mat is of size [n x (p + 1)]
      prediction.vec <-
        # ifelse(cbind(1, testX.mat) %*% t(weight.vec) > 0.5, 1,-1) # Should this be 0 or -1?
        cbind(1,testX.mat) %*% weight.vec
      return(prediction.vec)
    }
    # create the return list
    result.list <-
      list(
        mean.validation.loss.vec = mean.validation.loss.vec,
        mean.train.loss.vec = mean.train.loss.vec,
        selected.steps = selected.steps,
        weight.vec = weight.vec,
        predict = predict
      )
    
    return(result.list)
  }