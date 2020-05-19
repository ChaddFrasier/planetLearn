
#' sigmoid
#'
#' @param x the value to sigmoid
#'
#' @return the sigmoid version of the number given
#' @export
#'
#' @examples
#' ---------
#' 
sigmoid <- function( x ) {
  return( 1/(1+exp(-x)) )
}


minnaert <- function( emission, incidence, k ){
  return(0)
}