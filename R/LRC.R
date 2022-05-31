#' Tool to computate the Long-Run Cost Rate for a Continuous Time Markov Chain, CTMC.
#'
#'\code{LRC} is used to obtain the Long-Run Cost Rate of a homogeneous continuous time Markov chain.
#' @param X matrix, represents the rate matrix of a CTMC.
#' @param costs vector, represents the costs of the states of a CTMC.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @examples
#' \dontrun{library(modesto)
#' # A five states CTMC example
#' R <- matrix(c(0,1,0,0,0, 1/72,0,1,0,0, 0,2/72,0,1,0, 0,0,3/72,0,1/2, 0,0,0,4/72,0),5,5,byrow=TRUE)
#' LRC(X=R,costs=c(-80,-15,50,125,200))
#' }
#' @importFrom methods new
#' @importFrom markovchain period
#' @importFrom markovchain is.irreducible
#' @export LRC

LRC <- function(X,costs){
  if (missingArg(X))
    stop("The X argument is missing.")
  if (missingArg(costs))
    stop("The cost vector argument 'costs' is missing.")
  rate <- TRUE
  indicator <- LimDist(X,rate)$indicator
  if(indicator==1){
     occup_dist <- LimDist(X,rate)$Lim_dist
     output <- list("It was possible to calculate the long-run cost rate!", LRCR=occup_dist%*%costs)
     return(output)
  }
  else{return(list(indicator=0,"It was not possible to calculate the long-run cost rate."))}
}
