#' Tool to computate the Expected Total Cost vector for a Continuous Time Markov Chain, CTMC.
#'
#'\code{ETCt} is used to obtain the Expected Total Cost vector up to t of a homogeneous continuous time Markov chain.
#' @param R numeric, represents the rate matrix of a CTMC.
#' @param c vector, represents the costs of the states of a CTMC.
#' @param t numeric, represents the length of time.
#' @param epsilon numeric, represents the error bound of the approximation of M(t). Default value is 0.001.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @examples
#' library(modesto)
#' # A four states CTMC example
#' R <- matrix(c(0,1,0,0,0, 1/72,0,1,0,0, 0,2/72,0,1,0, 0,0,3/72,0,1/2, 0,0,0,4/72,0),5,5,byrow=TRUE)
#' ETCt(R,c(-80,-15,50,125,200),t=24,epsilon=0.001)
#' @export ETCt

ETCt <- function(R,c,t,epsilon=0.001){
  if (missingArg(R))
    stop("The rate matrix parameter, R, is missing.")
  if (missingArg(c))
    stop("The cost vector argument, c, is missing.")
  if (missingArg(t))
    stop("The length of time parameter, t, is missing.")
  M_t <- Mt(R,t,epsilon)$M_t
  output <- M_t%*%c
  return(list(Costs = output, M_t = M_t))
}
