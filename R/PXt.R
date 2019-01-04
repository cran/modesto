#' Tool to computate the transient probability distribution for a Continuous Time Markov Chain, CTMC.
#'
#'\code{Pt} is used to obtain the transient probability distribution of a homogeneous continuous time Markov chain at a point of time t.
#' @param X0 numeric vector, represents the probability distribution of the initial state.
#' @param R numeric, represents the rate matrix of a CTMC. Default value is 0.
#' @param t numeric, represents the length of time.
#' @param epsilon numeric, represents the error bound of the approximation of P(t). Default values is 0.01.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @examples
#' library(modesto)
#' R <- matrix(c(0,2,0,3,0,1,0,6,0),3,3,byrow=TRUE)
#' X0 <- c(1,0,0)
#' PXt(X0,R,t=0.5,epsilon=0.01) # A three states CTMC example
#' @export PXt
#'
PXt <- function(X0,R,t,epsilon){
  output <- t(X0)%*%Pt(R,t,epsilon)$Pt
  return(list(PXt= output,most.prob=which.max(output),least.prob=which.min(output)))
}
