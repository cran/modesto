#' Tool to computate the transition matrix for a Continuous Time Markov Chain, CTMC.
#'
#'\code{Pt2} is used to obtain the transition matrix of a homogeneous continuous time Markov chain for a period of time of t.
#' @param R numeric, represents the rate matrix of a CTMC.
#' @param t numeric, represents the length of time.
#' @param epsilon numeric, represents the error bound of the approximation of P(t). Default values is 0.001.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @examples
#' library(modesto)
#' # A two states CTMC example
#' Pt2(matrix(c(0,2,3,0),2,2,byrow=TRUE),t=0.7,epsilon=0.005)
#' # A four states CTMC example
#' R <- matrix(c(0,2,3,0,4,0,2,0,0,2,0,2,1,0,3,0),4,4,byrow=TRUE)
#' Pt2(R,t=0.7,epsilon=0.005)
#' # require(microbenchmark)
#' # microbenchmark(Pt(R,t=0.7,epsilon=0.005),Pt2(R,t=0.7,epsilon=0.005),times=1000L)
#' @export Pt2

Pt2 <- function(R,t,epsilon=0.001){
  rs <- rowSums(R)
  r <- max(rs)
  P_hat <- R/r
  n <- dim(R)[1]
  diag(P_hat) <- p_hat(rs=rs,r=r)
  A <- P_hat
  rt <- r*t
  c <- exp(-rt)
  B <- c*diag(1,n,n)
  sum <- c
  k <- 1
  while(sum < (1 - epsilon)){
    c <- (c*rt)/k
    B <- B + c*A
    A <- A%*%P_hat
    sum <- sum + c
    k <- k+1
  }
  return(list(Pt = round(B,4),M = k-1,P_hat = P_hat))
}

