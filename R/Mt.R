#' Tool to computate the Occupancy Matrix for a Continuous Time Markov Chain, CTMC.
#'
#'\code{Mt} is used to obtain the Occupancy matrix of a homogeneous continuous time Markov chain for a period of time [0,t].
#' @param R numeric, represents the rate matrix of a CTMC.
#' @param t numeric, represents the length of time.
#' @param epsilon numeric, represents the error bound of the approximation of M(t). Default value is 0.01.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @examples
#' library(modesto)
#' R <- matrix(c(0,1,0,0,0, 1/72,0,1,0,0, 0,2/72,0,1,0, 0,0,3/72,0,1/2, 0,0,0,4/72,0),5,5,byrow=TRUE)
#' Mt(R,t=24,epsilon=0.005) # A five states CTMC example
#' @export Mt

Mt <- function(R,t,epsilon=0.01){
  if (missingArg(R))
    stop("The rate matrix parameter, R, is missing.")
  if (missingArg(t))
    stop("The length of time parameter, t, is missing.")
  rs <- rowSums(R)
  r <- max(rs)
  P_hat <- R/r
  n <- dim(R)[1]
  for(j in 1:n){
    P_hat[j,j]<- 1 - (rs[j]/r)
  }
  A <- P_hat
  rT <- r*t
  k <- 0
  yek <- exp(-rT)
  ygk <- 1 - yek
  sum <- ygk
  B <- ygk*diag(1,n,n)

  while(sum/r < (t - epsilon)){
    k <- k+1
    yek <- yek*rT/k
    ygk <- ygk - yek
    B <- B + ygk*A
    A <- A%*%P_hat
    sum <- sum + ygk
  }
  return(list(M_t = round(B/r,4),iter = k,P_hat = P_hat))
}
