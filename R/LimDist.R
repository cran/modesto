#' Tool to computate the limiting distribution for a Continuous Time Markov Chain, CTMC.
#'
#' \code{LimDist} is used to obtain the limiting distribution of a homogeneous continuous time Markov chain.
#' @param X matrix, represents a rate matrix of a CTMC or the transition probability matrix of the DTMC associated to the CTMC.
#' @param rate boolean, if rate is equal to TRUE then the argument X represents the rate matrix of the CTMC. If rate is equal to FALSE then the argument X represents the probability transition matrix of the CTMC.
#' @param epsilon numeric, represents the error of approximation.
#' @param iter integer, represents the maximum of iterations.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @importFrom methods new
#' @importFrom methods missingArg
#' @importFrom markovchain period
#' @importFrom markovchain is.irreducible
#' @export LimDist

LimDist <- function(X,rate,epsilon=0.01,iter){
  if (missingArg(X)) {
    stop("The X argument is missing.")
  }
  if (missingArg(rate)){
    stop("The 'rate' argument is missing.")
  }
  if (missingArg(iter))
      iter <- 1000

  if(rate==TRUE){
    rs <- rowSums(X)
    r <- max(rs)
    P_hat <- X/r
    n <- dim(X)[1]
    for(j in 1:n){
      P_hat[j,j]<- 1 - (rs[j]/r)
    }
    P <-  methods::new("markovchain", transitionMatrix= P_hat)
    output <- P[1:dim(X)[1]]
    if(markovchain::is.irreducible(P) & markovchain::period(P)==1){
      j <- 2
      condition <- 1
      while(j < iter & condition > epsilon){
        Pmas <- output
        output <- Pmas%*%output
        condition <- max(abs(output - Pmas))
        j <- j+1
      }
      output <- list(indicator=rate,message="This is an irreducible Continuous Markov Chain.", Lim_dist = output[1,],error = condition,n_iter=j,P_hat = P[1:dim(X)[1]])
      class(output) <- "modesto"
      return(output)
    }
    else{
      return(c(indicator=!rate,message="This is not an irreducible Continuous Markov Chain."))
    }
  }
  if(rate==FALSE){
    P <- new("markovchain", transitionMatrix= X)
    output <- P[1:dim(X)[1]]
    if(markovchain::is.irreducible(P) & markovchain::period(P)==1){
      j <- 2
      condition <- 1
      while(j < iter & condition > epsilon){
        Pmas <- output
        output <- Pmas%*%output
        condition <- max(abs(output - Pmas))
        j <- j+1
      }
      output <- list(indicator=!rate,message="This is an irreducible Continuous Markov Chain.", Lim_dist = output[1,],error = condition, n_iter=j,P_hat = P[1:dim(X)[1]])
      class(output) <- "modesto"
      return(output)
    }
    else{
      output <- list(indicator=rate,message="This is not an irreducible Continuous Markov Chain.")
      class(output) <- "modesto"
      return(output)
    }
  }
}
