#' Tool to computate the limiting distribution for a Continuous Time Markov Chain, CTMC.
#'
#' \code{LimDist} is used to obtain the limiting distribution of a homogeneous continuous time Markov chain.
#' @param X matrix, represents a rate matrix of a CTMC or the transition probability matrix of the DTMC associated to the CTMC.
#' @param rate boolean, if rate is equal to FALSE then the argument X represents the transition probability matrix of the DTMC associated to the CTMC.
#' @references Ross, S, Introduction to Probability Models, Eleven Edition. Academic Press, 2014.
#' @references Kulkarni V, Introduction to modeling and analysis of stochastic systems. Second Edition. Springer-Verlag, 2011.
#' @author Carlos Alberto Cardozo Delgado <cardozorpackages@gmail.com>.
#' @importFrom methods new
#' @importFrom markovchain period
#' @importFrom markovchain is.irreducible
#' @export LimDist

LimDist <- function(X,rate){
  if(rate==TRUE){
    rs <- rowSums(X)
    r <- max(rs)
    P_hat <- X/r
    n <- dim(X)[1]
    for(j in 1:n){
      P_hat[j,j]<- 1 - (rs[j]/r)
    }
    P <-  methods::new("markovchain", transitionMatrix= P_hat)
    iter <- 1001
    epsilon <- 0.01
    output <- P[1:dim(X)[1]]
    if(markovchain::is.irreducible(P) & markovchain::period(P)==1){
      #print("This is a irreducible Continuous Markov Chain.")
      j <- 2
      condition <- 1
      while(j < iter & condition > epsilon){
        Pmas <- output
        output <- Pmas%*%output
        condition <- max(abs(output - Pmas))
        j <- j+1
      }
      return(list("This is a irreducible Continuous Markov Chain.",output,error = condition,niter=j,P_hat = P[1:dim(X)[1]]))
    }
    else{
      #print("This is not a irreducible Continuous Markov Chain.")
      return(c("This is not a irreducible Continuous Markov Chain."))
    }
  }
  if(rate==FALSE){
    iter <- 1001
    epsilon <- 0.01
    P <- new("markovchain", transitionMatrix= X)
    output <- P[1:dim(X)[1]]
    if(markovchain::is.irreducible(P) & markovchain::period(P)==1){
      #print("This is a irreducible Continuous Markov Chain.")
      j <- 2
      condition <- 1
      while(j < iter & condition > epsilon){
        Pmas <- output
        output <- Pmas%*%output
        condition <- max(abs(output - Pmas))
        j <- j+1
      }
      return(list("This is a irreducible Continuous Markov Chain.",output,error = condition, niter=j,P_hat = P[1:dim(X)[1]]))
    }
    else{
      #print("This is not a irreducible Continuous Markov Chain.")
      return(c("This is not a irreducible Continuous Markov Chain."))
    }
  }
}
