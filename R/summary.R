#' summary.modesto
#'
#' summary.modesto displays the summary of the calculated quantities from an object of class 'modesto'.
#' @param object an object of the class modesto. This object is returned from the call to LimDist() function.
#' @param ... other arguments.
#' @example
#' model <-LimDist(matrix(c(0,2,3,0),2,2,byrow=TRUE),rate=TRUE,epsilon=0.005) # A two states CTMC example
#' summary(model)
#' @export
summary.modesto <- function(object, ...) {
  if (object$indicator == TRUE) {
    cat(" ----------------------------------------------------------\n")
    cat("         Continuous Time Markov Chain Model, CTMC \n")
    cat(" ----------------------------------------------------------\n")
    cat(" Status: ", object$indicator, "\n")
    cat("",object$message,"\n")
    cat("\n ------------------------ Results  ---------------------- \n\n")
    cat(" Limiting distribution: ", object$Lim_dist, "\n")
    cat(" Error: ", object$error, "\n")
    cat(" Number of iterations: ", object$n_iter, "\n")
   }

  if (object$indicator == FALSE) {
    cat(" -------------------------------------------------------------- ")
    cat("\n Continuous Time Markov Chain Model, CTMC \n")
    cat(" --------------------------------------------------------------\n")
    cat(" Status: ", object$indicator, "\n")
    print(object$message)
  }
}
