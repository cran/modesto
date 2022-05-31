#' summary.modesto
#'
#' summary.modesto displays the summary of calculated quantities from an object of class 'modesto'.
#' @param object an object of the class 'modesto'. This object is returned from the call to LimDist() function.
#' @param ... other arguments.
#' @examples
#' # A two states CTMC example
#' model <-LimDist(matrix(c(0,2,3,0),2,2,byrow=TRUE),rate=TRUE,epsilon=0.005)
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
    cat(" Limiting distribution: ", round(object$Lim_dist,4), "\n")
    cat(" Error: ", round(object$error,4), "\n")
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
