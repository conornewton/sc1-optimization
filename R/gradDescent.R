# Author: Conor Newton (conornewton@gmail.com)

#' Finds the location of a local minimum of a function using gradient descent.
#'
#' @param f A real valued differentiable function.
#' @param x A initial estimate for the local minimum.
#' @param grad_f The gradient of f (Optional).
#' @return A local minimum of the function f.
#' @export
grad_descent <- function(f, x, grad_f) {
    n <- 10000 # Number of steps TODO: fix this
    step_size <- 0.001 # TODO: determine this at each stage

    if (missing(grad_f)) {
        # TODO: Find the gradient numerically
        return(0)
    }

    for (i in 1:n) {
        x <- x - step_size * grad_f(x)
    }

    return(x)
}
