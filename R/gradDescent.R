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
        # Finds the gradient numerically using finite differencing
        delta <- 0.00001
        grad_f <- function(x) {
            gen_f <- function(k) {
                delta_e_k <- rep(0, length(x))
                delta_e_k[k] <- delta
                return((f(x + delta_e_k) - f(x)) / delta)
            }
            return(sapply(seq_len(length(x)), gen_f))
        }
    }

    for (i in 1:n) {
        x <- x - step_size * grad_f(x)
    }

    return(x)
}
