# Author: Conor Newton (conornewton@gmail.com)

# TODO: refactor code

#' Finds the location of a local minimum of a function using gradient descent.
#'
#' @param f A real valued differentiable function.
#' @param x A initial estimate for the local minimum.
#' @param grad_f A function that calulates gradient f at a given point (optional).
#' @param n The maximum number of iterations (default 1000)
#' @param tol Stop searching when the square of the magnitude of the gradient is less than tol (default 1e-7)
#' @param step_method The function to be used to calculate the step size on each iteration (default BLS)
#' @return A local minimum of the function f.
#' @export
grad_descent <- function(f, x, grad_f, n = 999, tol = 1e-7, step_method = "BB") {

    if (missing(grad_f)) {
        # Finds the gradient numerically using finite differencing
        delta <- 0.00001
        grad_f <- function(x) {
            gen_f <- function(k) {
                delta_e_k <- rep(0, length(x))
                delta_e_k[k] <- delta
                return((f(x + delta_e_k) - f(x - delta_e_k)) / (2 *  delta))
            }
            return(sapply(seq_len(length(x)), gen_f))
        }
    }

    if (step_method == "BLS") {
        # Backtracking line search to calculate step size
        step_size <- function() {
            beta <- 0.5
            step <- 1
            t <-  0.5 * sum(grad_f(x) * grad_f(x))
            while (f(x) - f(x - step * grad_f(x)) < step * t) {
                step <- beta * step
            }
            return(step)
        }
    }
    else if (step_method == "BB") {
        # Barzilai-Borwein Method to detemine new step size
        step_size <- function() {
            # If first iteration we do not have an old_x value to use
            if (i == 1) {
                old_x <- x
                print("hello")
                return(0.00001)
            }
            grad_diff <- (grad_f(x) - grad_f(old_x))
            print(grad_diff)
            step <- sum((x - old_x) * grad_diff) / (sum(grad_diff * grad_diff))
            old_x <- x
            print(step)
            return(step)
        }
    }
    else {
        # Constant step-size
        step_size <- function() 0.00001
    }

    old_x <- x

    for (i in 1:n) {
        # Stop if the gradient is sufficiently small
        if (sum(grad_f(x) * grad_f(x)) < tol) return(x)
        x <- x - step_size() * grad_f(x)
    }
    return(x)
}
