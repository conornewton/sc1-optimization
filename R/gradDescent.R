# Author: Conor Newton (conornewton@gmail.com)

# TODO: Fix norm calculations

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
grad_descent <- function(f, x, grad_f, n = 999, tol = 1e-7, step_method = "BLS") {
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
        return(bls(f, x, grad_f, n, tol))
    }
    else if (step_method == "BB") {
        # Barzilai-Borwein Method to detemine new step size
        return(bb(f, x, grad_f, n, tol))
    }
    else if (is.numeric(step_method) && length(step_method) == 1) {
        # Constant step-size
        return(con(f, x, grad_f, n, tol, step_method))
    }
    else {
        # Default constant step-size
        warning("step_method argument invalid, defaulted to a constant step_size of 0.00001")
        return(con(f, x, grad_f, n, tol, 0.00001))
    }

}

bls <- function(f, x, grad_f, n, tol) {
    for (i in 1:n) {
        # Stop if the gradient is sufficiently small
        if (sum(grad_f(x) * grad_f(x)) < tol) return(x)

        beta <- 0.5
        step_size <- 1
        t <-  0.5 * sum(grad_f(x) * grad_f(x))
        while (f(x) - f(x - step_size * grad_f(x)) < step_size * t) {
            step_size <- beta * step_size
        }

        x <- x - step_size * grad_f(x)
    }
    return(x)
}

bb <- function(f, x, grad_f, n, tol) {
    step_size <- 0.00001
    for (i in 1:n) {
        # Stop if the gradient is sufficiently small
        if (sum(grad_f(x) * grad_f(x)) < tol) return(x)

        old_x <- x
        x <- x - step_size * grad_f(x)

        grad_diff <- (grad_f(x) - grad_f(old_x))
        step_size <- sum((x - old_x) * grad_diff) / (sum(grad_diff * grad_diff))
    }
    return(x)

}

con <- function(f, x, grad_f, n, tol, step_size) {
    for (i in 1:n) {
        # Stop if the gradient is sufficiently small
        if (sum(grad_f(x) * grad_f(x)) < tol) return(x)
        x <- x - step_size * grad_f(x)
    }
    return(x)
}
