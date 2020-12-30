# Author: Conor Newton (conornewton@gmail.com)

# TODO: choose a more precise method for determining step_size

#' Finds the location of a local minimum of a function using stochastic gradient descent.
#' @param f A real valued differentiable function
#' @param data The data set to b
#' @param w Initial guess for a local minimum of f
#' @return An estimate for a local minimum of \sum f.
#' @export
stoc_grad_descent <- function(f, data, w, step_size = 0.0001) {
    #  TODO: verify the length of w

    set.seed(NULL)
    rows <- sample(nrow(data)) # Permutations of rows

    for (i in 1:nrow(data)) {
        print(data[rows[i], 1:(ncol(data)-1)])
        print(data[rows[i], ncol(data)])
        w <- w - step_size * stoc_num_grad(f, w, data[rows[i], 1:(ncol(data)-1)], data[rows[i], ncol(data)])
    }

    print(w)
    return(w)
}

stoc_num_grad <- function(f, w, x, y) {
    # Need this to convert from a data frame
    x <- as.numeric(x)
    y <- as.numeric(y)

    delta <- 0.00001
    gen_f <- function(k) {
        delta_e_k <- rep(0, length(w))
        delta_e_k[k] <- delta
        return((f(w + delta_e_k, x, y) - f(w - delta_e_k, x, y)) / (2 *  delta))
    }
    return(sapply(seq_len(length(w)), gen_f))
}
