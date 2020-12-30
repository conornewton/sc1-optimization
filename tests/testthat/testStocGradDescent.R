test_that("basic sgd MSE tests", {
    f <- function(w, x, y) (sum(w * c(1, x)) - y)^2
    x <- 1:100
    y <- sapply(x, function(x) 20 * x + 2)
    data <- data.frame(x, y)

    expect_equal(stoc_grad_descent(f, data, c(0, 0)),
                 c(2, 20),
                 tolerance = 1)

    f <- function(w, x, y) (sum(w * c(1, x)) - y)^2
    y <- mapply(function(x1, x2) sum(c(20, 1) * c(x1, x2)) + 2, 1:100, 1:100)
    data <- data.frame(1:100, 1:100, y)

    expect_equal(stoc_grad_descent(f, data, c(0, 0, 0)),
                 c(2, 20, 1),
                 tolerance = 1)

    expect_equal(stoc_grad_descent(f, as.matrix(data), c(0, 0, 0)),
                 c(2, 20, 1),
                 tolerance = 1)
})
