test_that("grad_descent converges to global minimum on a convex function", {
    f <- function(x) x[1]^2 + x[2]^2 + 3 * x[1] + 2 * x[2] + 9
    grad_f <- function(x) c(2 * x[1] + 3, 2 * x[2] + 2)
    expect_equal(grad_descent(f, c(0, 0), grad_f, n = 10),
                 c(-1.5, -1),
                 tolerance = 1e-4)
})

test_that("grad_descent using numerical gradient", {
    f <- function(x) x[1]^2 + x[2]^2 + 3 * x[1] + 2 * x[2] + 9
    expect_equal(grad_descent(f, c(0, 0), n = 10),
                 c(-1.5, -1),
                 tolerance = 1e-4)
})
