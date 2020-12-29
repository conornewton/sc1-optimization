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

test_that("different step method", {
    f <- function(x) x[1]^2 + x[2]^2 + 3 * x[1] + 2 * x[2] + 9
    expect_equal(grad_descent(f, c(0, 0), step_method = "BB"),
                 c(-1.5, -1),
                 tolerance = 1e-4)
    expect_equal(grad_descent(f, c(0, 0), step_method = "BLS"),
                 c(-1.5, -1),
                 tolerance = 1e-4)
    expect_equal(grad_descent(f, c(0, 0), step_method = 0.01),
                 c(-1.5, -1),
                 tolerance = 1e-4)
})

test_that("rosebrock function", {
    f <- function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2

    expect_equal(grad_descent(f, c(0, 0), n = 100000, step_method = "BB"),
                 c(1, 1),
                 tolerance = 1e-2)
    expect_equal(grad_descent(f, c(0, 0), n = 100000, step_method = "BLS"),
                 c(1, 1),
                 tolerance = 1e-2)
})
