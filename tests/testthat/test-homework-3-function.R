x1 <- bis620_sparse_matrix(
  i = c(1, 2, 5, 6),
  j = c(2, 2, 6, 1),
  x = c(4.3, 5.6, 7, 10)
)

set.seed(1)

x2 <- matrix(rnorm(36), ncol = 6)

library(Matrix)

set.seed(1)

x3 <- Matrix(rnorm(36), ncol = 6)

x4 <- sparseMatrix(
  i = c(1, 1, 3, 6),
  j = c(2, 3, 5, 1),
  x = c(4.3, 5.6, 7, 10),
  dims = c(6, 6)
)

test_that("The dense_to_sparse function works", {
  expect_true(inherits(dense_to_sparse(x2), "data.frame"))
})

test_that("The + function works", {
  expect_true(inherits(x1+x1, "bis620_sparse_matrix"))
})

test_that("The + function works", {
  expect_true(inherits(x1+x2, "bis620_sparse_matrix"))
})

test_that("The + function works", {
  expect_true(inherits(x3+x1, "bis620_sparse_matrix"))
})

test_that("The + function works", {
  expect_true(inherits(x1+x4, "bis620_sparse_matrix"))
})

test_that("The - function works", {
  expect_true(inherits(x1-x1, "bis620_sparse_matrix"))
})

test_that("The - function works", {
  expect_true(inherits(x1-x2, "bis620_sparse_matrix"))
})

test_that("The - function works", {
  expect_true(inherits(x1-x3, "bis620_sparse_matrix"))
})

test_that("The - function works", {
  expect_true(inherits(x1-x4, "bis620_sparse_matrix"))
})

test_that("The * function works", {
  expect_true(inherits(x1*x1, "bis620_sparse_matrix"))
})

test_that("The * function works", {
  expect_true(inherits(x1*x2, "bis620_sparse_matrix"))
})

test_that("The * function works", {
  expect_true(inherits(x1*x3, "bis620_sparse_matrix"))
})

test_that("The * function works", {
  expect_true(inherits(x1*x4, "bis620_sparse_matrix"))
})

test_that("The / function works", {
  expect_true(inherits(x1/x1, "bis620_sparse_matrix"))
})

test_that("The / function works", {
  expect_true(inherits(x1/x2, "bis620_sparse_matrix"))
})

test_that("The / function works", {
  expect_true(inherits(x1/x3, "bis620_sparse_matrix"))
})

test_that("The / function works", {
  expect_true(inherits(x1/x4, "bis620_sparse_matrix"))
})

test_that("The %*% function works", {
  expect_true(inherits(x1%*%x1, "bis620_sparse_matrix"))
})

test_that("The %*% function works", {
  expect_true(inherits(x1%*%x2, "bis620_sparse_matrix"))
})

test_that("The %*% function works", {
  expect_true(inherits(x1%*%x3, "bis620_sparse_matrix"))
})

test_that("The %*% function works", {
  expect_true(inherits(x1%*%x4, "bis620_sparse_matrix"))
})

test_that("The %*% function works", {
  expect_true(inherits(x4%*%x1, "bis620_sparse_matrix"))
})

test_that("The print.bis620_sparse_matrix function works", {
  expect_true(inherits(print(x1), "dgCMatrix"))
})
