## 1
#' @title bis620_sparse_matrix
#'
#' @rdname bis620_sparse_matrix-class
#' @export
bis620_sparse_matrix <- setClass(Class = "bis620_sparse_matrix",
                                 representation(i = "numeric",
                                                j = "numeric",
                                                x = "numeric"))

## 2

############################## `+` ##############################

# change a dense matrix to sparse matrix

#' @title Convert dense matrix to data frame
#'
#' @description Convert a dense matrix to a data frame with three columns.
#' @param dense A dense matrix.
#' @export
dense_to_sparse <- function(dense){
  i <- c()
  j <- c()
  x <- c()
  for (col in 1:ncol(dense)){
    for (row in 1:nrow(dense)){
      if(dense[row,col]!=0){
        i <- c(i,row)
        j <- c(j,col)
        x <- c(x,dense[row,col])
      }
    }
  }
  data.frame(i=i,j=j,x=x)
}

# `+` for bis620 sparse matrix and bis620 sparse matrix

#' @title Plus operator performs on two matrix
#'
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @importFrom methods new
#' @rdname plus
#' @export
setMethod(
  "+",
  c(e1="bis620_sparse_matrix",e2="bis620_sparse_matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- data.frame(i=e2@i,j=e2@j,x=e2@x)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x + c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `+` for bis620 sparse matrix and dense matrix

#' @param e1 bis620_sparse_matrix
#' @param e2 matrix
#' @rdname plus
#' @export
setMethod(
  "+",
  c(e1="bis620_sparse_matrix",e2="matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x + c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `+` for dgeMatrix and bis620 sparse matrix

#' @param e1 dgeMatrix
#' @param e2 bis620_sparse_matrix
#' @rdname plus
#' @export
setMethod(
  "+",
  c(e1="dgeMatrix",e2="bis620_sparse_matrix"),
  function(e1, e2) {
    e1 <- dense_to_sparse(e1)
    e2 <- data.frame(i=e2@i,j=e2@j,x=e2@x)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x + c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `+` for bis620 sparse matrix and dgCMatrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname plus
#' @export
setMethod(
  "+",
  c(e1="bis620_sparse_matrix",e2="dgCMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x + c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

############################## `-` ##############################

# `-` for bis620 sparse matrix and bis620 sparse matrix

#' @title Minus operator performs on two matrix
#'
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname minus
#' @export
setMethod(
  "-",
  c(e1="bis620_sparse_matrix",e2="bis620_sparse_matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- data.frame(i=e2@i,j=e2@j,x=e2@x)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x - c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `-` for bis620 sparse matrix and dense matrix

#' @param e1 bis620_sparse_matrix
#' @param e2 matrix
#' @rdname minus
#' @export
setMethod(
  "-",
  c(e1="bis620_sparse_matrix",e2="matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x - c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `-` for bis620 sparse matrix and dgeMatrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dgeMatrix
#' @rdname minus
#' @export
setMethod(
  "-",
  c(e1="bis620_sparse_matrix",e2="dgeMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x - c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `-` for bis620 sparse matrix and dgCMatrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dgCmatrix
#' @rdname minus
#' @export
setMethod(
  "-",
  c(e1="bis620_sparse_matrix",e2="dgCMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x - c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

############################## `*` ##############################

# `*` for bis620 sparse matrix and bis620 sparse matrix

#' @title Multiplication operator performs on two matrix
#'
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname multiplication
#' @export
setMethod(
  "*",
  c(e1="bis620_sparse_matrix",e2="bis620_sparse_matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- data.frame(i=e2@i,j=e2@j,x=e2@x)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x * c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `*` for bis620 sparse matrix and dense matrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dense matrix
#' @rdname multiplication
#' @export
setMethod(
  "*",
  c(e1="bis620_sparse_matrix",e2="matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x * c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `*` for bis620 sparse matrix and dgeMatrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dgeMatrix
#' @rdname multiplication
#' @export
setMethod(
  "*",
  c(e1="bis620_sparse_matrix",e2="dgeMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x * c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `*` for bis620 sparse matrix and dgCMatrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname multiplication
#' @export
setMethod(
  "*",
  c(e1="bis620_sparse_matrix",e2="dgCMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x * c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

############################## `/` ##############################

# `/` for bis620 sparse matrix and bis620 sparse matrix

#' @title Division operator performs on two matrix
#'
#' @param e1 bis620_sparse_matrix
#' @param e2 bis620_sparse_matrix
#' @rdname division
#' @export
setMethod(
  "/",
  c(e1="bis620_sparse_matrix",e2="bis620_sparse_matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- data.frame(i=e2@i,j=e2@j,x=e2@x)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x / c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `/` for bis620 sparse matrix and dense matrix

#' @param e1 bis620_sparse_matrix
#' @param e2 matrix
#' @rdname division
#' @export
setMethod(
  "/",
  c(e1="bis620_sparse_matrix",e2="matrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x / c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `/` for bis620 sparse matrix and dgeMatrix
#' @param e1 bis620_sparse_matrix
#' @param e2 dgeMatrix
#' @rdname division
#' @export
setMethod(
  "/",
  c(e1="bis620_sparse_matrix",e2="dgeMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x / c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `/` for bis620 sparse matrix and dgCMatrix

#' @param e1 bis620_sparse_matrix
#' @param e2 dgCMatrix
#' @rdname division
#' @export
setMethod(
  "/",
  c(e1="bis620_sparse_matrix",e2="dgCMatrix"),
  function(e1, e2) {
    e1 <- data.frame(i=e1@i,j=e1@j,x=e1@x)
    e2 <- dense_to_sparse(e2)
    colnames(e1) <- c("i","j","x")
    colnames(e2) <- c("i","j","x")
    c <- merge(e1, e2, by = c("i", "j"), all = TRUE,
               suffixes = c("", "2"))
    c$x[is.na(c$x)] <- 0
    c$x2[is.na(c$x2)] <- 0
    c$x <- c$x / c$x2
    a <- bis620_sparse_matrix(
             i = c$i,
             j = c$j,
             x = c$x)
    a
  }
)

# `%*%` for bis620 sparse matrix and bis620 sparse matrix

#' @title Matrix multiplication operator performs on two matrix
#'
#' @param x bis620_sparse_matrix
#' @param y bis620_sparse_matrix
#' @rdname matrix_multiplication
#' @export
setMethod(
  "%*%",
  c(x="bis620_sparse_matrix",y="bis620_sparse_matrix"),
  function(x, y) {
    x <- data.frame(i=x@i,j=x@j,x=x@x)
    y <- data.frame(i=y@i,j=y@j,x=y@x)
    w <- c()
    e <- c()
    r <- c()
    for (row in unique(x$i)){
      for (col in unique(y$j)){
        p <- x[x$i == row,]
        q <- y[y$j == col,]
        full <- merge(p,q,by.x="j",by.y="i")
        full$x <- full$x.x*full$x.y
        w <- c(w, row)
        e <- c(e, col)
        r <- c(r, sum(full$x))
      }
    }
    a <- data.frame(i = w, j = e, x = r)
    a <- a[a$x != 0,]
    b <- bis620_sparse_matrix(i = a$i, j = a$j, x = a$x)
  }
)

# `%*%` for bis620 sparse matrix and dense matrix

#' @param x bis620_sparse_matrix
#' @param y matrix
#' @rdname matrix_multiplication
#' @export
setMethod(
  "%*%",
  c(x="bis620_sparse_matrix",y="matrix"),
  function(x, y) {
    x <- data.frame(i=x@i,j=x@j,x=x@x)
    y <- dense_to_sparse(y)
    w <- c()
    e <- c()
    r <- c()
    for (row in unique(x$i)){
      for (col in unique(y$j)){
        p <- x[x$i == row,]
        q <- y[y$j == col,]
        full <- merge(p,q,by.x="j",by.y="i")
        full$x <- full$x.x*full$x.y
        w <- c(w, row)
        e <- c(e, col)
        r <- c(r, sum(full$x))
      }
    }
    a <- data.frame(i = w, j = e, x = r)
    a <- a[a$x != 0,]
    b <- bis620_sparse_matrix(i = a$i, j = a$j, x = a$x)
  }
)

# `%*%` for bis620 sparse matrix and dgeMatrix

#' @param x bis620_sparse_matrix
#' @param y dgeMatrix
#' @rdname matrix_multiplication
#' @export
setMethod(
  "%*%",
  c(x="bis620_sparse_matrix",y="dgeMatrix"),
  function(x, y) {
    x <- data.frame(i=x@i,j=x@j,x=x@x)
    y <- dense_to_sparse(y)
    w <- c()
    e <- c()
    r <- c()
    for (row in unique(x$i)){
      for (col in unique(y$j)){
        p <- x[x$i == row,]
        q <- y[y$j == col,]
        full <- merge(p,q,by.x="j",by.y="i")
        full$x <- full$x.x*full$x.y
        w <- c(w, row)
        e <- c(e, col)
        r <- c(r, sum(full$x))
      }
    }
    a <- data.frame(i = w, j = e, x = r)
    a <- a[a$x != 0,]
    b <- bis620_sparse_matrix(i = a$i, j = a$j, x = a$x)
  }
)

# `%*%` for bis620 sparse matrix and dgCMatrix

#' @rdname matrix_multiplication
#' @param x bis620_sparse_matrix
#' @param y dgCMatrix
#' @export
setMethod(
  "%*%",
  c(x="bis620_sparse_matrix",y="dgCMatrix"),
  function(x, y) {
    x <- data.frame(i=x@i,j=x@j,x=x@x)
    y <- dense_to_sparse(y)
    w <- c()
    e <- c()
    r <- c()
    for (row in unique(x$i)){
      for (col in unique(y$j)){
        p <- x[x$i == row,]
        q <- y[y$j == col,]
        full <- merge(p,q,by.x="j",by.y="i")
        full$x <- full$x.x*full$x.y
        w <- c(w, row)
        e <- c(e, col)
        r <- c(r, sum(full$x))
      }
    }
    a <- data.frame(i = w, j = e, x = r)
    a <- a[a$x != 0,]
    b <- bis620_sparse_matrix(i = a$i, j = a$j, x = a$x)
  }
)

# `%*%` for dgCMatrix and bis620 sparse matrix
#' @param x dgCMatrix
#' @param y bis620_sparse_matrix
#' @rdname matrix_multiplication
#' @export
setMethod(
  "%*%",
  c(x="dgCMatrix",y="bis620_sparse_matrix"),
  function(x, y) {
    x <- dense_to_sparse(x)
    y <- data.frame(i=y@i,j=y@j,x=y@x)
    w <- c()
    e <- c()
    r <- c()
    for (row in unique(x$i)){
      for (col in unique(y$j)){
        p <- x[x$i == row,]
        q <- y[y$j == col,]
        full <- merge(p,q,by.x="j",by.y="i")
        full$x <- full$x.x*full$x.y
        w <- c(w, row)
        e <- c(e, col)
        r <- c(r, sum(full$x))
      }
    }
    a <- data.frame(i = w, j = e, x = r)
    a <- a[a$x != 0,]
    b <- bis620_sparse_matrix(i = a$i, j = a$j, x = a$x)
  }
)

#' @title Transpose for bis620_sparse_matrix
#'
#' @description Transpose function for bis620_sparse_matrix.
#' @param x bis620_sparse_matrix.
#' @return A transposed bis620_sparse_matrix.
#' @rdname transpose
#' @export
setMethod(
  "t",
  c(x="bis620_sparse_matrix"),
  function(x) {
    b <- bis620_sparse_matrix(i = x@j, j = x@i, x = x@x)
    b
  }
)

## 3

# change a dense matrix to sparse matrix

#' @title Print bis620_sparse_matrix
#'
#' @description Print a bis620_sparse_matrix in general matrix layout.
#' @param x A bis620_sparse_matrix.
#' @return A sparse matrix in general matrix layout.
#' @importFrom Matrix sparseMatrix
#' @rdname print_bis620_sparse_matrix
#' @export
setMethod(
  f="print",
  signature(x="bis620_sparse_matrix"),
  function(x) {
    print(sparseMatrix(
      i = x@i,
      j = x@j,
      x = x@x)
    )
  }
)
