#' Function creating a transformer object by PCA algorithm.
#'
#' This function return a transformer object for data.
#' @param components positive number of components for the transformed data.
#' @param scale boolean value to scale transformed data to the original data range.
#' @param discrete boolean value to handle discrete features in data.
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.pca(iris[,1:4])
#'
transformer.pca <- function (x, ...) UseMethod("transformer.pca")

transformer.pca.default <-
  function(x, component = 2, center = TRUE, scale. = FALSE, tol = NULL, ...)
  {
    chkDots(...)
    x <- as.matrix(x)
    x <- scale(x, center = center, scale = scale.)
    cen <- attr(x, "scaled:center")
    sc <- attr(x, "scaled:scale")
    if(any(sc == 0))
      stop("cannot rescale a constant/zero column to unit variance")
    n <- nrow(x)
    p <- ncol(x)
    k <- .assign_k(components, n, p)
    s <- svd(x, nu = 0, nv = k)
    j <- seq_len(k)
    s$d <- s$d / sqrt(max(1, n - 1))
    if (!is.null(tol)) {
      ## we get rank at least one even for a 0 matrix.
      rank <- sum(s$d > (s$d[1L]*tol))
      if (rank < k) {
        j <- seq_len(k <- rank)
        s$v <- s$v[,j , drop = FALSE]
      }
    }
    dimnames(s$v) <- list(colnames(x), paste0("PC", j))
    r <- list(sdev = s$d, coef = s$v,
              center = cen, # %||% FALSE,
              scale  = sc, #  %||% FALSE)
              technique = "pca")
    r$x <- x %*% s$v

    class(r) <- "transformer"
    r
  }
