#' Function creating a transformer object by NMF algorithm (Lee et al. 2001 based on Euclidean distance)
#'
#' This function return a transformer object for data.
#' @param components positive number of components for the transformed data, default is 2.
#' @param discrete boolean value to handle discrete features in data.
#' @param max_iter positive number to limit the iteration, default is 1000.
#' @param eps positive small float number to define the residual.
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.nmf(iris[,1:4])
#'
transformer.nmf <- function (x, ...) UseMethod("transformer.nmf")

transformer.nmf.default <- function (x, components=2, method = "nnmf_mm", max_iter = 1000, eps = 2.2204e-16)
{
  if (method == "nnmf_als") { # Kim et al. 2007
    cat("Alternating Least Squares Algorithm", "\n")
    nnmf_als(x, components, max_iter, eps)
  }
  else if (method == "nnmf_prob") {
    cat("Multinomial Algorithm", "\n")
    nnmf_prob(x, components, max_iter, eps)
  }
  else { # Lee et al. 2001 based on Euclidean distance
    cat("Multiplicative Update Algorithm (Default)", "\n")
    .nnmf_mm(x, components, max_iter, eps)
  }
}

.nnmf_mm <- function (x, components=NULL, max_iter, eps)
{
  print_iter <- 50
  x <- as.matrix(x)
  if (any(!is.finite(x)))
    stop("infinite or missing values in 'x'")
  dx <- dim(x)
  n <- dx[1L]
  m <- dx[2L]
  if (!n || !m)
    stop("0 extent dimensions")
  components <- .assign_k(components, n, m)
  W <- matrix(abs(rnorm(n * components)), n, components)
  H <- matrix(abs(rnorm(components * m)), components, m)
  Xr_old = W %*% H
  for (iter in 1:max_iter) {
    H = H * (t(W) %*% x)/((t(W) %*% W) %*% H + eps)
    W = W * t(H %*% t(x))/(W %*% (H %*% t(H)) + eps)
    if (iter%%print_iter == 0 & FALSE) { # turn off print
      Xr = W %*% H
      diff = sum(abs(Xr_old - Xr))
      Xr_old = Xr
      eucl_dist = distance2(x, W %*% H)
      errorx = mean(abs(x - W %*% H))/mean(x)
      cat("Iter = ", iter, "\t")
      cat("relative error = ", errorx, "\t")
      cat("diff = ", diff, "\t")
      cat("eucl dist = ", eucl_dist, "\n")
      if (errorx < 1e-05) {
        cat("Execution finishes at iteration = ", iter,
            "\n")
        break
      }
    }
  }
  n_comps <- nrow(H)
  HT <- t(H)
  colnames(HT) <- paste("PC", 1:n_comps, sep="")
  colnames(W) <- paste("PC", 1:n_comps, sep="")

  z <- c(list(x = W, coef = HT, sdev = NULL,
              scale = NULL, center = NULL,
              technique = "nmf",
              prediction=list(method = method, iter = max_iter, eps = eps))
              )
  class(z) <- "transformer"
  z
}

.distance2 <- function (x1, x2)
{
  temp <- x1 - x2
  sum(temp * temp)
}

.assign_k <- function(components, n, m)
{
  if(!is.null(components)) {
    stopifnot(length(components) == 1, is.finite(components), as.integer(components) > 0)
    return (min(as.integer(components), n, m))
  } else
    return (min(n, m))
}
