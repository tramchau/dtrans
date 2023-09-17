#' Function creating a transformer object by NMF algorithm (Lee et al. 2001 based on Euclidean distance)
#'
#' This function return a transformer object fitted by data.
#' @param component positive number of components for the transformed data, default is 2.
#' @param center boolean value to scale data. Parameter is passed to base::scale.
#' @param scaling boolean value to scale data. Parameter is passed to base::scale.
#' @param handle_discrete boolean value to handle discrete features in data.
#' @param max_iter positive number to limit the iteration, default is 1000.
#'
#' @details
#' This calculation is created based on NMFN::nmf function with some adjustments to fit into the purpose of the package. It creates a transformer object including several attributes to perform other functionalities.
#'
#' @return transformer.nmf return a class "transformer" containing the following components:
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.nmf(iris[,1:4])
#'
transformer.nmf <- function (x, ...) UseMethod("transformer.nmf")

transformer.nmf.default <-
  function (x, components=2, center = TRUE, scaling = FALSE, handle_discrete = NULL, max_iter = 1000)
{
  # method = "nnmf_mm"
  eps <- 2.2204e-16
  print_iter <- 50
  x <- as.matrix(x)
  if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
  dx <- dim(x)
  n <- dx[1L]
  m <- dx[2L]
  if (!n || !m) stop("0 extent dimensions")
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
              prediction=list(iter = max_iter))
  )
  class(z) <- "transformer"
  z
}

.distance2 <- function (x1, x2)
{
  temp <- x1 - x2
  sum(temp * temp)
}
