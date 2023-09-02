#NMFN::nmfn
#detach("package:NMFN", unload=TRUE)
nmf <- function (x, ...) UseMethod("nmf")

nmf.default <- function (x, k=NULL, method = "nnmf_mm", maxiter = 1000, eps = 2.2204e-16)
{
  if (method == "nnmf_als") { # Kim et al. 2007
    cat("Alternating Least Squares Algorithm", "\n")
    nnmf_als(x, k, maxiter, eps)
  }
  else if (method == "nnmf_prob") {
    cat("Multinomial Algorithm", "\n")
    nnmf_prob(x, k, maxiter, eps)
  }
  else { # Lee et al. 2001 based on Euclidean distance
    cat("Multiplicative Update Algorithm (Default)", "\n")
    nnmf_mm(x, k, maxiter, eps)
  }
}

nnmf_mm <- function (x, k=NULL, maxiter, eps)
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
  k <- assign_k(k, n, m)
  W <- matrix(abs(rnorm(n * k)), n, k)
  H <- matrix(abs(rnorm(k * m)), k, m)
  Xr_old = W %*% H
  for (iter in 1:maxiter) {
    H_bf = H
    W_bf = W
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
              prediction=list(H = H_bf, W = W_bf, eps = eps))
              )
  class(z) <- "transformer"
  z
}

distance2 <- function (x1, x2)
{
  temp <- x1 - x2
  sum(temp * temp)
}

assign_k <- function(k, n, m)
{
  if(!is.null(k)) {
    stopifnot(length(k) == 1, is.finite(k), as.integer(k) > 0)
    return (min(as.integer(k), n, m))
  } else
    return (min(n, m))
}
