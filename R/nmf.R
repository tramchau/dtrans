#' Function creating a transformer object by NMF algorithm
#'
#' This function create a transformer object based on the dataset. There are the other settings to preprocessing this dataset before creating the object.
#' @param components whole number greater than 0 set the target reduced dimensions for the transformed data, default is 2.
#' @param center boolean value or a numeric vector corresponding to dataset's columns. Parameter is passed to base::scale.
#' @param scaling boolean value or a numeric vector corresponding to dataset's columns. Parameter is passed to base::scale.
#' @param handle_discrete boolean value to handle discrete features in data.
#' @param max_iter positive number to limit the iteration, default is 1000.
#'
#' @details
#' This calculation is created based on NMFN::nnmf function (Lee et al. 2001 based on Euclidean distance). It creates a transformer object including the transformed data and other attributes of extra information regarding to the tranforming process by the algorithm.
#'
#' @return trans_nmf return a list with class "transformer" containing the following components:
#' \item{x}{transformed data.}
#' \item{center}{value of center using for scaling data.}
#' \item{scale}{value of scale using for scaling data.}
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.nmf(iris[,1:4])
#'

transformer.nmf <- function (x, components=2, center = FALSE, scaling = FALSE, handle_discrete = FALSE, max_iter = 1000) {
  # Validate input
  .validate_instantiate_input(x, components, center, scaling, handle_discrete)

  # extra input for nmf
  if (!is.numeric(max_iter) | length(max_iter) > 1 | ceiling(max_iter) != max_iter)
    stop("max_iter should be an positive interger number.")

  x <- as.matrix(x)

  if (any(x<0)) stop("Negative value in x")

  x <- scale(x, center = center, scale = scaling)
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")

  if (any(x<0) & center == TRUE) stop("Negative value in x due to center parameter")

  if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
  dx <- dim(x)
  n <- dx[1L]
  m <- dx[2L]
  if (!n || !m) stop("0 extent dimensions")
  W <- matrix(abs(rnorm(n * components)), n, components)
  H <- matrix(abs(rnorm(components * m)), components, m)
  ret <- .optimize_WH(x, W, H)
  H <- ret$H
  W <- ret$W
  HT <- t(H)
  colnames(HT) <- paste("PC", 1:components, sep="")
  colnames(W) <- paste("PC", 1:components, sep="")

  if (is.null(sc)) sc <- FALSE
  if (is.null(cen)) cen <- FALSE

  z <- c(list(x = W,
              coef = HT,
              sdev = NULL,
              explained_var = NULL,
              components = components,
              center = cen,
              scale = sc,
              technique = "nmf",
              others = list(eucl_dist=ret$eucl_dist,
                            relative_err=ret$relative_err,
                            stop_iter=ret$stop_iter)
              )
              #handle_discrete = handle_discrete)
  )
  class(z) <- "transformer"
  z
}

.optimize_WH <- function(x, W, H, update_H=TRUE, max_iter = 1000) {
  eps <- 2.2204e-16

  err <- c()
  for (iter in 1:max_iter) {
    if (update_H)
      H <- H * (t(W) %*% x)/((t(W) %*% W) %*% H + eps)

    W <- W * t(H %*% t(x))/(W %*% (H %*% t(H)) + eps)

    # errorx <- mean(abs(x - W %*% H))/mean(x)
    # err <- c(err, errorx)
    # if (iter > 100) {
    #   if (round(mean(err[(iter-100):(iter-1)]), 8) == round(errorx, 8)) {
    #     message("Converge at iteration = ", iter, "\n")
    #     break
    #   }
    # }
    eucl_dist <- .distance2(x, W %*% H)
    errorx <- mean(abs(x - W %*% H))/mean(x)

    if (errorx < 1e-05) {
      cat("Execution finishes at iteration = ", iter, "\n")
      break
    }
  }

  ret <- c(list(W = W, H = H, eucl_dist=eucl_dist, relative_err=errorx, stop_iter=iter))
  ret
}

.distance2 <- function (x1, x2)
{
  temp <- x1 - x2
  sum((temp) * temp)
}
