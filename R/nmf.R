#' Function creating a transformer object by NMF algorithm
#'
#' This function create a transformer object based on the dataset. There are the other settings to preprocessing this dataset before creating the object.
#' @param components whole number greater than 0 set the target reduced dimensions for the transformed data, default is 2.
#' @param center logical value or a numeric vector corresponding to dataset's columns. Parameter is passed to base::scale.
#' @param scaling logical value or a numeric vector corresponding to dataset's columns. Parameter is passed to base::scale.
#' @param handle_category character value to handle categorical features. The accepted values are 'label', 'onehot', and 'ignore'. Default value is NULL, if dataset contains character fields, the function return error. .
#' @param max_iter positive number to limit the iteration, default is 1000.
#'
#' @details
#' The function is created based on NMFN::nnmf function (Lee et al. 2001 based on Euclidean distance) with some adjustments to fit into the purpose of the package. It includes preprocessing data (scaling, categorical handling) before transforming data. It creates a transformer object with attributes to perform other functionalities.
#'
#' @return Return "transformer" class.
#'
#' @export
#' @examples
#' data(iris)
#' nmf <- transformer.nmf(iris[,1:4])
#' print(nmf)

transformer.nmf <- function (x, components=2, center = FALSE, scaling = FALSE, handle_category = NULL, max_iter = 1000) {
  # Validate input
  .validate_input(x, components, center, scaling, handle_category)
  
  if (!(is.null(handle_category))) {
    if (any(sapply(x, is.factor)) | any(sapply(x, is.character)))
      x <- .handle_category(x, handle_category)
    else handle_category <- NULL
  }
  
  # extra input for nmf
  if (length(max_iter) > 1)
    stop("max_iter should be a positive interger number.")
  if (!is.numeric(max_iter))
    stop("max_iter should be a positive interger number.")
  else {
    if (ceiling(max_iter) != max_iter | max_iter < 1)
      stop("max_iter should be a positive interger number.")
  }

  x <- as.matrix(x)

  # if (any(x<0)) stop("Negative value in x")

  x <- scale(x, center = center, scale = scaling)
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")

  if (any(x<0)) {
    if(length(center) ==1)
      if (center == TRUE) stop("NMF is not for negative data. (Negative value in x due to center=TRUE)")
      else stop("NMF is not for negative data. ('x' contains egative values)")
    else if (length(center) > 1) stop("NMF is not for negative data. (Negative value in x due to center parameter setting)")
    else stop("NMF is not for negative data. ('x' contains egative values)")
  }
  if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
  dx <- dim(x)
  n <- dx[1L]
  m <- dx[2L]
  if (!n || !m) stop("0 extent dimensions")
  W <- matrix(abs(rnorm(n * components)), n, components)
  H <- matrix(abs(rnorm(components * m)), components, m)
  # ret <- .optimize_WH(x, W, H, update_H=TRUE, max_iter)
  ret <- .Call('_dtrans_rcpp_optimize_WH', x, W, H, TRUE, max_iter)
  H <- ret$H
  W <- ret$W
  HT <- t(H)
  colnames(HT) <- paste("PC", 1:components, sep="")
  colnames(W) <- paste("PC", 1:components, sep="")

  if (is.null(sc)) sc <- FALSE
  if (is.null(cen)) cen <- FALSE

  z <- c(list(x = W,
              components = components,
              center = cen,
              scale = sc,
              handle_category = handle_category,
              technique = "nmf",
              fit_data = x,
              others = list(coef = HT,
                            eucl_dist=ret$eucl_dist,
                            relative_err=ret$relative_err,
                            stop_iter=ret$stop_iter)
              )
              #handle_category = handle_category)
  )
  class(z) <- "transformer"
  z
}

transformer.nmf.perf <- function (x, components=2, center = FALSE, scaling = FALSE, handle_category = NULL, max_iter = 1000, W, H, type='baser') {
  # Validate input
  .validate_input(x, components, center, scaling, handle_category)
  
  # extra input for nmf
  if (!is.numeric(max_iter) | length(max_iter) > 1 | ceiling(max_iter) != max_iter)
    stop("max_iter should be a positive interger number.")
  
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
  # W <- matrix(abs(rnorm(n * components)), n, components)
  # H <- matrix(abs(rnorm(components * m)), components, m)
  if (type == "baser") ret <- .optimize_WH(x, W, H, update_H=TRUE, max_iter)
  else ret <- .Call('_dtrans_rcpp_optimize_WH', x, W, H, TRUE, max_iter)

  H <- ret$H
  W <- ret$W
  HT <- t(H)
  colnames(HT) <- paste("PC", 1:components, sep="")
  colnames(W) <- paste("PC", 1:components, sep="")
  
  if (is.null(sc)) sc <- FALSE
  if (is.null(cen)) cen <- FALSE
  
  z <- c(list(x = W,
              components = components,
              center = cen,
              scale = sc,
              handle_category = handle_category,
              technique = "nmf",
              fit_data = x,
              others = list(coef = HT,
                            eucl_dist=ret$eucl_dist,
                            relative_err=ret$relative_err,
                            stop_iter=ret$stop_iter)
              )
         )
  class(z) <- "transformer"
  z
}

.optimize_WH <- function(x, W, H, update_H=TRUE, max_iter = 1000) {
  eps <- 2.2204e-16

  for (iter in 1:max_iter) {
    if (update_H) {
      H <- H * (t(W) %*% x)/((t(W) %*% W) %*% H + eps)
    }

    W <- W * t(H %*% t(x))/(W %*% (H %*% t(H)) + eps)

    errorx <- mean(abs(x - W %*% H))/mean(x)

    if (errorx < 1e-05) {
      # cat(paste0("Execution finishes at iteration = ", iter, 
      #            " before the maximum iteration as optimizing error < 1e-05"))
      break
    }
  }
  eucl_dist <- .distance2(x, W %*% H)

  ret <- c(list(W = W, H = H, eucl_dist=eucl_dist, relative_err=errorx, stop_iter=iter))
  ret
}

.distance2 <- function (x1, x2)
{
  temp <- x1 - x2
  sum((temp) * temp)
}
