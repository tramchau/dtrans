#' Function creating a transformer object from a dataset using PCA algorithm.
#'
#' This function return a transformer object fitted by data.
#' @param component positive number of components for the transformed data.
#' @param center boolean value to scale data. Parameter is passed to base::scale.
#' @param scaling boolean value to scale data. Parameter is passed to base::scale.
#' @param handle_discrete boolean value to handle discrete features in data.
#'
#' @details
#' This calculation is created based on stat::prcomp function with some adjustments to fit into the purpose of the package. It creates a transformer object including several attribute to perform other functionality.
#'
#' @return transformer.pca return a class "transformer" containing the following componenets:
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.pca(iris[,1:4])
#'
transformer.pca <-
  function(x, component = 2, center = TRUE, scaling = FALSE, handle_discrete = NULL, ...)
  {
    #chkDots(...)
    x <- as.matrix(x)
    x <- scale(x, center = center, scale = scaling)
    cen <- attr(x, "scaled:center")
    sc <- attr(x, "scaled:scale")
    if(any(sc == 0))
      stop("cannot rescale a constant/zero column to unit variance")
    s <- svd(x, nu = 0, nv = component)
    j <- seq_len(component)
    s$d <- s$d / sqrt(max(1, nrow(x) - 1))

    dimnames(s$v) <- list(colnames(x), paste0("PC", j))
    r <- list(x = x %*% s$v,
              sdev = s$d,
              coef = s$v,
              center = cen, # %||% FALSE,
              scale  = sc, #  %||% FALSE)
              technique = "pca")

    class(r) <- "transformer"
    r
  }

