#' Function creating a transformer object using PCA algorithm.
#'
#' This function return a transformer object fitted by data.
#' @param components positive number of components for the transformed data.
#' @param center boolean value to scale data. Parameter is passed to base::scale.
#' @param scaling boolean value to scale data. Parameter is passed to base::scale.
#' @param handle_category character value to handle discrete features in data either by 'label' or 'onehot'.
#'
#' @details
#' This calculation is created based on stat::prcomp function with some adjustments to fit into the purpose of the package. It creates a transformer object including several attribute to perform other functionality.
#'
#' @return transformer.pca return a class "transformer" containing the following components:
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.pca(iris[,1:4])

transformer.pca <- function(x, components = 2, center = FALSE, scaling = FALSE, handle_category = NULL) {
  # Validate input
  .validate_input(x, components, center, scaling, handle_category)
  
  if (!(is.null(handle_category)) & any(sapply(x, is.factor)))
    x <- .handle_category(x, handle_category)
  
  x <- as.matrix(x)
  x <- scale(x, center = center, scale = scaling)
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")
  if(any(sc == 0))
    stop("data should not be scaled with the scaling option, because there is at least one constant or zero column in data.")

  s <- svd(x, nu = 0, nv = components)
  s$d <- s$d / sqrt(max(1, nrow(x) - 1))
  explained_var <- s$d^2 / sum(s$d^2)

  j <- seq_len(components)
  dimnames(s$v) <- list(colnames(x), paste0("PC", j))
  if (is.null(sc)) sc <- FALSE
  if (is.null(cen)) cen <- FALSE

  r <- list(x = x %*% s$v,
            components = components,
            center = cen,
            scale  = sc,
            handle_category = handle_category,
            technique = "pca",
            fit_data = x,
            others = list(coef = s$v,
                          sdev = s$d,
                          explained_var = explained_var))

  class(r) <- "transformer"
  return(r)
}
