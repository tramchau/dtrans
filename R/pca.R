#' Function creating a transformer object using PCA algorithm.
#'
#' This function return a transformer object fitted by data.
#' @param components positive number of components for the transformed data.
#' @param center logical value to scale data. Parameter is passed to base::scale.
#' @param scaling logical value to scale data. Parameter is passed to base::scale.
#' @param handle_category character value to handle categorical features. The accepted values are 'label', 'onehot', and 'ignore'. Default value is NULL, if dataset contains character fields, the function return error. .
#'
#' @details
#' The function is created based on stat::prcomp function with some adjustments to fit into the purpose of the package. It includes preprocessing data (scaling, categorical handling) before transforming data. It creates a transformer object with attributes to perform other functionalities.
#'
#' @return Return "transformer" class.
#'
#' @export
#' @examples
#' data(iris)
#' pca <- transformer.pca(iris[,1:4])
#' print(pca)

transformer.pca <- function(x, components = 2, center = FALSE, scaling = FALSE, handle_category = NULL) {
  # Validate input
  .validate_input(x, components, center, scaling, handle_category)
  
  if (!(is.null(handle_category))) {
      if (any(sapply(x, is.factor)) | any(sapply(x, is.character)))
        x <- .handle_category(x, handle_category)
      else handle_category <- NULL
  }
  
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
