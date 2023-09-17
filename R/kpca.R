#' Function creating a transformer object by PCA algorithm.
#'
#' This function return a transformer object fitted by data.
#' @param component positive number of components for the transformed data.
#' @param center boolean value to scale data. Parameter is passed to base::scale.
#' @param scaling boolean value to scale data. Parameter is passed to base::scale.
#' @param handle_discrete boolean value to handle discrete features in data.
#' @param kernel string to indicate the kernel name, the allowed values are [].
#' @param sigma sigma...
#' @param th value to define warning threshold for eigenvalues.
#'
#' @details
#' This calculation is created based on stat::prcomp function with some adjustments to fit into the purpose of the package. It creates a transformer object including several attribute to perform other functionality.
#'
#' @return transformer.pca return a class "transformer" containing the following componenets:
#'
#' @export
#' @examples
#' data(iris)
#' x_trans <- transformer.kpca(iris[,1:4])
#'
transformer.kpca <- function (x, ...) UseMethod("transformer.kpca")

transformer.kpca.default <-
  function(x, component = 2, center = TRUE, scaling = FALSE, handle_discrete = NULL, kernel = "rbfdot", sigma = 0.1, th = 1e-4, ...){
    # x <- na.action(x) # para: , na.action = na.omit
    x <- as.matrix(x)
    m <- nrow(x)

    #km <- kernelMatrix(rbf,x)
    n <- nrow(x)
    res1 <- matrix(rep(0,n*n), ncol = n)
    for(i in 1:n) {
      for(j in i:n) {
        # res1[i,j]  <- kernel(x[i,],x[j,])
        res1[i,j] <- exp(sigma*(2*crossprod(x[i,],x[j,]) - crossprod(x[i,]) - crossprod(x[j,])))
      }
    }

    res1 <- res1 + t(res1)
    diag(res1) <- diag(res1)/2
    km <- res1

    ## center kernel matrix
    kc <- t(t(km - colSums(km)/m) -  rowSums(km)/m) + sum(km)/m^2

    ## compute eigenvectors
    res <- eigen(kc/m,symmetric=TRUE)

    if(res$values[component] < th)
      warning(paste("eigenvalues of the kernel matrix are below threshold!"))

    coeff <- t(t(res$vectors[,1:component])/sqrt(res$values[1:component]))

    r <- list(x = kc %*% coeff,
              sdev = 0,
              coef = coeff,
              center = FALSE, # %||% FALSE,
              scale  = FALSE, #  %||% FALSE)
              technique = "kpca",
              prediction=list(kernel = kernel),
              data = x)

    # eig(ret) <- res$values[1:component]
    class(r) <- "transformer"
    r
}
