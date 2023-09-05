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
#' x_trans <- transformer.kpca(iris[,1:4])
#'
transformer.kpca <- function (x, ...) UseMethod("transformer.kpca")

transformer.kpca.default <-
  function(x, kernel = "rbfdot", sigma = 0.1, features = 0, th = 1e-4, na.action = na.omit, ...){
    x <- na.action(x)
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

    if(res$values[features] < th)
      warning(paste("eigenvalues of the kernel matrix are below threshold!"))

    coeff <- t(t(res$vectors[,1:features])/sqrt(res$values[1:features]))

    r <- list(sdev = s$d, coef = coeff,
              center = FALSE, # %||% FALSE,
              scale  = FALSE, #  %||% FALSE)
              technique = "kpca",
              prediction=list(kernel = kernel),
              data = x)

    # eig(ret) <- res$values[1:features]
    r$x <- kc %*% coeff
    class(r) <- "transformer"
    r
}
