#' Function creating a transformer object by Kernel PCA algorithm.
#'
#' This function create a transformer object fitted by data.
#' @param components positive number of components for the transformed data.
#' @param center boolean value to scale data. Parameter is passed to base::scale.
#' @param scaling boolean value to scale data. Parameter is passed to base::scale.
#' @param handle_discrete boolean value to handle discrete features in data.
#' @param kernel string to indicate the kernel name. Currently, 'rbfdot' is allowed.
#' @param sigma numeric value to indicate the inverse kernel width for the Radial Basis kernel function "rbfdot".
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

transformer.kpca <- function(x, components = 2, center = FALSE, scaling = FALSE, handle_discrete = FALSE, kernel = "rbfdot", sigma = 0.1){
  # Validate input
  .validate_instantiate_input(x, components, center, scaling, handle_discrete)

  # Validate other extra input

  if (is.null(kernel)) stop("kernel should be set as 'rbfdot'.")
  if (kernel != 'rbfdot') stop("package currently supports 'rbfdot' kernel only.")
  if (!is.numeric(sigma) | length(sigma) > 1) stop("sigma should be single numeric value.")

  th = 1e-4
  x <- as.matrix(x)
  m <- nrow(x)

  x <- scale(x, center = FALSE, scale = scaling)
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")

  #km <- kernelMatrix(rbf,x)
  km <- .calc_rbfkernel_matrix(sigma, x)

  ## center kernel matrix
  kc <- t(t(km - colSums(km)/m) -  rowSums(km)/m) + sum(km)/m^2

  ## compute eigenvectors
  res <- eigen(kc/m,symmetric=TRUE)

  if(res$values[components] < th)
    warning(paste("eigenvalues of the kernel matrix are below threshold!"))

  pcv <- t(t(res$vectors[,1:components])/sqrt(res$values[1:components]))
  colnames(pcv) <- paste("PC", 1:components, sep="")
  if (is.null(sc)) sc <- FALSE
  if (is.null(cen)) cen <- FALSE

  r <- list(x = kc %*% pcv,
            coef = NULL,
            sdev = NULL,
            explained_var = NULL,
            components = components,
            center = cen, # %||% FALSE,
            scale  = sc, #  %||% FALSE)
            technique = "kpca",
            data = x,
            others=list(sigma = sigma,
                        eigenvalues = res$value,
                        eigenvectors = res$vectors)
            )

  # eig(ret) <- res$values[1:component]
  class(r) <- "transformer"
  r
}

.calc_rbfkernel_matrix <- function(sigma, x, y = NULL)
{
  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")
  if(!is(x,"matrix")) stop("x must be a matrix or a vector")
  n <- dim(x)[1]
  dota <- rowSums(x*x)/2
  if (is.null(y)){

    res <- crossprod(t(x))
    for (i in 1:n)
      res[i,]<- exp(2*sigma*(res[i,] - dota - rep(dota[i],n)))
  }
  if (is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2]))
      stop("matrixes must have the same number of columns")
    m <- dim(y)[1]
    dotb <- rowSums(y*y)/2
    res <- x%*%t(y)
    #print(res)
    for( i in 1:m) {
      res[,i]<- exp(2*sigma*(res[,i] - dota - rep(dotb[i],n)))
    }
  }
  return(res)

}
