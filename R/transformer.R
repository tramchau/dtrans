#' Function printing the output of the transformer object
#'
#' @export
print.transformer <- function(object, ...) {
  .validate_object(object)
  c <- object$component
  if (object$technique == "pca") {
    cat(sprintf("%d components(s) explain(s) %.2f%% of variance in data.\n", c, sum(object$others$explained_var[1:c])))
    cat(sprintf("\nStandard deviations of %d component(s):\n", c))
    print(object$others$sdev[1:c], ...)
    d <- dim(object$others$coef)
    cat(sprintf("\nCoefficients of features for %d components:\n", c))
    print(object$others$coef, ...)
  } else if (object$technique == "nmf") {

    cat(sprintf("Algorithm iterates %d times\n", object$others$stop_iter))
    cat(sprintf("Euclidean distance between data and transformed data * coefficients is %.5f\n", object$others$eucl_dist))
    cat(sprintf("Relative error between data and transformed data * coefficients is %.5f\n", object$others$relative_err))

    cat(sprintf("Coefficients of features for %d components (or H in NMF):\n", c))
    print(object$others$coef, ...)

  } else if (object$technique == "kpca") {

    cat(sprintf("Algorithm sigma %.2f\n", object$others$sigma))
    # cat("Eigenvalues:\n")
    # print(object$others$eigenvalues)
    # cat("\nEigenvectors:\n")
    # print(object$others$eigenvectors)
  }

  cat("\nReview of transformed data (the first six rows):\n")
  print(head(object$x), ...)
  cat("...")
}

#' Function printing the summary of intermediate algorithm output of the transformer object
#'
#' @export
summary.transformer <- function(object, ...)
{
  .validate_object(object)

  if (object$technique=='pca') {
    vars <- object$others$sdev^2
    vars <- vars/sum(vars)
    importance <- rbind("Standard deviation" = object$others$sdev,
                        "Proportion of Variance" = round(vars, 5),
                        "Cumulative Proportion" = round(cumsum(vars), 5))
    k <- ncol(object$others$coef)
    colnames(importance) <- paste0("PC", seq_len(length(vars)))
    object$importance <- importance
  }
  object$x <- NULL
  object$fit_data <- NULL
  object$others <- NULL
  class(object) <- 'summary.trans'
  object
}

#' Function transforming new data by the existing transformer object.
#'
#' This function return transformed data for new data.
#' @param object transformer object.
#' @param new_data new data having the same features to the data instantiate the transformer object.
#'
#' @details
#' This applies the transformer object metadata to transform new data.
#'
#' @return a dataset of transformed data.
#'
#' @export
#' @examples
#' data(iris)
#' data <- iris[sample(1:nrow(iris)),]
#' idx_train <- 1:140
#' idx_test <- 141:nrow(data)
#' x_trans <- transformer.kpca(iris[idx_train,1:4])
#' transform(x_trans, iris[idx_test,1:4])
#'
#' @method transform transformer
transform.transformer <- function(object, newdata)
{
  .validate_object(object)

  if (missing(newdata)) {
    stop("'newdata' should be specified to call this function.")
  }
  if (nrow(newdata) == 0) {
    stop("'newdata' should contain data")
  }
  if(length(dim(newdata)) != 2L)
    stop("'newdata' should be a matrix or data frame")

  if(ncol(newdata) != ncol(object$fit_data)) {
    cn <- colnames(object$fit_data)
    stop("'newdata' does not have the same columns with the object's fit data. The columns should be ", c(paste0(cn[-length(cn)], ", "), cn[length(cn)]))
  }
  # check for pca and nmf
  nm <- colnames(object$fit_data)
  if(!all(nm %in% colnames(newdata)))
    stop("'newdata' does not have named columns matching the fit data's. The columns should be ", colnames(object$fit_data))
  newdata <- newdata[, nm, drop = FALSE]

  x <- scale(newdata, center = object$center, scale = object$scale)

  if (object$technique == "pca") {
    return (x %*% object$others$coef)

  } else if (object$technique == "nmf"){
    n <- dim(x)[1L]
    W <- matrix(abs(rnorm(n * object$components)), n, object$components)
    ret <- .optimize_WH(x, W, t(object$others$coef), update_H=FALSE)
    H <- ret$H
    W <- ret$W
    return (W)

  } else if (object$technique == "kpca") {

    m <- nrow(x)
    km <- .calc_rbfkernel_matrix(object$others$sigma, x, object$fit_data)
    ## center kernel matrix
    kc <- t(t(km - colSums(km)/m) -  rowSums(km)/m) + sum(km)/m^2

    pcv <- t(t(object$others$eigenvector[,1:object$components])/
               sqrt(object$others$eigenvalues[1:object$components]))

    ret <- kc %*% pcv
    colnames(ret) <- paste0("PC", seq_len(object$components))
    return (ret)
  }
}

#' @export
inverse <- function(object, ...) UseMethod("inverse")

inverse.default <- function(object, data, ...) {
  inverse.transformer(object, data, ...)
}

#' Function inversing the tranformed data back to the original data.
#'
#' This function inverses the transformed data back to the original data based on the transformer object's attributes.
#' @param object transformer object.
#' @param data dataframe being inversed.
#'
#' @details
#' This function inverses the transformed data back to the original if using all set components. The inverse can happen partly for specific component to analyse the effect of components.
#'
#' @return a dataset after inversing.
#'
#' @export
#' @examples
#' data(iris)
#' iris[1:10,1:4]
#' x_trans <- transformer.kpca(iris[1:10,1:4])
#' inverse(x_trans, x_trans$x)
#'
inverse.transformer <- function(object, data, ...) {

  .validate_object(object)
  # pca
  if (object$technique == "pca" | object$technique == "nmf") {
    data_coef <- (data %*% t(object$others$coef))
  } else if (object$technique == "kpca") {

    m <- nrow(data)
    kmc <- .calc_rbfkernel_matrix(object$others$sigma, data, object$x)
    #kmc <- t(t(km - colSums(km)/m) -  rowSums(km)/m) + sum(km)/m^2

    m <- nrow(object$x)
    kxc <- .calc_rbfkernel_matrix(object$others$sigma, object$x)
    #kxc <- t(t(kx - colSums(kx)/m) -  rowSums(kx)/m) + sum(kx)/m^2
    dual_coef <- backsolve(kxc, object$fit_data)
    data_coef <- kmc %*% dual_coef
  }

  if (isFALSE(object$scale)) {sc <- F} else {sc = 1/object$scale}
  return (scale(data_coef, center = object$center, scale = sc))
}

#' Function plotting the transformed data.
#'
#' This function plots the transformed data.
#' @param object transformer object.
#' @param point_label optional label for each data point, default is NULL, all data points are plotted in black. If labels is set, the data points' colors are encoded accordingly.
#'
#' @details
#' This function
#'
#' @export
plot.transformer <- function(object, point_label=NULL,...) {
  .validate_object(object)
  .plotting(object$x, label = point_label, title=object$technique)
}

#' @export
plottrans <- function(object, ...) UseMethod("plottrans")

#' Function transforming and plotting the transformed data.
#'
#' This function transforms data and plots the transformed data.
#' @param object transformer object.
#' @param new_data new data having the same features to the data instantiate the transformer object.
#' @param point_label label for each data point, default is NULL, all data points are plotted in black. If labels is setted, the data points' color is encoded accordingly.
#'
#' @details
#' This function
#'
#' @export
plottrans.transformer <- function(object, new_data, point_label=NULL,...) {

  .validate_object(object)
  trans_data <- transform(object, new_data)
  .plotting(trans_data, label = point_label, title=object$technique)

}

.plotting <- function(data, label=NULL, title_=NULL, ...) {
  if (is.null(label)) {
    c <- 'black'
  } else c <- label
  n_comp <- ncol(data)
  if (n_comp == 1) {
    plot(data, ylab= "Component 1", ...)
  } else if (n_comp == 2) {
    plot(data[,1], data[,2],
         xlab="Component 1", ylab="Component 2", col=c, ...)
  } else {
    pairs(data)
  }
  title(main=title_)
}

.validate_instantiate_input <- function(x, components, center, scaling, handle_discrete) {
  if (!is.data.frame(x)) stop("'x' should be a dataframe.")
  if (ncol(x) <= 1) stop("'x' should have more than 1 column.")
  if (nrow(x) == 0) stop("'x' is empty, it should have data.")
  if (!is.numeric(components))
    stop("'components' should be an integer.")
  if (length(components) > 1)
    stop("'components' should be an integer.")
  if (ceiling(components) != components | components < 1)
    stop("'components' should be an integer greater than 0.")
  if(components > ncol(x))
    stop("'components' should be less than or equal to the dataset's column number")

  if ((length(center)==1 & (!is.logical(center))))
    stop("'center' should be a logical value or a numeric vector having same length with number of columns of x.")
  if (length(center)>1 & (length(center) != ncol(x) | !is.numeric(center)))
    stop("'center' should have a same length with number of columns of x.")

  if ((length(scaling)==1 & (!is.logical(scaling))))
    stop("'scaling' should be a logical value or a non-zero numeric vector having same length with column numbers of x.")

  if ((length(scaling)>1 & (length(scaling) != ncol(x) | !is.numeric(scaling))) |
      (!is.logical(scaling) & any(scaling == 0)))
    stop("'scaling' should be non-zero and have same length with the number of columns of x.")

  if (!is.logical(handle_discrete) | length(handle_discrete) > 1)
    stop("'handle_discrete' parameter should be single boonlean")
}

.validate_object <- function(object) {
  #if (class(object) != "Transformer") # No need to check, as all method is S3 method of transformer object.
  if (!(is.list(object))) stop("'transformer' object is coerced to invalid format, can't retrieve it's attributes")

  if (is.null(attributes(object)))
    stop("'transformer' object is coerced and does not have any attributes.")

  obj_attr_list <- list("x", "components",
                        "center", "scale", "technique", "fit_data", "others")
  if (!(all(attributes(object)$names %in% obj_attr_list)))
    stop("'transformer' object is coerced and having invalid or missing attributes.")
  # check others atribute for each technique
  if (object$technique == 'pca') {
    others_attr_list <- list("coef", "sdev", "explained_var")
    if (!(all(attributes(object$others)$names %in% others_attr_list)))
      stop("'transformer' object is coerced and having invalid or missing attributes.")
  } else if (object$technique == 'nmf') {
    others_attr_list <- list("coef", "eucl_dist", "relative_err", "stop_iter")
    if (!(all(attributes(object$others)$names %in% others_attr_list)))
      stop("'transformer' object is coerced and having invalid or missing attributes.")
  } else if (object$technique == 'kpca') {
    others_attr_list <- list("sigma", "eigenvalues", "eigenvectors")
    if (!(all(attributes(object$others)$names %in% others_attr_list)))
      stop("'transformer' object is coerced and having invalid or missing attributes.")
  }

}
