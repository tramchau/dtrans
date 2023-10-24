
.validate_input <- function(x, components, center, scaling, handle_category) {
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
  
  if (!is.null(handle_category)) {
    if (!is.character(handle_category) | length(handle_category) > 1 | !(handle_category %in% c('onehot', 'label', 'ignore')))
      stop("Possible values for 'handle_category' parameter are 'onehot', 'label', 'ignore'")
    
    if (any(sapply(x, is.factor)) & !(handle_category %in% c('onehot', 'label', 'ignore'))) {
      stop("'x' contains categorical columns, 'handle_category' should be set to one of these values [ 'onehot', 'label', 'ignore']")
    }
  } else {
    if (any(sapply(x, is.factor))) {
      stop("'x' contains categorical columns, 'handle_category' should be set to one of these values [ 'onehot', 'label', 'ignore']")
    }
  }
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