
.validate_input <- function(x, components, center, scaling, handle_category) {
  if (!(is.data.frame(x) | (is.matrix(x) & !(is.null(colnames(x)))))) stop("'x' should be a dataframe or a named matrix.")
  if (ncol(x) < 1) stop("'x' should have at least one column.")
  if (nrow(x) == 0) stop("'x' is empty, it should have data.")
  # missing data
  if (any(is.na.data.frame(x)) == TRUE) stop("'x' contrains NAs. NAs should be removed to continue.")
  
  if (!is.numeric(components))
    stop("'components' should be an integer.")
  if (length(components) > 1)
    stop("'components' should be an integer.")
  if (ceiling(components) != components | components < 1)
    stop("'components' should be an integer greater than 0.")
  if(components > ncol(x))
    stop("'components' should be less than or equal to the dataset's column number.")
  # print("validate")
  
  num_col <- ncol(x)
  if (!is.null(handle_category)) {
    if (length(handle_category) > 1)
      stop("Possible values for 'handle_category' parameter are 'onehot', 'label', 'ignore'")
    
    if (!is.character(handle_category) | !(handle_category %in% c('onehot', 'label', 'ignore')))
      stop("Possible values for 'handle_category' parameter are 'onehot', 'label', 'ignore'")
    
    if (handle_category %in% c("label", "onehot")) {
      if (length(center) > 1) stop("It is not supported to set column-based centers when handle_category is set to 'label' or 'onehot'")
      if (length(scaling) > 1) stop("It is not supported to set column-based scale when handle_category is set to 'label' or 'onehot'")
    } else if (handle_category == "ignore") {
      num_col <- ncol(x[!(sapply(x, is.factor) | sapply(x, is.character))])
    }
  } else {
    if (any(sapply(x, is.factor)) | any(sapply(x, is.character))) {
      stop("'x' contains categorical columns, 'handle_category' should be set to one of these values [ 'onehot', 'label', 'ignore']")
    }
  }
  
  
  if ((length(center)==1 & (!is.logical(center))))
    stop("'center' should be a logical value or a numeric vector having same length with number of columns of x.")
  if (length(center)>1 & (length(center) != num_col | !is.numeric(center)))
    stop("'center' should have a same length with number of columns of x.")
  
  if ((length(scaling)==1 & (!is.logical(scaling))))
    stop("'scaling' should be a logical value or a non-zero numeric vector having same length with column numbers of x.")
  
  if ((length(scaling)>1 & (length(scaling) != num_col | !is.numeric(scaling))) |
      (!is.logical(scaling) & any(scaling == 0)))
    stop("'scaling' should be non-zero and have same length with the number of columns of x.")
  
}

.validate_object <- function(object) {
  #if (class(object) != "Transformer") # No need to check, as all method is S3 method of transformer object.
  if (!(is.list(object))) stop("'transformer' object is coerced to invalid format, can't retrieve it's attributes")
  
  if (is.null(attributes(object)))
    stop("'transformer' object is coerced and does not have any attributes.")
  
  obj_attr_list <- list("x", "components",
                        "center", "scale", "handle_category", "technique", "fit_data", "others")
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


.validate_newdata <- function(object, newdata){

  if (missing(newdata)) stop("'newdata' should be specified to call this function.")
  if(!(is.data.frame(newdata) | is.matrix(newdata))) stop("'newdata' should be a matrix or data frame")
  if (nrow(newdata) == 0) stop("'newdata' should contain data")
  
  if(ncol(newdata) != ncol(object$fit_data)) {
    cn <- colnames(object$fit_data)
    stop("'newdata' does not have the same columns with the object's fit data. The columns should be ", c(paste0(cn[-length(cn)], ", "), cn[length(cn)]))
  }
  # check for pca and nmf
  nm <- colnames(object$fit_data)
  if(!all(nm %in% colnames(newdata)))
    stop("'newdata' does not have named columns matching the fit data's. The columns should be ", colnames(object$fit_data))
}