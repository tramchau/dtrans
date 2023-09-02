print.transformer <- function(x, print.x = FALSE, ...) {
  cat(sprintf("Standard deviations (1, .., p=%d):\n", length(x$sdev)))
  print(x$sdev, ...)
  d <- dim(x$coef)
  cat(sprintf("\nCoefficients (n x k) = (%d x %d):\n", d[1], d[2]))
  print(x$coef, ...)
  if (print.x && length(x$x)) {
    cat("\nRotated variables:\n")
    print(x$x, ...)
  }
  invisible(x)
}

summary.transformer <- function(object, ...)
{
  chkDots(...)
  if (is.null(object$sdev)) {
    stop("NMF object")
  }
  vars <- object$sdev^2
  vars <- vars/sum(vars)
  importance <- rbind("Standard deviation" = object$sdev,
                      "Proportion of Variance" = round(vars, 5),
                      "Cumulative Proportion" = round(cumsum(vars), 5))
  k <- ncol(object$coef)
  colnames(importance) <- c(colnames(object$coef), rep("", length(vars) - k))
  object$importance <- importance
  class(object) <- "summary.pca"
  object
}

print.summary.transformer <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...)
  {
    dr <- dim(x$coef); k <- dr[2]
    p <- length(x$sdev)
    if(k < p) {
      cat(sprintf("Importance of first k=%d (out of %d) components:\n", k, p))
      print(x$importance[, 1:k, drop=FALSE], digits = digits, ...)
    } else {
      cat("Importance of components:\n")
      print(x$importance, digits = digits, ...)
    }
    invisible(x)
  }

predict.transformer <- function(object, newdata, ...)
{
  chkDots(...)
  if (missing(newdata)) {
    if(!is.null(object$x)) return(object$x)
    else stop("no scores are available: refit with 'retx=TRUE'")
  }
  if(length(dim(newdata)) != 2L)
    stop("'newdata' must be a matrix or data frame")
  nm <- rownames(object$coef)
  if(!is.null(nm)) {
    if(!all(nm %in% colnames(newdata)))
      stop("'newdata' does not have named columns matching one or more of the original columns")
    newdata <- newdata[, nm, drop = FALSE]
  } else {
    if(NCOL(newdata) != NROW(object$coef) )
      stop("'newdata' does not have the correct number of columns")
  }
  ## next line does as.matrix
  if (object$technique == "pca") {
    scale(newdata, object$center, FALSE) %*% object$coef
    #scale(newdata, object$center, object$scale) %*% object$coef
  }
  else {
    H = object$prediction$H
    W = object$prediction$W
    eps = object$prediction$eps
    W = W * t(H %*% t(newdata))/(W %*% (H %*% t(H)) + eps)
    W
  }
}


plot.transformer <- function(object, label=NULL,...) {
  chkDots(...)
  if (is.null(label)) c <- 'black' else c <- label
  n_comp <- length(colnames(object$coef))
  if (n_comp == 1) {
    plot(object$x, ylab= "Component 1")
  } else if (n_comp == 2) {
    plot(object$x[,1], object$x[,2],
         xlab="Component 1", ylab="Component 2", col=c)
  } else {
    pairs(object$x)
  }

}
