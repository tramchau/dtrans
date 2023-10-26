test_that("Validate the structure of the output", {
  t <- transformer.pca(iris[,1:4], center=T, scaling=T)
  expect_s3_class(t, "transformer")
  
  dummy <- "a"
  class(dummy) <- "transformer"
  # coerced  
  expect_error(transform(dummy, iris[1:10,1:4]), "'transformer' object is coerced to invalid format, can't retrieve it's attributes")
  
  # dummy is list
  
  dummy <- list("a", "b")
  class(dummy) <- "transformer"
  expect_error(transform(dummy, iris[1:10,1:4]), "'transformer' object is coerced and having invalid or missing attributes.")
  
  # remove one attribute from transformer
  dummy <- t
  dummy$fit_data <- NULL
  expect_error(transform(dummy, iris[1:10,1:4]), "'transformer' object is coerced and having invalid or missing attributes.")
  
  dummy <- t
  dummy$new_att <- "new att"
  expect_error(transform(dummy, iris[1:10,1:4]), "'transformer' object is coerced and having invalid or missing attributes.")
  
  # check attribute names of object
  #expect_identical(sort(names(m)), sort(c("call", "formula", "data", "yname", "coef", "sigma",
  #                                        "vcov", "npar", "df.residual","residuals", "fitted.values")))
  
  # check the output message of the object
  #expect_output(print(m), "components(s) explain(s)")
})
