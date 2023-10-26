test_that("Validate the inputs - S3 methods", {
  
  t <- transformer.pca(iris[,1:4], center=T, scaling=T)
  
  expect_silent(transform(t, iris[1:10,1:4]))
  expect_silent(inverse(t, iris[1:10,1:2]))
  
  # transform
  expect_error(transform(t), "'newdata' should be specified to call this function.")
  expect_error(transform(t, "string"), "'newdata' should be a matrix or data frame")
  expect_error(transform(t, iris[FALSE,1:4]), "'newdata' should contain data")
  expect_error(transform(t, iris[1:10,1:3]), "'newdata' does not have the same columns with the object's fit data. Access fit_data attribute of the object to view the data structure.")
  
  rename_iris <- iris
  colnames(rename_iris) <- c("SL", "SW", "PL", "PW", "S")
  expect_error(transform(t, rename_iris[1:10,1:4]), "'newdata' does not have column names matching the fit data's. Access fit_data attribute of the object to view the data structure.")
  
  # inverse
  expect_error(inverse(t, iris[1:10,1]), "'data' should be a matrix or data frame")
  expect_error(inverse(t), "'data' should be specified to call this function.")
  expect_error(inverse(t, iris[FALSE,1:2]), "'data' should contain data")
  expect_error(inverse(t, iris[1:10,1:3]), "'data' does not have the same columns with the object's transformed data.")
  
  # plot
  expect_error(plot(t, plot_all=NULL), "plot_all parameter should be single logical value")
  expect_error(plot(t, plot_all='a'), "plot_all parameter should be single logical value")
  expect_error(plot(t, plot_all=c(T, F)), "plot_all parameter should be single logical value")
  
  # check the output message of the object
  expect_output(str(print(t)), "Standard deviations of")
  expect_output(str(summary(t)), "components")
  
  expect_true(is.matrix(transform(t, iris[1:10,1:4])))
  expect_true(is.matrix(inverse(t, iris[1:10,1:2])))
  
  expect_equal(ncol(transform(t, iris[1:10,1:4])), t$components)
  expect_equal(ncol(inverse(t, iris[1:10,1:2])), ncol(t$fit_data))
})
