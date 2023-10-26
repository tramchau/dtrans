test_that("Validate the inputs - instantiate functions", {
  data(iris)

  # valid inputs
  expect_silent(transformer.pca(iris[,1:4], center=T, scaling=T))

  # Validate dataset input
  expect_error(transformer.pca(iris[,1]), "'x' should be a dataframe or a named matrix.")

  # empty dataset
  expect_error(transformer.pca(iris[FALSE]), "'x' should have at least one column.")
  expect_error(transformer.pca(iris[FALSE,]), "'x' is empty, it should have data.")
  
  data(airquality)
  expect_error(transformer.pca(airquality), "'x' contrains NAs. NAs should be removed to continue.")
  # handle_category
  
  # Check component
  expect_error(transformer.pca(iris[,1:4], components='a'), "'components' should be an integer.")
  expect_error(transformer.pca(iris[,1:4], components=c(1, 2)), "'components' should be an integer.")
  expect_error(transformer.pca(iris[,1:4], components=1.2), "'components' should be an integer greater than 0.")
  expect_error(transformer.pca(iris[,1:4], components=0), "'components' should be an integer greater than 0.")
  expect_error(transformer.pca(iris[,1:4], components=-1), "'components' should be an integer greater than 0.")
  expect_error(transformer.pca(iris[,1:4], components=5), "'components' should be less than or equal to the dataset's column number.")
  
  # handle_category
  expect_error(transformer.pca(iris, handle_category=5), "Possible values for 'handle_category' parameter are 'onehot', 'label', 'ignore'")
  expect_error(transformer.pca(iris, handle_category=c("label", "onehot")), "Possible values for 'handle_category' parameter are 'onehot', 'label', 'ignore'")
  expect_error(transformer.pca(iris, handle_category="dummy"), "Possible values for 'handle_category' parameter are 'onehot', 'label', 'ignore'")
  expect_error(transformer.pca(iris, handle_category="label", center=c(1, 1, 1, 1)), "It is not supported to set column-based centers when handle_category is set to 'label' or 'onehot'")
  expect_error(transformer.pca(iris, handle_category="label", scaling=c(1, 1, 1, 1, 1)), "It is not supported to set column-based scale when handle_category is set to 'label' or 'onehot'")
  expect_error(transformer.pca(iris, handle_category="onehot", center=c(1, 1, 1, 1)), "It is not supported to set column-based centers when handle_category is set to 'label' or 'onehot'")
  expect_error(transformer.pca(iris, handle_category="onehot", scaling=c(1, 1, 1, 1)), "It is not supported to set column-based scale when handle_category is set to 'label' or 'onehot'")
  
  # expect_error(transformer.pca(iris), "'x' contains categorical columns, 'handle_category' should be set to one of these values [ 'onehot', 'label', 'ignore']")
  
  # Validate other param
  expect_error(transformer.pca(iris[,1:4], center=1), "'center' should be a logical value or a numeric vector having same length with number of columns of x.")
  expect_error(transformer.pca(iris[,1:4], center='a'), "'center' should be a logical value or a numeric vector having same length with number of columns of x.")
  expect_error(transformer.pca(iris[,1:4], center=c(1, 1)), "'center' should have a same length with number of columns of x.")
  expect_error(transformer.pca(iris[,1:4], center=c(1, F)), "'center' should have a same length with number of columns of x.")
  
  # Validate other param
  expect_error(transformer.pca(iris[,1:4], scaling=1), "'scaling' should be a logical value or a non-zero numeric vector having same length with column numbers of x.")
  expect_error(transformer.pca(iris[,1:4], scaling='a'), "'scaling' should be a logical value or a non-zero numeric vector having same length with column numbers of x.")
  expect_error(transformer.pca(iris[,1:4], scaling=c(1, 1)), "'scaling' should be non-zero and have same length with the number of columns of x.")
  expect_error(transformer.pca(iris[,1:4], scaling=c('a','b','c','d')), "'scaling' should be non-zero and have same length with the number of columns of x.")
})

