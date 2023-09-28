test_that("Validate the inputs - instantiate functions", {
  data(iris)

  # valid inputs
  expect_silent(trans_pca(iris[,1:4], center=T, scaling=T))
  expect_silent(trans_nmf(iris[,1:4], center=F, scaling=T))
  expect_silent(trans_kpca(iris[,1:4], center=T, scaling=T))

  # Validate dataset input
  expect_error(trans_pca(iris[,1]), "data must be a data frame.")

  # empty dataset
  expect_error(trans_pca(iris[FALSE]), "data is empty.")

  # Validate other param
  expect_error(trans_pca(iris[,1:4], center=1), "center must be either logical value or a numeric vector having same length with number of columns in dataset parameter")
  expect_error(trans_pca(iris[,1:4], center='a'), "center must be either logical value or a numeric vector having same length with number of columns in dataset parameter")
  expect_error(trans_pca(iris[,1:4], center=c(1, 1)), "center must be either logical value or a numeric vector having same length with number of columns in dataset parameter")
  expect_error(trans_pca(iris[,1:4], center=c(1, F)), "center must be either logical value or a numeric vector having same length with number of columns in dataset parameter")

})

test_that("Validate the structure of the output", {
  t <- trans_pca(iris[,1:4], center=T, scaling=T)
  expect_s3_class(t, "transformer")

  # check attribute names of object
  #expect_identical(sort(names(m)), sort(c("call", "formula", "data", "yname", "coef", "sigma",
  #                                        "vcov", "npar", "df.residual","residuals", "fitted.values")))

  # check the output message of the object
  #expect_output(print(m), "components(s) explain(s)")
})

test_that("Validate the numerical result from function", {
  #m <- mylm(Sepal.Length ~ Sepal.Width, iris)

  #expect_equal(as.numeric(m$coef[1]), 6.526, tolerance=1e-3)
  #expect_equal(as.numeric(m$coef[2]), -0.2233, tolerance=1e-3)
})
