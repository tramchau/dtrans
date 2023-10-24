test_that("Validate the inputs - instantiate functions", {
  data(iris)

  # valid inputs
  expect_silent(transformer.pca(iris[,1:4], center=T, scaling=T))
  expect_silent(transformer.nmf(iris[,1:4], center=F, scaling=T))
  expect_silent(transformer.kpca(iris[,1:4], center=T, scaling=T))

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

})

test_that("Validate the structure of the output", {
  t <- transformer.pca(iris[,1:4], center=T, scaling=T)
  expect_s3_class(t, "transformer")

  # check attribute names of object
  #expect_identical(sort(names(m)), sort(c("call", "formula", "data", "yname", "coef", "sigma",
  #                                        "vcov", "npar", "df.residual","residuals", "fitted.values")))

  # check the output message of the object
  #expect_output(print(m), "components(s) explain(s)")
})

test_that("Validate the transformed output from function", {
  # PCA
  pca1 <- transformer.pca(iris[,1:4], components=2,
                          center=F, scaling=F, handle_category = NULL)
  pca2 <- prcomp(iris[,1:4], rank. = 2, center=F, scale. =F)
  
  expect_equal(pca1$x, pca2$x, tolerance=1e-3)
  expect_equal(pca1$others$coef, pca2$rotation, tolerance=1e-3)
  
  # NMF - not able to compare because of the randomness
  # nmf1 <- transformer.nmf(iris[,1:4], components=2, max_iter = 1000,
  #                        center=F, scaling=F, handle_category=NULL)
  # library(NMFN)
  # nmf2 <- nnmf(iris[,1:4], k=2)
  # expect_equal(nmf1$x, nmf2$W, tolerance=1e-3)
  # expect_equal(nmf1$others$coef, t(nmf2$H), tolerance=1e-3)
  
  # KPCA - not able to compare because of the randomness
  kpca1 <- transformer.kpca(iris[,1:4], components=2, kernel="rbfdot", sigma=0.1,
                         center=F, scaling=F, handle_category=NULL)
  library(kernlab)
  kpca2 <- kpca(~., iris[,1:4], features=2)
  kpca2x <- rotated(kpca2)
  colnames(kpca2x) <- c('PC1', 'PC2')
  rownames(kpca2x) <- NULL
  expect_equal(kpca1$x, kpca2x, tolerance=1e-3)

})
