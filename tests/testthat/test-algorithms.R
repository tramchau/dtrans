test_that("Validate the transformed output from function", {
  # PCA
  pca1 <- transformer.pca(iris[,1:4], components=2,
                          center=F, scaling=F, handle_category = NULL)
  pca2 <- prcomp(iris[,1:4], rank. = 2, center=F, scale. =F)
  
  expect_equal(pca1$x, pca2$x, tolerance=1e-3)
  expect_equal(pca1$others$coef, pca2$rotation, tolerance=1e-3)
  
  # NMF - not able to compare because of the randomness
  nmf1 <- transformer.nmf(iris[,1:4], components=2, max_iter = 1000,
                          center=F, scaling=F, handle_category=NULL)
  library(NMFN)
  set.seed(0)
  nmf2 <- nnmf(iris[,1:4], k=2)
  
  set.seed(0)
  tmp <- as.matrix(iris[,1:4]) - (nmf2$W%*%nmf2$H)
  eucl_dist <- sum(tmp*tmp)
  errorx <- mean(abs(as.matrix(iris[,1:4]) - nmf2$W %*% nmf2$H))/mean(as.matrix(iris[,1:4]))
  
  expect_equal(nmf1$others$eucl_dist, eucl_dist, tolerance=1e-1)
  expect_equal(nmf1$others$relative_err, errorx, tolerance=1e-1)
  
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
