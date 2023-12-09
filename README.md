## Overview
‘dtrans’packagesupportstransforminghigh dimensional data into lower dimensional data by PCA, NMF, Kernel PCA:
- PCA (Principal Component Analysis) – is a popular algorithm, transforms data into a new coordinate system which captures the high data variance.
- NMF (Non-Negative Matrix Factorization) - is applied for positive data. It is more suitable for the data combined from different sources, layers.
- Kernel PCA uses kernel trick – projecting the non-linear-separated data into the linearly separated one in a higher dimensional space.

## Background
The algorithms are re-implemented from the existing CRAN R packages: stat [1], NMFN [2], kernlab [3]. They are creator function in 'dtrans' package to create the 'transformer' class.

Beside the basic S3 method for 'transformer' object such as summary, print, plot, two other S3 methods are defined to support data manipulation such as transform and inverse.

## Installation
```{r}
devtools::install_github("tramchau/dtrans")
```

## Features

Create 'transformer' object by 3 algorithms with 'dtrans' package:

```{r}
data("iris")
pca <- transformer.pca(iris)
kpca <- transformer.kpca(iris)
nmf <- transformer.nmf(iris)

```

S3 methods

```{r}
print(pca)
summary(pca)
plot(pca)
```

S3 methods for data manipulation

```{r}
# iris_new should be new data should have not been used for creating object
# the below is for demonstration of transform function based on created object on new data (same structure with the fitted data)
iris_new = iris[1:10, 1:5]
iris_new_trans = transform(pca, iris_new)

iris_new_inv = inverse(pca, iris_new_trans)
```

Preprocessing option while creating the 'transformer' object:

```{r}
pca <- transformer.pca(data, handle_category = 'onehot')
pca <- transformer.pca(data, center=T, scaling=T, handle_category = 'ignore')
 
```

# Reference

[1] Core Team contributors worldwide, R stats package: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/stats-package.html.

[2] Suhai (TImothy) Liu, Non-negative matrix factorization package: https://cran.r-project.org/web/packages/NMFN/index.html.

[3] Alexandros Karatzoglou, Kernlab: Kernel-based machine learning lab package: https://cran.r-project.org/web/packages/kernlab/index.html.
