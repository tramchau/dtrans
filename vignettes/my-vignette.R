## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, fig.width = 7.5, fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(dtrans)

## -----------------------------------------------------------------------------
data("iris")
data <- iris[sample(1:nrow(iris)),]
rownames(data) <- 1:nrow(data)
idx_train <- 1:130
idx_test <- 131:nrow(data)

pca <- transformer.pca(data[idx_train,1:4])
nmf <- transformer.nmf(data[idx_train,1:4])
kpca <- transformer.kpca(data[idx_train,1:4])


## -----------------------------------------------------------------------------
attributes(pca)

## -----------------------------------------------------------------------------
print(pca)

## -----------------------------------------------------------------------------
summary(pca)

## -----------------------------------------------------------------------------
par(mfrow=c(1, 2))
plot(pca)
plot(pca, point_label=data[idx_train,5])
par(mfrow=c(1, 1))

## -----------------------------------------------------------------------------
t_pca <- transform(pca, data[idx_test,1:4])
t_pca

## -----------------------------------------------------------------------------
plottrans(pca, data[idx_test,1:4])


## -----------------------------------------------------------------------------
print(inverse(pca, t_pca))

# Original data to compare with the inverse output
print(data[idx_test,1:4])

## -----------------------------------------------------------------------------
par(mfrow=c(1, 2))
plot(nmf)
plot(nmf, point_label=data[idx_train,5])
par(mfrow=c(1, 1))

# Transform test data
t_nmf <- transform(nmf, data[idx_test,1:4])
print(t_nmf)

# transform and plot data
plottrans(nmf, data[idx_test,1:4])

print(inverse(nmf, t_nmf))

# Original data to compare with the inverse output
print(data[idx_test,1:4])

## -----------------------------------------------------------------------------
par(mfrow=c(1, 2))
plot(kpca)
plot(kpca, point_label=data[idx_train,5])
par(mfrow=c(1, 1))


# Transform test data
t_kpca <- transform(kpca, data[idx_test,1:4])
print(t_kpca)

# transform and plot data
plottrans(kpca, data[idx_test,1:4])

# Inverse test transformed data
print(inverse(kpca, t_kpca))
print(data[idx_test,1:4])

# Inverse train transformed data
print(inverse(kpca, kpca$x[1:10,]))
print(data[1:10,1:4])

