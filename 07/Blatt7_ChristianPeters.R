# Name: Christian Peters

# No. 13)
# ======

data <- read.csv('wines.csv')

# a)

X <- as.matrix(data[, 1:13])

print(prcomp(X, scale = FALSE))

# As we can see, there is most likely a scaling issue. When looking at the variable
# 'Proline', we can see that it's values range from 278 to 1680, causing much more
# variance than any other variable (var(Proline) = 99166.72 which is more than
# two orders of magnitude greater than var(Mg) = 203.9893, the second biggest variance).
# As a result, the first principal component is nearly completely occupied by
# Proline and already explains more than 99% of the total variance.

# b)

res <- prcomp(X, scale = TRUE)
print(res)

# get the first three principal components
(best3 <- res$rotation[, 1:3])

# create the biplots
biplot(res, choices=c(1, 2))
# We can see that the first two PCs split the data into three clusters.
# Further analysis could reveal if there is any correspondence between these
# clusters and the different types of wine (which are three as well).
# The plot also shows that the variables Phenols, Proa, Flav, Proline OD and Hue
# are causing negative values of PC1 while AlcAsh and NonFlavPhenols are
# mostly responsible for positive values.
# The variables Ash, Color, Alcohol, Mg and Proline load strongly on PC2 in
# the positive direction.

biplot(res, choices=c(1, 3))
# In this plot we can't find any nice clusters
# PC3 seems to distinguish Ash and AlcAsh from the rest of the variables.

biplot(res, choices=c(2, 3))
# Also no clusters, for loadings see above.

# c)

# 1. Choose l so that at least 75% of the variance is explained:
vars <- res$sdev ** 2
l <- min(which(cumsum(vars) / sum(vars) > 0.75))
# l = 5, so choose the first 5 PCs according to this criterion

# 2. Choose all PCs that have more variance than the mean
l <- max(which(vars >= mean(vars)))
# l=3, so choose the first 3 PCs according to this criterion

# 3. Choose l using a scree plot
plot(vars, main = 'Scree Plot', ylab = 'Variance')
# According to this criterion, it makes sense to choose 3 PCs.

# Personally I would rely on the scree plot and choose the best 3 PCs because
# when knowing nothing else about the problem, this seems to be the most intuitive
# method.

# d)

# get U and scale X accordingly
U <- res$rotation
X <- scale(X)

# get the best two dimensional approximation and its error
best2DimApprox <- X %*% U[, 1:2] %*% t(U[, 1:2])
error2Dim <- sum((X - best2DimApprox)**2)
# 1026.1

# get the best four dimensional approximation and its error
best4DimApprox <- X %*% U[, 1:4] %*% t(U[, 1:4])
error4Dim <- sum((X - best4DimApprox)**2)
# 607.487

# e)

(x1_pca <- X[1,] %*% U[, 1:2])
(z1 <- x1_pca %*% t(U[, 1:2]))
