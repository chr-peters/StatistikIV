# Name: Christian Peters

# No. 14)
# ======

data <- read.table('Mammalia.txt', sep=" ")

# a)
X <- as.matrix(data[, 2:6])

pca <- prcomp(X, scale = TRUE)

# print the best two PCAs
print(pca$rotation[, 1:2])

# b)
biplot(pca)

# c)
# 1. Choose l so that at least 75% of the variance is explained:
vars <- pca$sdev ** 2
l <- min(which(cumsum(vars) / sum(vars) > 0.75))
# l = 2, so choose the first 2 PCs according to this criterion

# 2. Choose all PCs that have more variance than the mean
l <- max(which(vars >= mean(vars)))
# l=2, so choose the first 2 PCs according to this criterion

# 3. Choose l using a scree plot
plot(vars, main = 'Scree Plot', ylab = 'Variance')
# According to this criterion, it makes sense to choose 2 PCs.

# I personally would put most emphasis on the scree plot because it seems to
# be the most intuitive criterion when knowing nothing else about the problem.
# Therefore I would choose 2 PCs.

# d)
X <- scale(X)
U <- pca$rotation[, 1:2]
X_approx <- X %*% U %*% t(U)
error <- sum((X_approx - X)**2)
# 152.202

# No. 15)
# =======

wines <- read.csv('more_wines.csv')

# a)

# get the means
mean_barolo <- colMeans(wines[wines$Type=='barolo', 1:2])
mean_grignolino <- colMeans(wines[wines$Type=='grignolino', 1:2])

# estimate the inverse of the covariance matrix
cov_inv <- solve(cov(wines[, 1:2]))

# discrimination functions
d_barolo <- function(x) {
  t(mean_barolo) %*% cov_inv %*% x - 1/2 * t(mean_barolo) %*% cov_inv %*% mean_barolo
}
d_grignolino <- function(x) {
  t(mean_grignolino) %*% cov_inv %*% x - 1/2 * t(mean_grignolino) %*% cov_inv %*% mean_grignolino
}
d <- function(x) {
  if (d_barolo(x) > d_grignolino(x)) {
    return('barolo')
  }
  return('grignolino')
}

# e(x) = 'barolo' falls d_barolo(x) > d_grignolino(x), 'grignolino' sonst

# b)
plot(wines$Phenols, wines$Color, col=as.integer(wines$Type)+1, pch=16,
     xlab="Phenols", ylab="Color", main="Wines")
legend('topleft', inset = 0.02,legend=levels(wines$Type), col=2:3, pch = 16)

# visualize the separating line
x_grid <- seq(min(wines$Phenols), max(wines$Phenols), 0.02)
y_grid <- seq(min(wines$Color), max(wines$Color), 0.02)
m <- length(x_grid)
n <- length(y_grid)
plot_grid <- expand.grid(x_grid, y_grid)
predictions_grid <- as.factor(apply(plot_grid, 1, d))

contour(x_grid, y_grid, matrix(as.integer(predictions_grid), m, n),
        levels = c(1.5, 2.5), d = FALSE, add = TRUE, lty = 2)

# c)

# do the classification
predictions <- apply(wines[, 1:2], 1, d)

# get a (biased!) estimate of the classification error
error_rate <- 1 - mean(predictions == wines$Type)
# 0.1088435
