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
U <- pca$rotation[, 1:2]
X_approx <- X %*% U %*% t(U)
error <- sum((X_approx - X)**2)
# 5630740

# No. 15)
# =======

wines <- read.csv('more_wines.csv')

# a)

# get the means
mean_barolo <- colMeans(wines[wines$Type=='barolo', 1:2])
mean_grignolino <- colMeans(wines[wines$Type=='grignolino', 1:2])

# discrimination functions
d_barolo <- function(x) {
  -norm(x - mean_barolo, type='2')
}
d_grignolino <- function(x) {
  -norm(x - mean_grignolino, type='2')
}

# b)
plot(wines$Phenols, wines$Color, col=wines$Type, pch=16, xlab="Phenols", ylab="Color", main="Wines")
legend('topleft', inset = 0.02,legend=levels(wines$Type), col=1:2, pch = 16)

dPhenols = mean_barolo[1] - mean_grignolino[1]
dColor = mean_barolo[2] - mean_grignolino[2]
mPhenols = (mean_barolo[1] + mean_grignolino[1]) / 2
mColor = (mean_barolo[2] + mean_grignolino[2]) / 2
intercept <- dPhenols * mPhenols / dColor + mColor
slope = -dPhenols / dColor
abline(a=intercept, b=slope)

# c)

# do the classification
predictions <- apply(wines[, 1:2], 1, function(x) {
  if (d_barolo(x) > d_grignolino(x)) {
    return('barolo')
  }
  return('grignolino')
})

error_rate <- 1 - mean(predictions == wines$Type)
# 0.1496599
