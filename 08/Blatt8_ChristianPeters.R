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

# d)
U <- pca$rotation[, 1:2]
X_approx <- X %*% U %*% t(U)
error <- sum((X_approx - X)**2)
# 5630740

# No. 15)
# =======

wines <- read.csv('more_wines.csv')

# a)

plot(wines$Phenols, wines$Color, col=wines$Type, pch=16, xlab="Phenols", ylab="Color", main="Wines")
legend('topleft', legend=levels(wines$Type), col=1:2, pch = 16)
