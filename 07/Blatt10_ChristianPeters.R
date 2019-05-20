# Name: Christian Peters

# No. 13)
# ======

data <- read.csv('wines.csv')

# a)

X <- data[, 1:13]

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
print(best3 <- res$rotation[, 1:3])

# create the biplots
biplot(res, choices=c(1, 2))
biplot(res, choices=c(1, 3))
biplot(res, choices=c(2, 3))

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
# when knowing nothing else about the problem, this seems to be the most intuitive method.