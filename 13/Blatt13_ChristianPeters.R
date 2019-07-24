data <- read.table('mammalia2.txt')

# a)
model <- glm(N.offspr.high ~ Mass + Gestation + Lifespan + Birth.Mass, data = data, family = poisson)

# b)
# Lifespan: Expected Value decreases by factor exp(-0.014) (1.35%) if lifespan increases by 1
# Mass: Expected value increases by factor exp(0.0006) (0.06%) if mass increases by 1

# c)
beta <- coef(model)
