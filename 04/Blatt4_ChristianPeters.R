# Name: Christian Peters

# No. 8)
# ======

set.seed(1234)

cows <- read.csv('kuehe.txt', sep=' ')

# a)

# get differences
differences <- cows$final.weight - cows$initial.weight

# sort data to compute empirical distribution
differences <- sort(differences)

# find the supremum (test statistic)
d <- 0
n <- length(differences)
for (i in seq_along(differences)) {
  # test both differences before and after the 'jump'
  curD <- abs((i-1)/n - pnorm(differences[i], sd=50))
  if (curD > d) d <- curD
  
  curD <- abs(i/n - pnorm(differences[i], sd=50))
  if (curD > d) d <- curD
}
# the resulting value of the test statistic is d = 0.1733919

# critical value:
dCritical <- 1.358 / sqrt(n)
# 0.1883207

# As we can see, d is less than the critical value so H0 can't be rejected.

# b)

# compute the second, third and fourth moment
moment_2 <- mean((differences - mean(differences))**2)
moment_3 <- mean((differences - mean(differences))**3)
moment_4 <- mean((differences - mean(differences))**4)

# compute estimators for tau and kappa
tau <- moment_3 / moment_2 ** (3/2)
kappa <- moment_4 / moment_2 ** 2

# compute the two test statistics
teststat_tau <- sqrt(n) * tau
# -6.250766
teststat_kappa <- sqrt(n) * (kappa - 3)
# 11.47837

# get the critical values for the 5% niveau
tau_critical_min <- qnorm(0.025, sd=sqrt(6))
# -4.800912
kappa_critical_max <- qnorm(0.975, sd=sqrt(24))
# 9.601823

# As we can see, both estimators are more extreme than the corresponding
# critical values, so the Jarque-Bera-Test would reject H0 at the 5% niveau.

# shapiro-wilk test:
print(shapiro.test(differences))
# p-value = 0.03124

# As we can see, the shapiro wilk test also rejects H0 at the 5% niveau.

# c)
# The majority of tests rejected the hypothesis that the data follows a normal
# distribution so I come to the conclusion that the H0 hypothesis is not a
# suitable assumption.