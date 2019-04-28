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
# critical values, so the Jarque-Bera-Test rejects H0 at the 5% niveau.

# shapiro-wilk test:
print(shapiro.test(differences))
# p-value = 0.03124

# As we can see, the shapiro wilk test also rejects H0 at the 5% niveau.

# c)
# The majority of tests rejected the hypothesis that the data follows a normal
# distribution so I come to the conclusion that the H0 hypothesis is not a
# suitable assumption.

# No. 9)
# ======

# a)

# get data
untreated <- c(0.05, 0.22, 0.29, 1.44, 0.44, 0.3)
treated <- c(4.81, 0.68, 0.25, 0.33, 0.7, 1.41)

# sort data to compute the test statistic
untreated <- sort(untreated)
treated <- sort(treated)

# calculate test statistic
n <- 6
d <- 0
i <- 0
j <- 0
repeat {
  if (i < n && untreated[i+1] < treated [j+1]) {
    i <- i+1
  } else {
    j <- j+1
  }
  curD <- abs(i/n - j/n)
  if (curD > d) d <- curD
  if (i == n && j == n) {
    break
  }
}
# the resulting value of the test statistic is d = 0.5
# the (asymptotic) critical value for n = 6 is 0.52, so H0 can't be rejected

# b)

# calculate test statistic for the two sample t-test
n1 <- length(untreated)
n2 <- length(treated)
s1 <- var(untreated)
s2 <- var(treated)
t <- (mean(untreated) - mean(treated)) / sqrt(((n1-1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)) *
  sqrt((n1 * n2) / (n1 + n2))
# the value of the test statistic is t = -1.228638

# get the critical value
t_critical <- qt(0.025, df = 10)
# the critical value is -2.228139 which means that H0 can't be rejected