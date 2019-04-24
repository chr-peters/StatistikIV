# Name: Christian Peters

# No. 5)
# ======

cows <- read.csv('kuehe.txt', sep=' ')

# a)

# get weight differences
differences <- cows$final.weight - cows$initial.weight

# determine the groups
groups <- sapply(differences, function(x) {
  # use the quantiles to determine 4 equally probable groups (assuming H0)
  if (x <= qnorm(0.25, mean = 0, sd = 50)) {
    return(1)
  } else if (x <= qnorm(0.5, mean = 0, sd = 50)) {
    return(2)
  } else if (x <= qnorm(0.75, mean = 0, sd = 50)) {
    return(3)
  } else {
    return(4)
  }
})
groups <- table(groups)

# calculate the test statistic
n <- nrow(cows)
q <- sum((groups - n * 0.25) ** 2 / (n * 0.25))
# 5.230769

# get the corresponding chi-squared quantile
quantile_chisq <- qchisq(0.95, 3)
# 7.814728

# In this case, H0 can't be rejected because q < quantile_chisq

# b)

# determine the new groups
groups_delta <- sapply(differences, function(x) {
  if (x <= -50) {
    return(1)
  } else if (x <= -25) {
    return(2)
  } else if (x <= 0) {
    return(3)
  } else if (x <= 15) {
    return(4)
  } else {
    return(5)
  }
})
groups_delta <- table(groups_delta)

# get the corresponding probabilities assuming H0 is true
probabilities <- c(pnorm(-50, mean = 0, sd = 50),
                   pnorm(-25, mean = 0, sd = 50) - pnorm(-50, mean = 0, sd = 50),
                   pnorm(0, mean = 0, sd = 50) - pnorm(-25, mean = 0, sd = 50),
                   pnorm(15, mean = 0, sd = 50) - pnorm(0, mean = 0, sd = 50),
                   1 - pnorm(15, mean = 0, sd = 50))

# calculate the test statistic
q_delta <- sum((groups_delta - n * probabilities) ** 2 / (n * probabilities))
# 7.05987
quantile_chisq_delta <- qchisq(0.95, 4)
# 9.487729

# even in the new setup H0 can't be rejected

# No. 6)
# ======

goals <- scan('tore.txt', what = integer())

# a)
# print the distribution
goal_dist <- table(goals)
print(goal_dist)

# Statistical model:
# Let X be a random variable describing the number of goals scored by
# a student. Then X follows a binomial distribution described by the
# parameters n = 5 and p = pi (unknown).

# Hypotheses:
# H0: X ~ Bin(5, pi) vs H1: X !~ Bin(5, pi)

# b)
# estimate pi (goals / trials)
pi <- sum(goals) / (100 * 5)
# 0.796

# get the probabilities for the chisq test
goal_probabilities <- sapply(0:5, function(x) dbinom(x, 5, pi))

# calculate test statistic
q_goals <- sum((goal_dist - 100 * goal_probabilities) ** 2 / (100 * goal_probabilities))
# 29.47864
quantile_chisq_goals <- qchisq(0.95, 5)
# 11.0705

# In this case, we can reject H0 because q_goals > quantile_chisq_goals

# No. 7)
# ======

# get the data
election_data <- matrix(c(60, 90, 10, 40, 190, 190, 10, 10, 220, 150, 20, 10), nrow = 4)

# H0: Each party has the same chance of getting a vote regardless of the agegroup.
# H1: The agegroup influences voting behavior.

# calculate the test statistic
q_election <- 0
party_sums <- rowSums(election_data)
for (col in 1:3) {
  n_agegroup <- sum(election_data[,col])
  q_agegroup <- sum((election_data[, col] - party_sums * n_agegroup / 1000) ** 2 
                    / (party_sums * n_agegroup / 1000))
  q_election <- q_election + q_agegroup
}
# q_election = 108.0663

quantile_chisq_election <- qchisq(0.95, 6)
# 12.59159

# In this case, we can reject H0 which means that the agegroup has an impact on
# voting behavior.

# Calculate the p-Value:
p_value <- 1 - pchisq(q_election, 6)
# 0