# Name: Christian Peters

# No. 4) c)
# =========

set.seed(1234)

# how often to run the simulation? (how many q values to generate)
numRuns <- 10000

# store the Q values here
qVec <- numeric(numRuns)

# vector of probabilities assuming H0 is true
pi0 <- c(1/4, 1/6, 1/6, 1/6, 1/4)
n <- 109
for (i in seq_len(numRuns)) {
  # create a random observation assuming H0 is true
  curObservation <- rmultinom(1, n, pi0)
  
  # calculate the Q value
  qVec[i] <- sum((curObservation - n * pi0)**2 / (n * pi0))
}

# get the 95% quantile
threshold <- quantile(qVec, 0.95)

print(paste0('The 95% quantile is ', threshold, '.'))
# "The 95% quantile is 9.31192660550459."

# As we can see, this test would also not reject H0.