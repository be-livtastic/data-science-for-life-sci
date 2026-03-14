# ---- Setup: re-load population data ----
population <- read.csv("data/femaleControlsPopulation.csv")
population_weights <- unlist(population)

# ---- Normal Distribution and Central Limit Theorem ----
?set.seed #.Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R.
set.seed(1) # Set seed for reproducibility
n <- 10000

# Sample size of 5
avg_5 <- numeric(n)  # Pre-allocate a vector to store sample means

# For-loop to take samples
for (i in 1:n) {
  sample_5 <- sample(population_weights, size = 5)
  avg_5[i] <- mean(sample_5)
}
avg_5

# Sample size of 50
set.seed(1)
avg_50 <- numeric(n)
for (i in 1:n) {
  sample_50 <- sample(population_weights, size = 50)
  avg_50[i] <- mean(sample_50)
}
avg_50

# Visualizing the normal distributions of sample means for size 5 and size 50
par(mfrow=c(1,2))  # Set up the plotting area to have 1 row and 2 columns
hist(avg_5, main="Sample Size = 5", xlab="Sample Means", col="lightblue", border="black")
hist(avg_50, main="Sample Size = 50", xlab="Sample Means", col="lightgreen", border="black")
par(mfrow=c(1,1))  # Reset plotting area to default

# Observations: Sample size 5 → wide distribution while Sample size 50 is a more tight distribution
