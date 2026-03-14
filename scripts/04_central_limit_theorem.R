# ---- Setup: re-load and clean population data ----
population <- read.csv("data/femaleControlsPopulation.csv")
population_weights <- unlist(population)

# Remove NAs so that sample means are always numeric.
# The CLT applies to real-valued observations — NAs would silently produce NaN
# sample means and corrupt the avg_5 / avg_50 vectors.
population_weights <- population_weights[!is.na(population_weights)]
cat("Population size (after NA removal):", length(population_weights), "\n")

# ---- Normal Distribution and Central Limit Theorem ----
# The CLT states: given a population with mean μ and finite variance σ²,
# the distribution of sample means (x̄) approaches N(μ, σ²/n) as n increases —
# REGARDLESS of the shape of the original population distribution.
# Key insight: it is the MEAN that becomes normal, not individual observations.

?set.seed  # .Random.seed stores the RNG state; set.seed() makes results reproducible
set.seed(1)
n <- 10000  # number of repeated samples — large enough for a smooth histogram

# ---- Sample size of 5 ----
# Pre-allocating with numeric(n) is important: growing a vector inside a loop
# (e.g. avg_5 <- c(avg_5, mean(sample_5))) forces R to copy the entire vector
# on every iteration — O(n²) memory operations vs O(n) with pre-allocation.
avg_5 <- numeric(n)
for (i in 1:n) {
  avg_5[i] <- mean(sample(population_weights, size = 5))
}

# ---- Sample size of 50 ----
# Using the same seed so the two simulations are comparable — the only difference
# is sample size, not the random draws from the RNG.
set.seed(1)
avg_50 <- numeric(n)
for (i in 1:n) {
  avg_50[i] <- mean(sample(population_weights, size = 50))
}

# ---- Visualize: how sample size affects the spread of sample means ----
# Both histograms should be centred near the population mean (μ ≈ 23.9 g).
# The width (standard deviation of the distribution of means = SE = σ/√n)
# shrinks as n grows — that is the practical meaning of "larger samples are more precise".
par(mfrow = c(1, 2))
hist(avg_5,
     main = "Sample Size = 5",  xlab = "Sample Means",
     col  = "lightblue", border = "black")
hist(avg_50,
     main = "Sample Size = 50", xlab = "Sample Means",
     col  = "lightgreen", border = "black")
par(mfrow = c(1, 1))

# Observation: n=5 → wide, flat distribution (high SE); n=50 → narrow, tall distribution (low SE).
# This is the CLT in action: more data → the sample mean concentrates tightly around μ.
# Practical implication: the CLT is what justifies using z-scores and t-tests
# even when the raw data are not perfectly normal — as long as n is large enough.
