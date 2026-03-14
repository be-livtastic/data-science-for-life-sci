library(dplyr)

# ---- Setup: re-load data ----
dat <- read.csv("data/femaleMiceWeights.csv")
controls <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()
diff_in_means <- mean(treatment) - mean(controls)

dat_pheno <- read.csv("data/mice_pheno.csv")
chowPopulation <- dat_pheno[dat_pheno$Sex == "F" & dat_pheno$Diet == "chow", 3]

# ---- Monte Carlo Simulation ----
# Monte Carlo Simulation of t-statistic under the null hypothesis

set.seed(1)

# Function to generate one t-statistic under H0 (no diet effect)
ttestgenerator <- function(n, population) {
  group1 <- sample(population, n)
  group2 <- sample(population, n)

  tstat <- (mean(group1) - mean(group2)) /
    sqrt(var(group1)/n + var(group2)/n)

  return(tstat)
}

# Number of Monte Carlo simulations
B <- 5000

# Two different sample sizes
n_small <- 3
n_large <- 10

# Simulate t-statistics
tstats_small <- replicate(B, ttestgenerator(n_small, chowPopulation))
tstats_large <- replicate(B, ttestgenerator(n_large, chowPopulation))

# Histograms of simulated t-statistics
par(mfrow=c(1,2))
hist(tstats_small, breaks=40, probability=TRUE,
     main=paste("Monte Carlo t-stats (n =", n_small, ")"),
     xlab="t statistic", col="lightgray", border="white")
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="red", lwd=2)        # Normal
curve(dt(x, df=2*n_small-2), add=TRUE, col="blue", lwd=2)        # t-dist
legend("topright", legend=c("Normal","t distribution"),
       col=c("red","blue"), lwd=2, bty="n")

hist(tstats_large, breaks=40, probability=TRUE,
     main=paste("Monte Carlo t-stats (n =", n_large, ")"),
     xlab="t statistic", col="lightgray", border="white")
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="red", lwd=2)
curve(dt(x, df=2*n_large-2), add=TRUE, col="blue", lwd=2)
legend("topright", legend=c("Normal","t distribution"),
       col=c("red","blue"), lwd=2, bty="n")
par(mfrow=c(1,1))

# Observations: For small sample sizes, the t-statistic distribution deviates more from normality, while for larger sample sizes, it aligns closely with the normal distribution.

# ---- Permutation Test ----
set.seed(1)
# Combine treatment and control groups
combined <- c(treatment, controls)
n_treat <- length(treatment)
n_control <- length(controls)
B <- 10000
perm_diffs <- numeric(B)
for (b in 1:B) {
  permuted <- sample(combined)  # Randomly permute the combined data
  perm_treat <- permuted[1:n_treat]  # First n_treat elements as treatment
  perm_control <- permuted[(n_treat + 1):(n_treat + n_control)]  # Remaining as control
  perm_diffs[b] <- mean(perm_treat) - mean(perm_control)  # Calculate mean difference
}
# Calculate observed difference in means
obs_diff <- mean(treatment) - mean(controls)
# Calculate p-value
p_value_perm <- mean(abs(perm_diffs) >= abs(obs_diff))
p_value_perm

# Visualize permutation distribution
hist(perm_diffs, main="Permutation Distribution of Mean Differences", xlab="Mean Difference (Treatment - Control)", col="lightgray", border="white")
abline(v=obs_diff, col="red", lwd=2)
# Conclusion: The permutation test provides a non-parametric way to assess the significance of the observed difference in means, yielding a p-value that can be compared to the significance level.
# Observations: The permutation distribution centers around zero, and the observed difference (red line) lies in the tails, indicating a significant effect of diet on mice weights.
