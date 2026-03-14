library(dplyr)

# ---- Setup: re-load and clean data ----
dat       <- read.csv("data/femaleMiceWeights.csv")
controls  <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf")   %>% select(Bodyweight) %>% unlist()
controls  <- controls[!is.na(controls)]
treatment <- treatment[!is.na(treatment)]
diff_in_means <- mean(treatment) - mean(controls)

# chowPopulation is our "null" population: all female chow-diet mice.
# We sample from it twice (two groups, same diet) to simulate H0 — no treatment effect.
dat_pheno      <- read.csv("data/mice_pheno.csv")
chowPopulation <- dat_pheno[dat_pheno$Sex == "F" & dat_pheno$Diet == "chow", 3]

# Remove NAs before any sampling. sample() will draw NAs if they are present,
# causing var() and mean() inside ttestgenerator() to return NA/NaN.
chowPopulation <- chowPopulation[!is.na(chowPopulation)]
cat("Chow population size (after NA removal):", length(chowPopulation), "\n")

if (length(chowPopulation) < 4) stop("Not enough chow population data for Monte Carlo simulation.")

# ---- Monte Carlo Simulation of the t-statistic under H0 ----
# Goal: verify that when there is truly no diet effect (both groups drawn from the
# same population), the simulated t-statistics follow the theoretical t-distribution.
# This is a sanity check: if the t-distribution is a valid model for our test
# statistic, then the analytic p-values from t.test() are trustworthy.

set.seed(1)

ttestgenerator <- function(n, population) {
  # Draw two independent groups of size n from the same population.
  # Because they come from the same distribution, any difference in means
  # is purely random — this is exactly the null hypothesis scenario.
  group1 <- sample(population, n)
  group2 <- sample(population, n)

  # Guard: if all values in a group are identical, var() = 0 → division by zero.
  # This is extremely unlikely with real data but can happen with tiny n.
  var1 <- var(group1)
  var2 <- var(group2)
  if (var1 == 0 || var2 == 0) return(NA)

  # Welch t-statistic formula: same as what t.test() uses internally.
  tstat <- (mean(group1) - mean(group2)) / sqrt(var1/n + var2/n)
  return(tstat)
}

B       <- 5000
n_small <- 3   # very small — t-distribution should deviate noticeably from normal
n_large <- 10  # larger — should align more closely with normal

tstats_small <- replicate(B, ttestgenerator(n_small, chowPopulation))
tstats_large <- replicate(B, ttestgenerator(n_large, chowPopulation))

# Remove any NA results from degenerate draws
tstats_small <- tstats_small[!is.na(tstats_small)]
tstats_large <- tstats_large[!is.na(tstats_large)]

cat("Usable simulations (n_small):", length(tstats_small), "\n")
cat("Usable simulations (n_large):", length(tstats_large), "\n")

# ---- Plot: compare simulated t-stats to theoretical distributions ----
# Red curve = standard normal N(0,1)
# Blue curve = t-distribution with the appropriate degrees of freedom (2n-2 for equal-n groups)
# For small n: the t-distribution has heavy tails vs the normal — using normal quantiles
# for inference would underestimate p-values and inflate false positive rates.
par(mfrow = c(1, 2))

hist(tstats_small, breaks = 40, probability = TRUE,
     main  = paste("Monte Carlo t-stats (n =", n_small, ")"),
     xlab  = "t statistic", col = "lightgray", border = "white")
curve(dnorm(x, mean = 0, sd = 1),   add = TRUE, col = "red",  lwd = 2)
curve(dt(x, df = 2*n_small - 2),    add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Normal", "t distribution"),
       col = c("red", "blue"), lwd = 2, bty = "n")

hist(tstats_large, breaks = 40, probability = TRUE,
     main  = paste("Monte Carlo t-stats (n =", n_large, ")"),
     xlab  = "t statistic", col = "lightgray", border = "white")
curve(dnorm(x, mean = 0, sd = 1),   add = TRUE, col = "red",  lwd = 2)
curve(dt(x, df = 2*n_large - 2),    add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Normal", "t distribution"),
       col = c("red", "blue"), lwd = 2, bty = "n")

par(mfrow = c(1, 1))
# Observation: At n=3 the simulated distribution has much heavier tails than the normal,
# and aligns well with the t(df=4) curve. At n=10 the two curves nearly coincide.
# Takeaway: with small samples, using the t-distribution for p-values is essential.

# ---- Permutation Test ----
# The permutation test is a non-parametric alternative to the t-test.
# It makes NO distributional assumptions (no normality, no equal-variance requirement).
# Idea: if diet truly has no effect, then the group labels (hf vs chow) are arbitrary.
# We can randomly reassign labels many times and see how often the reassigned
# difference is as large as the one we actually observed.
set.seed(1)

combined  <- c(treatment, controls)  # pool both groups
n_treat   <- length(treatment)
n_control <- length(controls)
B         <- 10000

perm_diffs <- numeric(B)
for (b in 1:B) {
  permuted     <- sample(combined)                          # shuffle all labels
  perm_treat   <- permuted[1:n_treat]                       # first n_treat = "treatment"
  perm_control <- permuted[(n_treat + 1):(n_treat + n_control)]  # rest = "control"
  perm_diffs[b] <- mean(perm_treat) - mean(perm_control)
}

obs_diff     <- mean(treatment) - mean(controls)
p_value_perm <- mean(abs(perm_diffs) >= abs(obs_diff))

cat("\nObserved difference:", round(obs_diff, 3), "\n")
cat("Permutation p-value:", round(p_value_perm, 4), "\n")
# Compare with the parametric p-value from t.test — they should be close,
# which would validate that the t-test assumptions are reasonable here.
cat("Parametric p-value (Welch t-test):", round(t.test(treatment, controls)$p.value, 4), "\n")

# Visualise the permutation distribution
hist(perm_diffs,
     main   = "Permutation Distribution of Mean Differences",
     xlab   = "Mean Difference (Treatment - Control)",
     col    = "lightgray", border = "white")
abline(v = obs_diff,  col = "red",  lwd = 2)
abline(v = -obs_diff, col = "red",  lwd = 2, lty = 2)  # mirror for two-tailed test
legend("topright", legend = c("Observed |diff|"), col = "red", lwd = 2, bty = "n")

# Observation: the permutation distribution is centred at zero (as expected under H0),
# and the observed difference falls in the tails, confirming a significant diet effect.
# The permutation p-value and the parametric p-value should agree closely when
# the t-test assumptions are met — disagreement would signal a violated assumption.
