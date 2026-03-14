library(rafalib)
library(dplyr)

# ---- Setup: re-load data ----
dat      <- read.csv("data/femaleMiceWeights.csv")
controls  <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf")   %>% select(Bodyweight) %>% unlist()
controls  <- controls[!is.na(controls)]
treatment <- treatment[!is.na(treatment)]

# ---- Confidence Interval from t.test ----
# t.test() returns the CI for the DIFFERENCE in means (treatment - control).
# Interpretation: we are 95% confident the true mean difference lies in this range.
# NOTE: "95% confident" does NOT mean there's a 95% probability the true value
# is in THIS specific interval — that interval is fixed once data is collected.
# It means: if we repeated the experiment many times, 95% of such intervals
# would contain the true difference. (See the visualisation below.)
t_test_result <- t.test(treatment, controls)
t_test_result

# The CI here spans roughly -0.04 to 6.08. Because it barely includes zero,
# the result is statistically significant at α=0.05 — but the practical range
# of plausible effects is wide, so biological interpretation matters.

# ---- Load population and clean NAs before sampling ----
dat_pheno    <- read.csv("data/mice_pheno.csv")
chowPopulation <- dat_pheno[dat_pheno$Sex == "F" & dat_pheno$Diet == "chow", 3]

# Remove NAs explicitly. sample() does not skip NAs — if an NA is drawn,
# mean() and sd() return NA, which silently breaks every downstream calculation.
chowPopulation <- chowPopulation[!is.na(chowPopulation)]
cat("Chow population size (after NA removal):", length(chowPopulation), "\n")

if (length(chowPopulation) == 0) stop("No chow population data found after NA removal.")

# True population mean — in a real study we would never know this; here we have
# it because femaleControlsPopulation represents the full population, so we can
# use it to verify that our CIs behave as advertised.
mu_chow <- mean(chowPopulation)
cat("True population mean (mu_chow):", round(mu_chow, 3), "\n")

# ---- Construct a single 95% CI from one sample ----
N <- 30  # sample size

# Guard: can't sample more than the population
if (N > length(chowPopulation)) stop("Requested sample size N exceeds population size.")

set.seed(1)
chowSample      <- sample(chowPopulation, N)
mean_chowSample <- mean(chowSample)
se_chowSample   <- sd(chowSample) / sqrt(N)

# We use the Z critical value (qnorm) rather than the t critical value (qt)
# because N=30 is large enough for the CLT to justify the normal approximation.
# For smaller N (e.g. N < 20), prefer qt(1 - 0.05/2, df = N-1) for a more
# accurate (wider) interval that accounts for uncertainty in estimating σ.
Q        <- qnorm(1 - 0.05/2)  # ≈ 1.96 for 95% CI
interval <- c(mean_chowSample - Q * se_chowSample,
              mean_chowSample + Q * se_chowSample)
print(interval)

# Does this particular interval capture the true mean?
captured <- interval[1] < mu_chow & interval[2] > mu_chow
cat("True mean captured by this interval:", captured, "\n")

# ---- Visualise 250 CIs to demonstrate the 95% coverage property ----
# This is the core pedagogical point: the 95% is a long-run frequency property
# of the PROCEDURE, not a probability statement about any single interval.
# Drawing 250 intervals shows that roughly 5% (≈12–13) will miss the true mean.
B <- 250
mypar()
plot(mu_chow + c(-7, 7), c(1, 1), type = "n",
     xlab = "Bodyweight", ylab = "Sample index", ylim = c(1, B),
     main = "250 Confidence Intervals (red = missed true mean)")
abline(v = mu_chow, col = "red", lwd = 2)  # the true mean we are trying to capture

set.seed(1)
missed <- 0
for (b in 1:B) {
  chow     <- sample(chowPopulation, N)
  se_chow  <- sd(chow) / sqrt(N)
  interval <- c(mean(chow) - Q * se_chow, mean(chow) + Q * se_chow)
  covered  <- mu_chow >= interval[1] & mu_chow <= interval[2]
  color    <- ifelse(covered, "black", "red")
  if (!covered) missed <- missed + 1
  segments(interval[1], b, interval[2], b, col = color)
}
cat("\nIntervals that missed the true mean:", missed, "out of", B,
    sprintf("(%.1f%%)\n", 100 * missed / B))
# Expected: ~5% miss rate. Actual may vary slightly due to randomness,
# but should be close to 5% over many repetitions.
