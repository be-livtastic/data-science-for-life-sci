library(dplyr)

# ---- Setup: re-load and clean data ----
# mice_pheno.csv has a richer phenotype dataset including both sexes and larger N,
# which makes it more appropriate for power analysis than the small 24-mouse dataset.
datPheno <- read.csv("data/mice_pheno.csv")

# na.omit here is intentional: power is estimated by simulating repeated sampling.
# If NAs remain in the bodyweight column, sample() can draw them and t.test()
# will throw an error mid-simulation. Removing incomplete rows upfront prevents that.
datPheno <- na.omit(datPheno)

controlPheno <- filter(datPheno, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist()
hfPheno <- filter(datPheno, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist()

cat("Control population size:", length(controlPheno), "\n")
cat("HF population size:     ", length(hfPheno), "\n")

# ---- Background: Types of Errors ----
# Type I Error (α): Rejecting H0 when it is actually true (false positive).
#   Controlled by our significance threshold. α=0.05 means we accept a 5%
#   chance of a false positive when there is truly no effect.
#
# Type II Error (β): Failing to reject H0 when it is actually false (false negative).
#   This is the error researchers often underestimate. A "non-significant" result
#   does NOT mean there is no effect — it may just mean the study was underpowered.
#
# Power (1 - β): The probability of correctly detecting a real effect.
#   Conventional target is 80%, meaning we accept a 20% chance of missing a real effect.
#
# Power is affected by:
#   - Effect size: bigger difference between groups → easier to detect
#   - Sample size: more data → less variability in estimates → easier to detect
#   - Significance level (α): looser threshold → more power, but more false positives
#   - Variability: noisier measurements → harder to detect real effects

# ---- Single test with N=30: does it reach significance? ----
set.seed(1)
N <- 30
hf      <- sample(hfPheno, N)
control <- sample(controlPheno, N)
t.test(hf, control)$p.value
# With N=30, we may or may not get p<0.05 on any given sample — the result is
# stochastic. This motivates estimating power across many repetitions instead of
# drawing conclusions from a single test.

# ---- Estimate power at N=12 via simulation ----
# The reject() function encapsulates one simulated experiment:
# draw N mice from each population, run a Welch t-test, return TRUE if p < alpha.
# Using Welch's t-test (default) is appropriate here because we do not know in
# advance whether the two populations have equal variances.
alpha <- 0.05
N     <- 12
B     <- 2000  # number of simulated experiments — 2000 is a practical balance
               # between precision and computation time (SE of power estimate ≈ 0.01)

reject <- function(N, alpha = 0.05) {
  hf      <- sample(hfPheno, N)
  control <- sample(controlPheno, N)

  # Guard: t.test requires at least 2 observations per group
  if (length(hf) < 2 || length(control) < 2) return(NA)

  pval <- t.test(hf, control)$p.value
  return(pval < alpha)
}

# Verify a single run works before scaling up
reject(12)

# replicate() is preferred over a for-loop here because it is cleaner and
# marginally faster; it applies the expression B times and returns a vector.
rejections <- replicate(B, reject(N))

# Remove any NA results (would occur if sample() returned a degenerate group)
rejections <- rejections[!is.na(rejections)]
cat("\nEstimated power at N=12:", round(mean(rejections), 3),
    "— only ~21%, meaning we miss the real effect ~79% of the time.\n")

# ---- Power curve: how does power change with sample size? ----
# This is the most actionable result for study design: it tells you the minimum
# N needed to reliably detect the effect you expect.
Ns <- seq(5, 50, 5)
power <- sapply(Ns, function(N) {
  reps <- replicate(B, reject(N))
  reps <- reps[!is.na(reps)]
  mean(reps)
})

print(data.frame(N = Ns, Power = round(power, 3)))

plot(Ns, power, type = "b",
     xlab = "Sample Size per Group", ylab = "Power (1 - β)",
     main = "Power vs Sample Size",
     ylim = c(0, 1))
abline(h = 0.8, col = "red", lty = 2)  # 80% power — the conventional target
text(x = max(Ns) * 0.7, y = 0.82, "80% target", col = "red", cex = 0.8)

# Observation: power is very low at small N (≈21% at N=12) and increases
# as sample size grows, crossing 80% somewhere around N=25–30 for this effect size.
# Practical lesson: design studies with a pre-specified N based on a power
# calculation, not the other way around.
