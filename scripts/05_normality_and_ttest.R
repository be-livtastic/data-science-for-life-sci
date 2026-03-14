library(dplyr)

# ---- Setup: re-load and clean data ----
dat <- read.csv("data/femaleMiceWeights.csv")
controls  <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf")   %>% select(Bodyweight) %>% unlist()
controls  <- controls[!is.na(controls)]
treatment <- treatment[!is.na(treatment)]
diff_in_means <- mean(treatment) - mean(controls)

# Population data is needed to rebuild the null distribution for t_sim below
population <- read.csv("data/femaleControlsPopulation.csv")
population_weights <- unlist(population)
population_weights <- population_weights[!is.na(population_weights)]

# ---- Assessing Normality of Mice Weights ----
# The t-test assumes the data (or at least the sample means) are approximately normal.
datPheno <- read.csv("data/mice_pheno.csv")

# na.omit() drops any row that has a missing value in ANY column.
datPheno <- na.omit(datPheno)
cat("Rows after NA removal:", nrow(datPheno), "\n")

controlPheno <- filter(datPheno, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist()
hfPheno <- filter(datPheno, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist()

# Guard: ensure both groups have enough data to be meaningful
if (length(controlPheno) < 2) stop("Control phenotype group has fewer than 2 observations.")
if (length(hfPheno)      < 2) stop("HF phenotype group has fewer than 2 observations.")

# ---- Histograms: visual check for obvious non-normality ----
# Severe skew, bimodality, or heavy tails would all show up here.
par(mfrow = c(1, 2))
hist(controlPheno,
     main = "Control (chow) Bodyweights", xlab = "Bodyweight (g)",
     col  = "lightblue", border = "black")
hist(hfPheno,
     main = "High Fat Diet Bodyweights",  xlab = "Bodyweight (g)",
     col  = "lightgreen", border = "black")
par(mfrow = c(1, 1))

# ---- QQ-plots: more sensitive normality check ----
# A QQ-plot compares the empirical quantiles of the data to the theoretical
# quantiles of a normal distribution. Points lying on the red reference line
# indicate normality; systematic S-curves or heavy tails signal departures.
par(mfrow = c(1, 2))
qqnorm(controlPheno, main = "QQ-Plot: Control (chow)")
qqline(controlPheno, col = "red")
qqnorm(hfPheno,      main = "QQ-Plot: High Fat Diet")
qqline(hfPheno,      col = "red")
par(mfrow = c(1, 1))
# Observation: Both groups are reasonably normal with mild tail deviations —
# acceptable for t-test use, especially given the sample sizes.

# ---- Test for equality of variances ----
var_test <- var.test(treatment, controls)
var_test

cat("\nVariance ratio (treatment / controls):", round(var(treatment) / var(controls), 3), "\n")
cat("var.test p-value:", round(var_test$p.value, 4), "\n")
if (var_test$p.value < 0.05) {
  message("Variances are significantly different — use Welch's t-test (var.equal=FALSE).")
} else {
  message("No significant variance difference detected — either test is appropriate, but Welch's is still safer.")
}

# ---- Welch's t-test vs Student's t-test ----
# Student's t-test (var.equal=TRUE): pools the two variances into one estimate.
#   Pro: slightly more statistical power when variances truly are equal.
#   Con: inflated Type I error (false positives) when variances differ.
#
# Welch's t-test (var.equal=FALSE, the R default): estimates each variance
#   separately and uses the Welch–Satterthwaite equation to adjust degrees of
#   freedom. It is robust to unequal variances at very little cost when
#   variances actually are equal. Rule of thumb: always use Welch's.
t_test_welch   <- t.test(treatment, controls, var.equal = FALSE)  # Welch's (default, safer)
t_test_student <- t.test(treatment, controls, var.equal = TRUE)   # Student's (assumes equal var)

cat("\nWelch's t-test:\n");   print(t_test_welch)
cat("\nStudent's t-test:\n"); print(t_test_student)

# Compare the two: if results differ substantially, variances are unequal enough
# to matter — trust Welch's. If they agree, the assumption had little effect.
cat("\nDifference in p-values (Welch vs Student):",
    round(t_test_welch$p.value - t_test_student$p.value, 6), "\n")

# ---- Simulation-based t-statistic (t_sim) ----
# Here we standardise diff_in_means by the SD of the null distribution
# (built from random permutations of the same population).
# This is conceptually related to a t-statistic but is NOT the same thing:
# it uses empirical SD of the null distribution rather than the pooled SE from
# the actual samples — hence it will differ numerically from t_actual.
n <- 10000
nulls <- replicate(n, {
  control_sample   <- sample(population_weights, 12)
  treatment_sample <- sample(population_weights, 12)
  mean(treatment_sample) - mean(control_sample)
})

t_sim <- diff_in_means / sd(nulls)
t_sim

# ---- Manual calculation of t-statistic ----
# Replicating what t.test() does internally to build intuition.
# SE of the difference in means = sqrt(var1/n1 + var2/n2)  ← Welch's formula
n_control <- length(controls)
n_treat   <- length(treatment)
se_welch  <- sqrt(var(controls)/n_control + var(treatment)/n_treat)
t_actual  <- diff_in_means / se_welch
t_actual

# t_test_welch$statistic should match t_actual (they use the same formula)
t_test_welch$statistic

# Summary:
# t_sim    ≈ diff / SD(null distribution from permutation)  → empirical approach
# t_actual = diff / SE(Welch)                               → analytic approach
# They are related in spirit but differ because t_sim's denominator is estimated
# from the full population null distribution, not just the two observed samples.
