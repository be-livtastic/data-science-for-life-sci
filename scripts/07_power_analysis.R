library(dplyr)

# ---- Setup: re-load and clean data ----
datPheno <- read.csv("data/mice_pheno.csv")

datPheno <- na.omit(datPheno)

controlPheno <- filter(datPheno, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist()
hfPheno <- filter(datPheno, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist()

cat("Control population size:", length(controlPheno), "\n")
cat("HF population size:     ", length(hfPheno), "\n")

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
# stochastic. 

# ---- Estimate power at N=12 via simulation ----
# The reject() function encapsulates one simulated experiment:.
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
