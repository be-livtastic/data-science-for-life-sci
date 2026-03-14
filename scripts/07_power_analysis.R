library(dplyr)

# ---- Setup: re-load data ----
datPheno <- read.csv("data/mice_pheno.csv")
datPheno <- na.omit(datPheno)
controlPheno <- filter(datPheno, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist()
hfPheno <- filter(datPheno, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist()

# ---- Power Calculations ----
# Types of Errors
# Type I Error (α): Rejecting the null hypothesis when it is true (false positive).
# Type II Error (β): Failing to reject the null hypothesis when it is false (false negative).

# Power of a test: The probability of correctly rejecting the null hypothesis when it is false (1 - β).
# Factors affecting power:
# Effect Size: Larger effect sizes increase power.
# Sample Size: Larger sample sizes increase power.
# Significance Level (α): Higher α levels increase power but also increase the risk of Type I errors.
# Variability: Lower variability in the data increases power.

set.seed(1)

N <- 30
hf <- sample(hfPheno, N)
control <- sample(controlPheno, N)
t.test(hf, control)$p.value
# Fail to get significant result due to small sample size

alpha <- 0.05
N <- 12
B <- 2000
reject <- function(N, alpha=0.05) {
  hf <- sample(hfPheno, N)
  control <- sample(controlPheno, N)
  pval <- t.test(hf, control)$p.value
  return(pval < alpha)
}
reject(12)

rejections <- replicate(B, reject(N))
mean(rejections) # Estimated power with sample size of 12 which is about 21%

# Improving power by increasing sample size
Ns <- seq(5, 50, 5)
power <- sapply(Ns, function(N) {
  rejections <- replicate(B, reject(N))
  mean(rejections)
})
power
plot(Ns, power, type="b", xlab="Sample Size", ylab="Power", main="Power vs Sample Size")
abline(h=0.8, col="red", lty=2) # Desired power level
# Observations: Power increases with sample size, reaching the desired 80% power at a certain sample size.
# Conclusion: To achieve adequate power (e.g., 80%), researchers should consider increasing sample size based on power analysis results.
