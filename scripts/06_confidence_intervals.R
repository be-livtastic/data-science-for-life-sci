library(rafalib)
library(dplyr)

# ---- Setup: re-load data ----
dat <- read.csv("data/femaleMiceWeights.csv")
controls <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()

# ---- Confidence Intervals ----
# 95% Confidence Interval using t.test
t_test_result <- t.test(treatment, controls)
t_test_result
# The 95% confidence interval suggests that we can be 95% confident that the true difference in means between the treatment and control groups lies within this range.
# The confidence interval gives the range of plausible true differences: -0.043 to 6.085
# This means the true average difference could be: slightly negative (treatment a tiny bit lighter), or as large as about 6 units heavier.

dat_pheno <- read.csv("data/mice_pheno.csv")
chowPopulation <- dat_pheno[dat_pheno$Sex == "F" & dat_pheno$Diet == "chow", 3]

mu_chow <- mean(chowPopulation, na.rm=TRUE)
print(mu_chow)

N <- 30 #
chowSample <- sample(chowPopulation, N) # Take a random sample of size N from the chow population
print(chowSample)
mean_chowSample <- mean(chowSample)
print(mean_chowSample)
se_chowSample <- sd(chowSample)/sqrt(N)
print(se_chowSample)

Q <- qnorm(1 - 0.05/2) # Z-score for 95% confidence interval
interval <- c(mean_chowSample - Q * se_chowSample, mean_chowSample + Q * se_chowSample)
print(interval)
interval[1] < mu_chow & interval[2] > mu_chow # Check if the true population mean is within the confidence interval
# Repeat the sampling and interval calculation multiple times to see how often the true mean is captured
# Observations: In repeated samples, the confidence intervals will vary, and approximately 95% of them should contain the true population mean.

# ---- Visualizing Multiple Confidence Intervals ----
B <- 250
mypar()
plot(mean(chowPopulation, na.rm=TRUE) + c(-7,7), c(1,1), type="n", xlab="Bodyweight", ylab="interval", ylim=c(1,B))
abline(v=mean(chowPopulation, na.rm=TRUE), col="red", lwd=2) # True mean line
set.seed(1)
for (b in 1:B) {
  chow <- sample(chowPopulation, N)
  se_chow <- sd(chow)/sqrt(N)
  interval <- c(mean(chow) - Q * se_chow, mean(chow) + Q * se_chow)
  covered <-
    mean(chowPopulation, na.rm=TRUE) <= interval[2] & mean(chowPopulation, na.rm=TRUE) >= interval[1]
  color <- ifelse(covered, "black", "red")
  segments(interval[1], b, interval[2], b, col=color)
  lines(interval, c(b,b), col=color, lwd=2)
}
# Observations: Most intervals (black lines) contain the true mean (red line), while a few (red lines) do not, illustrating the concept of confidence intervals.
# Conclusion: This visualization demonstrates that while most confidence intervals capture the true population mean, a small proportion do not, consistent with the 95% confidence level.
