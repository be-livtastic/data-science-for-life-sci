# Install and load required packages mentioned in course book
install.packages("rafalib")
library(rafalib)

install.packages("Rtools")     # Needed for building some R packages
install.packages("devtools")   # Development tools
install.packages("downloader") # For downloading files from URLs
library(downloader)

install.packages("dplyr")
library(dplyr)

# ---- Download female mice weights dataset ----
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
download(url, destfile = filename)

# Read the data into R
dat <- read.csv(filename)

# Filter data to include only chow diet mice
controls <- filter(dat, Diet == "chow")

# Extract the Bodyweight column (as a data frame)
select(controls, Bodyweight)

# Convert Bodyweight column to a numeric vector
unlist(select(controls, Bodyweight))

# Same operation as above, written as a single pipeline
controls <- filter(dat, Diet == "chow") %>%
  select(Bodyweight) %>%
  unlist()

# Extract Bodyweight for Treatment group using a single pipeline
treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()

# Random Variables and Statistical Inference
mean(controls)
mean(treatment)

# Calculate the difference in means between treatment and control groups
diff_in_means <- mean(treatment) - mean(controls)
diff_in_means

# ---- Sampling from the Mice Population to see the mean is the same as to the controls/treatment groups----
population<- read.csv("femaleControlsPopulation.csv")
head(population)
poplation_weights <- unlist(population)

mean(sample(poplation_weights, 12)) # Repeat the sampling process multiple times to see the variability in sample means


# Testing null hypothesis by simulating random assignment of mice to treatment and control groups
control_sample <- sample(poplation_weights, 12)
treatment_sample <- sample(poplation_weights, 12)

mean(treatment_sample) - mean(control_sample)

# Repeat the random assignment process many times to build a null distribution
n<- 10000
nulls<-replicate(n, {
  control_sample <- sample(poplation_weights, 12)
  treatment_sample <- sample(poplation_weights, 12)
  mean(treatment_sample) - mean(control_sample)
})

# Visualize the null distribution
hist(nulls, main="Null Distribution of Mean Differences", xlab="Mean Difference (Treatment - Control)")
abline(v=diff_in_means, col="red", lwd=2)

# Calculate the p-value
p_value <- mean(abs(nulls) >= abs(diff_in_means))
p_value
p_value < 0.05
# Conclusion: If p-value is less than 0.05, we reject the null hypothesis and conclude that the diet has a significant effect on mice weights.


# --- Normal Distribution and Central Limit Theorem ----
?set.seed #.Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R.
set.seed(1) # Set seed for reproducibility
# n was already defined as 10000

# sample size of 5
avg_5 <- numeric(n)  # Pre-allocate a vector to store sample means

# For-loop to take samples
for (i in 1:n) {
  sample_5 <- sample(poplation_weights, size = 5)
  avg_5[i] <- mean(sample_5)
}
avg_5

# sample size of 50
set.seed(1)
avg_50 <- numeric(n)
for (i in 1:n) {
  sample_50 <- sample(poplation_weights, size = 50)
  avg_50[i] <- mean(sample_50)
}
avg_50

# Visualizing the normal distributions of sample means for size 5 and size 50
par(mfrow=c(1,2))  # Set up the plotting area to have 1 row and 2 columns
hist(avg_5, main="Sample Size = 5", xlab="Sample Means", col="lightblue", border="black")
hist(avg_50, main="Sample Size = 50", xlab="Sample Means", col="lightgreen", border="black")
par(mfrow=c(1,1))  # Reset plotting area to default

#Observations: Sample size 5 → wide distribution while Sample size 50 is a more tight distribution

# ---- Assessing Normality of Mice Weights ----
datPheno <- read.csv("mice_pheno.csv") # Load the mice phenotype dataset
datPheno <- na.omit(datPheno) # Remove rows with missing values)
head(datPheno)
controlPheno <- filter( datPheno, Sex == "F" & Diet == "chow") %>%
select( Bodyweight) %>% unlist()

hfPheno <- filter( datPheno, Sex == "F" & Diet == "hf") %>%
select( Bodyweight) %>% unlist()

par(mfrow=c(1,2))
hist(controlPheno, main="Control Group Bodyweights", xlab="Bodyweight", col="lightblue", border="black")
hist(hfPheno, main="High Fat Diet Group Bodyweights", xlab="Bodyweight", col="lightgreen", border="black")
par(mfrow=c(1,1))

# qq-plots to confirm distribution are relatively close to being normal
#If the points fall on the identity line, then the data is close to the theoretical distribution.
par(mfrow=c(1,2))
qqnorm(controlPheno, main="QQ-Plot: Control Group")
qqline(controlPheno, col="red")
qqnorm(hfPheno, main="QQ-Plot: High Fat Diet Group")
qqline(hfPheno, col="red")
par(mfrow=c(1,1))

#Observations: Both groups show some deviations from normality, especially in the tails, but overall they are reasonably close to a normal distribution.

# ---- Using t-test to compare means of two groups ----
t_sim <- diff_in_means / sd(nulls)
t_sim

# Manual calculation of t-statistic
n_control <- length(controls)
n_treat <- length(treatment)
# Calculate pooled standard deviation
se_pooled <- sqrt(
  var(controls)/n_control + var(treatment)/n_treat
)

t_actual <- diff_in_means / se_pooled
t_actual

# Using built-in t.test function
t_test_result <- t.test(treatment, controls)
t_test_result$statistic  # t-statistic
# Conclusion: Both manual calculation and t.test function yield similar t-statistics and p-values, confirming the significant effect of diet on mice weights.
#t_sim has a different value than t_actual because t_sim is calculated based on the null distribution generated from random assignments, while t_actual is calculated using the actual sample data.

# ---- Confidence Intervals ----
# 95% Confidence Interval using t.test
t_test_result
# The 95% confidence interval suggests that we can be 95% confident that the true difference in means between the treatment and control groups lies within this range.
# The confidence interval gives the range of plausible true differences: -0.043 to 6.085
# This means the true average difference could be: slightly negative (treatment a tiny bit lighter), or as large as about 6 units heavier.

dat<- read.csv("mice_pheno.csv")
chowPopulation <- dat[dat$Sex == "F" & dat$Diet == "chow", 3]

mu_chow <- mean(chowPopulation, na.rm=TRUE)
print(mu_chow)

N<- 30 #
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

# --- Visualizing Multiple Confidence Intervals ---
B <- 250
mypar()
plot(mean(chowPopulation)+c(-7,7), c(1,1), type="n", xlab="Bodyweight", ylab="interval", ylim = c(1,B))
abline(v=mean(chowPopulation), col="red", lwd=2) # True mean line
set.seed(1)
for(b in 1:B){
  chow <- sample(chowPopulation, N)
  se_chow <- sd(chow)/sqrt(N)
  interval <- c(mean(chow) - Q * se_chow, mean(chow) + Q * se_chow)
  covered <-
    mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered, "black", "red")
  segments(interval[1], b, interval[2], b, col=color)
  lines(interval, c(b,b), col=color, lwd=2)
}
# Observations: Most intervals (black lines) contain the true mean (red line), while a few (red lines) do not, illustrating the concept of confidence intervals.
# Conclusion: This visualization demonstrates that while most confidence intervals capture the true population mean, a small proportion do not, consistent with the 95% confidence level.

# --- Power Calculations ---
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

hf <- sample(hfPheno,N)
control <- sample(controlPheno,N)
t.test(hf,control)$p.value
# Fail to get significant result due to small sample size

alpha <- 0.05
N <- 12
B <- 2000
reject <- function(N, alpha=0.05){
  hf <- sample(hfPheno,N)
  control <- sample(controlPheno,N)
  pval <- t.test(hf,control)$p.value
  return(pval < alpha)
}
reject (12)

rejections <- replicate(B, reject(N))
mean(rejections) # Estimated power with sample size of 12 which is about 21%

#Improving power by increasing sample size
Ns <- seq(5, 50, 5)
power <- sapply(Ns, function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})
power
plot(Ns, power, type="b", xlab="Sample Size", ylab="Power", main="Power vs Sample Size")
abline(h=0.8, col="red", lty=2) # Desired
# Observations: Power increases with sample size, reaching the desired 80% power at a certain sample size.
# Conclusion: To achieve adequate power (e.g., 80%), researchers should consider increasing sample size based on power analysis results.

# --- Monte Carlo Simulation ---
# --- Monte Carlo Simulation of t-statistic under the null hypothesis ---

set.seed(1)

# function to generate one t-statistic under H0 (no diet effect)
ttestgenerator <- function(n, population){
  group1 <- sample(population, n)
  group2 <- sample(population, n)

  tstat <- (mean(group1) - mean(group2)) /
    sqrt(var(group1)/n + var(group2)/n)

  return(tstat)
}

# number of Monte Carlo simulations
B <- 5000

# two different sample sizes
n_small <- 3
n_large <- 10

# simulate t-statistics
tstats_small <- replicate(B, ttestgenerator(n_small, chowPopulation))
tstats_large <- replicate(B, ttestgenerator(n_large, chowPopulation))

# Histograms of simulated t-statistics
par(mfrow = c(1,2))
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

#--- Premutation Test ---
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

# --- End of code.R ---
