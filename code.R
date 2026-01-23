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
t_test_result$p.value    # p-value
# Conclusion: Both manual calculation and t.test function yield similar t-statistics and p-values, confirming the significant effect of diet on mice weights.
#t_sim has a different value than t_actual because t_sim is calculated based on the null distribution generated from random assignments, while t_actual is calculated using the actual sample data.

# ---- Confidence Intervals ----

