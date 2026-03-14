library(dplyr)

# ---- Setup: re-load data ----
dat <- read.csv("data/femaleMiceWeights.csv")
controls <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()
diff_in_means <- mean(treatment) - mean(controls)

population <- read.csv("data/femaleControlsPopulation.csv")
population_weights <- unlist(population)

# ---- Assessing Normality of Mice Weights ----
datPheno <- read.csv("data/mice_pheno.csv") # Load the mice phenotype dataset
datPheno <- na.omit(datPheno) # Remove rows with missing values
head(datPheno)
controlPheno <- filter(datPheno, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist()

hfPheno <- filter(datPheno, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist()

par(mfrow=c(1,2))
hist(controlPheno, main="Control Group Bodyweights", xlab="Bodyweight", col="lightblue", border="black")
hist(hfPheno, main="High Fat Diet Group Bodyweights", xlab="Bodyweight", col="lightgreen", border="black")
par(mfrow=c(1,1))

# QQ-plots to confirm distributions are relatively close to being normal
# If the points fall on the identity line, then the data is close to the theoretical distribution.
par(mfrow=c(1,2))
qqnorm(controlPheno, main="QQ-Plot: Control Group")
qqline(controlPheno, col="red")
qqnorm(hfPheno, main="QQ-Plot: High Fat Diet Group")
qqline(hfPheno, col="red")
par(mfrow=c(1,1))

# Observations: Both groups show some deviations from normality, especially in the tails, but overall they are reasonably close to a normal distribution.

# ---- Using t-test to compare means of two groups ----
# Rebuild nulls from the population
n <- 10000
nulls <- replicate(n, {
  control_sample <- sample(population_weights, 12)
  treatment_sample <- sample(population_weights, 12)
  mean(treatment_sample) - mean(control_sample)
})

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
# t_sim has a different value than t_actual because t_sim is calculated based on the null distribution generated from random assignments, while t_actual is calculated using the actual sample data.
