library(dplyr)

# ---- Setup: re-load data from 02_data_exploration.R ----
dat <- read.csv("data/femaleMiceWeights.csv")
controls <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()
diff_in_means <- mean(treatment) - mean(controls)

# ---- Sampling from the Mice Population to see the mean is the same as the controls/treatment groups ----
population <- read.csv("data/femaleControlsPopulation.csv")
head(population)
population_weights <- unlist(population)

mean(sample(population_weights, 12)) # Repeat the sampling process multiple times to see the variability in sample means

# Testing null hypothesis by simulating random assignment of mice to treatment and control groups
control_sample <- sample(population_weights, 12)
treatment_sample <- sample(population_weights, 12)

mean(treatment_sample) - mean(control_sample)

# Repeat the random assignment process many times to build a null distribution
n <- 10000
nulls <- replicate(n, {
  control_sample <- sample(population_weights, 12)
  treatment_sample <- sample(population_weights, 12)
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
