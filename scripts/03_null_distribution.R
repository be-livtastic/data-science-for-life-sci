library(dplyr)

# ---- Setup: re-load data from 02_data_exploration.R ----
dat <- read.csv("data/femaleMiceWeights.csv")
controls <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()
controls  <- controls[!is.na(controls)]
treatment <- treatment[!is.na(treatment)]
diff_in_means <- mean(treatment) - mean(controls)

# ---- Load and clean the female controls population ----
# femaleControlsPopulation.csv represents the full population of female chow-diet
# mice — our "ground truth" for what random variation looks like under no treatment.
# We use this to build a null distribution by simulation.
population <- read.csv("data/femaleControlsPopulation.csv")
population_weights <- unlist(population)

# Remove NAs before any sampling. sample() does not skip NAs — if an NA is drawn,
# mean() returns NA for that replicate and poisons the null distribution.
population_weights <- population_weights[!is.na(population_weights)]
cat("Population size (after NA removal):", length(population_weights), "\n")

# ---- One sample from the population ----
# Taking a single sample of 12 (matching the actual experiment size) shows how
# much the sample mean can vary even when there is truly no treatment effect.
mean(sample(population_weights, 12))

# ---- Simulate the null hypothesis: random group assignment ----
# The null hypothesis says diet has NO effect, so any observed difference in means
# is purely due to random assignment of animals to groups.
# We test this by repeatedly drawing two groups from the SAME population
# and recording the difference in means each time.
control_sample   <- sample(population_weights, 12)
treatment_sample <- sample(population_weights, 12)
mean(treatment_sample) - mean(control_sample)

# ---- Build the null distribution with 10,000 simulations ----
# 10,000 is large enough that the empirical distribution is stable and the
# p-value estimate has low Monte Carlo error (SE ≈ sqrt(p*(1-p)/n) ≈ 0.002).
n <- 10000
nulls <- replicate(n, {
  control_sample   <- sample(population_weights, 12)
  treatment_sample <- sample(population_weights, 12)
  mean(treatment_sample) - mean(control_sample)
})

# ---- Visualize: where does our observed difference fall in the null distribution? ----
# If diff_in_means falls deep in the tail, it is unlikely to have arisen by chance.
hist(nulls,
     main = "Null Distribution of Mean Differences",
     xlab = "Mean Difference (Treatment - Control)",
     col  = "lightgray", border = "white")
abline(v = diff_in_means, col = "red", lwd = 2)

# ---- Calculate the p-value ----
# The p-value is the fraction of null simulations that produced a difference
# at least as extreme (in either direction) as the one we actually observed.
# We use abs() because we are doing a two-tailed test — a large negative difference
# would be equally surprising under H0.
p_value <- mean(abs(nulls) >= abs(diff_in_means))
p_value
p_value < 0.05

# Conclusion: p < 0.05 means fewer than 5% of random assignments produce a
# difference this large, so we reject H0 and conclude the hf diet has a
# detectable effect on bodyweight.
