# ============================================
# Understanding T-Test Components
# ============================================

# Load test data
source("exercises/test_data.R")

# STEP 1: Calculate the difference in means
# ------------------------------------------
# Why: This is the "effect" we're measuring
mean_control <- mean(control)
mean_treatment <- mean(treatment)
diff_means <- mean_treatment - mean_control

print("Step 1: Calculate difference in means")
print(paste("Control mean:", round(mean_control, 2)))
print(paste("Treatment mean:", round(mean_treatment, 2)))
print(paste("Difference:", round(diff_means, 2)))

# STEP 2: Calculate standard error
# ------------------------------------------
# Why: Tells us how much means vary due to random sampling
# Formula: SE = sqrt(variance1/n1 + variance2/n2)

n_control <- length(control)
n_treatment <- length(treatment)

variance_control <- var(control)
variance_treatment <- var(treatment)

standard_error <- sqrt(variance_control/n_control + variance_treatment/n_treatment)

standard_error <- sqrt(var(control)/length(control) + var(treatment)/length(treatment)) #same as above, such shorter

print("\nStep 2: Calculate standard error")
print(paste("SE:", round(standard_error, 2)))

# STEP 3: Calculate t-statistic
# Why: Standardizes the difference so we can compare to known distribution
# Formula: t = (difference in means) / (standard error)

t_statistic <- diff_means / standard_error

print("\nStep 3: Calculate t-statistic")
print(paste("t-statistic:", round(t_statistic, 2)))

# STEP 4: Calculate p-value
# ------------------------------------------
# Why: Tells us probability of seeing this difference by chance
# Uses t-distribution with degrees of freedom

degrees_freedom <- n_control + n_treatment - 2
p_value <- 2 * pt(-abs(t_statistic), df = degrees_freedom)

print("\nStep 4: Calculate p-value")
print(paste("Degrees of freedom:", degrees_freedom))
print(paste("p-value:", round(p_value, 4)))

# STEP 5: Compare with R's built-in t.test()
# ------------------------------------------
result <- t.test(treatment, control)
print("\n=== Comparison with R's t.test() ===")
print(result)
