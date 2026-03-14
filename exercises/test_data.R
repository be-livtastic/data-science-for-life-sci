# Load the mice data we've been working with
library(dplyr)

# Read your existing mice phenotype data
dat <- read.csv("data/mice_pheno.csv")

# Create test groups (like we did before)
control <- dat |>
  filter(Sex == "F" & Diet == "chow") |>
  pull(Bodyweight)

treatment <- dat |>
  filter(Sex == "F" & Diet == "hf") |>
  pull(Bodyweight)

# Check what we have
print(paste("Control group:", length(control), "mice"))
print(paste("Treatment group:", length(treatment), "mice"))