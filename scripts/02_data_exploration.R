library(dplyr)
library(downloader)

# ---- Download female mice weights dataset ----
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "data/femaleMiceWeights.csv"
download(url, destfile = filename)

# Read the data into R
dat <- read.csv(filename)

# ---- Sanity checks after loading ----
# Always inspect the data before doing anything with it.
stopifnot(all(c("Diet", "Bodyweight") %in% names(dat)))  # ensure expected columns exist
cat("Rows loaded:", nrow(dat), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(dat)))

# ---- Step-by-step: from data frame to numeric vector ----
# Showing the intermediate steps makes it clear why unlist() is needed.
# filter() returns a data frame, select() returns a data frame — unlist() collapses # nolint
# it to a plain numeric vector so we can do arithmetic on it (mean, var, etc.).

# Step 1: filter rows
controls_df <- filter(dat, Diet == "chow")

# Step 2: select the column we care about (still a data frame)
select(controls_df, Bodyweight)

# Step 3: flatten to a numeric vector
unlist(select(controls_df, Bodyweight))

# Same three steps as a single pipeline — the preferred style going forward
controls <- filter(dat, Diet == "chow") %>%
  select(Bodyweight) %>%
  unlist()

treatment <- filter(dat, Diet == "hf") %>%
  select(Bodyweight) %>%
  unlist()

# ---- Guard: make sure both groups have data ----
# If a diet label were misspelled in the CSV, filtering would silently return
# an empty vector and all downstream statistics would be meaningless.
if (length(controls) == 0) stop("No control (chow) mice found — check the Diet column values.")
if (length(treatment) == 0) stop("No treatment (hf) mice found — check the Diet column values.")
cat("Control group size:", length(controls), "\n")
cat("Treatment group size:", length(treatment), "\n")

# ---- Remove any NAs in bodyweight ----
# Even though this dataset is clean, being explicit about NA removal protects
# against future data updates or copy-paste into a different dataset.
controls  <- controls[!is.na(controls)]
treatment <- treatment[!is.na(treatment)]

# ---- Basic summary statistics ----
# These are the observed means — they will vary from experiment to experiment
# due to random sampling. The key question is whether the observed difference
# is larger than what we'd expect by chance alone (addressed in script 03).
mean(controls)
mean(treatment)

# The observed difference in means: this is our test statistic.
# A positive value means the hf (high fat) diet group weighed more on average.
diff_in_means <- mean(treatment) - mean(controls)
diff_in_means
