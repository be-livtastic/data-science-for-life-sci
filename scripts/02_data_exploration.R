library(dplyr)
library(downloader)

# ---- Download female mice weights dataset ----
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "data/femaleMiceWeights.csv"
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
