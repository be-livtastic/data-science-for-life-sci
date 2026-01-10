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

treatment <- filter(dat, Diet == "hf") %>% select(Bodyweight) %>% unlist()

# ---- Download sleep dataset ----
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url, filename)
