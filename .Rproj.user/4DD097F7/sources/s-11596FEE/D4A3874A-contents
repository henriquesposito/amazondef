# Inductive (pre)Analysis

# Load packages
library(dplyr)

# Load data
BR_Presidential_Speeches <- readRDS("~/GitHub/amazondef/BR_Presidential_Speeches.Rds")

# Wrangle data
BR_Presidential_Speeches$date <- poldis::extract_date(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- poldis::extract_title(BR_Presidential_Speeches)

# Text Cleaning

# Text pre-analysis
