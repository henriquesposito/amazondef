# Inductive (pre)Analysis

# Load packages
library(dplyr)
library(stringr)

# Load data
BR_Presidential_Speeches <- readRDS("~/GitHub/amazondef/BR_Presidential_Speeches.Rds") # your path might differ

# Wrangle data
# Get exact dates 
BR_Presidential_Speeches$date <- poldis::extract_date(BR_Presidential_Speeches$text)
# There are a lot of NAs... It seems some dates in texta re missing the year.
# Get titles (first sentence)
BR_Presidential_Speeches$title <- poldis::extract_title(BR_Presidential_Speeches$text)

# For dates that are missing the year, let's complete them!
d <- BR_Presidential_Speeches$title
d <- stringr::str_extract(d, "(\\S+)\\s*DE\\s*(\\S+)")
d <- paste0(d, "-", BR_Presidential_Speeches$year)
d <- gsub(" DE ", "-", d)
# load months data from poldis
load("~/GitHub/poldis/R/sysdata.rda") # your path might differ
months <- as.data.frame(months)
for (k in seq_len(nrow(months))) {
  d <- gsub(paste0(months$months[k]),
              paste0(months$number[k]),
              d, ignore.case = TRUE,
              perl = T)
}
d <- stringr::str_replace_all(d, "NA-", "?-?-")
d <- ifelse(stringr::str_detect(d, "[A-Z]|[a-z]"), paste0("?-?-", BR_Presidential_Speeches$year), d)
# Let's complete our dates column now
BR_Presidential_Speeches$date <- ifelse(is.na(BR_Presidential_Speeches$date),
                                        d, BR_Presidential_Speeches$date)

min(BR_Presidential_Speeches$date)

# Text Cleaning

# Text pre-analysis
