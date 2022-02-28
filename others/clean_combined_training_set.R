# Get and wrangle combined training set
library(readxl)
training_set_combined <- read_excel("data/training_set_combined.xlsx")
library(dplyr)
summary(as.factor(training_set_combined$decision_falsepositive)) # We have 3 ones ...
summary(as.factor(training_set_combined$decision_code)) # Not consistent
# Fix data issues first
training_set_combined$decision_falsepositive[103] <- "false positive"
training_set_combined$decision_falsepositive[225] <- "false positive"
training_set_combined$decision_code <- gsub("ecnomic|only economic", "economic",
                                            training_set_combined$decision_code)
training_set_combined$decision_code <- gsub("consevation", "conservation",
                                            training_set_combined$decision_code)
training_set_combined$decision_code <- gsub("soverignty|sovereigty|sovereingty", "sovereignty",
                                            training_set_combined$decision_code)
training_set_combined$decision_code <- gsub("only", "", training_set_combined$decision_code)
training_set_combined$decision_code <- gsub(" and ", ", ", training_set_combined$decision_code)
# Get categories back in as variables 
ts <- training_set_combined %>%
  mutate(code_check = ifelse(code_henrique == code_livio, 1, 0),
         false_positives = ifelse(grepl("^false positive$", decision_falsepositive), 1, 0),
         sov = ifelse(grepl("sovereignty", decision_code), 1, substring(code_henrique, 0, 1)),
         EI = ifelse(grepl("economic", decision_code), 1, substring(code_henrique, 2, 2)),
         SD = ifelse(grepl("social", decision_code), 1, substring(code_henrique, 3, 3)),
         con = ifelse(grepl("conservation", decision_code), 1, substring(code_henrique, 4, 4))) %>%
  select(-c(ID, check_decision_fp, check_code, decision_falsepositive, code_henrique, code_livio, decision_code))
