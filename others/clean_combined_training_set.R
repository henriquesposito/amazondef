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
  mutate(false_positives = ifelse(grepl("^false positive$", decision_falsepositive), 1, 0),
         sov = ifelse(grepl("sovereignty", decision_code), 1, substring(code_henrique, 0, 1)),
         EI = ifelse(grepl("economic", decision_code), 1, substring(code_henrique, 2, 2)),
         SD = ifelse(grepl("social", decision_code), 1, substring(code_henrique, 3, 3)),
         con = ifelse(grepl("conservation", decision_code), 1, substring(code_henrique, 4, 4))) %>%
  select(-c(ID, decision_falsepositive, code_henrique, code_livio, decision_code)) %>%
  mutate_all(~replace(., is.na(.), 0))
# Save data
# saveRDS(ts, "training_set_final.Rds")

# Check intercoder reliability for each category (without false positives)
check_reliability <- training_set_combined %>%
  filter(!grepl("^false positive$", decision_falsepositive)) %>% 
  mutate(code_livio = ifelse(is.na(code_livio), "0000", code_livio), 
         code_check = ifelse(code_henrique == code_livio, 1, 0),
         sov_h = substring(code_henrique, 0, 1),
         EI_h = substring(code_henrique, 2, 2),
         SD_h = substring(code_henrique, 3, 3),
         con_h = substring(code_henrique, 4, 4),
         sov_l = substring(code_livio, 0, 1),
         EI_l = substring(code_livio, 2, 2),
         SD_l = substring(code_livio, 3, 3),
         con_l = substring(code_livio, 4, 4)) %>% 
  mutate(check_sov = ifelse(sov_h == sov_l, 1, 0),
         check_ei = ifelse(EI_h == EI_l, 1, 0),
         check_sd = ifelse(SD_h == SD_l, 1, 0),
         check_con = ifelse(con_h == con_l, 1, 0)) %>% 
  select(-c(ID, decision_falsepositive, code_henrique, code_livio, decision_code,
            sov_h, EI_h, SD_h, con_h, sov_l, EI_l, SD_l, con_l))

# For all categories
sum(check_reliability$code_check)/550 # Intercoder reliability od 56% for all together but..
# Get intercoder scores per category
sum(check_reliability$check_sov)/550 # 91%
sum(check_reliability$check_ei)/550 # 76%
sum(check_reliability$check_sd)/550 # 86%
sum(check_reliability$check_con)/550 #89%
