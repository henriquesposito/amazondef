# Data Wrangling

# This script explains how the data was coded and where it comes from.
# It is intended for transparency, rather than reproduciability.
# For more information on the data, please contact the authors.
# The dataset is available in the repository,
# feel free to use but cite us.

# # Load packages
# library(dplyr)
# library(stringr)
# #Load poldis
# remotes::install_github(henriquesposito/poldis)
# # Get Data, join and save into repository
# # This portion explains how/where data was gathered and wrangled before it was saved into the package.
# # It is commented out as it is here for transparency rather than reproduciability.
# # Please contact authors for access to original datasets for reproduction.
#
# # Load BR_oral from AP repository
# # load("~/GitHub/authenticity_performances/data/BR_oral.rda") # your path my differ here
# # Select only some variables
# BR_oral <- BR_oral %>%
#   dplyr::select("presid", "date", "party", "text") %>%
#   rename(president = "presid", year = "date")
#
# # Wrangle data
# # Get exact dates 
# BR_oral$date <- poldis::extract_date(BR_oral$text)
# # There are a lot of NAs... It seems some dates in text are missing the year...
# # Get titles (first sentence)
# BR_oral$title <- poldis::extract_title(BR_oral$text)
# # For dates that are missing the year, let's complete them!
# d <- BR_oral$title
# d <- stringr::str_extract(d, "(\\S+)\\s*DE\\s*(\\S+)")
# d <- paste0(d, "-", BR_oral$year)
# d <- gsub(" DE ", "-", d)
# # load months data from poldis
# # load("~/GitHub/poldis/R/sysdata.rda") # your path might differ
# months <- as.data.frame(months)
# for (k in seq_len(nrow(months))) {
#   d <- gsub(paste0(months$months[k]),
#               paste0(months$number[k]),
#               d, ignore.case = TRUE,
#               perl = T)
# }
# d <- stringr::str_replace_all(d, "NA-", "?-?-")
# d <- ifelse(stringr::str_detect(d, "[A-Z]|[a-z]"), paste0("?-?-", BR_oral$year), d)
# # Let's complete our dates column now
# BR_oral$date <- ifelse(is.na(BR_oral$date), d, BR_oral$date)
# 
# # Merge with data fro missing ttexts from January 2020 until october 2021
# BR_speeches_2020_21 <- read_excel("C:/Users/h-spo/Desktop/BR_speeches_2020-21.xlsx")
# BR_speeches_2020_21 <- BR_speeches_2020_21 %>%
#   dplyr::rename(prsident = "President", year = "Year", text = "Text", date = "Date", title = "Title")
# BR_speeches_2020_21$party <- NA_character_ # Bolsonaro does not have a party anymore
# BR_speeches_2020_21$date <- as.character(BR_speeches_2020_21$date)
# BR_speeches_2020_21 <- BR_speeches_2020_21[order(BR_speeches_2020_21$date),]
#
# # Join and save dataset for use
# BR_Presidential_Speeches <- dplyr::full_join(BR_oral, BR_speeches_2020_21)
# # Get location
# BR_Presidential_Speeches$location <- poldis::extract_location(BR_Presidential_Speeches$title)
# saveRDS(BR_Presidential_Speeches, file = "BR_Presidential_Speeches.Rds")
#
# # # Merge with data fro missing texts from October 2020 until December 2021
# Speeches_BR_oct_dec_21 <- read_excel("C:/Users/h-spo/Desktop/Speeches_BR_oct-dec-21.xlsx")
# Speeches_BR_oct_dec_21$party <- NA_character_ # Bolsonaro does not have a party anymore
# Speeches_BR_oct_dec_21$date <- as.character(Speeches_BR_oct_dec_21$date)
# Speeches_BR_oct_dec_21$location <- poldis::extract_location(Speeches_BR_oct_dec_21$title)
# Speeches_BR_oct_dec_21 <- Speeches_BR_oct_dec_21[order(Speeches_BR_oct_dec_21$date),]
# 
# # # Join and save dataset for use
# BR_Presidential_Speeches <- dplyr::full_join(BR_Presidential_Speeches, Speeches_BR_oct_dec_21)
# Get missing locations
# BR_Presidential_Speeches$location <- ifelse(grepl("^NA$", BR_Presidential_Speeches$location),
#                                             poldis::extract_location(BR_Presidential_Speeches$text),
#                                             BR_Presidential_Speeches$location)
# BR_Presidential_Speeches$location <- ifelse(grepl("", BR_Presidential_Speeches$location),
#                                             poldis::extract_location(BR_Presidential_Speeches$text),
#                                             BR_Presidential_Speeches$location)
# BR_Presidential_Speeches$location <- ifelse(grepl("Dir", BR_Presidential_Speeches$location),
#                                             poldis::extract_location(BR_Presidential_Speeches$location),
#                                             BR_Presidential_Speeches$location)
# BR_Presidential_Speeches$location <- ifelse(grepl("Sind", BR_Presidential_Speeches$location),
#                                             poldis::extract_location(BR_Presidential_Speeches$location),
#                                             BR_Presidential_Speeches$location)
# summary(as.factor(BR_Presidential_Speeches$location))
# 
# saveRDS(BR_Presidential_Speeches, file = "BR_Presidential_Speeches.Rds")
