# Prepare final data for anylsis
# Amazon statements dataset
library(dplyr)
final_labeled_data <- readRDS("~/Documents/GitHub/amazondef/data/final_labeled_data.Rds")
final_labeled_data$false_positives <- as.factor(ifelse(final_labeled_data$false_positives > 0.5, 1, 0))
final_labeled_data$sov <- as.factor(ifelse(as.numeric(final_labeled_data$sov) > 0.5, 1, 0))
final_labeled_data$EI <- as.factor(ifelse(as.numeric(final_labeled_data$EI) > 0.5, 1, 0))
final_labeled_data$SD <- as.factor(ifelse(as.numeric(final_labeled_data$SD) > 0.5, 1, 0))
final_labeled_data$con <- as.factor(ifelse(as.numeric(final_labeled_data$con) > 0.5, 1, 0))
summary(final_labeled_data)
fp <- dplyr::filter(final_labeled_data, false_positives == "1" & sov == "0" & EI == "0" & SD == "0" & con == "0")
final_data <- dplyr::anti_join(final_labeled_data, fp, "AM2")
summary(final_data)
# re-assign location with new version of extract_location()
# function that does not contain "Brazil" as a match
final_data$location <- poldis::extract_location(final_data$title)
summary(as.factor(final_data$location))
# if NA for location in title, use full text
final_data$location <- ifelse(grepl("^NA$", final_data$location),
                              poldis::extract_location(final_data$text),
                              final_data$location)
summary(as.factor(final_data$location))
# a few obs were not matched still, let's see them
final_data$location <- ifelse(nchar(final_data$location) > 40, "NA", final_data$location)
summary(as.factor(final_data$location))
nm <- final_data %>% dplyr::filter(location == "NA") %>% select(AM2, text)
View(nm) # we can hand code some of these
nm$location_na <- c("Para", "Acre", "Bahia", "Para", "Para", "Para", "United States of America",
                    "Para",  "Minas Gerais", "Rio Grande do Sul", "Para", "United States of America",
                    "Rio Grande do Sul", "Amazonas", "Para", "NA", "Acre", "Para",
                    "Rio Grande do Sul", "Para", "Para", "NA", "NA", "Amazonas")
nm <- dplyr::select(nm, AM2, location_na)
fd_location <- dplyr::full_join(final_data, nm, "AM2")
fd_location$location <- ifelse(grepl("NA", fd_location$location), fd_location$location_na, fd_location$location)
final_data_as <- fd_location %>% select(-location_na)
summary(as.factor(final_data_as$location))
# save
# saveRDS(final_data_as, "final_data_as.Rds")

# Let's redo location for the raw data as well
# Get location from title and text
BR_Presidential_Speeches <- readRDS("~/Documents/GitHub/amazondef/data/BR_Presidential_Speeches.Rds")
# Replace on title and text
BR_Presidential_Speeches$text <- stringr::str_replace_all(BR_Presidential_Speeches$text, " - | -| -", "-")
BR_Presidential_Speeches$title <- stringr::str_replace_all(BR_Presidential_Speeches$title, " - | -| -", "-")
# Replace "para" to avoid confusion with state name in Brazil
BR_Presidential_Speeches$text <- stringr::str_replace_all(BR_Presidential_Speeches$text,"^para$", "pra")
BR_Presidential_Speeches$title <- stringr::str_replace_all(BR_Presidential_Speeches$title, "^para$", "pra")
# Make it lower case, remove extra space, and add coma space
BR_Presidential_Speeches$text <- tolower(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tolower(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::stripWhitespace(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::stripWhitespace(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- textclean::add_comma_space(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- textclean::add_comma_space(BR_Presidential_Speeches$title)
# A few more things
BR_Presidential_Speeches$text <- stringi::stri_trans_general(BR_Presidential_Speeches$text, id = "latin-ascii")
BR_Presidential_Speeches$title <- stringi::stri_trans_general(BR_Presidential_Speeches$title, id = "latin-ascii")
BR_Presidential_Speeches$text <- stringr::str_squish(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- stringr::str_squish(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::removeNumbers(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::removeNumbers(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::removePunctuation(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::removePunctuation(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- trimws(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- trimws(BR_Presidential_Speeches$title)
# get locations from title
BR_Presidential_Speeches$location <- poldis::extract_location(BR_Presidential_Speeches$title)
summary(as.factor(BR_Presidential_Speeches$location))
# get location from text if not found
BR_Presidential_Speeches$location <- ifelse(grepl("^NA$", BR_Presidential_Speeches$location),
                                            poldis::extract_location(BR_Presidential_Speeches$text),
                                            BR_Presidential_Speeches$location)
summary(as.factor(BR_Presidential_Speeches$location))
# I am not going to had code the 313 NAs here for now, but if we need we can do so...
# saveRDS(BR_Presidential_Speeches, "BR_presid_speeches_final.Rds")
