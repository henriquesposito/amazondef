# In this script we prepare the data with some text pre-processing,
# by extracting Amazonian statements, and 
# by adding locations for all speeches.

# Install poldis
# devtools::install_github("henriquesposito/poldis")
library(poldis)
library(dplyr)
library(readxl)

# Load data
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

# Make it latin SCII
BR_Presidential_Speeches$text <- stringi::stri_trans_general(BR_Presidential_Speeches$text, id = "latin-ascii")
BR_Presidential_Speeches$title <- stringi::stri_trans_general(BR_Presidential_Speeches$title, id = "latin-ascii")

# get context and update amazon specific data
Amazon_speeches <- dplyr::filter(BR_Presidential_Speeches, grepl("amazon", text))

# Get 2 sentences before and 2 sentences after with poldis
Amazon_speeches$AM2 <- poldis::extract_context("amazon", Amazon_speeches$text, "sentences", 2)

# Get and save long data coding
amazon_speeches_long <- Amazon_speeches %>%
  select(president, year, party, title, date, text, AM2) %>% 
  tidyr::unnest(cols = c(AM2))

# save the data
#saveRDS(amazon_speeches_long, "amazon_speeches_long.Rds") # 2048 obs

# Get a sample for hand coding
training_set_22 <- amazon_speeches_long %>%
  select(AM2) %>%
  dplyr::slice_sample(n = 1024)
# export to Excel
# xlsx::write.xlsx(training_set, file = "training_set.xlsx")

# A few more things
BR_Presidential_Speeches$text <- stringr::str_squish(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- stringr::str_squish(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::removeNumbers(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::removeNumbers(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::removePunctuation(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::removePunctuation(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- trimws(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- trimws(BR_Presidential_Speeches$title)

# Get locations from title
BR_Presidential_Speeches$location <- poldis::extract_location(BR_Presidential_Speeches$title)
summary(as.factor(BR_Presidential_Speeches$location))

# Get location from text if not found
BR_Presidential_Speeches$location <- ifelse(grepl("^NA$", BR_Presidential_Speeches$location),
                                            poldis::extract_location(BR_Presidential_Speeches$text),
                                            BR_Presidential_Speeches$location)
summary(as.factor(BR_Presidential_Speeches$location))

# Get a excel file just to replace NAs
# writexl::write_xlsx(BR_Presidential_Speeches, "BR_presid_speeches_final.xlsx")
# Most of the NAs left are TV or Radio Announcements in case we want to mark these as such.
BR_presid_speeches_final <- read_excel("data/BR_presid_speeches_final.xlsx")
summary(as.factor(BR_presid_speeches_final$location))

# Last thing here, let's add a dummy variable for whether certain speech mentions the stem "amazon" or not.
# As well as let's add a different categorical variable for location.
BR_presid_speeches_final <- BR_presid_speeches_final %>% 
  mutate(amazon_speech = ifelse(grepl("amazon", text), 1, 0),
         location_cat = case_when(grepl("Amazonas|Para|Roraima|Acre|Amapa|Rondonia|Mato-Grosso|Tocantins|Maranhao", location) ~ "Amazonian States",
                                  grepl("Alagoas|Bahia|Ceará|Goias|Minas Gerais|Espirito Santo|Paraiba|Parana|Pernambuco|Piaui|Rio de Janeiro|Rio Grande do Norte|Rio Grande do Sul|Santa Catarina|Sao Paulo|Brazil|Sergipe", location) ~ "Non Amazonian States",
                                  grepl("Distrito Federal", location) ~ "Brasilia",
                                  grepl("NA", location) ~ "Non Identified",
                                  !grepl("Amazonian States|Non Amazonian States|Brasilia|Non Identified", location) ~ "International"))
summary(as.factor(BR_presid_speeches_final$amazon_speech))
summary(as.factor(BR_presid_speeches_final$location_cat))

# save for analysis
saveRDS(BR_presid_speeches_final, "BR_presid_speeches_final.Rds")
