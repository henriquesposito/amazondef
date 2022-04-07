# Get location from title and text
BR_Presidential_Speeches <- readRDS("~/GitHub/amazondef/data/BR_Presidential_Speeches.Rds")

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

# get locations from title
BR_Presidential_Speeches$location <- poldis::extract_location(BR_Presidential_Speeches$title)
summary(as.factor(BR_Presidential_Speeches$location))

# get location from text if not found
BR_Presidential_Speeches$location <- ifelse(grepl("^NA$", BR_Presidential_Speeches$location),
                                            poldis::extract_location(BR_Presidential_Speeches$text),
                                            BR_Presidential_Speeches$location)
summary(as.factor(BR_Presidential_Speeches$location))

# get context and update amazon specific data
Amazon_speeches <- dplyr::filter(BR_Presidential_Speeches, grepl("amazon", text))
Amazon_speeches$context <- poldis::context("amazon", var = Amazon_speeches$text, level = "sentences")
Amazon_speeches$ccontext <- unlist(lapply(Amazon_speeches$context, function(x) paste(x, collapse = " | ")))

# Get 2 lines before and 2 lines after
context2 <- function(string, var) {
  match <- paste0("([^.]+\\.){0,2}[^.]+(", string, ").*?\\.([^.]+\\.){0,2}")
  s <- stringr::str_extract_all(var, match)
  s
}
Amazon_speeches$AM2 <- context2("amazon", var = Amazon_speeches$text)
Amazon_speeches$ID <- as.numeric(random::randomNumbers(n = 946, min = 1000, max = 2000, col = 1))

# Save Rds
saveRDS(Amazon_speeches, "amazon_speeches.Rds")

# Get and save long data coding
library(dplyr)
amazon_speeches_long <- Amazon_speeches %>%
  select(ID, president, year, party, title, location, date, text, AM2) %>% 
  tidyr::unnest(cols = c(AM2))
saveRDS(amazon_speeches_long, "amazon_speeches_long.Rds") # 2014 obs

# Get a sample for hand coding
training_set <- amazon_speeches_long %>%
  select(ID, AM2) %>%
  dplyr::slice_sample(n = 672)
# export to Excel
xlsx::write.xlsx(training_set, file = "training_set.xlsx")
