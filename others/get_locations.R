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

# Save Rds
saveRDS(Amazon_speeches, "amazon_speeches.Rds")
