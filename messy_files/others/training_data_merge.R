# First, load datasets
library(readxl)
training_set_Sposito <- read_excel("data/training_set_Sposito.xlsx")
training_set_livio <- read_excel("data/training_set_livio.xlsx")
# get variable names standardized
library(dplyr)
l <- training_set_livio %>%
  dplyr::slice_head(n = 605) %>%
  rename(sov = sovereignty, ED = 'economic development', SD = 'social development',
         con = conservation, cm2 = 'comment 2') %>% 
  dplyr::mutate(comments = paste0("1 -", comment, " 2 - ", cm2)) %>% 
  dplyr::select(AM2, sov, ED, SD, con, comments)
l <- tibble::rownames_to_column(l, "ID")

h <- training_set_Sposito %>%
  rename(sov = Sovereignty, ED = 'Economic Development', SD = 'Social Development',
         con = Conservation, comments = Comments) %>% 
  dplyr::select(AM2, sov, ED, SD, con, comments)
h <- tibble::rownames_to_column(h, "ID")

# Get and merge only the comments data for later
comments <- data.frame(comments = paste0(l$comments, " 3 - ", h$comments))

# Add the same comments for both datasets
l <- select(l, -comments)
l$comments <- comments$comments
h <- select(h, -comments)
h$comments <- comments$comments

# Merge and remove duplicates
ts <- rbind(l, h)
ts <- ts %>% 
  dplyr::distinct() %>%
  dplyr::mutate(dup = duplicated())
  
# Save data in excel format
xlsx::write.xlsx(ts, file = "ts.xlsx")
