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
  dplyr::select(ID, AM2, sov, ED, SD, con, comments) %>% 
  arrange(ID)

h <- training_set_Sposito %>%
  rename(sov = Sovereignty, ED = 'Economic Development', SD = 'Social Development',
         con = Conservation, comments = Comments) %>% 
  dplyr::select(ID, AM2, sov, ED, SD, con, comments) %>% 
  arrange(ID)

# Get and merge only the comments data for later
comments <- data.frame(ID = l$ID,
                       comments = paste0(l$comments, " 3 - ", h$comments))
comments <- arrange(comments, ID)

# Add the same comments for both datasets
l <- select(l, -comments)
l$comments <- comments$comments
h <- select(h, -comments)
h$comments <- comments$comments

# Merge and remove duplicates
ts <- rbind(l, h)
ts <- ts %>%
  dplyr::distinct() %>%
  arrange(ID) %>%
  mutate(dup = ifelse(duplicated(ID), 1, 0))

# Save data in excel format
xlsx::write.xlsx(ts, file = "ts.xlsx")
