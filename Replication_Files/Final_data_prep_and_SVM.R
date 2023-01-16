# In this script we prepare the data with some text pre-processing,
# by extracting Amazonian statements, and 
# by adding locations for all speeches.
# We also run the SVM model and save the final data for analysis.

# To install poldis run:
# devtools::install_github("henriquesposito/poldis")
library(poldis)
library(dplyr)
library(readxl)

# Load data
BR_Presidential_Speeches <- readRDS("~/Documents/GitHub/amazondef/data/BR_Presidential_Speeches.Rds")

# Replace on title and text
BR_Presidential_Speeches$text <- stringr::str_replace_all(BR_Presidential_Speeches$text,
                                                          " - | -| -", "-")
BR_Presidential_Speeches$title <- stringr::str_replace_all(BR_Presidential_Speeches$title,
                                                           " - | -| -", "-")

# Replace "para" to avoid confusion with state name in Brazil
BR_Presidential_Speeches$text <- stringr::str_replace_all(BR_Presidential_Speeches$text,
                                                          "^para$", "pra")
BR_Presidential_Speeches$title <- stringr::str_replace_all(BR_Presidential_Speeches$title,
                                                           "^para$", "pra")

# Make it lower case, remove extra space, and add coma space
BR_Presidential_Speeches$text <- tolower(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tolower(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::stripWhitespace(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::stripWhitespace(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- textclean::add_comma_space(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- textclean::add_comma_space(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- stringr::str_squish(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- stringr::str_squish(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- trimws(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- trimws(BR_Presidential_Speeches$title)

# Make it latin SCII
BR_Presidential_Speeches$text <- stringi::stri_trans_general(BR_Presidential_Speeches$text,
                                                             id = "latin-ascii")
BR_Presidential_Speeches$title <- stringi::stri_trans_general(BR_Presidential_Speeches$title,
                                                              id = "latin-ascii")

# get context and update amazon specific data
Amazon_speeches <- dplyr::filter(BR_Presidential_Speeches, grepl("amazon", text))

# Get 2 sentences before and 2 sentences after with poldis
Amazon_speeches$AM2 <- poldis::extract_context("amazon", Amazon_speeches$text,
                                               "sentences", 2)

# Get and save long data coding
amazon_speeches_long <- Amazon_speeches %>%
  select(president, year, party, title, date, text, AM2) %>% 
  tidyr::unnest(cols = c(AM2))

# More cleaning
amazon_speeches_long$text <- stringr::str_squish(amazon_speeches_long$text)
amazon_speeches_long$title <- stringr::str_squish(amazon_speeches_long$title)
amazon_speeches_long$text <- trimws(amazon_speeches_long$text)
amazon_speeches_long$title <- trimws(amazon_speeches_long$title)

# Get locations from title
amazon_speeches_long$location <- poldis::extract_location(amazon_speeches_long$title)
#summary(as.factor(amazon_speeches_long$location))

# Get location from text if not found
amazon_speeches_long$location <- ifelse(grepl("^NA$", amazon_speeches_long$location),
                                        poldis::extract_location(amazon_speeches_long$text),
                                        amazon_speeches_long$location)
summary(as.factor(amazon_speeches_long$location))
amazon_speeches_long2 <- amazon_speeches_long

amazon_speeches_long <- rbind(amazon_speeches_long, amazon_speeches_long2)

# save the data
#saveRDS(amazon_speeches_long, "amazon_speeches_long.Rds") # 2048 obs

# Get a sample for hand coding
training_set_22 <- amazon_speeches_long %>%
  select(AM2) %>%
  dplyr::slice_sample(n = 1024)
# export to Excel
# xlsx::write.xlsx(training_set, file = "training_set.xlsx")

# Remove numbers and punctuation
BR_Presidential_Speeches$text <- tm::removeNumbers(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::removeNumbers(BR_Presidential_Speeches$title)
BR_Presidential_Speeches$text <- tm::removePunctuation(BR_Presidential_Speeches$text)
BR_Presidential_Speeches$title <- tm::removePunctuation(BR_Presidential_Speeches$title)

# Get locations from title
BR_Presidential_Speeches$location <- poldis::extract_location(BR_Presidential_Speeches$title)
summary(as.factor(BR_Presidential_Speeches$location))

# Get location from text if not found
BR_Presidential_Speeches$location <- ifelse(grepl("^NA$", BR_Presidential_Speeches$location),
                                            poldis::extract_location(BR_Presidential_Speeches$text),
                                            BR_Presidential_Speeches$location)
summary(as.factor(BR_Presidential_Speeches$location))

# Get a excel file just to replace NAs and review things...
# writexl::write_xlsx(BR_Presidential_Speeches, "BR_presid_speeches_final.xlsx")
# Most of the NAs left are TV or Radio Announcements in case we want to mark these as such.
#BR_presid_speeches_final <- read_excel("data/BR_presid_speeches_final.xlsx")
#summary(as.factor(BR_presid_speeches_final$location))

# Last thing here, let's add a dummy variable for whether certain speech
# mentions the stem "amazon" or not.
# As well as let's add a different categorical variable for location.
BR_presid_speeches_final <- readRDS("~/Documents/GitHub/amazondef/Replication_Files/BR_presid_speeches_final.Rds")
BR_presid_speeches_final <- BR_presid_speeches_final %>% 
  mutate(amazon_speech = ifelse(grepl("amazon", text), 1, 0),
         location_cat = case_when(grepl("Amazonas|Para|Roraima|Acre|Amapa|Rondonia|Mato-Grosso|Tocantins|Maranhao", location) ~ "Amazonian States",
                                  grepl("Alagoas|Bahia|Ceará|Goias|Minas Gerais|Espirito Santo|Paraiba|Parana|Pernambuco|Piaui|Rio de Janeiro|Rio Grande do Norte|Rio Grande do Sul|Santa Catarina|Sao Paulo|Brazil|Sergipe", location) ~ "Non Amazonian States",
                                  grepl("Distrito Federal", location) ~ "Brasilia",
                                  grepl("NA", location) ~ "Non Identified",
                                  !grepl("Amazonian States|Non Amazonian States|Brasilia|Non Identified", location) ~ "International"))
#summary(as.factor(BR_presid_speeches_final$amazon_speech))
#summary(as.factor(BR_presid_speeches_final$location_cat))

# save for analysis
#saveRDS(BR_presid_speeches_final, "BR_presid_speeches_final.Rds")

#### Supervised Machine Learning Model using SVM (support vector machine) algorithm

# Load packages
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)
library(lsa)
library(e1071)
library(RTextTools)
library(SnowballC)

# Load final hand coded data (hand-coded by authors)
# notice that this data is anonymized; that is, it only contains the
# Amazonian statement and no other identifying info.
final_hand_coded_data <- readRDS("~/Documents/GitHub/amazondef/Replication Files/final_hand_coded_data1.Rds")

# Load final data to label (obs not randomly selected for hand coding)
final_unlabeled_data <- readRDS("~/Documents/GitHub/amazondef/Replication Files/final_unlabeled_data1.Rds")

# Wrapper function to clean Amazonian statement
clean_text <- function(v) {
  out <- stringi::stri_trans_general(v, 'latin-ascii')
  out <- tolower(out)
  out <- tm::removeNumbers(out)
  out <- tm::removePunctuation(out)
  out <- tm::removeWords(out, stopwords('pt'))
  out <- stringr::str_squish(out)
  out <- trimws(out)
}

final_hand_coded_data$AM2 <- clean_text(final_hand_coded_data$AM2)
final_unlabeled_data$AM2 <- clean_text(final_unlabeled_data$AM2)

# Split Data into training and validations sets
sample_size = floor(0.8*nrow(final_hand_coded_data))
set.seed(1201)
picked <- sample(seq_len(nrow(final_hand_coded_data)), size = sample_size)
ttrain = final_hand_coded_data[picked,]
vvalid = final_hand_coded_data[-picked,]

# Create matrixes
train_matrix <- create_matrix(ttrain["AM2"], weighting = weightTfIdf)
valid_matrix <- create_matrix(vvalid["AM2"], originalMatrix = train_matrix,
                              weighting = weightTfIdf)
train_matrix_total <- create_matrix(final_hand_coded_data["AM2"],
                                    weighting = weightTfIdf)
score_matrix <- create_matrix(final_unlabeled_data["AM2"],
                              originalMatrix = train_matrix_total,
                              weighting = weightTfIdf)

# Model false positives
tune_fp <- tune(svm, train_matrix, as.numeric(ttrain$false_positives),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_fp$best.model
# match best parameters and validate
model_fp <- svm(x = train_matrix,
                y = as.numeric(ttrain$false_positives),
                type = "eps-regression", kernel = "radial",
                cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
# predict validation labels
pred_fp_v <- predict(model_fp, valid_matrix)
pred_fp_v <- ifelse(pred_fp_v > 0.45, 1, 0)
# get metrics
table(pred = pred_fp_v, true = vvalid$false_positives)
yardstick::accuracy(table(true = vvalid$false_positives, pred = pred_fp_v))
#roc_svm_fp <- data.frame(cbind(tpred = as.numeric(pred_fp_v),label = as.numeric(vvalid$false_positives)))
#WVPlots::ROCPlot(roc_svm_fp, 'tpred', 'label', 1, title = "ROC Validation Predictions SVM model for fp")
# predict labels
model_fp_total <- svm(x = train_matrix_total,
                      y = as.numeric(final_hand_coded_data$false_positives),
                      type = "eps-regression", kernel = "radial",
                      cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
pred_fp <- predict(model_fp_total, score_matrix)
# add to dataset
final_unlabeled_data$false_positives <- pred_fp
# left as a probability so we can set the threshold later

# Model sovereignty
tune_sov <- tune(svm, train_matrix, as.numeric(ttrain$sov),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_sov$best.model
# match best parameters
model_sov <- svm(x = train_matrix, y = as.numeric(ttrain$sov),
                 type = "eps-regression", kernel = "radial",
                 cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
# predict validation labels
pred_sov_v <- predict(model_sov, valid_matrix)
pred_sov_v <- ifelse(pred_sov_v > 0.45, 1, 0)
# get metrics
table(pred = pred_sov_v, true = vvalid$sov)
yardstick::accuracy(table(true = vvalid$sov, pred = pred_sov_v))
# roc_svm_sov <- data.frame(cbind(tpred = as.numeric(pred_sov_v), label = as.numeric(vvalid$sov)))
# WVPlots::ROCPlot(roc_svm_sov, 'tpred', 'label', 1,
#                  title = "ROC Validation Predictions SVM model for sov")
# predict labels
model_sov_total <- svm(x = train_matrix_total,
                       y = as.numeric(final_hand_coded_data$sov),
                       type = "eps-regression", kernel = "radial",
                       cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
pred_sov <- predict(model_sov_total, score_matrix)
# add to dataset
final_unlabeled_data$sov <- pred_sov

# Model economic integration
tune_EI <- tune(svm, train_matrix, as.numeric(ttrain$EI),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_EI$best.model
# match best parameters
model_EI <- svm(x = train_matrix, y = as.numeric(ttrain$EI),
                type = "eps-regression", kernel = "radial",
                cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
# predict validation labels
pred_EI_v <- predict(model_EI, valid_matrix)
pred_EI_v <- ifelse(pred_EI_v > 0.45, 1, 0)
# get metrics
table(pred = pred_EI_v, true = vvalid$EI)
yardstick::accuracy(table(true = vvalid$EI, pred = pred_EI_v))
# roc_svm_EI <- data.frame(cbind(tpred = as.numeric(pred_EI_v), label = as.numeric(vvalid$EI)))
# WVPlots::ROCPlot(roc_svm_EI, 'tpred', 'label', 1,
#                  title = "ROC Validation Predictions SVM model for EI")
# predict labels
model_EI_total <- svm(x = train_matrix_total,
                      y = as.numeric(final_hand_coded_data$EI),
                      type = "eps-regression", kernel = "radial",
                      cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
pred_EI <- predict(model_EI_total, score_matrix)
# add to dataset
final_unlabeled_data$EI <- pred_EI

# Model social development
tune_SD <- tune(svm, train_matrix, as.numeric(ttrain$SD),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_SD$best.model
# match best parameters
model_SD <- svm(x = train_matrix, y = as.numeric(ttrain$SD),
                type = "eps-regression", kernel = "radial",
                cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
# predict validation labels
pred_SD_v <- predict(model_SD, valid_matrix)
pred_SD_v <- ifelse(pred_SD_v > 0.45, 1, 0)
# get metrics
table(pred = pred_SD_v, true = vvalid$SD)
yardstick::accuracy(table(true = vvalid$SD, pred = pred_SD_v))
# roc_svm_SD <- data.frame(cbind(tpred = as.numeric(pred_SD_v), label = as.numeric(vvalid$SD)))
# WVPlots::ROCPlot(roc_svm_SD, 'tpred', 'label', 1,
#                  title = "ROC Validation Predictions SVM model for SD")
# predict labels
model_SD_total <- svm(x = train_matrix_total,
                      y = as.numeric(final_hand_coded_data$SD),
                      type = "eps-regression", kernel = "radial",
                      cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
pred_SD <- predict(model_SD_total, score_matrix)
# add to dataset
final_unlabeled_data$SD <- pred_SD

# Model conservation
tune_con <- tune(svm, train_matrix, as.numeric(ttrain$con),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_con$best.model
# match best parameters
model_con <- svm(x = train_matrix, y = as.numeric(ttrain$con),
                 type = "eps-regression", kernel = "radial",
                 cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
# predict validation labels
pred_con_v <- predict(model_con, valid_matrix)
pred_con_v <- ifelse(pred_con_v > 0.45, 1, 0)
# get metrics
table(pred = pred_con_v, true = vvalid$con)
yardstick::accuracy(table(true = vvalid$con, pred = pred_con_v))
# roc_svm_con <- data.frame(cbind(tpred = as.numeric(pred_con_v), label = as.numeric(vvalid$con)))
# WVPlots::ROCPlot(roc_svm_con, 'tpred', 'label', 1,
#                  title = "ROC Validation Predictions SVM model for con")
# predict labels
model_con_total <- svm(x = train_matrix_total,
                       y = as.numeric(final_hand_coded_data$con),
                       type = "eps-regression", kernel = "radial",
                       cost = 5000, gamma = 9.67118e-05, episilon = 0.1)
pred_con <- predict(model_con_total, score_matrix)
# add to dataset
final_unlabeled_data$con <- pred_con

# Add an other variable
final_unlabeled_data$other <- ifelse(final_unlabeled_data$sov < 0.45 &
                                       final_unlabeled_data$EI < 0.45 &
                                       final_unlabeled_data$SD < 0.45 &
                                       final_unlabeled_data$con < 0.45, 1, 0)
summary(as.factor(final_unlabeled_data$other))

# Merge data and save final labeled dataset
fdata <- final_unlabeled_data %>%
  select(AM2, sov, EI, SD, con, false_positives) %>%
  mutate(hand_coded = 0)
ttdata <- mutate(final_hand_coded_data, hand_coded = 1)
final_data <- rbind(fdata, ttdata)

#### Prepare final data
amazon_speeches_long <- readRDS("~/Documents/GitHub/amazondef/Replication_Files/amazon_speeches_long.Rds")
# Some cleaning to make sure merge is correct
amazon_speeches_long$AM2 <- clean_text(amazon_speeches_long$AM2)
# Merge all the info
final_labeled_data <- dplyr::full_join(amazon_speeches_long, final_data, "AM2")
# Save with probabilities
#saveRDS(final_labeled_data, "final_labeled_data.Rds")

# Now let's just get this data into the proper format to facilitate for analysis
final_labeled_data$false_positives <- as.factor(ifelse(
  final_labeled_data$false_positives > 0.5, 1, 0))
final_labeled_data$sov <- as.factor(ifelse(as.numeric(final_labeled_data$sov) > 0.45, 1, 0))
final_labeled_data$EI <- as.factor(ifelse(as.numeric(final_labeled_data$EI) > 0.45, 1, 0))
final_labeled_data$SD <- as.factor(ifelse(as.numeric(final_labeled_data$SD) > 0.45, 1, 0))
final_labeled_data$con <- as.factor(ifelse(as.numeric(final_labeled_data$con) > 0.45, 1, 0))
#summary(final_labeled_data)
fp <- dplyr::filter(final_labeled_data, false_positives == "1" & sov == "0" &
                      EI == "0" & SD == "0" & con == "0")
final_data <- dplyr::anti_join(final_labeled_data, fp, "AM2")

# Add other variable
final_data$other <- ifelse(final_data$sov == "0" & final_data$EI == "0"
                           & final_data$SD == "0" & final_data$con == "0", 1, 0)
#summary(final_data)

# Add location categorical (just to check further)
final_data$location <- poldis::extract_location(final_data$title)
#summary(as.factor(final_data$location))
# if NA for location in title, use full text
final_data$location <- ifelse(grepl("^NA$", final_data$location),
                              poldis::extract_location(final_data$text),
                              final_data$location)
#summary(as.factor(final_data$location))
# a few obs were not matched still, let's see them
final_data$location <- ifelse(nchar(final_data$location) > 40, "NA", final_data$location)
#summary(as.factor(final_data$location))
nm <- final_data %>% dplyr::filter(location == "NA") %>% select(AM2, text)
View(nm) # we can hand code some of these
nm$location_na <- c("NA", "NA", "NA", "Roraima", "Roraima", "Bahia", "Para", "Para",
                    "Rio Grande do Sul", "Acre", "Acre", "Minas Gerais", "Rio Grande do Sul",
                    "Rio Grande do Sul", "Para", "Para", "Para", "Para", "Para", "Para",
                    "Para", "Para", "United States of America", "United States of America")
nm <- dplyr::select(nm, AM2, location_na)
fd_location <- dplyr::full_join(final_data, nm, "AM2")
fd_location$location <- ifelse(grepl("NA", fd_location$location),
                               fd_location$location_na, fd_location$location)
final_data_as <- fd_location %>% select(-location_na)
summary(as.factor(final_data_as$location))
# add location categorical
final_data_as <- final_data_as %>% 
  mutate(location_cat = case_when(grepl("Amazonas|Para|Roraima|Acre|Amapa|Rondonia|Mato-Grosso|Tocantins|Maranhao", location) ~ "Amazonian States",
                                  grepl("Alagoas|Bahia|Ceará|Goias|Minas Gerais|Espirito Santo|Paraiba|Parana|Pernambuco|Piaui|Rio de Janeiro|Rio Grande do Norte|Rio Grande do Sul|Santa Catarina|Sao Paulo|Brazil|Sergipe", location) ~ "Non Amazonian States",
                                  grepl("Distrito Federal", location) ~ "Brasilia",
                                  grepl("NA", location) ~ "Non Identified",
                                  !grepl("Amazonian States|Non Amazonian States|Brasilia|Non Identified", location) ~ "International"))
# Fix a few minor mistakes
final_data_as <- final_data_as %>% 
  mutate(location_cat = ifelse(grepl("parana|paraiba|mato grosso|mato grosso do sul",
                            location, ignore.case = TRUE),
                      "Non Amazonian States", location_cat),
         location_cat = ifelse(grepl("paraguay", location, ignore.case = TRUE),
                      "International", location_cat),
         location_cat = factor(ifelse(grepl("Bolivia|Peru|Ecuador|Colombia|Venezuela|Guyana|Suriname",
                                   location), "Amazonian Countries", location_cat),
                      levels = c("International", "Amazonian States",
                                 "Amazonian Countries", "Brasilia",
                                 "Non Amazonian States")))
#summary(as.factor(final_data_as$location_cat))

# Save
#saveRDS(final_data_as, "final_data_as.Rds")
