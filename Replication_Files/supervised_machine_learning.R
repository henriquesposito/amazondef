# Supervised Machine Learning Model - SVM

# Load final hand coded data
final_hand_coded_data <- readRDS("~/Documents/GitHub/amazondef/Replication Files/final_hand_coded_data.Rds")
# Load final labeling data
final_unlabeled_data <- readRDS("~/Documents/GitHub/amazondef/Replication Files/final_unlabeled_data.Rds")

# Split Data into Training and Testing in R 
sample_size = floor(0.8*nrow(final_hand_coded_data))
set.seed(1201)
picked <- sample(seq_len(nrow(final_hand_coded_data)), size = sample_size)
ttrain = final_hand_coded_data[picked,]
vvalid = final_hand_coded_data[-picked,]

# Create matrixes
train_matrix <- create_matrix(ttrain["AM2"], weighting = weightTfIdf)
valid_matrix <- create_matrix(vvalid["AM2"], originalMatrix = train_matrix, weighting = weightTfIdf)
score_matrix <- create_matrix(final_unlabeled_data["AM2"], originalMatrix = train_matrix, weighting = weightTfIdf)

# Model fp
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
model_fp <- svm(x = train_matrix, y = as.numeric(ttrain$false_positives),
                type = "eps-regression", kernel = "radial",
                cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
# predict validation labels
pred_fp_v <- predict(model_fp, valid_matrix)
pred_fp_v <- ifelse(pred_fp_v > 0.4, 1, 0)
# get metrics
table(pred = pred_fp_v, true = vvalid$false_positives)
yardstick::accuracy(table(true = vvalid$false_positives, pred = pred_fp_v))
roc_svm_fp <- data.frame(cbind(tpred = as.numeric(pred_fp_v), label = as.numeric(vvalid$false_positives)))
WVPlots::ROCPlot(roc_svm_fp, 'tpred', 'label', 1, title = "ROC Validation Predictions SVM model for fp")
# predict labels
pred_fp <- predict(model_fp, score_matrix)
# add to dataset
final_unlabeled_data$false_positives <- pred_fp # left as a probability so we can set the threshold later

# Model sov
# tune
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
                 cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
# predict validation labels
pred_sov_v <- predict(model_sov, valid_matrix)
pred_sov_v <- ifelse(pred_sov_v > 0.4, 1, 0)
# get metrics
table(pred = pred_sov_v, true = vvalid$sov)
yardstick::accuracy(table(true = vvalid$sov, pred = pred_sov_v))
roc_svm_sov <- data.frame(cbind(tpred = as.numeric(pred_sov_v), label = as.numeric(vvalid$sov)))
WVPlots::ROCPlot(roc_svm_sov, 'tpred', 'label', 1, title = "ROC Validation Predictions SVM model for sov")
# predict labels
pred_sov <- predict(model_sov, score_matrix)
# add to dataset
final_unlabeled_data$sov <- pred_sov

# Model EI
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
                cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
# predict validation labels
pred_EI_v <- predict(model_EI, valid_matrix)
pred_EI_v <- ifelse(pred_EI_v > 0.4, 1, 0)
# get metrics
table(pred = pred_EI_v, true = vvalid$EI)
yardstick::accuracy(table(true = vvalid$EI, pred = pred_EI_v))
roc_svm_EI <- data.frame(cbind(tpred = as.numeric(pred_EI_v), label = as.numeric(vvalid$EI)))
WVPlots::ROCPlot(roc_svm_EI, 'tpred', 'label', 1, title = "ROC Validation Predictions SVM model for EI")
# predict labels
pred_EI <- predict(model_EI, score_matrix)
# add to dataset
final_unlabeled_data$EI <- pred_EI

# Model SD
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
                cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
# predict validation labels
pred_SD_v <- predict(model_SD, valid_matrix)
pred_SD_v <- ifelse(pred_SD_v > 0.4, 1, 0)
# get metrics
table(pred = pred_SD_v, true = vvalid$SD)
yardstick::accuracy(table(true = vvalid$SD, pred = pred_SD_v))
roc_svm_SD <- data.frame(cbind(tpred = as.numeric(pred_SD_v), label = as.numeric(vvalid$SD)))
WVPlots::ROCPlot(roc_svm_SD, 'tpred', 'label', 1, title = "ROC Validation Predictions SVM model for SD")
# predict labels
pred_SD <- predict(model_SD, score_matrix)
# add to dataset
final_unlabeled_data$SD <- pred_SD

# Model con
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
                 cost = 5000, gamma = 9.541985e-05, episilon = 0.1)
# predict validation labels
pred_con_v <- predict(model_con, valid_matrix)
pred_con_v <- ifelse(pred_con_v > 0.4, 1, 0)
# get metrics
table(pred = pred_con_v, true = vvalid$con)
yardstick::accuracy(table(true = vvalid$con, pred = pred_con_v))
roc_svm_con <- data.frame(cbind(tpred = as.numeric(pred_con_v), label = as.numeric(vvalid$con)))
WVPlots::ROCPlot(roc_svm_con, 'tpred', 'label', 1, title = "ROC Validation Predictions SVM model for con")
# predict labels
pred_con <- predict(model_con, score_matrix)
# add to dataset
final_unlabeled_data$con <- pred_con

# let's add an other variable just to keep track here of all the obs not labeled
final_unlabeled_data$other <- ifelse(final_unlabeled_data$sov < 0.4 & final_unlabeled_data$EI < 0.4 & final_unlabeled_data$SD < 0.4 & final_unlabeled_data$con < 0.4, 1, 0)
summary(as.factor(final_unlabeled_data$other))

###### Merge data and save labeled dataset final
fdata <- final_unlabeled_data %>%
  select(am2_2, sov, EI, SD, con, false_positives) %>%
  rename(AM2 = am2_2) %>% mutate(hand_coded = 0)
ttdata <- select(final_hand_coded_data, -AM2) %>% 
  rename(AM2 = am2_2) %>% 
  mutate(hand_coded = 1)
final_data <- rbind(fdata, ttdata)
# load original dataset
amazon_speeches_long <- readRDS("~/Documents/GitHub/amazondef/Replication_Files/amazon_speeches_long.Rds")
# do some cleaning to make sure merge is correct
amazon_speeches_long$AM2 <- stringi::stri_trans_general(amazon_speeches_long$AM2, 'latin-ascii')
amazon_speeches_long$AM2 <- stringr::str_squish(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tolower(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removeNumbers(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removePunctuation(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- trimws(amazon_speeches_long$AM2)
# merge all the info
final_labeled_data <- dplyr::full_join(amazon_speeches_long, final_data, "AM2")
# save with probabilities
saveRDS(final_labeled_data, "final_labeled_data.Rds")

# Now let's just get this data into the proper format to facilitate for analysis
final_labeled_data$false_positives <- as.factor(ifelse(final_labeled_data$false_positives > 0.5, 1, 0))
final_labeled_data$sov <- as.factor(ifelse(as.numeric(final_labeled_data$sov) > 0.45, 1, 0))
final_labeled_data$EI <- as.factor(ifelse(as.numeric(final_labeled_data$EI) > 0.45, 1, 0))
final_labeled_data$SD <- as.factor(ifelse(as.numeric(final_labeled_data$SD) > 0.45, 1, 0))
final_labeled_data$con <- as.factor(ifelse(as.numeric(final_labeled_data$con) > 0.45, 1, 0))
summary(final_labeled_data)
fp <- dplyr::filter(final_labeled_data, false_positives == "1" & sov == "0" & EI == "0" & SD == "0" & con == "0")
final_data <- dplyr::anti_join(final_labeled_data, fp, "AM2")

# Add other variable
final_data$other <- ifelse(final_data$sov == "0" & final_data$EI == "0" & final_data$SD == "0" & final_data$con == "0", 1, 0)
summary(final_data)

# Add location
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
nm$location_na <- c("NA", "NA", "NA", "Roraima", "Roraima", "Bahia", "Para", "Para",
                    "Rio Grande do Sul", "Acre", "Acre", "Minas Gerais", "Rio Grande do Sul",
                    "Rio Grande do Sul", "Para", "Para", "Para", "Para", "Para", "Para",
                    "Para", "Para", "United States of America", "United States of America")
nm <- dplyr::select(nm, AM2, location_na)
fd_location <- dplyr::full_join(final_data, nm, "AM2")
fd_location$location <- ifelse(grepl("NA", fd_location$location), fd_location$location_na, fd_location$location)
final_data_as <- fd_location %>% select(-location_na)
summary(as.factor(final_data_as$location))
# add location categorical
final_data_as <- final_data_as %>% 
  mutate(location_cat = case_when(grepl("Amazonas|Para|Roraima|Acre|Amapa|Rondonia|Mato-Grosso|Tocantins|Maranhao", location) ~ "Amazonian States",
                                  grepl("Alagoas|Bahia|Ceará|Goias|Minas Gerais|Espirito Santo|Paraiba|Parana|Pernambuco|Piaui|Rio de Janeiro|Rio Grande do Norte|Rio Grande do Sul|Santa Catarina|Sao Paulo|Brazil|Sergipe", location) ~ "Non Amazonian States",
                                  grepl("Distrito Federal", location) ~ "Brasilia",
                                  grepl("NA", location) ~ "Non Identified",
                                  !grepl("Amazonian States|Non Amazonian States|Brasilia|Non Identified", location) ~ "International"))

# Save
saveRDS(final_data_as, "final_data_as.Rds")
