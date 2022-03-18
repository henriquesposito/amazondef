# SVM model classification final
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)
library(lsa)
library(e1071)
library(RTextTools)
library(SnowballC)

##### First, let's try to tune the model parameters in the Economic Integration variable
# Get hand coded final training set
training_set_final <- readRDS("~/Documents/GitHub/amazondef/data/training_set_final.Rds")
# Get validation dataset
validation_final <- readRDS("~/Documents/GitHub/amazondef/data/validation_final.Rds")
# Get training texts standardized
training_set_final$AM2 <- stringi::stri_trans_general(training_set_final$AM2, 'latin-ascii')
training_set_final$AM2 <- stringr::str_squish(training_set_final$AM2)
training_set_final$AM2 <- tolower(training_set_final$AM2)
training_set_final$AM2 <- tm::removeNumbers(training_set_final$AM2)
training_set_final$AM2 <- tm::removePunctuation(training_set_final$AM2)
training_set_final$AM2 <- trimws(training_set_final$AM2)
t <- select(training_set_final, AM2) # save for later
# get validation standardized
v <- select(validation_final, AM2) # save for merge later
validation_final$AM2 <- stringi::stri_trans_general(validation_final$AM2, 'latin-ascii')
validation_final$AM2 <- stringr::str_squish(validation_final$AM2)
validation_final$AM2 <- tolower(validation_final$AM2)
validation_final$AM2 <- tm::removeNumbers(validation_final$AM2)
validation_final$AM2 <- tm::removePunctuation(validation_final$AM2)
validation_final$AM2 <- trimws(validation_final$AM2)
# Rbind train and validation data
tdata <- rbind(training_set_final[,-1], validation_final)
tdata$AM2 <- tm::removeWords(tdata$AM2, tm::stopwords("pt"))
tdata$AM2 <- SnowballC::wordStem(tdata$AM2, 'pt')
tdata$AM2 <- tm::stripWhitespace(tdata$AM2)
trainData <- tdata[1:605,]
testData <- tdata[606:819,]
# create matrix
t_matrix <- create_matrix(trainData$AM2, weighting = weightTfIdf)
s_matrix <- create_matrix(testData$AM2, originalMatrix = t_matrix, weighting = weightTfIdf)
# model ei
model_EI <- svm(x = t_matrix, y = as.numeric(trainData$EI), kernel = "linear")
# predict EI
pred_EI <- predict(model_EI, s_matrix)
pred_EI <- ifelse(pred_EI > 0.5, 1, 0)
# table ei
table(pred = pred_EI, true = testData$EI)
# accuracy
yardstick::accuracy(table(true = testData$EI, pred = pred_EI))
# ROC
roc_svm_ei <- data.frame(cbind(tpred = as.numeric(pred_EI), label = as.numeric(testData$EI)))
WVPlots::ROCPlot(roc_svm_ei, 'tpred', 'label', 1, title = "ROC Test Predictions SVM EI")
# tune parameters to get better results (try with numeric and factor)
tune_ei = tune(svm, t_matrix, as.numeric(trainData$EI),
              ranges =list(cost=c(0.001,0.01,0.1, 1,5,10,100, 1000, 10000, 100000, 1000000)))
tune_ei$best.model 
# match best parameters and predict again
model_EI <- svm(x = t_matrix, y = as.numeric(trainData$EI),
                type = "eps-regression", kernel = "radial",
                cost = 10000, gamma = 0.0001125366, episilon = 0.1)
pred_EI <- predict(model_EI, s_matrix)
pred_EI <- ifelse(pred_EI > 0.5, 1, 0)
table(pred = pred_EI, true = testData$EI)
yardstick::accuracy(table(true = testData$EI, pred = pred_EI))
roc_svm_ei <- data.frame(cbind(tpred = as.numeric(pred_EI), label = as.numeric(testData$EI)))
WVPlots::ROCPlot(roc_svm_ei, 'tpred', 'label', 1, title = "ROC Test Predictions SVM EI")
# both accuracy and AUC scores increase as a result!!!
# this should also improve considerable once we add more training data below!

###### Fit data and predict
# Let's get all the other texts in
amazon_speeches_long <- readRDS("~/Documents/GitHub/amazondef/data/amazon_speeches_long.Rds")
amazon_speeches_long$AM2 <- stringi::stri_trans_general(amazon_speeches_long$AM2, 'latin-ascii')
amazon_speeches_long$AM2 <- stringr::str_squish(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tolower(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removeNumbers(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removePunctuation(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- trimws(amazon_speeches_long$AM2)
# remove overlaps in text
tv <- rbind(t, v) # save for merge later
fulldata <- amazon_speeches_long[!(amazon_speeches_long$AM2 %in% tv$AM2),]
ff <- select(fulldata, AM2) # save for merge later
# do some more pre-processing
fulldata$AM2 <- tm::removeWords(fulldata$AM2, tm::stopwords("pt"))
fulldata$AM2 <- SnowballC::wordStem(fulldata$AM2, 'pt')
fulldata$AM2 <- tm::stripWhitespace(fulldata$AM2)
# get matrix
train_matrix <- create_matrix(tdata["AM2"], weighting = weightTfIdf)
score_matrix <- create_matrix(fulldata["AM2"], originalMatrix = train_matrix, weighting = weightTfIdf)
# find and fit best models for each category
# model choice fp
tune_fp <- tune(svm, train_matrix, as.numeric(tdata$false_positives),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_fp$best.model
# match best parameters
model_fp <- svm(x = train_matrix, y = as.numeric(tdata$false_positives),
                type = "eps-regression", kernel = "radial",
                cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# predict labels
pred_fp <- predict(model_fp, score_matrix)
# add to dataset
fulldata$false_positives <- pred_fp # left as a probability so we can set the threshold later

# Model sov
tune_sov <- tune(svm, train_matrix, as.numeric(tdata$sov),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_sov$best.model
# match best parameters
model_sov <- svm(x = train_matrix, y = as.numeric(tdata$sov),
                type = "eps-regression", kernel = "radial",
                cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# predict labels
pred_sov <- predict(model_sov, score_matrix)
# add to dataset
fulldata$sov <- pred_sov 

# Model EI
tune_EI <- tune(svm, train_matrix, as.numeric(tdata$EI),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_EI$best.model
# match best parameters
model_EI <- svm(x = train_matrix, y = as.numeric(tdata$EI),
                 type = "eps-regression", kernel = "radial",
                 cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# predict labels
pred_EI <- predict(model_EI, score_matrix)
# add to dataset
fulldata$EI <- pred_EI 

# Model SD
tune_SD <- tune(svm, train_matrix, as.numeric(tdata$SD),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_SD$best.model
# match best parameters
model_SD <- svm(x = train_matrix, y = as.numeric(tdata$SD),
                 type = "eps-regression", kernel = "radial",
                 cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# predict labels
pred_SD <- predict(model_SD, score_matrix)
# add to dataset
fulldata$SD <- pred_SD

# Model con
tune_con <- tune(svm, train_matrix, as.numeric(tdata$con),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_con$best.model
# match best parameters
model_con <- svm(x = train_matrix, y = as.numeric(tdata$con),
                 type = "eps-regression", kernel = "radial",
                 cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# predict labels
pred_con <- predict(model_con, score_matrix)
# add to dataset
fulldata$con <- pred_con

# let's add an other variable just to keep track here of all the obs not labeled
fulldata$other <- ifelse(fulldata$false_positives & fulldata$sov < 0.5 & fulldata$EI < 0.5 & fulldata$SD < 0.5 & fulldata$con < 0.5, 1, 0)
summary(as.factor(fulldata$other))
# almost 500 is too many maybe

###### Merge data and save labeled dataset for now
fdata <- fulldata %>%
  select(AM2, sov, EI, SD, con, false_positives)
fdata$AM2 <- ff$AM2
tdata$AM2 <- tv$AM2
final_labeled_data <- rbind(tdata, fdata)
final_labeled_data <- merge(amazon_speeches_long, final_labeled_data, by = "AM2")
# save
#saveRDS(final_labeled_data, "final_labeled_data.Rds")

######### Let's get another random sample from the variables labeled as other to correct by hand
fulldata$am2_2 <- ff$AM2 # just in case this is needed for later
validation_other_other <- dplyr::filter(fulldata, other == 1) %>% slice_sample(n = 188)
# get the final label data
label_final <- fulldata[!(fulldata$AM2 %in% validation_other_other$AM2),]
validation_other <- select(validation_other_other, AM2, sov, EI, SD, con, false_positives)
xlsx::write.xlsx(validation_other, "validation_other.xlsx")
# save some variables for later merge
lf <- label_final %>% mutate(AM2 = am2_2) %>% select(AM2)
ss <- select(validation_other_other, am2_2) %>% rename(AM2 = am2_2)
tvt <- rbind(tv, ss)
# Save also for reference other datasets to start from here in the future
#saveRDS(label_final, "label_final.Rds") # These are in the data folder if need
#saveRDS(validation_other_other, "validation_other_other.Rds") # these are in data folder
# get the final label data
# After all is settled, merge data and run it again 
validation_other_combined <- read_excel("data/validation_other_combined.xlsx")
# validation_other_other <- readRDS("~/Documents/GitHub/amazondef/data/validation_other_other.Rds")
# validation_other_other$false_positives <- validation_other_combined$false_positives
# validation_other_other$sov <- validation_other_combined$sov
# validation_other_other$EI <- validation_other_combined$EI
# validation_other_other$SD <- validation_other_combined$SD
# validation_other_other$con <- validation_other_combined$con
# # add data to training data
# v_train <- select(validation_other_other, AM2, sov, EI, SD, con, false_positives)
# tdata <- rbind(tdata, v_train)
# label_final <- readRDS("~/Documents/GitHub/amazondef/data/label_final.Rds")
# fulldata <- label_final 
# # create matrixes 
# train_matrix <- create_matrix(tdata["AM2"], weighting = weightTfIdf)
# score_matrix <- create_matrix(label_final["AM2"], originalMatrix = train_matrix, weighting = weightTfIdf)
# # find and fit best models for each category
# # model choice fp
# tune_fp <- tune(svm, train_matrix, as.numeric(tdata$false_positives),
#                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
#                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
#                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
#                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
#                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
#                                     700, 800, 900, 1000, 5000, 10000, 50000,
#                                     100000, 500000, 1000000)))
# tune_fp$best.model
# # match best parameters
# model_fp <- svm(x = train_matrix, y = as.numeric(tdata$false_positives),
#                 type = "eps-regression", kernel = "radial",
#                 cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# # predict labels
# pred_fp <- predict(model_fp, score_matrix)
# # add to dataset
# fulldata$false_positives <- pred_fp # left as a probability so we can set the threshold later
# 
# # Model sov
# tune_sov <- tune(svm, train_matrix, as.numeric(tdata$sov),
#                  ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
#                                      0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
#                                      3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
#                                      16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
#                                      60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
#                                      700, 800, 900, 1000, 5000, 10000, 50000,
#                                      100000, 500000, 1000000)))
# tune_sov$best.model
# # match best parameters
# model_sov <- svm(x = train_matrix, y = as.numeric(tdata$sov),
#                  type = "eps-regression", kernel = "radial",
#                  cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# # predict labels
# pred_sov <- predict(model_sov, score_matrix)
# # add to dataset
# fulldata$sov <- pred_sov 
# 
# # Model EI
# tune_EI <- tune(svm, train_matrix, as.numeric(tdata$EI),
#                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
#                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
#                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
#                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
#                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
#                                     700, 800, 900, 1000, 5000, 10000, 50000,
#                                     100000, 500000, 1000000)))
# tune_EI$best.model
# # match best parameters
# model_EI <- svm(x = train_matrix, y = as.numeric(tdata$EI),
#                 type = "eps-regression", kernel = "radial",
#                 cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# # predict labels
# pred_EI <- predict(model_EI, score_matrix)
# # add to dataset
# fulldata$EI <- pred_EI 
# 
# # Model SD
# tune_SD <- tune(svm, train_matrix, as.numeric(tdata$SD),
#                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
#                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
#                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
#                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
#                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
#                                     700, 800, 900, 1000, 5000, 10000, 50000,
#                                     100000, 500000, 1000000)))
# tune_SD$best.model
# # match best parameters
# model_SD <- svm(x = train_matrix, y = as.numeric(tdata$SD),
#                 type = "eps-regression", kernel = "radial",
#                 cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# # predict labels
# pred_SD <- predict(model_SD, score_matrix)
# # add to dataset
# fulldata$SD <- pred_SD
# 
# # Model con
# tune_con <- tune(svm, train_matrix, as.numeric(tdata$con),
#                  ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
#                                      0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
#                                      3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
#                                      16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
#                                      60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
#                                      700, 800, 900, 1000, 5000, 10000, 50000,
#                                      100000, 500000, 1000000)))
# tune_con$best.model
# # match best parameters
# model_con <- svm(x = train_matrix, y = as.numeric(tdata$con),
#                  type = "eps-regression", kernel = "radial",
#                  cost = 5000, gamma = 9.510223e-05, episilon = 0.1)
# # predict labels
# pred_con <- predict(model_con, score_matrix)
# # add to dataset
# fulldata$con <- pred_con
# 
# # let's add an other variable just to keep track here of all the obs not labeled
# fulldata$other <- ifelse(fulldata$sov < 0.5 & fulldata$EI < 0.5 & fulldata$SD < 0.5 & fulldata$con < 0.5, 1, 0)
# summary(as.factor(fulldata$other))
# 
# ###### Merge data and save labeled dataset final
# fdata <- fulldata %>%
#   select(AM2, sov, EI, SD, con, false_positives)
# fdata$AM2 <- lf$AM2
# tdata$AM2 <- tv$AM2
# final_labeled_data <- rbind(tdata, fdata)
# final_labeled_data <- merge(amazon_speeches_long, final_labeled_data, by = "AM2")
# # save
# #saveRDS(final_labeled_data, "final_labeled_data.Rds")