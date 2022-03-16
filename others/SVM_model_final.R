# SVM model classification final
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)
library(lsa)
library(e1071)

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
# Rbind train and validation data
score <- rbind(training_set_final[,-1], validation_final)
trainData <- score[1:605,]
testData <- score[606:819,]
# create matrix
t_matrix <- create_matrix(trainData["AM2"])
s_matrix <- create_matrix(testData["AM2"], originalMatrix = t_matrix)
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
                cost = 100, gamma = 0.0001138434, episilon = 0.1)
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
fulldata <- amazon_speeches_long[!(amazon_speeches_long$AM2 %in% score$AM2),]
# get matrix
train_matrix <- create_matrix(score["AM2"])
score_matrix <- create_matrix(fulldata["AM2"], originalMatrix = train_matrix)
# find and fit best models for each category
# model choice fp
tune_fp <- tune(svm, train_matrix, as.numeric(score$false_positives),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_fp$best.model
# match best parameters
model_fp <- svm(x = train_matrix, y = as.numeric(score$false_positives),
                type = "eps-regression", kernel = "radial",
                cost = 16, gamma = 9.675859e-05, episilon = 0.1)
# predict labels
pred_fp <- predict(model_fp, score_matrix)
# add to dataset
fulldata$false_positives <- pred_fp # left as a probability so we can set the threshold later

# Model sov
tune_sov <- tune(svm, train_matrix, as.numeric(score$sov),
                ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                    0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                    60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                    700, 800, 900, 1000, 5000, 10000, 50000,
                                    100000, 500000, 1000000)))
tune_sov$best.model
# match best parameters
model_sov <- svm(x = train_matrix, y = as.numeric(score$sov),
                type = "eps-regression", kernel = "radial",
                cost = 25, gamma = 9.675859e-05, episilon = 0.1)
# predict labels
pred_sov <- predict(model_sov, score_matrix)
# add to dataset
fulldata$sov <- pred_sov 

# Model EI
tune_EI <- tune(svm, train_matrix, as.numeric(score$EI),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_EI$best.model
# match best parameters
model_EI <- svm(x = train_matrix, y = as.numeric(score$EI),
                 type = "eps-regression", kernel = "radial",
                 cost = 25, gamma = 9.675859e-05, episilon = 0.1)
# predict labels
pred_EI <- predict(model_EI, score_matrix)
# add to dataset
fulldata$EI <- pred_EI 

# Model SD
tune_SD <- tune(svm, train_matrix, as.numeric(score$SD),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_SD$best.model
# match best parameters
model_SD <- svm(x = train_matrix, y = as.numeric(score$SD),
                 type = "eps-regression", kernel = "radial",
                 cost = 30, gamma = 9.675859e-05, episilon = 0.1)
# predict labels
pred_SD <- predict(model_SD, score_matrix)
# add to dataset
fulldata$SD <- pred_SD

# Model con
tune_con <- tune(svm, train_matrix, as.numeric(score$con),
                 ranges =list(cost=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001,
                                     0.005, 0.01, 0.05, 0.1, 0.5, 1, 2,
                                     3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                                     16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50,
                                     60, 70, 80, 90, 100, 200, 300, 400, 500, 600,
                                     700, 800, 900, 1000, 5000, 10000, 50000,
                                     100000, 500000, 1000000)))
tune_con$best.model
# match best parameters
model_con <- svm(x = train_matrix, y = as.numeric(score$con),
                 type = "eps-regression", kernel = "radial",
                 cost = 30, gamma = 9.675859e-05, episilon = 0.1)
# predict labels
pred_con <- predict(model_con, score_matrix)
# add to dataset
fulldata$con <- pred_con

###### Merge data and save labeled dataset final
fulldata <- fulldata %>%
  select(AM2, sov, EI, SD, con, false_positives)

final_labeled_data <- rbind(score, fulldata)
final_labeled_data <- merge(amazon_speeches_long, final_labeled_data, by = "AM2")
# save
# saveRDS(final_labeled_data, "final_alebeled_data.Rds")
