# Validation set
# Here we compare how the LSA/GLM model compares to the SVM when we use
# our training set to label a validation set hand-coded

############## LSA/GLM
# LSA and GLM simple models to classify data
# Load packages
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)
library(lsa)
# Get hand coded final training set
training_set_final <- readRDS("~/Documents/GitHub/amazondef/data/training_set_final.Rds")
# Get full dataset and remove hand coded observations
validation_final <- readRDS("~/Documents/GitHub/amazondef/data/validation_final.Rds")
# Get training texts standardized
training_set_final$AM2 <- stringi::stri_trans_general(training_set_final$AM2, 'latin-ascii')
training_set_final$AM2 <- stringr::str_squish(training_set_final$AM2)
training_set_final$AM2 <- tolower(training_set_final$AM2)
training_set_final$AM2 <- tm::removeNumbers(training_set_final$AM2)
training_set_final$AM2 <- tm::removePunctuation(training_set_final$AM2)
training_set_final$AM2 <- trimws(training_set_final$AM2)
train <- training_set_final %>% select(AM2, sov, EI, SD, con, false_positives) # save to combine later
# Get validation texts standardize
validation_final$AM2 <- stringi::stri_trans_general(validation_final$AM2, 'latin-ascii')
validation_final$AM2 <- stringr::str_squish(validation_final$AM2)
validation_final$AM2 <- tolower(validation_final$AM2)
validation_final$AM2 <- tm::removeNumbers(validation_final$AM2)
validation_final$AM2 <- tm::removePunctuation(validation_final$AM2)
validation_final$AM2 <- trimws(validation_final$AM2)
# Rbind train and validation data
score <- rbind(train, validation_final) # Need this cause we are working with matrixes

##### Now let's run a simple linear model based on a LSA topic model
# Clean and get a dtm
txtcorp_lsa <- VCorpus(VectorSource(as.character(score$AM2)))
txtcorp_lsa <- tm::tm_map(txtcorp_lsa, removeWords, stopwords('pt'))
TDM_lsa <- TermDocumentMatrix(txtcorp_lsa, control = list(weighting = weightTfIdf))
# Get 40 latent topics (just for the sake of it)
lsaTDM <- lsa(TDM_lsa, 40) # This takes a few minutes

# Extract the document LSA values
docVectors <- as.data.frame(lsaTDM$dk)

##### FP model
# Append all the target variables
docVectors_fp <- docVectors
docVectors_fp$false_positives <- score$false_positives
# Get training and score data
train_fp <- docVectors_fp[1:605,]
score_fp <- docVectors_fp[606:819,]
# Fit the simple model
set.seed(1234)
fit_fp <- glm(false_positives~., train_fp, family = 'binomial')
# Predict in training sample
train_lsa_fp <- predict(fit_fp, train_fp, type = 'response')
# Predict on score data
score_lsa_fp <- predict(fit_fp, score_fp, type = 'response')
score_lsa_fp <- ifelse(score_lsa_fp >= 0.5, 1, 0)
summary(as.factor(score_lsa_fp)) # 20 false positives labelled
# append validation data
validation_final$fp_label <- score_lsa_fp
# check accuracy
yardstick::conf_mat(table(validation_final$fp_label, validation_final$false_positives)) 
yardstick::accuracy(table(validation_final$false_positives, validation_final$fp_label)) # accuracy of 86%
roc_lsa_fp <- data.frame(cbind(as.numeric(validation_final$fp_label), as.numeric(validation_final$false_positives)))
roc_lsa_fp <- dplyr::rename(roc_lsa_fp, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_fp, 'tpred', 'label', 1, title = "ROC Test Predictions FP") # AUC 0.56

##### Model sovereignty
# Append all the target variables
docVectors_sov <- docVectors
docVectors_sov$sov <- score$sov
# Get training and score data
train_sov <- docVectors_sov[1:605,]
score_sov <- docVectors_sov[606:819,]
# Fit the simple model
set.seed(1234)
fit_sov <- glm(as.factor(sov)~., train_sov, family = 'binomial')
# Predict on score data
score_lsa_sov <- predict(fit_sov, score_sov, type = 'response')
score_lsa_sov <- ifelse(score_lsa_sov >= 0.5, 1, 0)
summary(as.factor(score_lsa_sov)) # 19 labbeled sov
# append validation data
validation_final$sov_label <- score_lsa_sov
# check accuracy
yardstick::conf_mat(table(validation_final$sov_label, validation_final$sov)) 
yardstick::accuracy(table(validation_final$sov, validation_final$sov_label)) # accuracy of 818%
roc_lsa_sov <- data.frame(cbind(as.numeric(validation_final$sov_label), as.numeric(validation_final$sov)))
roc_lsa_sov <- dplyr::rename(roc_lsa_sov, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_sov, 'tpred', 'label', 1, title = "ROC Test Predictions SOV") # AUC 0.53 ?

##### Model EI
docVectors_EI <- docVectors
docVectors_EI$EI <- score$EI
# Get training and score data
train_EI <- docVectors_EI[1:605,]
score_EI <- docVectors_EI[606:819,]
# Fit the simple model
set.seed(1234)
fit_EI <- glm(as.factor(EI)~., train_EI, family = 'binomial')
# Predict in training sample
train_lsa_EI <- predict(fit_EI, train_EI, type = 'response')
# Predict on score data
score_lsa_EI <- predict(fit_EI, score_EI, type = 'response')
score_lsa_EI <- ifelse(score_lsa_EI >= 0.5, 1, 0)
summary(as.factor(score_lsa_EI)) # 64
# append validation data
validation_final$EI_label <- score_lsa_EI
# check accuracy
yardstick::conf_mat(table(validation_final$EI_label, validation_final$EI)) 
yardstick::accuracy(table(validation_final$EI, validation_final$EI_label)) # accuracy of 636%
roc_lsa_EI <- data.frame(cbind(as.numeric(validation_final$EI_label), as.numeric(validation_final$EI)))
roc_lsa_EI <- dplyr::rename(roc_lsa_EI, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_EI, 'tpred', 'label', 1, title = "ROC Test Predictions EI") # AUC 0.6

# Model SD
docVectors_SD <- docVectors
docVectors_SD$SD <- score$SD
# Get training and score data
train_SD <- docVectors_SD[1:605,]
score_SD <- docVectors_SD[606:819,]
# Fit the simple model
set.seed(1234)
fit_SD <- glm(as.factor(SD)~., train_SD, family = 'binomial')
# Predict in training sample
train_lsa_SD <- predict(fit_SD, train_SD, type = 'response')
# Predict on score data
score_lsa_SD <- predict(fit_SD, score_SD, type = 'response')
score_lsa_SD <- ifelse(score_lsa_SD >= 0.5, 1, 0)
summary(as.factor(score_lsa_SD)) # 11 labeled obs
# append validation data
validation_final$SD_label <- score_lsa_SD
# check accuracy
yardstick::conf_mat(table(validation_final$SD_label, validation_final$SD)) 
yardstick::accuracy(table(validation_final$SD, validation_final$SD_label)) # accuracy of 77%
roc_lsa_SD <- data.frame(cbind(as.numeric(validation_final$SD_label), as.numeric(validation_final$SD)))
roc_lsa_SD <- dplyr::rename(roc_lsa_SD, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_SD, 'tpred', 'label', 1, title = "ROC Test Predictions SD") # 0.47????

##### Model conservation
docVectors_con <- docVectors
docVectors_con$con <- score$con
# Get training and score data
train_con <- docVectors_con[1:605,]
score_con <- docVectors_con[606:819,]
# Fit the simple model
set.seed(1234)
fit_con <- glm(as.factor(con)~., train_con, family = 'binomial')
# Predict on score data
score_lsa_con <- predict(fit_con, score_con, type = 'response')
score_lsa_con <- ifelse(score_lsa_con >= 0.5, 1, 0)
summary(as.factor(score_lsa_con)) # We got 345 here
# append validation data
validation_final$con_label <- score_lsa_con
# check accuracy
yardstick::conf_mat(table(validation_final$con_label, validation_final$con)) 
yardstick::accuracy(table(validation_final$con, validation_final$con_label)) # accuracy of 70%
roc_lsa_con <- data.frame(cbind(as.numeric(validation_final$con_label), as.numeric(validation_final$con)))
roc_lsa_con <- dplyr::rename(roc_lsa_con, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_con, 'tpred', 'label', 1, title = "ROC Test Predictions CON") #0.6
# the models do not look good here at all...

#################### SVM
library(RTextTools)
# get a doc matrix
doc_matrix <- create_matrix(score$AM2, language = "portuguese", removeNumbers = TRUE,
                            stripWhitespace = TRUE, removeStopwords = TRUE,
                            removeSparseTerms = .998, removePunctuation = TRUE)
##### Fp model
# container
set.seed(1234)
container_fp <- create_container(doc_matrix, score$false_positives,
                              trainSize = 1:605, testSize = 606:819, virgin = FALSE)
# SVM model
SVM_fp <- train_model(container_fp,"SVM")
SVM_CLASSIFY_fp <- classify_model(container_fp, SVM_fp)
analytics_fp <- create_analytics(container_fp, SVM_CLASSIFY_fp)
summary(analytics_fp)
doc_summary_fp <- analytics_fp@document_summary
# Get accuracy for svm
yardstick::conf_mat(table(doc_summary_fp$SVM_LABEL, doc_summary_fp$MANUAL_CODE))
yardstick::accuracy(table(doc_summary_fp$MANUAL_CODE, doc_summary_fp$SVM_LABEL)) # Accuracy of 0.88
roc_svm_fp <- data.frame(cbind(as.numeric(doc_summary_fp$SVM_LABEL), as.numeric(doc_summary_fp$MANUAL_CODE)))
roc_svm_fp <- dplyr::rename(roc_svm_fp, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_svm_fp, 'tpred', 'label', 1, title = "ROC Test Predictions SVM FP")

##### Sov model
# container
set.seed(1234)
container_sov <- create_container(doc_matrix, score$sov,
                                 trainSize = 1:605, testSize = 606:819, virgin = FALSE)
# SVM model
SVM_sov <- train_model(container_sov,"SVM")
SVM_CLASSIFY_sov <- classify_model(container_sov, SVM_sov)
analytics_sov <- create_analytics(container_sov, SVM_CLASSIFY_sov)
summary(analytics_sov)
doc_summary_sov <- analytics_sov@document_summary
# Get accuracy for svm
yardstick::conf_mat(table(doc_summary_sov$SVM_LABEL, doc_summary_sov$MANUAL_CODE))
yardstick::accuracy(table(doc_summary_sov$MANUAL_CODE, doc_summary_sov$SVM_LABEL)) # Accuracy of 0.88
roc_svm_sov <- data.frame(cbind(as.numeric(doc_summary_sov$SVM_LABEL), as.numeric(doc_summary_sov$MANUAL_CODE)))
roc_svm_sov <- dplyr::rename(roc_svm_sov, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_svm_sov, 'tpred', 'label', 1, title = "ROC Test Predictions SVM sov")

##### Model EI
set.seed(1234)
container_EI <- create_container(doc_matrix, score$EI,
                                  trainSize = 1:605, testSize = 606:819, virgin = FALSE)
# SVM model
SVM_EI <- train_model(container_EI,"SVM")
SVM_CLASSIFY_EI <- classify_model(container_EI, SVM_EI)
analytics_EI <- create_analytics(container_EI, SVM_CLASSIFY_EI)
summary(analytics_EI)
doc_summary_EI <- analytics_EI@document_summary
# Get accuracy for svm
yardstick::conf_mat(table(doc_summary_EI$SVM_LABEL, doc_summary_EI$MANUAL_CODE))
yardstick::accuracy(table(doc_summary_EI$MANUAL_CODE, doc_summary_EI$SVM_LABEL)) # Accuracy of 0.88
roc_svm_EI <- data.frame(cbind(as.numeric(doc_summary_EI$SVM_LABEL), as.numeric(doc_summary_EI$MANUAL_CODE)))
roc_svm_EI <- dplyr::rename(roc_svm_EI, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_svm_EI, 'tpred', 'label', 1, title = "ROC Test Predictions SVM EI")

##### Model SD
set.seed(1234)
container_SD <- create_container(doc_matrix, score$SD,
                                  trainSize = 1:605, testSize = 606:819, virgin = FALSE)
# SVM model
SVM_SD <- train_model(container_SD,"SVM")
SVM_CLASSIFY_SD <- classify_model(container_SD, SVM_SD)
analytics_SD <- create_analytics(container_SD, SVM_CLASSIFY_SD)
summary(analytics_SD)
doc_summary_SD <- analytics_SD@document_summary
# Get accuracy for svm
yardstick::conf_mat(table(doc_summary_SD$SVM_LABEL, doc_summary_SD$MANUAL_CODE))
yardstick::accuracy(table(doc_summary_SD$MANUAL_CODE, doc_summary_SD$SVM_LABEL)) # Accuracy of 0.88
roc_svm_SD <- data.frame(cbind(as.numeric(doc_summary_SD$SVM_LABEL), as.numeric(doc_summary_SD$MANUAL_CODE)))
roc_svm_SD <- dplyr::rename(roc_svm_SD, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_svm_SD, 'tpred', 'label', 1, title = "ROC Test Predictions SVM SD")

##### Model Con
set.seed(1234)
container_con <- create_container(doc_matrix, score$con,
                                  trainSize = 1:605, testSize = 606:819, virgin = FALSE)
# SVM model
SVM_con <- train_model(container_con,"SVM")
SM_CLASSIFY_con <- classify_model(container_con, SVM_con)
analytics_con <- create_analytics(container_con, SVM_CLASSIFY_con)
summary(analytics_con)
doc_summary_con <- analytics_con@document_summary
# Get accuracy for svm
yardstick::conf_mat(table(doc_summary_con$SVM_LABEL, doc_summary_con$MANUAL_CODE))
yardstick::accuracy(table(doc_summary_con$MANUAL_CODE, doc_summary_con$SVM_LABEL)) # Accuracy of 0.88
roc_svm_con <- data.frame(cbind(as.numeric(doc_summary_con$SVM_LABEL), as.numeric(doc_summary_con$MANUAL_CODE)))
roc_svm_con <- dplyr::rename(roc_svm_con, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_svm_con, 'tpred', 'label', 1, title = "ROC Test Predictions SVM con")
# SVM better now we need to tune parameters before labelling the remaining obs!
