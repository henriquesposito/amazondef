# LSA and GLM simple models to classify data
# Load packages
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)
# Get hand coded final training set
training_set_final <- readRDS("~/Documents/GitHub/amazondef/data/training_set_final.Rds")
# Get full dataset and remove hand coded observations
amazon_speeches_long <- readRDS("~/Documents/GitHub/amazondef/data/amazon_speeches_long.Rds")
# Get training texts standardized
training_set_final$AM2 <- stringi::stri_trans_general(training_set_final$AM2, 'latin-ascii')
training_set_final$AM2 <- stringr::str_squish(training_set_final$AM2)
training_set_final$AM2 <- tolower(training_set_final$AM2)
training_set_final$AM2 <- tm::removeNumbers(training_set_final$AM2)
training_set_final$AM2 <- tm::removePunctuation(training_set_final$AM2)
training_set_final$AM2 <- trimws(training_set_final$AM2)
train <- training_set_final %>% select(AM2, sov, EI, SD, con, false_positives) # save to combine later
t <- dplyr::select(training_set_final, AM2) # used to remove duplicates later
# Get other texts standardize
amazon_speeches_long$AM2 <- stringi::stri_trans_general(amazon_speeches_long$AM2, 'latin-ascii')
amazon_speeches_long$AM2 <- stringr::str_squish(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tolower(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removeNumbers(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removePunctuation(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- trimws(amazon_speeches_long$AM2)
# Remove train obs, keep only score, then rbind train and score data
score <- amazon_speeches_long[!(amazon_speeches_long$AM2 %in% t$AM2),]
score <- score %>%
  select(AM2) %>%
  mutate(sov = NA, EI = NA, SD = NA, con = NA, false_positives = NA) # Added NA for reference
score <- rbind(train, score) # Need this cause we are working with matrixes

##### Now let's run a simple linear model based on a LSA topic model
# Clean and get a dtm
txtcorp_lsa <- VCorpus(VectorSource(as.character(score$AM2)))
txtcorp_lsa <- tm::tm_map(txtcorp_lsa, removeWords, stopwords('pt'))
TDM_lsa <- TermDocumentMatrix(txtcorp_lsa,
                              control = list(weighting = weightTfIdf))
# Get 20 latent topics (just for the sake of it)
lsaTDM <- lsa(TDM_lsa, 20) # This takes a few minutes
# Extract the document LSA values
docVectors <- as.data.frame(lsaTDM$dk)

##### FP model
# Append all the target variables
docVectors_fp <- docVectors
docVectors_fp$false_positives <- score$false_positives
# Get training and score data
train_fp <- docVectors_fp[1:605,]
score_fp <- docVectors_fp[606:2014,]
# Fit the simple model
set.seed(1234)
fit_fp <- glm(false_positives~., train_fp, family = 'binomial')
# Predict in training sample
train_lsa_fp <- predict(fit_fp, train_fp, type = 'response')
train_lsa_fp <- ifelse(train_lsa_fp >= 0.5, 1, 0) # set threshold at 0.5
yardstick::conf_mat(table(train_lsa_fp, score$false_positives[1:605])) 
yardstick::accuracy(table(score$false_positives[1:605], train_lsa_fp)) 
# accuracy of 93.9% here, this is good!
# Let's check the AUC
roc_lsa_fp <- data.frame(cbind(as.numeric(train_lsa_fp), as.numeric(score$false_positives[1:605])))
roc_lsa_fp <- dplyr::rename(roc_lsa_fp, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_fp, 'tpred', 'label', 1, title = "ROC Test Predictions")
# The AUC is 0.83, that is ok but not great though...
# Predict on score data
score_lsa_fp <- predict(fit_fp, score_fp, type = 'response')
score_lsa_fp <- ifelse(score_lsa_fp >= 0.5, 1, 0)
summary(as.factor(score_lsa_fp)) # We got 96 false positives here!!!
# this seems to be rather accurate...
# Let me take out to see:
fp_lsa <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(fp_score = score_lsa_fp) %>% 
  filter(score_lsa_fp == "1")
fp_lsa

##### Model sovereignty
# Append all the target variables
docVectors_sov <- docVectors
docVectors_sov$sov <- score$sov
# Get training and score data
train_sov <- docVectors_sov[1:605,]
score_sov <- docVectors_sov[606:2014,]
# Fit the simple model
set.seed(1234)
fit_sov <- glm(as.factor(sov)~., train_sov, family = 'binomial')
# Predict in training sample
train_lsa_sov <- predict(fit_sov, train_sov, type = 'response')
train_lsa_sov <- ifelse(train_lsa_sov >= 0.5, 1, 0) # set threshold at 0.5
yardstick::conf_mat(table(train_lsa_sov, score$sov[1:605])) 
yardstick::accuracy(table(score$sov[1:605], train_lsa_sov)) 
# accuracy of 83.3% here ...
# Let's check the AUC
roc_lsa_sov <- data.frame(cbind(as.numeric(train_lsa_sov), as.numeric(score$sov[1:605])))
roc_lsa_sov <- dplyr::rename(roc_lsa_sov, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_sov, 'tpred', 'label', 1, title = "ROC Test Predictions")
# The AUC is 0.59, that is not great though...
# Predict on score data
score_lsa_sov <- predict(fit_sov, score_sov, type = 'response')
score_lsa_sov <- ifelse(score_lsa_sov >= 0.5, 1, 0)
summary(as.factor(score_lsa_sov)) # We got 114 here
# Let's take out to see:
sov_lsa <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(sov_score = score_lsa_sov) %>% 
  filter(score_lsa_sov == "1")
sov_lsa # Idk here

##### Model EI
docVectors_EI <- docVectors
docVectors_EI$EI <- score$EI
# Get training and score data
train_EI <- docVectors_EI[1:605,]
score_EI <- docVectors_EI[606:2014,]
# Fit the simple model
set.seed(1234)
fit_EI <- glm(as.factor(EI)~., train_EI, family = 'binomial')
# Predict in training sample
train_lsa_EI <- predict(fit_EI, train_EI, type = 'response')
train_lsa_EI <- ifelse(train_lsa_EI >= 0.5, 1, 0) # set threshold at 0.5
yardstick::conf_mat(table(train_lsa_EI, score$EI[1:605])) 
yardstick::accuracy(table(score$EI[1:605], train_lsa_EI)) 
# accuracy of 74% here, this is low ...
# Let's check the AUC
roc_lsa_EI <- data.frame(cbind(as.numeric(train_lsa_EI), as.numeric(score$EI[1:605])))
roc_lsa_EI <- dplyr::rename(roc_lsa_EI, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_EI, 'tpred', 'label', 1, title = "ROC Test Predictions")
# The AUC is 0.72, that is not too bad though...
# Predict on score data
score_lsa_EI <- predict(fit_EI, score_EI, type = 'response')
score_lsa_EI <- ifelse(score_lsa_EI >= 0.5, 1, 0)
summary(as.factor(score_lsa_EI)) # We got 429 here
# Let's take out to see:
EI_lsa <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(EI_score = score_lsa_EI) %>% 
  filter(score_lsa_EI == "1")
EI_lsa # have to check here but IDK ...

# Model SD
docVectors_SD <- docVectors
docVectors_SD$SD <- score$SD
# Get training and score data
train_SD <- docVectors_SD[1:605,]
score_SD <- docVectors_SD[606:2014,]
# Fit the simple model
set.seed(1234)
fit_SD <- glm(as.factor(SD)~., train_SD, family = 'binomial')
# Predict in training sample
train_lsa_SD <- predict(fit_SD, train_SD, type = 'response')
train_lsa_SD <- ifelse(train_lsa_SD >= 0.5, 1, 0) # set threshold at 0.5
yardstick::conf_mat(table(train_lsa_SD, score$SD[1:605])) 
yardstick::accuracy(table(score$SD[1:605], train_lsa_SD)) 
# accuracy of 78.8% here ...
# Let's check the AUC
roc_lsa_SD <- data.frame(cbind(as.numeric(train_lsa_SD), as.numeric(score$SD[1:605])))
roc_lsa_SD <- dplyr::rename(roc_lsa_SD, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_SD, 'tpred', 'label', 1, title = "ROC Test Predictions")
# The AUC is 0.59, that is not great though...
# Predict on score data
score_lsa_SD <- predict(fit_SD, score_SD, type = 'response')
score_lsa_SD <- ifelse(score_lsa_SD >= 0.5, 1, 0)
summary(as.factor(score_lsa_SD)) # We got 140 here
# Let's take out to see:
SD_lsa <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(SD_score = score_lsa_SD) %>% 
  filter(score_lsa_SD == "1")
SD_lsa # Idk here

##### Model conservation
docVectors_con <- docVectors
docVectors_con$con <- score$con
# Get training and score data
train_con <- docVectors_con[1:605,]
score_con <- docVectors_con[606:2014,]
# Fit the simple model
set.seed(1234)
fit_con <- glm(as.factor(con)~., train_con, family = 'binomial')
# Predict in training sample
train_lsa_con <- predict(fit_con, train_con, type = 'response')
train_lsa_con <- ifelse(train_lsa_con >= 0.5, 1, 0) # set threshold at 0.5
yardstick::conf_mat(table(train_lsa_con, score$con[1:605])) 
yardstick::accuracy(table(score$con[1:605], train_lsa_con)) 
# accuracy of 84.6% here ...
# Let's check the AUC
roc_lsa_con <- data.frame(cbind(as.numeric(train_lsa_con), as.numeric(score$con[1:605])))
roc_lsa_con <- dplyr::rename(roc_lsa_con, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_lsa_con, 'tpred', 'label', 1, title = "ROC Test Predictions")
# The AUC is 0.77, that is not great though...
# Predict on score data
score_lsa_con <- predict(fit_con, score_con, type = 'response')
score_lsa_con <- ifelse(score_lsa_con >= 0.5, 1, 0)
summary(as.factor(score_lsa_con)) # We got 345 here
# Let's take out to see:
con_lsa <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(con_score = score_lsa_con) %>% 
  filter(score_lsa_con == "1")
con_lsa # Idk here
