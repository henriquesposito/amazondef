# Classify texts

# Load packages
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)

# Get hand coded final training set
training_set_final <- readRDS("~/GitHub/amazondef/data/training_set_final.Rds")
# Get full dataset and remove hand coded observations
amazon_speeches_long <- readRDS("~/GitHub/amazondef/data/amazon_speeches_long.Rds")

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

# Let's run a classification model for false positives, just as a test

# Get a corpus and DTM
txtcorp <- VCorpus(VectorSource(as.character(score$AM2)))
txtcorp <- tm::tm_map(txtcorp, removeWords, stopwords('pt'))
txtModelMatrix <- DocumentTermMatrix(txtcorp)
txtModelMatrix <- as.matrix(txtModelMatrix)

# Get a training set from 605 first obs coded
set.seed(1234)
train <- txtModelMatrix[1:605,]
pred_score <- txtModelMatrix[606:2014,] # rest are to be scored

##### Model FP
# Build an elastic net classifier for fp
textFit_fp <- cv.glmnet(train,
                        y            = as.factor(score$false_positives[1:605]),
                        family       = 'binomial',
                        type.measure = 'auc',
                        nfolds       = 5,
                        intercept    = F)
textFit_fp

# Explore influential terms
bestTerms_fp <- subset(as.matrix(coefficients(textFit_fp)), as.matrix(coefficients(textFit_fp)) !=0)
bestTerms_fp <- data.frame(term   = rownames(bestTerms_fp), impact = bestTerms_fp)
bestTerms_fp <- bestTerms_fp[order(bestTerms_fp[,2], decreasing = T),]
head(bestTerms_fp,10) # Seems accurate
tail(bestTerms_fp, 10)

# Predict the training set and see acurracy
training_fp   <- predict(textFit_fp, train, type = 'class')
yardstick::conf_mat(table(training_fp, score$false_positives[1:605]))
yardstick::accuracy(table(score$false_positives[1:605], training_fp)) 
# 93.4% accuracy, this is pretty okay actually!

# Get an AUC curves
roc_fp <- data.frame(cbind(as.numeric(training_fp), as.numeric(score$false_positives[1:605])))
roc_fp <- dplyr::rename(roc_fp, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_fp, 'tpred', 'label', 1, title = "ROC Test Predictions")
# It seems our model might fail on the side of missing true positives...

# Fit the model in score data
scored_fp <- predict(textFit_fp, pred_score, type = 'class')
summary(as.factor(scored_fp)) # We predicted 28 false positives, this seems low ...

# Let's see a few of these
fp <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(fp_score = scored_fp) %>% 
  filter(fp_score == "1")
fp # These obs seem to be indeed false positives! Though we are missing a few...
# Let's fit a similar model for the other codes
# Start with sovereignty

##### Model sov
# Build an elastic net classifier for sov
set.seed(1234)
textFit_sov <- cv.glmnet(train,
                         y            = as.factor(score$sov[1:605]),
                         family       = 'binomial',
                         type.measure = 'auc',
                         nfolds       = 5,
                         intercept    = F)
textFit_sov

# Explore influential terms
bestTerms_sov <- subset(as.matrix(coefficients(textFit_sov)), as.matrix(coefficients(textFit_sov)) !=0)
bestTerms_sov <- data.frame(term   = rownames(bestTerms_sov), impact = bestTerms_sov)
bestTerms_sov <- bestTerms_sov[order(bestTerms_sov[,2], decreasing = T),]
head(bestTerms_sov,10) # Seems accurate
tail(bestTerms_sov, 10)

# Predict the training set and see acurracy
training_sov   <- predict(textFit_sov, train, type = 'class')
yardstick::conf_mat(table(training_sov, score$sov[1:605]))
yardstick::accuracy(table(score$sov[1:605], training_sov)) 
# 88% accuracy, not super much here...

# Get an AUC curves
roc_sov <- data.frame(cbind(as.numeric(training_sov), as.numeric(score$sov[1:605])))
roc_sov <- dplyr::rename(roc_sov, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_sov, 'tpred', 'label', 1, title = "ROC Test Predictions")

# Fit the model in score data
scored_sov <- predict(textFit_sov, pred_score, type = 'class')
summary(as.factor(scored_sov)) # We predicted 45 sov statements, this seems low as well ...

# Let's see a few of these
sov <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(sov_score = scored_sov) %>% 
  filter(sov_score == "1")
sov # Looking quickly, it also seems this is ok... thoug, again, we appear to miss some codes ...

##### Model EI
set.seed(1234)
textFit_EI <- cv.glmnet(train,
                         y            = as.factor(score$EI[1:605]),
                         family       = 'binomial',
                         type.measure = 'auc',
                         nfolds       = 5,
                         intercept    = F)
textFit_EI

# Explore influential terms
bestTerms_EI <- subset(as.matrix(coefficients(textFit_EI)), as.matrix(coefficients(textFit_EI)) !=0)
bestTerms_EI <- data.frame(term   = rownames(bestTerms_EI), impact = bestTerms_EI)
bestTerms_EI <- bestTerms_EI[order(bestTerms_EI[,2], decreasing = T),]
head(bestTerms_EI,10) # Seems accurate
tail(bestTerms_EI, 10)

# Predict the training set and see acurracy
training_EI   <- predict(textFit_EI, train, type = 'class')
yardstick::conf_mat(table(training_EI, score$EI[1:605]))
yardstick::accuracy(table(score$EI[1:605], training_EI)) 
# 87% accuracy, not super much here either...

# Get an AUC curves
roc_EI <- data.frame(cbind(as.numeric(training_EI), as.numeric(score$EI[1:605])))
roc_EI <- dplyr::rename(roc_EI, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_EI, 'tpred', 'label', 1, title = "ROC Test Predictions")
# AUC score here is really high tough, why?

# Fit the model in score data
scored_EI <- predict(textFit_EI, pred_score, type = 'class')
summary(as.factor(scored_EI)) # We predicted 413 EI statements ...

# Let's see a few of these
EI <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(EI_score = scored_EI) %>% 
  filter(EI_score == "1")
EI # Did not have the time to look here...

##### Model SD
set.seed(1234)
textFit_SD <- cv.glmnet(train,
                         y            = as.factor(score$SD[1:605]),
                         family       = 'binomial',
                         type.measure = 'auc',
                         nfolds       = 5,
                         intercept    = F)
textFit_SD

# Explore influential terms
bestTerms_SD <- subset(as.matrix(coefficients(textFit_SD)), as.matrix(coefficients(textFit_SD)) !=0)
bestTerms_SD <- data.frame(term   = rownames(bestTerms_SD), impact = bestTerms_SD)
bestTerms_SD <- bestTerms_SD[order(bestTerms_SD[,2], decreasing = T),]
head(bestTerms_SD,10) # Seems accurate
tail(bestTerms_SD, 10)

# Predict the training set and see acurracy
training_SD   <- predict(textFit_SD, train, type = 'class')
yardstick::conf_mat(table(training_SD, score$SD[1:605]))
yardstick::accuracy(table(score$SD[1:605], training_SD)) 
# 85% accuracy, seems low ...

# Get an AUC curves
roc_SD <- data.frame(cbind(as.numeric(training_SD), as.numeric(score$SD[1:605])))
roc_SD <- dplyr::rename(roc_SD, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_SD, 'tpred', 'label', 1, title = "ROC Test Predictions")

# Fit the model in score data
scored_SD <- predict(textFit_SD, pred_score, type = 'class')
summary(as.factor(scored_SD)) # We predicted 125 SD statements, this seems low as well ...

# Let's see a few of these
SD <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(SD_score = scored_SD) %>% 
  filter(SD_score == "1")
SD # Did not have the time to look into it just yet...

#### Model con
set.seed(1234)
textFit_con <- cv.glmnet(train,
                         y            = as.factor(score$con[1:605]),
                         family       = 'binomial',
                         type.measure = 'class',
                         nfolds       = 5,
                         intercept    = F)
textFit_con

# Explore influential terms
bestTerms_con <- subset(as.matrix(coefficients(textFit_con)), as.matrix(coefficients(textFit_con)) !=0)
bestTerms_con <- data.frame(term   = rownames(bestTerms_con), impact = bestTerms_con)
bestTerms_con <- bestTerms_con[order(bestTerms_con[,2], decreasing = T),]
head(bestTerms_con,10) # Seems accurate
tail(bestTerms_con, 10)

# Predict the training set and see acurracy
training_con   <- predict(textFit_con, train, type = 'class')
yardstick::conf_mat(table(training_con, score$con[1:605]))
yardstick::accuracy(table(score$con[1:605], training_con)) 
# 88% accuracy, not super much here...

# Get an AUC curves
roc_con <- data.frame(cbind(as.numeric(training_con), as.numeric(score$con[1:605])))
roc_con <- dplyr::rename(roc_con, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_con, 'tpred', 'label', 1, title = "ROC Test Predictions")
# AUC 0.81 is not bad though

# Fit the model in score data
scored_con <- predict(textFit_con, pred_score, type = 'class')
summary(as.factor(scored_con)) # We predicted 308 con statements, this seems low as well ...

# Let's see a few of these
con <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(con_score = scored_con) %>% 
  filter(con_score == "1")
con # Did not have the time to look into it just yet...

# We can discuss the findings once we take our time to look into it,
# but we can always play around with the model parameters and/or method.
# This does not look so bad though for a first attempt from what I see!


# What if we use only stems...
##### Stemming: Let's try it out for false_positives

# Get a package that stems for portuguese language
devtools::install_github("dfalbel/ptstem")
library(ptstem)

# Agreesive stemming that replaces terms with different ends but same stem
stem_corp <- ptstem::ptstem(score$AM2, algorithm = "rslp", complete = TRUE)
score_st <- score %>% mutate(AM2_st = stem_corp)

# Get a corpus and DTM
txtcorp_st <- VCorpus(VectorSource(as.character(score_st$AM2_st)))
txtcorp_st <- tm::tm_map(txtcorp_st, removeWords, stopwords('pt'))
txtModelMatrix_st <- DocumentTermMatrix(txtcorp_st)
txtModelMatrix_st <- as.matrix(txtModelMatrix_st)

set.seed(1234)
train_st <- txtModelMatrix_st[1:605,]
pred_score_st <- txtModelMatrix_st[606:2014,] # rest are to be scored

# Build an elastic net classifier for steemmed fp
textFit_fp_st <- cv.glmnet(train_st,
                           y            = as.factor(score_st$false_positives[1:605]),
                           family       = 'binomial',
                           type.measure = 'auc',
                           nfolds       = 5,
                           intercept    = F)

# Predict the training set and see acurracy
training_fp_st   <- predict(textFit_fp_st, train_st, type = 'class')
yardstick::conf_mat(table(training_fp_st, score_st$false_positives[1:605]))
yardstick::accuracy(table(score_st$false_positives[1:605], training_fp_st)) 
# 93% stemmed versus 93.4% accuracy non steemed

# Fit the model in score data
scored_fp_st <- predict(textFit_fp_st, pred_score_st, type = 'class')
summary(as.factor(scored_fp_st)) # We predicted 23 false positives, this is lower than previous... 
# I will just stop here, it seems that stemming does not help, maybe the opposite will though.
# That is, less data cleaning such as not removing stopwords might help.

##### Let's try not removing stowords
txtcorp_dirty <- VCorpus(VectorSource(as.character(score$AM2)))
txtModelMatrix_dirty <- DocumentTermMatrix(txtcorp_dirty)
txtModelMatrix_dirty <- as.matrix(txtModelMatrix_dirty)

# Get a training set from 605 first obs coded
set.seed(1234)
train_dirty <- txtModelMatrix_dirty[1:605,]
pred_score_dirty <- txtModelMatrix_dirty[606:2014,] # rest are to be scored

# Build an elastic net classifier for fp with stopwords in
textFit_fp_dirty <- cv.glmnet(train_dirty,
                              y            = as.factor(score$false_positives[1:605]),
                              family       = 'binomial',
                              type.measure = 'auc',
                              nfolds       = 5,
                              intercept    = F)

# Predict the training set and see acurracy
training_fp_dirty   <- predict(textFit_fp_dirty, train_dirty, type = 'class')
yardstick::conf_mat(table(training_fp_dirty, score$false_positives[1:605]))
yardstick::accuracy(table(score$false_positives[1:605], training_fp_dirty)) 
# 92% stemmed versus 93.4% accuracy previously

# Fit the model in score data
scored_fp_dirty <- predict(textFit_fp_dirty, pred_score_dirty, type = 'class')
summary(as.factor(scored_fp_dirty)) # We predicted 20 false positives, this is much lower than previous... 
# I will just stop here, it seems that keeping stopwords also does not help.
# Let's try other supervised machine learning models!
