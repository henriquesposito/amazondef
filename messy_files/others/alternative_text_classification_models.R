# Run and compare alternative models to classify texts
# Load packages
library(dplyr)
library(stringr)
library(glmnet)
library(tm)
library(yardstick)
library(tidymodels)
library(textrecipes)
library(RTextTools)
library(lsa)
library(ggplot2)
library(discrim)
library(naivebayes)

# First let's just get the data ready!
# Get hand coded final training set
training_set_final <- readRDS("~/Documents/GitHub/amazondef/data/training_set_final.Rds")
# Get training texts standardized
training_set_final$AM2 <- stringi::stri_trans_general(training_set_final$AM2, 'latin-ascii')
training_set_final$AM2 <- stringr::str_squish(training_set_final$AM2)
training_set_final$AM2 <- tolower(training_set_final$AM2)
training_set_final$AM2 <- tm::removeNumbers(training_set_final$AM2)
training_set_final$AM2 <- tm::removePunctuation(training_set_final$AM2)
training_set_final$AM2 <- trimws(training_set_final$AM2)
# Split the training set into train and test for testing
data <- training_set_final %>% select(AM2, sov, EI, SD, con, false_positives)
train_size <- floor(0.75 * nrow(data))
in_rows <- sample(c(1:nrow(data)), size = train_size, replace = FALSE)
train <- data[in_rows, ]
test <- data[-in_rows, ]

##### Now let's run a simple linear model based on a LSA topic model
# Clean and get a dtm
txtcorp_lsa <- VCorpus(VectorSource(as.character(data$AM2)))
txtcorp_lsa <- tm::tm_map(txtcorp_lsa, removeWords, stopwords('pt'))
TDM_lsa <- TermDocumentMatrix(txtcorp_lsa, control = list(weighting = weightTfIdf))
# Get 20 latent topics
lsaTDM <- lsa(TDM_lsa, 20) # This takes a few minutes
# Extract the document LSA values
docVectors <- as.data.frame(lsaTDM$dk)
# Append the target var
docVectors$false_positives <- data$false_positives
# Sample train and test data (avoid overfitting)
train_lsa <- docVectors[in_rows,]
pred_score_lsa <- docVectors[-in_rows,]
# Fit the simple model
set.seed(1234)
fit_lsa <- glm(false_positives~., train_lsa, family = 'binomial')
# Predict in sample
set.seed(1234)
pred_lsa <- predict(fit_lsa, pred_score_lsa, type = 'response')
pred_lsa <- ifelse(pred_lsa >= 0.5, 1, 0)
summary(as.factor(pred_lsa))
yardstick::conf_mat(table(pred_lsa, test$false_positives)) 
yardstick::accuracy(table(test$false_positives, pred_lsa)) 
# accuracy of 95.4% here, this is really good!
# Let's check the AUC
roc_fp_lsa <- data.frame(cbind(as.numeric(pred_lsa), as.numeric(test$false_positives)))
roc_fp_lsa <- dplyr::rename(roc_fp_lsa, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_fp_lsa, 'tpred', 'label', 1, title = "ROC Test Predictions LSA/GLM")
# The AUC is 0.8, that is ok but not great though...
# Let me take out to see:
fp_lsa <- test %>%
  select(AM2) %>%
  mutate(fp_score = pred_lsa) %>% 
  filter(pred_lsa == "1")
fp_lsa # This seems okay actually!
# But a few false positives that we can perhaps remove by changing the threshold.
# Before we proceed with this model, let's try and see how the other "fancier" models perform!

##### How about we fit a fancier linear model with glmnet
# Elastic net model (cv.glmnet)
# Get a corpus and DTM
txtcorp <- VCorpus(VectorSource(as.character(data$AM2)))
txtcorp <- tm::tm_map(txtcorp, removeWords, stopwords('pt'))
txtModelMatrix <- DocumentTermMatrix(txtcorp)
txtModelMatrix <- as.matrix(txtModelMatrix)
# Get a training set from 605 first obs coded
train_el <- txtModelMatrix[in_rows,]
pred_el <- txtModelMatrix[-in_rows,] # rest are to be scored
# Build an elastic net classifier for fp
set.seed(1234)
textFit_fp <- cv.glmnet(train_el,
                        y            = train$false_positives,
                        family       = 'binomial',
                        type.measure = 'auc',
                        nfolds       = 5,
                        intercept    = F)
# Predict the pred set and see accuracy
pred_el_fp   <- predict(textFit_fp, pred_el, type = 'class')
summary(as.factor(pred_el_fp)) # 2 false positives positives???
yardstick::conf_mat(table(pred_el_fp, test$false_positives))
yardstick::accuracy(table(test$false_positives, pred_el_fp)) 
# 92.8% accuracy, this is pretty okay actually!
# Get an AUC curves
roc_el_fp <- data.frame(cbind(as.numeric(pred_el_fp), as.numeric(test$false_positives)))
roc_el_fp <- dplyr::rename(roc_el_fp, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_el_fp, 'tpred', 'label', 1, title = "ROC Test Predictions EL")
# AUC = 0.58 is not good ...

##### Let's run a classification model for false positives with tidymodels
# see here for more info:https://smltar.com/mlclassification.html#classfirstattemptlookatdata
# Here we use tidymodels to build a naive bayes and a regularized linear model (lasso)
# Get training and score sets and remove stopwords
data$false_positives <- as.factor(ifelse(is.na(data$false_positives), 0, data$false_positives))
# Just so that models runs...
txtcorp_tidy <- VCorpus(VectorSource(as.character(data$AM2)))
txtcorp_tidy <- tm::tm_map(txtcorp_tidy, removeWords, stopwords('pt'))
# We need to get an "rsplit" object here, so have to do a little trick
d <- data
traint <- d[in_rows, ]
testt <- d[-in_rows, ]
d <- bind_rows(traint, testt, .id = "dataset") %>% 
  mutate(dataset = factor(dataset, labels = c("train", "test")))
train_ids <- which(d$dataset == "train")
split <- initial_split(d, strata = "false_positives")
split$in_id <- train_ids
train_split <- training(split) %>% # get trained data = labelled
  select(-dataset) %>% 
  mutate(false_positives = as.factor(false_positives))
test_split <- testing(split) %>% # get non labeled data
  select(-dataset) %>% 
  mutate(false_positives = as.factor(false_positives))
# Set up the recipe (data model)
model_tidy <- recipe(false_positives ~ AM2, data = train_split)
# Tokenize
model_tidy <- model_tidy %>%
  step_tokenize(AM2) %>%
  step_tokenfilter(AM2, max_tokens = 1e3) %>%
  step_tfidf(AM2) # We keep only the top 1000 words here, can change later on.
# Build a workflow for tidymodel
model_tidy_wf <- workflow() %>% add_recipe(model_tidy)
# Let's try a naive bayes model first (using discrim package)
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")
# Fit NB model to training data
nb_fit <- model_tidy_wf %>%
  add_model(nb_spec) %>%
  fit(data = train_split)
augment(nb_fit, new_data = test_split) %>% conf_mat(truth = false_positives, estimate = .pred_class)
augment(nb_fit, new_data = test_split) %>% accuracy(truth = false_positives, estimate = .pred_class)
# accuracy is of 91.1% but it seems that it predicts no obs as an fp ???
a <- augment(nb_fit, new_data = test_split)
# Get an AUC curve
roc_nb_fp <- data.frame(cbind(as.numeric(a$.pred_class), as.numeric(test$false_positives)))
roc_nb_fp <- dplyr::rename(roc_nb_fp, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_nb_fp, 'tpred', 'label', 1, title = "ROC Test Predictions EL")
# NB models does no better than random null models...

##### Let's use rtexttools to apply an SVM and a Random Forest model
# see also: https://journal.r-project.org/archive/2013/RJ-2013-001/RJ-2013-001.pdf
# term document matrix
doc_matrix <- create_matrix(data$AM2, language = "portuguese", removeNumbers = TRUE,
                            stripWhitespace = TRUE, removeStopwords = TRUE,
                            removeSparseTerms = .998, removePunctuation = TRUE)
# container
container <- create_container(doc_matrix, data$false_positives,
                              trainSize = 1:453, testSize = 454:605, virgin = FALSE)
# train and test are identical in size to the ones used above though sampling might differ.
# SVM model
SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)
# random forest
RF <- train_model(container,"RF")
RF_CLASSIFY <- classify_model(container, RF)
# model comparison
analytics <- create_analytics(container, cbind(SVM_CLASSIFY, RF_CLASSIFY))
summary(analytics)
doc_summary <- analytics@document_summary
# Get accuracy and roc for svm
yardstick::conf_mat(table(doc_summary$SVM_LABEL, doc_summary$MANUAL_CODE))
yardstick::accuracy(table(doc_summary$MANUAL_CODE, doc_summary$SVM_LABEL)) 
roc_fp_svm <- data.frame(cbind(as.numeric(doc_summary$SVM_LABEL), as.numeric(doc_summary$MANUAL_CODE)))
roc_fp_svm <- dplyr::rename(roc_fp_svm, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_fp_svm, 'tpred', 'label', 1, title = "ROC Test Predictions RF")
# Get accuracy and roc for RF
recall_accuracy(analytics@document_summary$MANUAL_CODE,
                analytics@document_summary$FORESTS_LABEL) # just a differen way
roc_fp_rf <- data.frame(cbind(as.numeric(doc_summary$FORESTS_LABEL), as.numeric(doc_summary$MANUAL_CODE)))
roc_fp_rf <- dplyr::rename(roc_fp_rf, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_fp_rf, 'tpred', 'label', 1, title = "ROC Test Predictions RF")
# That is all folks!!!!
# All this shit and we will likely end up with the simplest model... LSA/GLM

# Get a sample for validation
amazon_speeches_long <- readRDS("~/Documents/GitHub/amazondef/data/amazon_speeches_long.Rds")
t <- dplyr::select(training_set_final, AM2) # used to remove duplicates later
amazon_speeches_long$AM2 <- stringi::stri_trans_general(amazon_speeches_long$AM2, 'latin-ascii')
amazon_speeches_long$AM2 <- stringr::str_squish(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tolower(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removeNumbers(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- tm::removePunctuation(amazon_speeches_long$AM2)
amazon_speeches_long$AM2 <- trimws(amazon_speeches_long$AM2)
# Remove train obs, keep only a validation set from non scored data
validation <- amazon_speeches_long[!(amazon_speeches_long$AM2 %in% t$AM2),] %>%
  select(ID, AM2) %>%
  dplyr::slice_sample(n = 214)
# export to Excel
library(xlsx)
xlsx::write.xlsx(validation, file = "validation.xlsx")
