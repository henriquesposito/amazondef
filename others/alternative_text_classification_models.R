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
TDM_lsa <- TermDocumentMatrix(txtcorp_lsa, control = list(weighting = weightTfIdf))
# Get 20 latent topics
lsaTDM <- lsa(TDM_lsa, 20) # This takes a few minutes
# Extract the document LSA values
docVectors <- as.data.frame(lsaTDM$dk)
# Append the target var
docVectors$false_positives <- score$false_positives
# Sample (avoid overfitting)
set.seed(1234)
train_lsa <- docVectors[1:605,]
pred_score_lsa <- docVectors[606:2014,]
# Fit the simple model
fit <- glm(false_positives~., train_lsa, family = 'binomial')
# Predict in sample
train_lsa <- predict(fit, train_lsa, type = 'response')
train_lsa <- ifelse(train_lsa >= 0.5, 1, 0) # set threshold at 0.5
yardstick::conf_mat(table(train_lsa, score$false_positives[1:605])) 
yardstick::accuracy(table(score$false_positives[1:605], train_lsa)) 
# accuracy of 95.4% here, this is really good!
# Let's check the AUC
roc_fp_lsa <- data.frame(cbind(as.numeric(train_lsa), as.numeric(score$false_positives[1:605])))
roc_fp_lsa <- dplyr::rename(roc_fp_lsa, tpred = "X1", label = "X2")
WVPlots::ROCPlot(roc_fp_lsa, 'tpred', 'label', 1, title = "ROC Test Predictions")
# The AUC is 0.79, that is ok but not great though...
# Predict on score
pred_score_lsa <- predict(fit, pred_score_lsa, type = 'response')
pred_score_lsa <- ifelse(pred_score_lsa >= 0.5, 1, 0)
summary(as.factor(pred_score_lsa)) # We got 96 false positives here!!!
# this seems to be rather accurate...
# Let me take out to see:
fp_lsa <- score[606:2014,] %>%
  select(AM2) %>%
  mutate(fp_score = pred_score_lsa) %>% 
  filter(pred_score_lsa == "1")
fp_lsa # This seems good actually!
# But a few false positives that we can perhaps remove by changing the threshold.
# Before we proceed with this model, let's try and see how the other "fancier" models perform!

##### How about we fit a fancier linear model with glmnet
# Elastic net model (cv.glmnet)
# Get a corpus and DTM
txtcorp <- VCorpus(VectorSource(as.character(score$AM2)))
txtcorp <- tm::tm_map(txtcorp, removeWords, stopwords('pt'))
txtModelMatrix <- DocumentTermMatrix(txtcorp)
txtModelMatrix <- as.matrix(txtModelMatrix)
# Get a training set from 605 first obs coded
train <- txtModelMatrix[1:605,]
pred_score <- txtModelMatrix[606:2014,] # rest are to be scored
# Build an elastic net classifier for fp
set.seed(1234)
textFit_fp <- cv.glmnet(train,
                        y            = as.factor(score$false_positives[1:605]),
                        family       = 'binomial',
                        type.measure = 'auc',
                        nfolds       = 5,
                        intercept    = F)
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

##### Let's run a classification model for false positives with tidymodels
# see here for more info:https://smltar.com/mlclassification.html#classfirstattemptlookatdata
# Here we use tidymodels to build a naive bayes and a regularized linear model (lasso)
# Get training and score sets and remove stopwords
score$false_positives <- as.factor(ifelse(is.na(score$false_positives), 0, score$false_positives))
# Just so that models runs...
txtcorp_tidy <- VCorpus(VectorSource(as.character(score$AM2)))
txtcorp_tidy <- tm::tm_map(txtcorp_tidy, removeWords, stopwords('pt'))
train <- score[1:605,] 
test <- score[606:2014,]
# We need to get an "rsplit" object here, so have to do a little trick
data <- bind_rows(train, test, .id = "dataset") %>% 
  mutate(dataset = factor(dataset, labels = c("train", "test")))
train_ids <- which(data$dataset == "train")
split <- initial_split(data, strata = "false_positives")
split$in_id <- train_ids
train_split <- training(split) %>% # get trained data = labelled
  select(-dataset)
test_split <- testing(split) %>% # get non labeled data
  select(-dataset)
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
# 10 fold cross validation, for evaluation
set.seed(1234)
nb_folds <- vfold_cv(train_split) 
nb_wf <- workflow() %>%
  add_recipe(model_tidy) %>%
  add_model(nb_spec)
nb_rs <- fit_resamples(nb_wf, nb_folds,
                       control = control_resamples(save_pred = TRUE))
nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)
nb_rs_metrics # accuracy of 0.91 is not bad but why is the AUC score so low?
nb_rs_predictions %>% # Nothing changes much
  group_by(id) %>%
  roc_curve(truth = false_positives, .pred_1) %>%
  autoplot() +
  labs(color = NULL, title = "ROC curve for Training FP",
       subtitle = "Each resample fold is shown in a different color")
# We do no better than a null model??? 
# I will stop here...

# Fit LASSO model to compare (we use glmnet, what a surprise)
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
lasso_wf <- workflow() %>%
  add_recipe(model_tidy) %>%
  add_model(lasso_spec)
set.seed(1234)
lasso_rs <- fit_resamples(lasso_wf, nb_folds,
                          control = control_resamples(save_pred = TRUE))
lasso_rs_metrics <- collect_metrics(lasso_rs)
lasso_rs_predictions <- collect_predictions(lasso_rs)
lasso_rs_metrics # Again better than NB already... accuracy = 0.936 and AUC = 0.8
lasso_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = false_positives, .pred_0) %>%
  autoplot() +
  labs(color = NULL, title = "ROC curve for FP lasso",
       subtitle = "Each resample fold is shown in a different color")
# This is more likely, but insane...
# how much does the AUC curve differ by folds??? fold 7 is awesome BTW
conf_mat_resampled(lasso_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")
# Let's try to tune the LASSO parameters before classifying the texts
# This is almost cheating but, oh well
tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
lambda_grid <- grid_regular(penalty(), levels = 30) # 30 different lambda values to try out
tune_wf <- workflow() %>%
  add_recipe(model_tidy) %>%
  add_model(tune_spec)
set.seed(1234)
tune_rs <- tune_grid(tune_wf, nb_folds, grid = lambda_grid,
                     control = control_resamples(save_pred = TRUE))
collect_metrics(tune_rs) # get metrics for each model
tune_rs %>% show_best("roc_auc") 
chosen_auc <- tune_rs %>%
  select_by_one_std_err(metric = "roc_auc", -penalty) # get best model
final_lasso <- finalize_workflow(tune_wf, chosen_auc) # final model
fitted_lasso <- fit(final_lasso, train_split) # fit to data
fitted_lasso %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(-estimate) # terms that contribute to obs being a fp
final_fitted <- last_fit(final_lasso, split)
collect_metrics(final_fitted)
collect_predictions(final_fitted) %>%
  conf_mat(truth = false_positives, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
# Ok, so although all test obs were made 0,
# the LASSO model only scored 29 false positives, this seems little...
# It resembles the cv.glmnet model scores above as they are similar
# (i.e. glmnet includes LASSO and ridge errors or something like this)

##### Let's use rtexttools to apply an SVM and a Random Forest model
# see also: https://journal.r-project.org/archive/2013/RJ-2013-001/RJ-2013-001.pdf
# SVM model

# random forest