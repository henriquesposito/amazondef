# Run an alternative model to classify texts

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

##### Let's run a classification model for false positives with tidymodels
# see here for more info:https://smltar.com/mlclassification.html#classfirstattemptlookatdata

# Get training and score sets and remove stopwords
score$false_positives <- as.factor(ifelse(score$false_positives == 1, "fp", "non-fp")) # Just so that models runs...
txtcorp_tidy <- VCorpus(VectorSource(as.character(score$AM2)))
txtcorp_tidy <- tm::tm_map(txtcorp_tidy, removeWords, stopwords('pt'))
train_tidy <- score[1:605,]
pred_score_tidy <- score[606:2014,] # rest are to be scored

# Set up the recipe (data model)
model_tidy <- recipe(false_positives ~ AM2, data = score)

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
  fit(data = train_tidy)

################ Stopped here last

##### Let's use rtexttools for comparison

# Clean, extract text and get into correct object
cleanTrain <- cleanCorpus(VCorpus(VectorSource(train)), stops)
cleanTrain
cleanTrain <- unlist(lapply(cleanTrain, content))
cleanTrain[1]
trainDTMm <- create_matrix(cleanTrain, language="english")

# Create the container
# trainSize; if you want to split within the single matrix but best practice is to bring it in separate to mimic really new data processing 
container <- create_container(matrix    = trainDTMm,
                              labels    = diabetes$readmitted[idx], 
                              trainSize = 1:length(idx), 
                              virgin=FALSE)

# Build Models, can take ages for complex algos
#models <- train_models(container, algorithms=c("GLMNET","SVM")) #"SVM","SLDA","BOOSTING","BAGGING", "RF","GLMNET","TREE","NNET"
#saveRDS(models, 'rtexttools_models.rds')
models <- readRDS('rtexttools_models.rds')


# Score the original training data
results <- classify_models(container, models)
head(results)

# Append Actuals
results$actual <- diabetes$readmitted[idx]

# Confusion Matrix
table(results$GLMNET_LABEL, results$actual)
table(results$SVM_LABEL, results$actual)

# Accuracy GLMNET_LABEL
autoplot(conf_mat(table(results$GLMNET_LABEL, results$actual)))
accuracy(table(results$GLMNET_LABEL, results$actual))

# Accuracy SVM_LABEL
autoplot(conf_mat(table(results$SVM_LABEL, results$actual)))
accuracy(table(results$SVM_LABEL, results$actual))

# Now let's apply the models to "new" documents
# Clean, extract text and get into correct object
cleanTest <- cleanCorpus(VCorpus(VectorSource(test)), stops)
cleanTest <- unlist(lapply(cleanTest, content))

# You have to combine the matrices to the original to get the tokens joined
allDTM  <- c(cleanTrain, cleanTest)
allDTMm <- create_matrix(allDTM, language="english")
containerTest <- create_container(matrix    = allDTMm,
                                  labels    = c(diabetes$readmitted[idx], diabetes$readmitted[-idx]),
                                  trainSize = 1:length(idx),
                                  testSize  = (length(idx)+1):8500,
                                  virgin=T)

#testFit <- train_models(containerTest, algorithms=c("GLMNET", "SVM"))
#saveRDS(testFit, 'rtexttools_testFit.rds')
testFit <-readRDS('rtexttools_testFit.rds')
resultsTest <- classify_models(containerTest, testFit)

# Append Actuals
resultsTest$actual <- diabetes$readmitted[-idx]

# Confusion Matrix
summary(resultsTest$GLMNET_PROB)
summary(resultsTest$SVM_PROB)
table(resultsTest$SVM_LABEL, resultsTest$actual)
table(resultsTest$GLMNET_LABEL, resultsTest$actual)