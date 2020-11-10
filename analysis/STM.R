#Here I use a dataset of foreign affairs speeches by Brazilian ministers/presidents from 2003 to 2019.The process showed below is useful particularly for non-English text. 

library(stm)
library(quanteda)
library(ggplot2)

##PRE-PROCESSING##
#Import data and transform to corpus
data <- corpus(Discursos_Presidenciais2, text_field = "text")

#Create tokens and select features which you want out or in and then convert tokens file to dfm (dfm allows further treatment of the tokenized text)
toks <- tokens(data)

data <- tokens_select(toks, selection = "remove", min_nchar = 3)

stw1<-readLines("stopwordspt.txt")
stw2<-readLines("stopwordspt2.txt")

data <- dfm(data, remove = c(stopwords("portuguese"), stw1, stw2), remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, tolower=TRUE)

topfeatures(data, n = 500)

data <- dfm_wordstem(data, language = "portuguese") #https://github.com/quanteda/quanteda/issues/368

data <- dfm_trim(data, min_docfreq = 0.075, max_docfreq = 0.90, docfreq_type = "prop") #this cuts off words above or below certain frequency

topfeatures(data, n = 50)#useful to see most frequent words before proceeding into the analysis (we may decide to add or remove extra features)

#Convert dfm to stm and indicate metadata. Note that this also words it you want to run LDA thans to the Quanteda package. out <- convert (data, to = °topicmodels°). In that case, you have to use the ldatuning package and proceed with model selection and use the function FindTopicsNumber and FindtopicsNumber_plot.
out <- convert(data, to = 'stm')
meta<-out$meta

##MODEL SELECTION## 
#The STM is a generative and inductive model. The number of topics is not known beforehand. There are certain ways to evaluate what is the "best" number of topics to be chosen. 
many.models<-searchK(out$documents,out$vocab, K=c(20,40,60,80,100,120,140))
print(many.models$results)
plot(many.models) #After analyzing residuals, held-out probability and other info, we can run a few models with different k to fine-tune our model selection.Remember that residuals indicate the difference between the observed value of the dependent variable and the predicted value. The lower the better. Held-out likelihood is the likelihood of the model on the data not used in the initial estimation (held-out documents).
#We can also use the function selectModel (stm package).

#Here I plot the semantic coherence and exclusitivity of each pre-selected model. In this part, you have to run stm for each of the k you selected before.
topic.model <- stm(out$documents,out$vocab,data = meta, K=70)
summary(topic.model)
topic.model2 <- stm(out$documents,out$vocab,data = meta, K=80)
summary(topic.model2)
topic.model3 <- stm(out$documents,out$vocab,data = meta, K=90)
summary(topic.model3)
topic.model4 <- stm(out$documents,out$vocab,data = meta, K=100)
summary(topic.model4)

#You can choose to add interaction terms and then plot them using EstimateEffects and Plot functions
topic.model <- stm(out$documents,out$vocab,K=140, prevalence=~extreme99*s(date), data = meta)

prep17 <- estimateEffect(c(17)~extreme99*s(date),topic.model1, meta=out$meta)

plot(prep17, covariate = "date", model = topic.model, method = "continuous", xlab = "Date", moderator = "extreme99", moderator.value = "Moderate", linecol = "blue", ylim = c(0, .03), printlegend = F, add=T)

plot(prep17, covariate = "date", model = topic.model, method = "continuous", xlab = "date", moderator = "extreme99", moderator.value = "Extreme", linecol = "red", printlegend = F)

legend("topleft", y=0.4, legend = c("Radical/Conservative", "Center"),lwd = 2, col = c("red", "blue"))

#Model selection using LDA
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

#Plotting STM results 
docs <- out$documents

model1<-as.data.frame(cbind(c(1:7),exclusivity(topic.model), semanticCoherence(model=topic.model, docs), "Mod1"))
model2<-as.data.frame(cbind(c(1:8),exclusivity(topic.model2), semanticCoherence(model=topic.model2, docs), "Mod2"))
model3<-as.data.frame(cbind(c(1:9),exclusivity(topic.model3), semanticCoherence(model=topic.model3, docs), "Mod3"))
model4<-as.data.frame(cbind(c(1:10),exclusivity(topic.model4), semanticCoherence(model=topic.model4, docs), "Mod4"))
model5<-as.data.frame(cbind(c(1:11),exclusivity(topic.model5), semanticCoherence(model=topic.model5, docs), "Mod5"))
model6<-as.data.frame(cbind(c(1:12),exclusivity(topic.model6), semanticCoherence(model=topic.model6, docs), "Mod6"))

models <-rbind(model1, model2, model3, model4)

colnames(models)<-c("K","Exclusivity", "SemanticCoherence", "Model")

models$Exclusivity<-as.numeric(as.character(models$Exclusivity))

models$SemanticCoherence<-as.numeric(as.character(models$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

plotexcoer<-ggplot(models, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for k=70, k=80, k=90 and k=100")

plotexcoer #if an error appears, run dev.off()

#Another way to plot semantic coherence and exclusivity is using the package stminsights
library(stminsights)
library(ggplot2)
diag <- get_diag(models = list(
  k_100 = topic.model1,
  k_120 = topic.model2,
  k_140 = topic.model3,
  k_160 = topic.model4),
  outobj = out)

diag %>%
  ggplot(aes(x = coherence, y = exclusivity, color = statistic)) +
  geom_text(aes(label = name), nudge_x = .1) + geom_point() +
  labs(x = 'Semantic Coherence', y = 'Exclusivity') + theme_light()

##ANALYSIS AND INTERPRETATION##
#Run the STM model of choice
topic.model <- stm(out$documents,out$vocab,data = meta, K=80)
plot(topic.model5)

topicNames<-c("Topic 1","Topic 2","Topic 3","Topic 4","Topic 5","Topic 6", "Topic 7","Topic 8","Topic 9","Topic 10") #This changes topics names. See below findThoughts.

plot.STM(topic.model1)
plot.STM(topic.model,type="summary",custom.labels="",topic.names=topicNames)
summary(topic.model1)

#Plot main words accross all topics (works for LDA and STM)
library(tidytext)

ap_topics <- tidy(topic.model)
ap_topics

library(ggplot2)
library(dplyr)
library(scales)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Find thoughts returns the most representative documents for a given topic. We use this to get a better sense of topical content
thoughts <- findThoughts(topic.model,texts=Discursos_Presidenciais2$text, topics=c(23), n=5)
plot(thoughts)
thoughts

#We can also have a larger list of words associated with each topic
#To see more words associated with each topic:
library(tidyr)

td_beta <- tidytext::tidy(topic.model) 

betaT1<-td_beta %>% mutate(topic = paste0("Topic ", topic), term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 30") #beta values for topic k

betaplotT1<-ggplot(betaT1[betaT1$beta>0.004,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta), title = "Word probabilities for Topic")#plot word probabilities higher than 0.003 for the topic

betaplotT1

#Estimate effects according to a certain covariate
prep <- estimateEffect(c(4,10,14,17,23,31,45,56,57)~party,topic.model, meta=out$meta)

#Plot according to the method of difference
plot.estimateEffect(prep, "party", model="topic.model", method="difference", cov.value1="PT", cov.value2="PSL", xlab="PT <----> PSL", main="Emphasis Before v. After Impeachment", ci.level = 0.95)

#Plot according to the method of point estimate
plot.estimateEffect(prep, "rating", model="topic.model5", method="pointestimate", cov.value1="Pre-Impeachment", cov.value2="Post-Impeachment", xlab="more emphasis before <----> more emphasis after", main="Emphasis Before v. After Impeachment", ci.level = 0.9)

#Notice that the plot according to the continuous method requires a numeric covariate. 

#If we want to analyze topic proportions, we can use the made.dt function
topicprop <- make.dt(topic.model, meta)
write.csv(topicprop, "testprop.csv")

######
#We can also use the textProcessor and PrepDocuments functions in pre-processing
data <- pres

processed <- textProcessor(data$documents, metadata=data, lowercase = T, removenumbers = TRUE, removepunctuation = TRUE, stem = TRUE, wordLengths = c(4, Inf), sparselevel = 1, language = "portuguese", verbose = TRUE, onlycharacter = FALSE, striphtml = FALSE, customstopwords = c("é", "ser", "brasil", "português", "english", "todo", "está", "o", "em", "voltar", "foto", "pais", "país", "países", "sobre", "et", "senhor", "então", "ano", "paises", "tudo", "todo", "e", "hoje", "la", "les", "ser", "porque", "sobr", "sobre", "todos", "tudo", "aqui", "acho", "coisa", "outros", "outras", "grande", "senhor", "senhora", "ainda", "aliás", "desse", "desses", "dessas", "dessa", "apenas", "vamos", "portanto", "dizer", "muito", "muitos", "muita", "muitas", "cada", "quero", "cada", "coisa", "menos", "creio", "alguma", "alguns", "algumas", "vezes", "pouco"))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

#######Scrape Twitter data###########

# Mining Twitter data
library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(plyr)
library(RColorBrewer)

#Set API keys

api_key <- "cDcCDv640NgA4MZsLXhdvfZWL"
api_secret <- "bXrSHz240EapjCv6qumJDsZ3B4Kw8l5ui7cco4f89mjM9cMSE8"

access_token <- "1085200731475533824-nG2IFWl4gTMkGtcS7DGjFDdQbKFb4z"
access_token_secret <- "t0LAbFcuicelQvCpawrRbOxKh4787jeN03YC5Lg01fmNQ"

#And authorize Twitter API 

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets.bolsonaro = searchTwitter('#criminalizacomunismo exclude:retweets', n=100000, lang = "pt")
#since = '2016-12-12', until = "2016-12-13"
tweets.text = twListToDF(tweets.bolsonaro)
tweet.corpus = Corpus(VectorSource(tweets.text))

write.csv(tweets.text, file="crimicomutweets.csv")

#Remove certain characters and words
tweet.removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweet.removeATUser = function(x) gsub("@[a-z,A-Z]*","",x)
tweet.removeEmoji = function(x) gsub("\\p{So}|\\p{Cn}","",x, per = TRUE)
tweet.removeSpecialChar = function(x) gsub("[^[:alnum:]///' ]", "",x)
tweet.removeHash = function(x) gsub("#[a-z,A-Z]*","",x)

tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeURL))
tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeATUser))
tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeEmoji))
tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeSpecialChar))
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
tweet.corpus = tm_map(tweet.corpus, content_transformer(tolower))
tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeHash))

#world like "and" and "the" should be removed
tweet.corpus = tm_map(tweet.corpus, removeWords, c(stopwords("english"), "RT", "rt", "relnofollowtwitt", "hreftwittercomdownloadandroida", "hreftwittercomdownloadiphon", "trumpswar", "trump", "iphonea", "appa", "web", "hrefwwwsmartnewscom", "relnofollowsmartnew", "ubudeufcucucbueufcuba","jimvillhau", ))
tweet.corpus = tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)

#keep the root of the words
tweet.corpus = tm_map(tweet.corpus, stemDocument)

#Stopwords
stopWords <- c("t.co","http","https","amp","t","t.c","c","rt")


##SOURCES AND FURTHER INFO##
#https://cran.r-project.org/web/views/NaturalLanguageProcessing.html
#https://www.youtube.com/watch?v=evTuL-RcRpc&feature=youtu.be 
#https://www.youtube.com/watch?v=pTqV0Aqj and at https://www.tidytextmining.com/topicmodeling.html 
#https://www.youtube.com/watch?v=evTuL-RcRpc
#https://quanteda.io/articles/pkgdown/examples/twitter.html,
#https://quanteda.io/articles/pkgdown/quickstart.html#creating-a-corpus
#https://cran.r-project.org/web/packages/stminsights/vignettes/intro.html
#pre-processing n-grams in the stm package:  https://github.com/bstewart/stm/issues/152 and also https://github.com/chris-billingham/topic_modelling/blob/master/gsr_model.R 
#https://www.youtube.com/watch?v=WZ0iTSxz3I4
#see also https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
#see also https://compsocialscience.github.io/summer-institute/2019/materials/day3-text-analysis/basic-text-analysis/rmarkdown/Basic_Text_Analysis_in_R.html
#https://cran.r-project.org/web/views/NaturalLanguageProcessing.html
#https://www.youtube.com/watch?v=evTuL-RcRpc&feature=youtu.be 
#On how to use stm on Twitter data: https://gist.github.com/sagarnanduunc/82acbef8a23fe368a78af052f3e591ac
#Can be useful and is in Portuguese: https://rstudio-pubs-static.s3.amazonaws.com/342346_fcc3e8b926c7498285d49099140bd247.html
