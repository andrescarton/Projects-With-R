# Mini-Project 1 - Sentiment Analysis in Social Networks

# Setting the working directory
setwd("C:/Users/andre/Documents/FCD/BigDataRAzure/Mini-Projeto 1")
getwd()

## Step 1 - Packages and Authentication

# Installing and Loading the twitterR Packageinstall.packages("twitteR")
install.packages("twitteR")
install.packages("httr")
library(twitteR)
library(httr)

# Loading the library with cleanup functions
source('utils_1.R')

# Twitter authentication keys
key <- "EkZGfFA14vVloolZqlJfWbklv"
secret <- "jWXFDQREeBvaguVDkHzZ9hQvfm6KJKSB9fOCEXcRnJ1cbmaQ8L"
token <- "1344471742778060806-ydgX37AqF2OvGsun5XbyYgEZP9NCVA"
tokensecret <- "VDjtN6K59RyeB9uBct86A9vjBb9oJO1Nh40qnTiSKygDR"
setup_twitter_oauth(key, secret, token, tokensecret)

## Step 2 - Connecting and capturing tweets

# Checking the user's timeline
userTimeline("dsacademybr")

# Capturing the tweets
tema <- "BigData"
qtd_tweets <- 100
lingua <- "pt"
tweetdata = searchTwitter(tema, n = qtd_tweets, lang = lingua)

# Viewing the first lines of the tweetdata object
head(tweetdata)


## Step 3 - Treatment of data collected through text mining

# Installing the package for Text Mining.
install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
library(tm)
options(warn=-1)

# Treatment (cleaning, organization and transformation) of collected data
# Using utils script with already prepared functions
tweetlist <- sapply(tweetdata, function(x) x$getText())
tweetlist <- iconv(tweetlist, to = "utf-8", sub="")
tweetlist <- limpaTweets(tweetlist)
tweetcorpus <- Corpus(VectorSource(tweetlist))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, stopwords()))

# Converting the Corpus object to plain text
# tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument)
termo_por_documento = as.matrix(TermDocumentMatrix(tweetcorpus), control = list(stopwords = c(stopwords("portuguese"))))

## Step 4 - Wordcloud, association between words and dendrogram
# Installing the wordcloud package
install.packages("RColorBrewer")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)

# Generating a word cloud
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

# Converting the text object to array format
tweettdm <- TermDocumentMatrix(tweetcorpus)
tweettdm

# Finding the words that appear most frequently
findFreqTerms(tweettdm, lowfreq = 11)

# Fetching associations
findAssocs(tweettdm, 'datascience', 0.60)

# Removing sparse terms (not often used)
tweet2tdm <- removeSparseTerms(tweettdm, sparse = 0.9)

# scaling the data
tweet2tdmscale <- scale(tweet2tdm)

# Distance Matrix
tweetdist <- dist(tweet2tdmscale, method = "euclidean")

# Preparing the dendrogram
tweetfit <- hclust(tweetdist)

# Creating the dendrogram (checking how the words are grouped together)
plot(tweetfit)

# Checking the groups
cutree(tweetfit, k = 4)

# Visualizing the word groups in the dendrogram
rect.hclust(tweetfit, k = 3, border = "red")


## Step 5 - Sentiment Analysis

# Creating a function to evaluate sentiment
install.packages("stringr")
install.packages("plyr")
library(stringr)
library(plyr)

sentimento.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  
  # Creating an array of scores with lapply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Mapping the positive and negative words
pos = readLines("palavras_positivas.txt")
neg = readLines("palavras_negativas.txt")

# Creating mass data for testing
teste = c("Big Data is the future", "awesome experience",
          "analytics could not be bad", "learn to use big data")

# Testing the function on our dummy data mass
testesentimento = sentimento.score(teste, pos, neg)
class(testesentimento)

# Checking the score
# 0 - expression has no word in our positive and negative word lists or
# found a negative word and a positive word in the same sentence
# 1 - expression has a word with a positive connotation
# -1 - expression has word with negative connotation
testesentimento$score


## Step 6 - Generating Sentiment Analysis Score

# Tweets by country
catweets = searchTwitter("ca", n = 500, lang = "en")
usatweets = searchTwitter("usa", n = 500, lang = "en")

# Getting text
catxt = sapply(catweets, function(x) x$getText())
usatxt = sapply(usatweets, function(x) x$getText())

# Vector of tweets from countries
paisTweet = c(length(catxt), length(usatxt))

# Joining the texts
paises = c(catxt, usatxt)

# Applying function to calculate sentiment score
scores = sentimento.score(paises, pos, neg, .progress = 'text')

# Calculating the score by country
scores$paises = factor(rep(c("ca", "usa"), paisTweet))
scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)

# Calculating the total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

# Score global
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)
boxplot(score ~ paises, data = scores)

# Generating a histogram with lattice
install.packages("lattice")
library("lattice")
histogram(data = scores, ~score|paises, main = "AnÃ¡lise de Sentimentos", xlab = "", sub = "Score")



## Using Naive Bayes Classifier for sentiment analysis
# https://cran.r-project.org/src/contrib/Archive/Rstem/
# https://cran.r-project.org/src/contrib/Archive/sentiment/

install.packages("Rstem_0.4-1.tar.gz", sep = "", repos = NULL, type = "source")
install.packages("sentiment_0.2.tar.gz",sep = "", repos = NULL, type = "source")
install.packages("ggplot2")
library(Rstem)
library(sentiment)
library(ggplot2)

# Collecting the tweets
tweetpt = searchTwitter("bigdata", n = 1500, lang = "pt")

# Getting the text
tweetpt = sapply(tweetpt, function(x) x$getText())

# Removing special characters
tweetpt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetpt)
# Removing @
tweetpt = gsub("@\\w+", "", tweetpt)
# Removing punctuation
tweetpt = gsub("[[:punct:]]", "", tweetpt)
# Removing digits
tweetpt = gsub("[[:digit:]]", "", tweetpt)
# Removing html links
tweetpt = gsub("http\\w+", "", tweetpt)
# Removing unnecessary spaces
tweetpt = gsub("[ \t]{2,}", "", tweetpt)
tweetpt = gsub("^\\s+|\\s+$", "", tweetpt)

# Creating function for tolower
try.error = function(x)
{
  # Creating missing value
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# Lower case
tweetpt = sapply(tweetpt, try.error)

# Removing the NAs
tweetpt = tweetpt[!is.na(tweetpt)]
names(tweetpt) = NULL

# Sorting emotion
class_emo = classify_emotion(tweetpt, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]

# Replacing NA's with "Neutral"
emotion[is.na(emotion)] = "Neutro"

# Sorting polarity
class_pol = classify_polarity(tweetpt, algorithm = "bayes")
polarity = class_pol[,4]

# Generating a dataframe with the result
sent_df = data.frame(text = tweetpt, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

# Sorting the dataframe
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))


# Emotions found
ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categorias", y = "Numero de Tweets") 

# Polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categorias de Sentimento", y = "Numero de Tweets")











