# Auxiliary functions

# Function to clean up tweets
limpaTweets <- function(tweet){
  # Remove http links
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("http\\w+", "", tweet)
  # Remove retweets
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Remove â#Hashtagâ
  tweet = gsub("#\\w+", " ", tweet)
  # Remove usernames â€œ@peopleâ€
  tweet = gsub("@\\w+", " ", tweet)
  # Remove punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Remove the numbers
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Remove unnecessary spaces
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Converting character encoding and converting to lowercase
  tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
  tweet <- tryTolower(tweet)
  tweet <- iconv(tweet, from = "UTF-8", to = "ASCII")
}

# Function for cleaning Corpus
limpaCorpus <- function(myCorpus){
  library(tm)
  myCorpus <- tm_map(myCorpus, tolower)
  # Remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # Remove the numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
}

# Convert to lowercase
tryTolower = function(x)
{
  # Create a missing data (NA)
  y = NA
  # handle the error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if there is no error, make it lowercase
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # Return the result
  return(y)
}


