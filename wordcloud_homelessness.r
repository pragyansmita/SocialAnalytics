#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("tm")

library("twitteR")
library("wordcloud")
library("tm")

#necessary file for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

# to get your consumerKey and consumerSecret see the twitteR documentation for instructions
# https://apps.twitter.com/app/12468441/keys
consumer_key <- 'c2tNsuCoTOKEYODzs8eSEaUgh'
consumer_secret <- 'XuFZtszml3rLixJ7Hke8u6kcAt5Jw060It4Paw0NbCrkOzSUnk'
access_token <- '528020973-zC9vkOaUMfIkFolHkw1qwOhnJEPmwA7vkksToBjB'
access_secret <- 'sjkwX2lEH961FNFnc11lWxWWSTJotJd8DyX33FGqUz9K6'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)
			
tryTolower = function(x)
{
   # create missing value
   # this is where the returned value will be
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error = function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
      y = tolower(x)
   return(y)
}
 
			
#the cainfo parameter is necessary only on Windows
search_word <- "VeteranHomelessness"
r_stats <- searchTwitter(search_word, n=1500)
#should get 1500
length(r_stats)
#[1] 1500

#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
r_stats_text <- sapply(r_stats_text, function(x) tryTolower(x))

#create corpus
# r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
# #clean up
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
# wordcloud(r_stats_text_corpus)					


# library(RColorBrewer)
# pal2 <- brewer.pal(8,"Dark2")
# wordcloud(r_stats_text_corpus,min.freq=2,max.words=100, random.order=T, colors=pal2)


# Now we will prepare the above text for sentiment analysis
# First we will remove retweet entities from the stored tweets (text)
r_stats_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", r_stats_text)
# Then remove all "@people"
r_stats_text = gsub("@\\w+", "", r_stats_text)
# Then remove all the punctuation
r_stats_text = gsub("[[:punct:]]", "", r_stats_text)
# Then remove numbers, we need only text for analytics
r_stats_text = gsub("[[:digit:]]", "", r_stats_text)
# the remove html links, which are not required for sentiment analysis
r_stats_text = gsub("http\\w+", "", r_stats_text)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
r_stats_text = gsub("[ \t]{2,}", "", r_stats_text)
r_stats_text = gsub("^\\s+|\\s+$", "", r_stats_text)

# if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
#clean up


#r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(r_stats_text_corpus,min.freq=2,max.words=100, random.order=T, colors=pal2)
