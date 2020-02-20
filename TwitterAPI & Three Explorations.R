# Data collection
# 0 package preperation
if (!require("rtweet")) install.packages("rtweet")
if (!require("tm")) install.packages("tm")
if (!require("igraph")) install.packages("igraph")

library('rtweet')
library("tm")
library("igraph")

# create token to get access to Twitter API
token <- create_token(app = "app",
                      consumer_key = "cosumer_key",
                      consumer_secret = "consumer_secret",
                      access_token = "access_token",
                      access_secret = "access_secret",
                      set_renv = TRUE)


help(search_tweets) #check the document of this function and its parameters

# search Tweets by search query(keywords): search_tweets() function
Tweets_data <- search_tweets("#HIV", n = 1000 ,include_rts = TRUE, retryonratelimit = TRUE,lang = "en")

data.frame(Tweets_data)

write_as_csv(Tweets_data, "#HIV_data.csv", prepend=TRUE,na="",fileEncoding = "UTF-8")

# -------------------------------------------
# Exploration
# Exploration 1: trend of discussion

ts_plot(Tweets_data, by = "2 hours") #plot the frequency of tweets over a specified interval of time.


# Questions: In some days, HIV is disucssed more frequently than other days, is there any story?

# -------------------------------------------
# Exploration 2: frequent words

mt.v <- VectorSource(Tweets_data$text)
mt.c <- SimpleCorpus(mt.v)
inspect(mt.c)

mt.c.p <- tm_map(mt.c, content_transformer(tolower))
mt.c.p <- tm_map(mt.c.p, removeNumbers)
mt.c.p <- tm_map(mt.c.p, removeWords, stopwords("english"))
mt.c.p <- tm_map(mt.c.p, removePunctuation)
mt.c.p <- tm_map(mt.c.p, stripWhitespace)

inspect(mt.c.p)

dtm <- TermDocumentMatrix(mt.c.p)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 50)

write_as_csv(d,"#HIV_fre_words.csv",prepend=TRUE,na="",fileEncoding = "UTF-8")

library(wordcloud2)
set.seed(1234)

wordcloud2(d ,size=2,color='random-light')

# Questions: what are the most frequent words? what are the meanings of these words? 

# ------------------------------------------------------------------------
# Exploration 3: active users 
mt2.v <- VectorSource(Tweets_data$user_id)
mt2.c <- SimpleCorpus(mt2.v)
inspect(mt2.c)

mt2.c.p <- tm_map(mt2.c, content_transformer(tolower))
mt2.c.p <- tm_map(mt2.c.p, removeWords, stopwords("english"))
mt2.c.p <- tm_map(mt2.c.p, removePunctuation)
mt2.c.p <- tm_map(mt2.c.p, stripWhitespace)
inspect(mt2.c.p)

dtm2 <- TermDocumentMatrix(mt2.c.p)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)

d2 <- data.frame(name = names(v2),freq=v2)
active <- head(d2,10)
active_user <- lookup_users(active$name)
active_name <- active_user$screen_name
active_name

write_as_csv(active_user,"#HIV_activeuser.csv",prepend=TRUE,na="",fileEncoding = "UTF-8")
# Question: Who are they? why they tweet so frequently? what are their tweets talking about?
