
install.packages("twitteR")
library("twitteR")

install.packages("ROAuth")
library("ROAuth")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)
### https://apps.twitter.com/

cred <- OAuthFactory$new(consumerKey='yPv2Z0W5ZTuiMVaZdAytqH8H0',
                         consumerSecret='E1UIuJczP1JRM0VFb36Z1BScMcJobPGtzqx9XUBU2nTkwJcIo7',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#save(cred, file="twitter authentication.Rdata")
#load("twitter authentication.Rdata")

setup_twitter_oauth("yPv2Z0W5ZTuiMVaZdAytqH8H0", 
                    "E1UIuJczP1JRM0VFb36Z1BScMcJobPGtzqx9XUBU2nTkwJcIo7",
                    "995572125254782976-VRxUhTLGjBeUn53q0CJTeIhPliDXjnS", # Access token
                    "TPX35FMKDruUOukEaLOZhVE8uxvpBcrZXMnWs8pUwHaW2")  # Access token secret key

Tweets <- userTimeline('narendramodi', n = 1000)
TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "Tweets_namo.csv")
getwd()

word_tweets<- searchTwitter('lockdown', n=1000, lang="en", resultType = "recent")
class(word_tweets)
word_tweets[1:20]

fin_txt<-sapply(Tweets, function(x) x$getText())

str(fin_txt)

#https://apps.twitter.com/ 

library(tm)
fin_corpus<- Corpus(VectorSource(fin_txt))
inspect(fin_corpus[100])


fin_clean<-tm_map(fin_corpus, removePunctuation)
fin_clean<-tm_map(fin_clean, content_transformer(tolower))
fin_clean<-tm_map(fin_clean, removeWords, stopwords("english"))
fin_clean<-tm_map(fin_clean,removeNumbers)
fin_clean<-tm_map(fin_clean, stripWhitespace)

fin_clean<-tm_map(fin_clean, removeWords, c("gameofthrones")) ## clean some words

library(wordcloud)
wordcloud(fin_clean, random.order = F, max.words = 5,colors=rainbow(50))
wordcloud(fin_clean, rot.per=0.5, random.order=TRUE,colors=brewer.pal(8, "Dark2"))




