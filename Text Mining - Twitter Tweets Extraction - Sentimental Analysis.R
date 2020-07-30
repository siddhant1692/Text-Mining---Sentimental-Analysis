
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

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(fin_clean)
dtm <- t(tdm)
tdm <- as.matrix(tdm)
# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 20)
w_sub
windows()
barplot(w_sub, las=3, col = rainbow(20))

# Term mcdonalds repeats in all most all documents
fin_clean <- tm_map(fin_clean, removeWords, c('apple','air','can','mcds','macbook','product','windows'))
fin_clean <- tm_map(fin_clean, stripWhitespace)
tdm <- TermDocumentMatrix(fin_clean)
tdm <- as.matrix(tdm)

# Word cloud
install.packages("wordcloud")
library(wordcloud)
windows()
wordcloud(words = names(w_sub), freq = w_sub) # wordcloud with only subset of words

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(20), scale=c(3,1), rot.per = 0.3)

# lOADING +VE AND -VE dictonaries
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w_sub1), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
p_names <- names(freq_pos)
windows()
wordcloud(p_names,freq_pos,scale=c(4,1),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w_sub1), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
n_names <- names(freq_neg)
windows()
wordcloud(n_names,freq_neg,scale=c(5,1),colors = brewer.pal(8,"Dark2"))

#### emotion mining ####
install.packages("syuzhet")
library("syuzhet")
library(lubridate,ggplot2)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

x <- get_nrc_sentiment(fin_txt)
head(x,n=5)

fin_txt[4]
get_nrc_sentiment('happy')
get_nrc_sentiment('boring')

get_sentiment('boring',method="afinn")
get_sentiment('happy',method="afinn")
#each sentences by eight 
example<-get_sentences(fin_txt)
nrc_data<-get_nrc_sentiment(example)


# Bar plot for emotion mining
windows()
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')



sentiment_vector<-get_sentiment(example,method="bing")
sentiment_afinn<-get_sentiment(example,method="afinn")
sentiment_nrc<-get_sentiment(example,method="nrc")

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)

windows()
plot(sentiment_vector,type='l',maim='Plot trajectory',xlab='Narative time',ylab='Emotional valence')
abline(h=0,color='red')

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

##Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.
ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="l", 
  main ="Narendra Modi Tweets using Transformed values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
#Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
positive<-example[which.max(sentiment_vector)]
################################################
my_example_text <- "I begin this story with a neutral statement.  
  Basically this is a very silly test.  
You are testing the Syuzhet package using short, inane sentences.  
I am actually very happy today. 
I have finally finished writing this package.  
Tomorrow I will be very sad. 
I won't have anything left to do. 
I might get angry and decide to do something horrible.  
I might destroy the entire package and start from scratch.  
Then again, I might find it satisfying to have completed my first R package. 
Honestly this use of the Fourier transformation is really quite elegant.  
You might even say it's beautiful!"

#x-axis represents the passage of time from the beginning to the end of the text, and the y-axis measures the degrees of positive and negative sentiment

s_v <- get_sentences(my_example_text)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)



