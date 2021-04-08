#install.packages('sentimentr')
#install.packages('srtweet')
library(rtweet)
library(sentimentr)
library(ggplot2)

#Grab tweets
tweets <- search_tweets(q = "#corona", n = 500, 
                        include_rts = FALSE, lang = "en")


#Cleans text
tweets$strippedtext <- gsub("http.*", "", tweets$text)
tweets$strippedtext <- gsub("https.*", "", tweets$text)

#Get sentiment with sentimentr (tweet level)
sentiment = sentiment_by(tweets$strippedtext)
sentiment

View(tweets)

#Sentiment analysis
summary(sentiment$ave_sentiment)

qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Sentiment Frequencies of Tweets on Corona")

#Can do overall level sentiment if i join all the tweets together of a certain time i guess
#Or can do sentiment of tweets at a certain time and find the avg sentiment of that time period. 
#We need to explore the rtweets and sentimentr package to see if there are other stuff we can analyze
#We can keep filtering groups from the tweets df and find the avg sentiments and display those as our 