# Daniel Crawford (dsc160130) and Abhishek Thurlapati (vxt)
#install.packages('sentimentr')
#install.packages('srtweet')
library(rtweet)
library(sentimentr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

#Grab tweets
tweets <- search_tweets(q = "gme", n = 1000, 
                        include_rts = FALSE, lang = "en", type="mixed")


#Cleans text
tweets$strippedtext <- gsub("http.*", "", tweets$text)
tweets$strippedtext <- gsub("https.*", "", tweets$text)

#Get sentiment with sentimentr (tweet level)
sentiment = sentiment_by(tweets$strippedtext)
emotion = emotion_by(tweets$strippedtext)
sentiment
emotion

sort(colMeans(is.na(tweets)))

View(tweets)

#Sentiment analysis
summary(sentiment$ave_sentiment)

tweets$predict_sentiment <- sentiment$ave_sentiment

# Histogram of sentiment values
qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Sentiment Frequencies of Tweets on \"coronavirus\"")

# Verified vs Unverified Sentiment Plot
verified_group = tweets %>% group_by(verified) %>% summarise(avg_sentiment=mean(predict_sentiment), count = n())

ggplot(verified_group, mapping = aes(x=verified, weight=avg_sentiment, fill=count)) + 
  geom_bar() + 
  labs(y = "Sentiment Value", x = "Verified", title = "Comparison of Sentiment Values Between Verified and Unverified Twitter Users")

# Sentiment Values over a short time period
timePeriodSentiment <- tweets %>% mutate(timePeriod = floor_date(created_at, "15minutes")) %>% 
  group_by(timePeriod) %>% 
  summarise(avg_sentiment = mean(predict_sentiment))

ggplot(timePeriodSentiment, mapping = aes(x=timePeriod, y=avg_sentiment)) + 
  geom_line(geom="line") + 
  geom_point() +
  labs(y = "Avg Sentiment Value", x = "Time Period (15 minutes)", title = "Average Sentiment Values Over Time Intervals")


# Sentiment Values based on Emotions of a tweet
emotion_group <- inner_join(emotion, sentiment, by = "element_id") %>% 
                  filter(ave_emotion != 0) %>%
                  group_by(emotion_type) %>%
                  summarise(avg_sentiment = mean(ave_sentiment), count = n()) %>%
                  arrange(avg_sentiment)

ggplot(emotion_group, mapping = aes(x=reorder(emotion_type, avg_sentiment), fill=count, weight=avg_sentiment)) +
          geom_bar() + 
          scale_fill_continuous(low="blue", high="red") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          labs(y = "Sentiment Value", x = "Emotion", title = "Sentiment Values of Tweets on GME Based on Emotion")
