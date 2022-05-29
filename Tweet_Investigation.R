#author: John McGrath
#date: 5/29/2022
#project: Twitter Sentiment Analysis
#description: Identify prevailing sentiment from 750K random tweets made over a several week period
#Ouput ggplot bars showing disparity between positive/negative words in tweets

install.packages("tidytext")

library(tidyverse)
library(tidytext)

#Import the data
twitter_data <- read_csv("twitter_sentiment.csv")
View(twitter_data)

#Pre-Process the data
tidy_twitter <- twitter_data %>% 
  unnest_tokens(word,tweet) %>% 
  
  #Get rid of filler words
  anti_join(stop_words)

#kick out more weird words

tidy_twitter <- subset(tidy_twitter, !(word %in% c('http', 'quot','1','2','3','4','5','6','twitpic.com')))
#Get the most common words

common_tweet_words <- tidy_twitter %>% 
  count(word, sort = TRUE)

View(common_tweet_words)

#Take the top 100 most commonly appearing words

top_100_words <- common_tweet_words %>%                                      # Top N highest values by group
  arrange(desc(n)) %>% 
  slice(1:100)

View(top_100_words)

sentiments <- top_100_words %>% 
              inner_join(get_sentiments("bing"))

#plot different sentiments among the top 100

sentiments %>% 
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y = "Contribution to sentiment",
       x = NULL)+
  coord_flip()





