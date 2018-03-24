#Load Libraries
library("twitteR")
library("openssl")
library("httpuv")
library("twitteR")
library("tm")
library("stringr")
library("dplyr")

library(httr)

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("bot_trolls_impact",
                   key = "bfXW5dpxo5C7gdJBwK8kQWx6U",
                   secret = "X6f89oUF2WnjRbrR3QUf5G8XpG3UpviyLNoUYknS79J5Z5zHam"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))
stop_for_status(req)
#content(req)

#Save your Twitter account Keys and Tokens. If you do not have a twitter account. https://apps.twitter.com/

consumer_key<-"bfXW5dpxo5C7gdJBwK8kQWx6U"
consumer_secret<-"X6f89oUF2WnjRbrR3QUf5G8XpG3UpviyLNoUYknS79J5Z5zHam"
access_token <- "51416190-96EMvxfML5p6lZm3XUc5ma2gM3e3pmuHED4WNFlLG"
access_secret <- "CqauQctg5zXl1XQUiOeAiHnpQMgQNFsduwtlF5yNpYp2O"

#Connects you to Twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token , access_secret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

twitter_token <- create_token(
  app = "bot_trolls_impact",
  consumer_key = "bfXW5dpxo5C7gdJBwK8kQWx6U",
  consumer_secret = "X6f89oUF2WnjRbrR3QUf5G8XpG3UpviyLNoUYknS79J5Z5zHam")

library(rtweet)
starbucks_tweet <- search_tweets(
  "@Starbucks", n = 50000, retryonratelimit = TRUE)


## lookup data on those accounts
starbucks_flw_data <- lookup_users(starbucks_tweet$user_id)
starbucks_flw_data=starbucks_flw_data[!duplicated(starbucks_flw_data$user_id), ]


mergedData_starbucks=as.data.frame(merge(starbucks_tweet,starbucks_flw_data,by = "user_id"))
write.csv(mergedData_starbucks,"starbucks.csv")
library(stargazer)
write(stargazer(mergedData_starbucks,type = "html",digits = 1,float.env = "sidewaystable", title  = "Descriptive Statistics Starbucks Data"),"tb3.html")


#--------------------------------------------------------------#

#Plot and compare histograms of bots and non-bots 


library(ggplot2)
# Favorite Count ->
favortie_count_plot <- ggplot(mergedData_starbucks, aes( favourites_count)) +
  geom_density(alpha = 0) +      geom_freqpoly(binwidth = 500)+ggtitle("Favorite Count Density") +
  xlim(c(0, 50000))+ylim(c(0,2500))
favortie_count_plot

#followers_count	friends_count	listed_count
followers_count_plot <- ggplot(testDataBotOrNot, aes( followers_count)) +
  geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Follower's Count Density ") +
  xlim(c(0, 5000))+ylim(c(0,1500))
followers_count_plot

friends_count_plot <- ggplot(testDataBotOrNot, aes( friends_count)) +
  geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Friends's Count Density ") +
  xlim(c(0, 5000))+ylim(c(0,700))
friends_count_plot

status_count_plot <- ggplot(testDataBotOrNot, aes( statuses_count)) +
  geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Status Count Density ") +
  xlim(c(0, 10000))+ylim(c(0,1000))
status_count_plot

listed_count_plot <- ggplot(testDataBotOrNot, aes( listed_count)) +
  geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Listed Count Density ") +
  xlim(c(0, 10000))+ylim(c(0,200))
listed_count_plot
#--------------------------------------------------------------#


