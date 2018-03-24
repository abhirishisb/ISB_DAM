# Title : BOT OR NOT.
# written by    Abhishek Rishabh
# written on    March 6 2018
# written at    Latitude:17.435281017° 26' 7.01'' N
#               Longitude:78.340006078° 20' 24.02'' E
# Dataset from Kaggle. 

#--------------------------------------------------------------#
library(text2vec)
library(data.table)
library(stringr)
library(tm)
#library(RWeka)
library(tokenizers)
#library(slam)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(data.table)
library(magrittr)
library(ggplot2)
# get data

  testDataBotOrNot=training_data_2_csv_UTF
  testDataBotOrNot$BOT_OR_NOT=ifelse(testDataBotOrNot$bot==1,"BOT","NOT")
  summary(testDataBotOrNot)
  attach(testDataBotOrNot)

#--------------------------------------------------------------#

# Descriptive Statistics

library(stargazer)
  onlybotData = as.data.frame(testDataBotOrNot[which(bot == 1), ])
  nonbotData = as.data.frame(testDataBotOrNot[which(bot == 0), ])
  options(digits = 1)
  stargazer(
    as.data.frame(testDataBotOrNot),
    title = "Descriptive Statistics of Complete Dataset",
    summary = TRUE,
    type = "html"
  )
  stargazer(onlybotData,
          title = "Descriptive Statistics of Only Bot Dataset",
          summary = TRUE,
          type = "html")
  stargazer(nonbotData,
          title = "Descriptive Statistics of Non Bot Dataset",
          summary = TRUE,
          type = "html")

#--------------------------------------------------------------#

#Plot and compare histograms of bots and non-bots 
  
  
library(ggplot2)
# Favorite Count ->
    favortie_count_plot <- ggplot(testDataBotOrNot, aes( favourites_count, colour = BOT_OR_NOT)) +
    geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Favorite Count Density Comparison") +
    xlim(c(0, 20000))+ylim(c(0,300))+theme_classic()
  favortie_count_plot
    
    #followers_count	friends_count	listed_count
    followers_count_plot <- ggplot(testDataBotOrNot, aes( followers_count, colour = BOT_OR_NOT)) +
      geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Follower's Count Density Comparison") +
      xlim(c(0, 20000))+ylim(c(0,250))
    followers_count_plot
    
    friends_count_plot <- ggplot(testDataBotOrNot, aes( friends_count, colour = BOT_OR_NOT)) +
      geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Friends's Count Density Comparison") +
      xlim(c(0, 20000))+ylim(c(0,250))
    friends_count_plot
    
    status_count_plot <- ggplot(testDataBotOrNot, aes( statuses_count, colour = BOT_OR_NOT)) +
      geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Status Count Density Comparison") +
      xlim(c(0, 20000))+ylim(c(0,250))
    status_count_plot
    
    listed_count_plot <- ggplot(testDataBotOrNot, aes( listed_count, colour = BOT_OR_NOT)) +
      geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 500)+ggtitle("Listed Count Density Comparison") +
      xlim(c(0, 20000))+ylim(c(0,150))
    listed_count_plot
#--------------------------------------------------------------#

# ANOVA - see significant difference between group 
    BOT_OR_NOT <- ordered(BOT_OR_NOT,levels = c("BOT", "NOT"))
    levels(BOT_OR_NOT)
    res.aov.favorites <- aov(favourites_count ~ BOT_OR_NOT)
    TukeyHSD(res.aov.favorites)
    res.aov.followers <- aov(followers_count ~ BOT_OR_NOT)
    TukeyHSD(res.aov.favorites)
    res.aov.friends <- aov(friends_count ~ BOT_OR_NOT)
    TukeyHSD(res.aov.friends)
    res.aov.listed <- aov(listed_count ~ BOT_OR_NOT)
    TukeyHSD(res.aov.listed)
    res.aov.statuses <- aov(statuses_count ~ BOT_OR_NOT)
    TukeyHSD(res.aov.statuses)
    res.aov.profile <- aov(default_profile ~ BOT_OR_NOT)
    TukeyHSD(res.aov.profile)
    res.aov.profile.image <- aov(default_profile_image ~ BOT_OR_NOT)
    TukeyHSD(res.aov.profile.image)
    res.aov.extended.profile <- aov(has_extended_profile ~ BOT_OR_NOT)
    TukeyHSD(res.aov.extended.profile)
    res.aov.verified <- aov(verified ~ BOT_OR_NOT)
    TukeyHSD(res.aov.verified)
    
#--------------------------------------------------------------#

# Model Building
    #Naive Bayes
    #Logistic Regression
    # Random Forest
    library(naivebayes)
    # select features (independent variables)
    ind.Var.naive.bayes=as.data.frame(testDataBotOrNot[,c("followers_count","friends_count","listed_count","favourites_count","verified","statuses_count","default_profile","default_profile_image","has_extended_profile","bot")])
    # select dependent variable
    dep.Var.naive.bayes=as.data.frame(testDataBotOrNot[,c("BOT_OR_NOT")])
    # run naive bayes classifier
    
    naive.bayes.dataset=cbind(ind.Var.naive.bayes,dep.Var.naive.bayes)
    naive.bayes.results=naive_bayes(BOT_OR_NOT~.,data=naive.bayes.dataset)
    naive.bayes.predict <- predict(naive.bayes.results, mergedData_starbucks, type="class")
    #table(naive.bayes.predict, testDataBotOrNot$BOT_OR_NOT,dnn=c("Prediction","Actual"))
    mergedData_starbucks$predicted=cbind(mergedData_starbucks,naive.bayes.predict)
  tweetd1=as.data.frame(mergedData_starbucks$text)
    
