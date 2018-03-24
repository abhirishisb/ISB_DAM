
mergedData_starbucks$sentiments <- get_sentiment(mergedData_starbucks$text) %>% as.numeric()

 ggplot(mergedData_starbucks, aes(mergedData_starbucks$sentiments))+  geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 1)+xlab("Sentiments")
 +ggtitle("Overall Sentiment Distribution")

sentiment.of.tweets.botornot <- ggplot(mergedData_starbucks, aes(mergedData_starbucks$sentiments, colour = mergedData_starbucks$predicted$naive.bayes.predict)) +
  geom_density(alpha = 0.5) +      geom_freqpoly(binwidth = 1)+ggtitle("Tweet Sentiment Comparison of Bots vs Humans") +
  geom_vline(data=mergedData_starbucks, aes(xintercept=mean(mergedData_starbucks$sentiments),  colour=mergedData_starbucks$predicted$naive.bayes.predict),
linetype="dashed", size=0.5)+xlab("Sentiments") + labs(color='Predicted Bot or Not') 

sentiment.of.tweets.botornot


ggplot(mergedData_starbucks, aes(mergedData_starbucks$predicted$naive.bayes.predict, mergedData_starbucks$sentiments)) +
  geom_boxplot()