build_barchart = function(dtm){
tsum = apply(dtm, 2, sum) 
tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
test = as.data.frame(round(tsum[1:15],0))

barplot_horiz = ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red")+ 
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(barplot_horiz)

}