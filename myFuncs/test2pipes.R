#get Data

rm(list=ls())    # clear workspace
try(require(text2vec) || install.packages("text2vec"))
try(require(data.table) || install.packages("data.table"))
try(require(stringr) || install.packages("stringr"))
try(require(tm) || install.packages("tm"))
try(require(RWeka) || install.packages("RWeka"))
try(require(tokenizers) || install.packages("tokenizers"))
try(require(slam) || install.packages("slam"))
try(require(wordcloud) || install.packages("wordcloud"))
try(require(ggplot2) || install.packages("ggplot2"))

library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(data.table)
library(magrittr)
library(ggplot2)
# get Data
temp.text = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/International%20Business%20Machines%20(IBM)%20Q3%202016%20Results%20-%20Earnings%20Call%20Transcript.txt')  #IBM Q3 2016 analyst call transcript
userstopwords=c("phone","nokia")

source("text.clean.R") #clean 
source("build_dtm_tcm.R") #dtm
source("build_charts.R") #build charts
source("build_wordcloud.R") # build charts - wordcloud
source("build_barchart.R") # build charts - barchart
source("distill.cog.R") # build charts - distill COG

thedata <- list(temp.text,userstopwords)  %>% with(text.clean(temp.text,userstopwords)) %>%
 build_dtm_tcm %>% build_charts

temp.text1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/Ice-cream-dataset.txt')  #IBM Q3 2016 analyst call transcript
userstopwords=c("phone","nokia")

thedata1 <- list(temp.text1,userstopwords)  %>% with(text.clean(temp.text1,userstopwords)) %>%
  build_dtm_tcm %>% build_charts
