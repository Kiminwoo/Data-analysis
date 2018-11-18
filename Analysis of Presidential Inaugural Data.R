
install.packages("KoNLP")

install.packages("RColorBrewer")

install.packages("wordcloud")


library(KoNLP)
library(RColorBrewer)
library(wordcloud)

useSejongDic


pal2 <- brewer.pal(8,"Dark2")

pal2

text <- readLines(file.choose())


noun <- sapply(text,extractNoun,USE.NAMES = F)


noun2 <- unlist(noun)


word_count <- table(noun2)

word_count
wordcloud(names(word_count),freq = word_count,scale = c(6,0.3),min.freq = 3, random.order = F , rot.per = .1 , colors = pal2)


rm(list=ls())