require(wordcloud)
d <- read.csv("Most_Frequent_Words.csv")

colorVec <- rgb(runif(50,0.2,0.8),runif(50,0.2,0.8),runif(50,0.2,0.8))
wordcloud(d$Terms,d$Frequency,colors = colorVec,scale = c(2.5,0.5))


