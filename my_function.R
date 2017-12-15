#My functions

#갯수 카운팅 해서 표로 만들기 using ggplot2
yn_count <- function(df,var,xlab,ylab="count",width=0.5,color="#04B486"){
  require(ggplot2)
  require(dplyr)
  reorder <- reorder(var, table(var)[var])
  ggplot(df, aes(x=reorder)) + geom_bar(width=width,fill=color) + coord_flip() + labs(x=xlab, y=ylab)
}
#yn_count(iris,iris$Species,"species") #test code

#
na.rm <- function(df,var){
  require(dplyr)
  filter(df, is.na(var)==FALSE)
}

library(MASS)
narm(Cars93,Luggage.room)


