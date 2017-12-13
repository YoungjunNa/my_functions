#My functions
yn_count <- function(df,var,xlab,ylab="count",width=0.5,color="#04B486"){
  require(ggplot2)
  require(dplyr)
  reorder <- reorder(var, table(var)[var])
  ggplot(df, aes(x=reorder)) + geom_bar(width=width,fill=color) + coord_flip() + labs(x=xlab, y=ylab)
}

#yn_count(iris,iris$Species,"species")

na_rm <- function(df,x){
  require(dplyr)
  filter(df, is.na(x)==FALSE)
}