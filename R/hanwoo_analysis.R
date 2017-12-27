#data analysis ###############
library(ggplot2)
library(plotly)
library(dplyr)
library(KoNLP)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)

df <- read.csv("example.txt")
df_steer <- filter(df, SexNm=="거세")
df_cow <- filter(df, SexNm=="암")

### 농가분포####
wc <- Corpus(VectorSource(df$farmerNm))
wc_data<-tm_map(wc,stripWhitespace)
wc_data<-tm_map(wc_data,removeNumbers)
wc_data<-tm_map(wc_data, removePunctuation)
tdm_wc<-TermDocumentMatrix(wc_data) #Creates a TDM
TDM1<-as.matrix(tdm_wc) #Convert this into a matrix format
v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word

wordcloud(wc_data, max.words = Inf, min.freq = 1, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2")) 

farmer <- group_by(df_steer, farmerNm) %>% summarise(출하두수=n())
colnames(farmer) <- c("농가명","출하두수")
farmer[order(-farmer$출하두수),] %>% head(10)


### 성별 분포#####
a <-ggplot(df, aes(SexNm)) + geom_bar(width=0.5) + labs(x="Sex", y="두수") + scale_x_discrete(limits=c("거세","암"))
ggplotly(a)

ggplot(df, aes(x=month,y=weight))+geom_point()

### KPN 분석 ####
kpn <- group_by(df_steer, kpn) %>% summarise(n=n(),marbling=mean(근내지방),weight=mean(weight),three=prop(qgrade,5),two=prop(qgrade,4),one=prop(qgrade,1),one_plus=prop(qgrade,2),two_plus=prop(qgrade,3), A=prop(wgrade,1),B=prop(wgrade,2),C=prop(wgrade,3),month=mean(month))
kpn <- mutate(kpn, high_quality=(one_plus+two_plus)) ##add high_quality(>1+)
kpn <- mutate(kpn, AMG=(weight/month)) ##add high_quality(>1+)

kpn <- filter(kpn, n > 5)
kpn[order(-kpn$marbling),] %>% head(10)

### 농가별 육량/육질등급 분포 ####
prop <- function(x,y){
  p <- prop.table(table(x))*100
  p <- round(p, 1)
  return(p[y])
}

df <- filter(df, is.na(wgrade)==FALSE)

#wgrade
wgrade <- group_by(df_steer, farmerNm) %>% summarise(n=n(),A=prop(wgrade,1),B=prop(wgrade,2),C=prop(wgrade,3),month=mean(month))
wgrade$C[is.na(wgrade$C)] <- 0 #NA -> 0

wgrade <- filter(wgrade, n > 12)

wgrade[order(wgrade$C),] #C 등급 적은
filter(wgrade, is.na(C)==TRUE)

wgrade[order(-wgrade$C),] #C 등급 많은
wgrade[order(-wgrade$A),] #A 등급 많은

#grade
qgrade <- group_by(df_steer, farmerNm) %>% summarise(n=n(),three=prop(qgrade,5),two=prop(qgrade,4),one=prop(qgrade,1),one_plus=prop(qgrade,2),two_plus=prop(qgrade,3))

qgrade <- mutate(qgrade, high_quality=(one_plus+two_plus)) ##add high_quality(>1+)

qgrade <- filter(qgrade, n > 12)

qgrade[order(-qgrade$two_plus),] #1++ 등급 많은
qgrade[order(-qgrade$high_quality),] #고급육 출현 많은

#overall
grade <- group_by(df_steer, farmerNm) %>% summarise(n=n(),three=prop(qgrade,5),two=prop(qgrade,4),one=prop(qgrade,1),one_plus=prop(qgrade,2),two_plus=prop(qgrade,3), A=prop(wgrade,1),B=prop(wgrade,2),C=prop(wgrade,3),month=mean(month))
grade <- mutate(grade, high_quality=(one_plus+two_plus)) ##add high_quality(>1+)

grade <- filter(grade, n > 12)

grade$A[is.na(grade$A)] <- 0 #NA -> 0
grade$B[is.na(grade$B)] <- 0 #NA -> 0
grade$C[is.na(grade$C)] <- 0 #NA -> 0
