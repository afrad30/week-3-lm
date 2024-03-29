library("datasets")

data("ChickWeight")

library("tidyverse")

datack=as_tibble(ChickWeight)

datack1 = as.integer(datack$Chick)
datack2 = as.double(datack$Chick)
datack3 = as.character(datack$Chick)
datack4 = as.factor(datack$Chick)
datack5 = as.ordered(datack$Chick)
datack
class(datack)
max(datack$weight)
mean(datack$weight)
median(datack$weight)
sd(datack$weight)#standard deviation
summary(datack$weight)

sort(datack$weight)
rev(datack$weight)
table(datack$weight)
unique(datack$Chick)
unique(datack$Diet)

glimpse(datack)
summary(datack)

seq(1, 10)
seq(1,20, by=2)

rep(1,4)
rep("s",11)



subtract_and_divide<- function(x,y){
  x-y
  x/y
}

subtract_and_divide(4,8)

x <- 5
class(x)  # Output will be "numeric"

y <- c(1, 2, 3)
class(y)

v1= c(1,2,3,4,5,5,6)
v2= c(2,2,2,2,2,2,2)

data.frame(v1,v2)
data= data.frame("number"=v1, "categ"=as.factor(v2))
data$car=c('s','t','t','g','h','b','j')
data
data$same = as.factor(100)
data
as_tibble(data)


dim(data)
str(data)
nrow(data)
ncol(data)
names(data)
view(data)


save <- read.csv("C:/Users/afrad/Downloads/cces.csv")
View(save)

getwd()
setwd("D:/BAYESIAN STATISTICS/CODES/week-3-lm")
getwd()







