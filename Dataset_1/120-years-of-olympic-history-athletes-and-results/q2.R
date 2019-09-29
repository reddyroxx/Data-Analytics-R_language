#Q2

#Assumption : Edition = Years
setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/1st/120-years-of-olympic-history-athletes-and-results")
library(dplyr)

dataset <- read.csv("athlete_events.csv",TRUE,",")

k <- subset(dataset,select = c(Name,Year,Medal))
c <- count(k,Name,Year)
pp <- count(c,Name)
x <- subset(pp,pp$nn>1)
print("Players with multiple appearances ")
print(x)



k <- filter(dataset,Medal=="Gold"|Medal=="Silver" | Medal=="Bronze")
c <- count(k,Name,Year)
pp <- count(c,Name)
x <- subset(pp,pp$nn>1)
print("Players who have won at multiple appearances")
print(x)

#3rd part
k <- filter(dataset,Medal=="Gold"|Medal=="Silver" | Medal=="Bronze")
c <- count(k,Name,Year)
x <- subset(c,c$n==max(c$n))
print("Players who have won maximum medals in a single appearance")
print(x)

