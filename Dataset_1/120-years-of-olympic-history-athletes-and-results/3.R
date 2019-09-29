setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/1st/120-years-of-olympic-history-athletes-and-results")

library(dplyr)

#dataset <- data.frame(read.csv("/home/metri/Desktop/DA/athlete_events.csv"))

team <- group_by(dataset, Team, Sex)
male_team <- summarize(subset(team, Sex == "M"), n=n())$n  #contains no_of_males per team arranged in asc order of teams
female_team <- summarize(subset(team, Sex == "F"), n=n())$n


getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(mean(male_team), na.rm =True)
print(median(male_team))
print(getmode(male_team))
print(IQR(male_team))
print(sd(male_team))
print(quantile(male_team, 0.9))

print(mean(female_team), na.rm =True)
print(median(female_team))
print(getmode(female_team))
print(IQR(female_team))
print(sd(female_team))
print(quantile(female_team, 0.9))

png(file = "boxplot.png")
boxplot(x,data=male_team, xlab="No Of Males", ylab="Number", main="Male")
dev.off()