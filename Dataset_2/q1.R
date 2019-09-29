setwd("/home/pruthvi/Desktop/Sem5/Data Analytics/Assignments/2nd")


library(dplyr)
library(splitstackshape)
library(moments)
library(sampling)
library(psych)
library(ggcorrplot)
dataset <- read.csv("kc_house_data.csv",TRUE,",")

#random Sampling

df_random <- dataset %>% 
  filter(id %in% sample(unique(id),0.75*dim(dataset)[1]))




#systematic sampling
k <- as.integer(seq(from=1, to=nrow(dataset), length.out = nrow(dataset)/4))
df_systematic <-dataset[k,]




#clustered sampling

#df_cluster <- cluster(dataset,c("bedrooms"), size = 0.6*length(unique(dataset$bedrooms)))

bdrooms <- sort(unique(dataset$bedrooms))
p <- quantile(x = bdrooms,probs = .6)
bdrooms <- bdrooms[bdrooms<p]
df <- subset(dataset,dataset$bedrooms %in% bdrooms)
df_cluster <- cluster(df,c("bedrooms"), size = 5,method = "srswor")
df_cluster <- getdata(df,df_cluster)

#df_cluster <- cluster(dataset)



#stratified sampling
df_stratified <- stratified(dataset, c("floors"), 0.7)



boxplot(dataset$price,df_random$price,df_systematic$price,df_cluster$price,df_stratified$price,names =  c("Population","Random","Systematic","Cluster","Stratified"),ylab = "Prices" , main = "Comparing Population with Sampling models")

pop <- c(summary(dataset$price)["Mean"],summary(dataset$price)["Median"],"StDev" = sd(dataset$price),"Skew" = skewness(dataset$price),"Kurtosis" = kurtosis(dataset$price))
ran <- c(summary(df_random$price)["Mean"],summary(dataset$price)["Median"],"StDev" = sd(df_random$price),"Skew" = skewness(df_random$price),"Kurtosis" = kurtosis(df_random$price))
sys <- c(summary(df_systematic$price)["Mean"],summary(df_systematic$price)["Median"],"StDev" = sd(df_systematic$price),"Skew" = skewness(df_systematic$price),"Kurtosis" = kurtosis(df_systematic$price))
clu <- c(summary(df_cluster$price)["Mean"],summary(df_cluster$price)["Median"],"StDev" = sd(df_cluster$price),"Skew" = skewness(df_cluster$price),"Kurtosis" = kurtosis(df_cluster$price))
stf <- c(summary(df_stratified$price)["Mean"],summary(df_stratified$price)["Median"],"StDev" = sd(df_stratified$price),"Skew" = skewness(df_stratified$price),"Kurtosis" = kurtosis(df_stratified$price))

#sampling errors
print("Sampling errors")
print(pop - ran)
print(pop-sys)
print(pop - clu)
print(pop - stf)


#Q2

#a)

gr <- dataset$grade
#nr_gr <- lapply(gr, function(x) (x-mean(gr))/sd(gr))
nr_gr <- lapply(gr,function(x) (x-min(gr))/(max(gr)-min(gr)))
nr_gr <- unlist(nr_gr, use.names=FALSE)

#b)


normalization <- function(l){
  l <- (l-mean(l))/sd(l)
  
  l
}

n_prices <- normalization(dataset$price)
n_bdrooms <- normalization(dataset$bedrooms)
n_sq_liv <- normalization(dataset$sqft_living)
n_sq_lo <- normalization(dataset$sqft_lot)
n_grades <- normalization(dataset$grade)
n_sq_ab <- normalization(dataset$sqft_above)
n_sq_ba <- normalization(dataset$sqft_basement)

#n_prices <- unlist(lapply(prices, function(x) (x-mean(prices))/sd(prices)),use.names = FALSE)
#n_bdrooms <- unlist(lapply(bdrooms, function(x) (x-mean(bdrooms))/sd(bdrooms)),use.names = FALSE)
#n_sq_liv <- unlist(lapply(sq_liv, function(x) (x-mean(sq_liv))/sd(sq_liv)),use.names = FALSE)
#n_sq_lo <- unlist(lapply(sq_lo, function(x) (x-mean(sq_lo))/sd(sq_lo)),use.names = FALSE)
#n_grades <- unlist(lapply(grades, function(x) (x-mean(grades))/sd(grades)),use.names = FALSE)
#n_sq_ab <- unlist(lapply(sq_ab, function(x) (x-mean(sq_ab))/sd(sq_ab)),use.names = FALSE)
#n_sq_ba <- unlist(lapply(sq_ba, function(x) (x-mean(sq_ba))/sd(sq_ba)),use.names = FALSE)





df <- data.frame(n_prices,n_bdrooms,n_sq_liv,n_sq_lo,n_grades,n_sq_ab,n_sq_ba)

par(mfrow=c(1,2))
hist(df$n_prices,xlab = "Normalized Prices",main = "Price")
boxplot(df$n_prices,xlab = "Normalized Prices",main = "Price")

hist(df$n_bdrooms,xlab = "Normalized number of bedrooms",main = "Bedrooms")
boxplot(df$n_bdrooms,xlab = "Normalized number of bedrooms",main = "Bedrooms")

hist(df$n_sq_liv,xlab = "Normalized Living area in sqft",main = "Living Area in sqft")
boxplot(df$n_sq_liv,xlab = "Normalized Living area in sqft",main = "Living Area in sqft")

hist(df$n_sq_lo,xlab = "Normalized Lot area in sqft",main = "Lot area in sqft")
boxplot(df$n_sq_lo,xlab = "Normalized Lot area in sqft",main = "Lot area in sqft")

hist(df$n_grades,xlab = "Normalized Grades",main = "Grade")
boxplot(df$n_grades,xlab = "Normalized Grades",main = "Grade")


hist(df$n_sq_ab,xlab = "Normalized Above area in sqft",main = "Above area in sqft")
boxplot(df$n_sq_ab,xlab = "Normalized Above area in sqft",main = "Above area in sqft")

hist(df$n_sq_ba,xlab = "Normalized Basement area in sqft",main = "Basement area in sqft")
boxplot(df$n_sq_ba,xlab = "Normalized Basement area in sqft",main = "Basement area in sqft")

par(mfrow = c(1,1))

#c)
sk <- skewness(df)
kr <- kurtosis(df)


#d)

euc_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

m_sq_liv <- mean(dataset$sqft_living)
m_sq_lo <- mean(dataset$sqft_lot)
m_sq_ab <- mean(dataset$sqft_above)

m_n_sq_liv <- mean(df$n_sq_liv)
m_n_sq_lo <- mean(df$n_sq_lo)
m_n_sq_ab <- mean(df$n_sq_ab)

liv_lo_cmp <- euc_dist(m_sq_liv,m_sq_lo)
liv_ab_cmp <- euc_dist(m_sq_liv,m_sq_ab)
ab_lo_cmp <- euc_dist(m_sq_ab,m_sq_lo)

n_liv_lo_cmp <- euc_dist(m_n_sq_liv,m_n_sq_lo)
n_liv_ab_cmp <- euc_dist(m_n_sq_liv,m_n_sq_ab)
n_ab_lo_cmp <- euc_dist(m_n_sq_ab,m_n_sq_lo)




#Q3

#a)
df <- dataset
df$id <- NULL
df$date <- NULL
df$zipcode <- NULL

n_df<- apply(df,2,normalization)

k <- cor(n_df)
ggcorrplot(k)

#b)

yy <- colnames(df)
s_devs <- data.frame("features" = yy,"devs" = apply(df, 2, sd))
s_devs <- arrange(s_devs,devs)

low_std_dev <- head(s_devs,n=5)

print("Attributes with lowest standard deviations")
print(low_std_dev[,1])





#c)


#pc <- principal(k,nfactors = 2)
data <- n_df[, unlist(lapply(df, is.numeric))]
pca <- prcomp(data, center=TRUE, scale=TRUE)
u <- unlist(pca[2])
pca1 <- list(u[c(1:18)])
pca2 <- list(u[c(19:36)])

std_dev <- pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

original <- t(t(pca$x %*% t(pca$rotation)) * pca$scale + pca$center)

#fviz_eig(pca)

pca_gen <- t(t(pca$x[,1:2] %*% t(pca$rotation[,1:2])) * pca$scale + pca$center)
