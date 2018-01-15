library(asbio) # Contains function ConDis.matrix
library(reshape2) # To access melt function
library(ggplot2)
library(VIM)
library(Amelia)
library(RcppEigen)
library(HSAUR2)
library(outliers)  # To find outliers in given data.

# Question 1
library(asbio)        # Contains function ConDis.matrix
x <- c(3,4,2,1,7,6,5) # Loading value into vector x
y <- c(4,3,7,6,5,2,1) # Loading vectors into vector y
ConDis.matrix(x,y)    # Function that generates concordant and discordant matrix.
#No of concordant pairs are 6 (Number of 1's in lower part of matrix).
#No of discordant pairs are 15 (Number of -1's in lower part of matrix).

#Question 2(a)
library(reshape2)  # To access melt function
a <- rnorm(500)   # Generates 500 values of normally distributed data randomly. 
b <- rt(500,5)    # Generates 500 values of T distributed data randomly.
c <- rbinom(500,1,0.5) # Generates 500 values of Binomial distributed data randomly.
d <- rchisq(500,5) # Generates 500 values of Chi-Squared distributed data randomly.
df <- data.frame(a,b,c,d) # Dataframe(df) containing 4 variables.
df2 <- melt(df, variable.name= "groupVar") #Melting all the variables in df to a single variable by name groupVar.
#Question 2(b)
library(ggplot2) # To access qplot function.
qplot(groupVar,value,data=df2)

#Question 3(a)
GSAF <- read.csv("E:/Masters/IDA/Assignments/Assignment2/ISE 5103 GSAF.csv", header = TRUE, sep = ",")
#Question 3(b)
GSAFdata <- GSAF[c(GSAF$Year>1999), ] #New dataframe containing observations from 2000 year.
#Question 3(c)
Rdate <- as.Date(GSAFdata$Date,"%d%b%y")
GSAFdata <- data.frame(GSAFdata,Rdate)
#Question 3(d)
library(VIM) # To access aggr function.
m <- aggr(GSAFdata$Rdate) # dataframe to find missingness
summary(m)# summary of m
#Question 3(e)
GSAFdata <- GSAFdata[!is.na(GSAFdata$Rdate), ]# Deleting null values in dataframe.

# Question 4(a):
library(Amelia) # To access freetrade dataframe.
library(VIM)    # To acess aggr function (To explore missingness)
data(freetrade) # Load data sets.
miss <- aggr(freetrade,prop= F,numbers= T)# aggr used to calculate and plot missing values.
summary(miss) # Summary of miss

#1.From the first plot we can know the number of missing values in each variable of the dataframe.
#2.From second graph(combinations) we can get number of missing values of each variable by adding numbers corresponding to the red squares(which represent missing values) for a given variable.
#eg: Number of missing values in tariff=(52+4+2 = 58)
#3.Summary gives the number of missing values in each variable and the missings in combination of variables.

#Question 4(b)
tab <- table(freetrade$country,freetrade$tariff) # creating table for country and tariff variables.
chisq.test(tab) # To perform Chi-squared test for tab.

freetrade2 <- freetrade[!(freetrade$country =='Nepal'), ] #creating dataframe removing observations(rows) where Nepal is the country.
tab2 <- table(freetrade2$country,freetrade2$tariff) 
chisq.test(tab2)  

freetrade3 <- freetrade[!(freetrade$country =='Philippines'), ] ##creating dataframe removing observations(rows) where philippines is the country.
tab3 <- table(freetrade3$country,freetrade3$tariff)
chisq.test(tab3)  #  p < 0.05
#In all the above three observations p < 0.05 (Reject null hypothesis,Not independent).So,missingness in the tariff variable is dependent with country variable.

#No,the answer dosenot change if we remove Nepal or Philippines

#problem 5(a) - 1
corMat <- cor(mtcars, use="complete.obs", method="kendall") # creating correlation matrix by kendall method.
#problem 5(a) - 2
library(RcppEigen) # Library to access eigen
eigen(corMat) # Gives Eigen values and Eigen vectors.
#problem 5(a) - 3
pc <- prcomp (corMat,scale= T) # principal compnents of correlation matrix are obtained by centering and scaling by a factor 2.
#problem 5(a) - 4
#Results are different since principal componets are diffrent from eigen values.
#problem 5(a) - 5
angle <- function(a,b){
  inner.prod <- a%*%b 
  xlength <- norm(a,type="2")# type 2 for row sum
  ylength <- norm(b,type="2")
  theta <- acos(inner.prod / (xlength * ylength))
  as.numeric(theta) # convert from matrix to numeric.
}
angle(pc$x[,1],pc$x[,2])

#problem 5(b) - 1
library(HSAUR2) # To access heptathlon
data("heptathlon") # loads heptathlon
apply(heptathlon[,1:8],2,hist)
#problem 5(b) - 2
apply(heptathlon[,1:8],2,grubbs.test)

match_vector = NULL
index = NULL
outlier_identification = function(){
  for (d in 1:ncol(heptathlon)) {
    
    match_vector[d] <- match(outlier(heptathlon[, d]), heptathlon[,d])  
    
  }
  return(match_vector)
}
index <- names(which.max(table(outlier_identification()))) #returns the name of index of first max in a table.
heptathlon <- heptathlon[-as.numeric(index),]
#problem 5(b) - 3
largeValues <- function(y,u){
  for (v in 1:nrow(heptathlon)) {
    heptathlon[v,u] <- y - heptathlon[v,u]
    
  }
  
  return(heptathlon)
}
heptathlon <- largeValues(max(heptathlon$run200m), "run200m")
heptathlon <- largeValues(max(heptathlon$run800m), "run800m")
heptathlon <- largeValues(max(heptathlon$hurdles), "hurdles")

head(heptathlon)
#problem 5(b) - 4
heptathlon[,8] <- NULL
Hpca <- prcomp(heptathlon, scale. = T)
#problem 5(b) - 5
ggbiplot::ggbiplot(Hpca, obs.scale = 1, var.scale = 1,  ellipse = TRUE, circle=TRUE, labels = row.names(heptathlon), labels.size = 3)
#1.Vector represent event and dot represent names
#2.shorts, longjumps, and hurdles are in correlation with PC1.
#3.run200m and high jump are in correlation with PC2
#problem 5(b) - 6
plot(heptathlon, Hpca$x[,1])

#problem 5(c) - 1
train0 <- read.csv("train.0")
train1 <- read.csv("train.1")
train2 <- read.csv("train.2")
#Principal components are used in images to analyse and compress the image without losing much data.
#problem 5(c) - 2
par(mfrow = c(2,3))
train0.pca <- prcomp(train0) #training data set 1
plot(train0.pca, xlab = "pc components")# plots the proportion of variance.
screeplot(train0.pca, type = "line")
train1.pca <- prcomp(train1) # for training data set 2
plot(train1.pca, xlab = "pc components")
screeplot(train1.pca, type = "line") # to describe major percentage of data better to approx 7 Principal components
train2.pca <- prcomp(train2) # for training data set 3
plot(train2.pca, xlab = "pc components")
screeplot(train2.pca,  type = "line") 
train2.summary <- summary(train2.pca)

#problem 6(a)
crime <- read.csv("E:/Masters/IDA/Assignments/Assignment2/test.csv", header = TRUE, sep = ",")  # Reading downloaded file and named dataframe as crime.
#Url : https://www.kaggle.com/account/login?ReturnUrl=%2fc%2fsf-crime%2fdownload%2ftest.csv.zip
#Description of data variables:
#Dates - Date on which crime took place.
#Day Of Week - Day of the week on which crime occurred.
#PdDistrict - name of the police department district.
#Address - Address of crime incident
#X - Longitude
#Y - Latitude
#This dataframe gives information about crimes that took place in San Fransisco from 2003 to 2015. 
#problem 6(b)
#ncol(crime) # To find number of variables (Columns) in crime dataframe.
nrow(crime) # To find number of rows (Observations) in crime dataframe.

mean(crime$X)  # Mean value of Longitude in dataframe
median(crime$X) # Median value of Longitude in dataframe
sd(crime$X)    # Standard deviation of Longitude in dataframe
var(crime$X)   # variance of Longitude in dataframe
mad(crime$X)   # Median Absolute Deviation of Longitude in dataframe

mean(crime$Y)   # Mean value of Latitude in dataframe
median(crime$Y) # Median value of Latitude in dataframe
sd(crime$Y)     # Standard deviation of Latitide in dataframe
var(crime$Y)    # variance of Latitude in dataframe
mad(crime$Y)    # Median Absolute Deviation of Latitude in dataframe

library(outliers) # To find outliers in given data.          
outlier(crime$X)  # Finding outliers in Longitude.  
outlier(crime$Y)  # Finding outliers in Latitude.

library(VIM) # Library to find missing values.
miss_crime <- aggr(crime,prop= F,numbers= T) #
summary(miss_crime) # Gives summary of miss_crime 
#1. There is 1 outlier in X,i.e. -120.5
#2. There is 1 outlier in Y,i.e. 90
#3. There are no missing values in the given data.
#This can be observed from the plots from aggr function and by doing summary of the dataframe.
