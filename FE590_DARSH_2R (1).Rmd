---
output: pdf_document
---

```{r global_options, include=FALSE}
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```



# FE590.  Assignment #2.


## `r format(Sys.time(), "%Y-%m-%d")`


I pledge on my honor that I have not given or received any unauthorized assistance on this assignment/examination. I further pledge that I have not copied any material from a book, article, the Internet or any other source except where I have expressly cited the source.

By filling out the following fields, you are signing this pledge.  No assignment will get credit without being pledged.

Name<- "Darsh Kachhara"

CWID<- 10474181

Date<- 03/15/2022


# Instructions
In this assignment, you should use R markdown to answer the questions below. Simply type your R code into embedded chunks as shown above.
When you have completed the assignment, knit the document into a PDF file, and upload both the .pdf and .Rmd files to Canvas.
```{r}
CWID = 10474181 #Place here your Campus wide ID number, this will personalize
#your results, but still maintain the reproduceable nature of using seeds.
#If you ever need to reset the seed in this assignment, use this as your seed
#Papers that use -1 as this CWID variable will earn 0's so make sure you change
#this value before you submit your work.
personal = CWID %% 10000
set.seed(personal)#You can reset the seed at any time in your code,
#but please always set it to this seed.
```
# Question 1 





Create a .csv file consisting of daily adjusted close prices for 10 different stocks and 2 ETF's.  You should have at least two years of data for every asset and ETF and should include the date for your data to make sure that you are including everything appropriately.  After creating the file, put it in your working directory (or move your working directory to where its stored).  Read the data into R.

```{r}
#insert r code here
#QQQ SPY JNJ NVDA TSLA BRK WMT TTCEHY TSM V BAC UNH
library(readr)
library(MASS)
WMT <- read_csv("WMT.csv")
BRK <- read_csv("BRK-B.csv")
JNJ <- read_csv("JNJ.csv")
NVDA <- read_csv("NVDA.csv")
QQQ <- read_csv("QQQ (1).csv")
SPY <- read_csv("SPY (1).csv")
TTCEHY <- read_csv("TCEHY.csv")
TSLA<- read_csv("TSLA.csv")
TSM <- read_csv("TSM.csv")
UNH <- read_csv("UNH.csv")
V <- read_csv("V.csv")
BAC <- read_csv("BAC.csv")

DATA <- data.frame(QQQ$`Adj Close`, SPY$`Adj Close`, JNJ$`Adj Close`, NVDA$`Adj Close`, TSLA$`Adj Close`, BRK$`Adj Close`, WMT$`Adj Close`, TTCEHY$`Adj Close`, TSM$`Adj Close`, V$`Adj Close`, BAC$`Adj Close`, UNH$`Adj Close`)
data2 <- data.frame(DATA,DATA^2)
```


## 1. List the names of the variables in the data set.

```{r}
#insert r code here
ls(DATA)
```


## 2. As the date will be unimportant, remove that field from your data frame

```{r}
#insert r code here
# I didn't add the date in the data  frame in the first place 
```

## 3. What is the range of each quantitative variable? Answer this question using the range() function with the sapply() function e.g., sapply(cars, range). Print a simple table of the ranges of the variables. The rows should correspond to the variables. The first column should be the lowest value of the corresponding variable, and the second column should be the maximum value of the variable. The columns should be suitably labeled.

```{r}
#insert r code here
range_table<-sapply(DATA,range)
rownames(range_table)<-c("min","max")
range_table
```

## 4. What is the mean and standard deviation of each variable? Create a simple table of the means and standard deviations.

```{r}
#insert r code here
func <- function(x)
  {
  c(mean(x),sd(x))
    }
table2<- sapply(DATA,func)
rownames(table2)<-c("mean","standarddeviation")
table2
```



## 5.  Using the regsubsets function in the leaps library, regress one of your ETF's on the remaining assets.

```{r}
#insert r code here
library(leaps)
regsubsets1<- regsubsets(DATA$QQQ..Adj.Close.~.,data=DATA)
summary(regsubsets1)
names(summary(regsubsets1))
# The validation part
cp=summary(regsubsets1)$cp
cp
i=which.min(cp)
i
bic=summary(regsubsets1)$bic
bic
j<-which.min(bic)
j
adjr2_1=summary(regsubsets1)$adjr2
k=which.max(adjr2_1)
c(i,j,k)

plot(cp,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Mallows C"[P]))
points(i,cp[i],pch=19,col="red")

plot(bic,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Bayes Information Criterion"))
points(j,bic[j],pch=19,col="red")

plot(adjr2_1,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Adjusted R-squared"))
points(k,adjr2_1[k],pch=19,col="red")


```



### a. Print a table showing what variables would be selected using best subset selection for all predictors up to order 2 (i.e. asset and asset^2).  Determine the optimal model using BIC and output the model, including its coeffecients.

```{r}
#insert r code here
regsubsets2<- regsubsets(data2$QQQ..Adj.Close.~.,data=data2)
names(summary(regsubsets2))
bic2=summary(regsubsets2)$bic
bic2
j2=which.min(bic2)
coef(regsubsets2,j2)
plot(bic2,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Bayes Information Criterion"))
points(j2,bic2[j2],pch=19,col="red")

```

### b. Print a table showing what variables would be selected using forward subset selection for all predictors up to order 2 (i.e. asset and asset^2). Determine the optimal model using BIC and output the model, including its coeffecients.


```{r}
#insert r code here
regsubsets3<- regsubsets(data2$QQQ..Adj.Close.~.,data=data2,method="forward")
names(summary(regsubsets3))
bic3=summary(regsubsets3)$bic
bic3
j3=which.min(bic3)
coef(regsubsets3,j3)
plot(bic3,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Bayes Information Criterion"))
points(j3,bic3[j3],pch=19,col="red")

```

### c. Print a table showing what variables would be selected using backward subset selection for all predictors up to order 2 (i.e. asset and asset^2). Determine the optimal model using adjusted R^2 and output the model, including its coeffecients.


```{r}
#insert r code here
regsubsets4<- regsubsets(data2$QQQ..Adj.Close.~.,data=data2,method="backward")
names(summary(regsubsets4))
adjr4=summary(regsubsets4)$adjr2
adjr4
k4=which.max(adjr4)
coef(regsubsets4,k4)
plot(adjr4,type='b',col="blue",xlab="Number of Predictors",ylab=expression("Adjusted R-squared"))
points(k4,adjr4[k4],pch=19,col="red")
```



# Question 2 

Using the data set that you loaded for the first problem, choose the other ETF, and create a data frame consisting of simple lagged returns going up to 10 days back.  Create another field in this data frame that looks to the direction of the ETF moving one day into the future, this direction should be listed as a factor, not a number.

## 1. Split your data into a training set and a testing set and run LDA on the direction using all 10 different lags on the training set.  How accurate is your model?

```{r}
#insert r code here
#Q2) 1.
qqq<-QQQ$`Adj Close`
Direction_1=sign(log(qqq[12:1259]/qqq[11:1258]))
Direction_1[Direction_1==1]="Up"
Direction_1[Direction_1==0]="Same"
Direction_1[Direction_1==(-1)]="Down"
Direction_1=as.factor(Direction_1)
Log_Lag1=log(qqq[11:1258]/qqq[10:1257])
Log_Lag2=log(qqq[11:1258]/qqq[9:1256])
Log_Lag3=log(qqq[11:1258]/qqq[8:1255])
Log_Lag4=log(qqq[11:1258]/qqq[7:1254])
Log_Lag5=log(qqq[11:1258]/qqq[6:1253])
Log_Lag6=log(qqq[11:1258]/qqq[5:1252])
Log_Lag7=log(qqq[11:1258]/qqq[4:1251])
Log_Lag8=log(qqq[11:1258]/qqq[3:1250])
Log_Lag9=log(qqq[11:1258]/qqq[2:1249])
Log_Lag10=log(qqq[11:1258]/qqq[1:1248])
Log_Pred1=data.frame(Direction_1,Log_Lag1,Log_Lag2,Log_Lag3,Log_Lag4,Log_Lag5,Log_Lag6,Log_Lag7,Log_Lag8,Log_Lag9,Log_Lag10)
train_1 <- sample(1259,800,replace=FALSE)
lda.fit_stocks=lda(Direction_1~.,data=Log_Pred1,subset=train_1)
lda.pred_stocks=predict(lda.fit_stocks,Log_Pred1[-train_1,])
lda.class_1=lda.pred_stocks$class
table(lda.class_1,Log_Pred1$Direction_1[-train_1])
mean(lda.class_1==Log_Pred1$Direction_1[-train_1])#accuracy
#2)
#splitting into 5 pieces
K<- 5
len<- length(Log_Pred1$Direction_1)
folds<- seq(1:K)
member<- sample(folds,len,replace=T)
cv.error=rep(0,K)
for (i in 1:K)
{
  train=Log_Pred1[member!=i,]
  test=Log_Pred1[member==i,]
  temp.mod=lda(Direction_1~.,data=train)
  temp.predict<- predict(temp.mod,test)
  cv.error[i]=mean((test$Direction_1==temp.predict$class)^2)
}
cv.error
mean(cv.error)

#LOOCV
K1=len
cv.error1=rep(0,K)
for (i in 1:K1)
{
  
  temp1.mod=lda(Direction_1~.,data=Log_Pred1[-i,])
  cv.error1[i]=(Log_Pred1$Direction_1[i]==predict(temp1.mod,Log_Pred1[i,])$class)^2
}
cv.error1
mean(cv.error1)
```



## 2. Create code to determine the estimate for the expected test error using K=5 cross validation.  Do this by actually splitting the date into five pieces and give the average of the test error, not just by using a command from a package (such as cv.glm).

## 3. Determine the LOOCV estimate of the expected test error of your model.  How do your answers to each part of this question compare?  Do you see any noticable differences between your answer?  Why do you think that is?

# Question 3 

This question should be answered using the Weekly data set, which is part of the ISLR package. This data contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.

## 1. What does the data represent?

```{r}
#insert r code here
library(ISLR)
# The data represents Weekly percentage returns for the S&P 500 stock index between 1990 and 2010
```



## 2. Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?

```{r}
#insert r code here
regression_weekly_1 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,data = Weekly,family = binomial)
summary(regression_weekly_1)
#only the intercept and lag 2 and found to be significant
```




## 3. Fit a logistic regression model using a training data period from 1990 to 2008, using the predictors from the previous problem that you determined were statistically significant. Test your model on the held out data (that is, the data from 2009 and 2010) and express its accuracy.


```{r}
#insert r code here
training_weekly_1 <- Weekly[Weekly$Year <= 2008,]

testing_weekly_1 <- Weekly[!Weekly$Yea<= 2008,]

direction_Test_1 <- testing_weekly_1 $Direction

regression_weekly_2 <- glm(Direction ~ Lag2,data = training_weekly_1,family = binomial)

Predicting_weekly_regression_2 <- predict(regression_weekly_2, testing_weekly_1, type = "response")
showcase <- rep("Down", 104)

showcase[Predicting_weekly_regression_2 > 0.5] <- "Up"

table(showcase, direction_Test_1)

rl <-mean(showcase==direction_Test_1)
```

## 4. Repeat Part 3 using LDA.

```{r}
#insert r code here
regression_weekly_3 <- lda(Direction ~ Lag2,data = training_weekly_1 )
showcase_2 <- predict(regression_weekly_3 , testing_weekly_1)$class
table(showcase_2, direction_Test_1)
rlda<- mean(showcase_2== direction_Test_1)
```


## 5. Repeat Part 3 using QDA.

```{r}
#insert r code here
regression_weekly_4 <- qda(Direction ~ Lag2,data = training_weekly_1 )
showcase_3 <- predict(regression_weekly_4, testing_weekly_1)$class
table(showcase_3, direction_Test_1)
rqda<- mean(showcase_3== direction_Test_1)

```

## 6. Repeat Part 3 using KNN with K = 1, 2, 3.

```{r}
#insert r code here
library(FNN)
KNN_training <- matrix(training_weekly_1$Lag2)
KNN_testing <- matrix(testing_weekly_1$Lag2)
Direction_KNN <- training_weekly_1$Direction
KNN1 <- knn(KNN_training,KNN_testing,Direction_KNN ,k=1)
table(KNN1,direction_Test_1)
r1   <- mean(KNN1 == direction_Test_1)

KNN2 <- knn(KNN_training,KNN_testing,Direction_KNN,k=2)
table(KNN1,direction_Test_1)
r2   <- mean(KNN2 == direction_Test_1)

KNN3 <- knn(KNN_training,KNN_testing,Direction_KNN,k=3)
table(KNN3,direction_Test_1)
r3   <- mean(KNN3 == direction_Test_1)
```


## 7. Which of these methods in Parts 3, 4, 5, and 6 appears to provide the best results on this data?

```{r}
#insert r code here
result_total <- data.frame(rl,rlda,rqda,r3,r1,r2)
result_total
```


# Question 4

## Write a function that works in R to gives you the parameters from a linear regression on a data set of $n$ predictors.  You can assume all the predictors and the prediction is numeric.  Include in the output the standard error of your variables.  You cannot use the lm command in this function or any of the other built in regression models.  


```{r}
#insert r code here
# Simple Linear Regression

# Importing the dataset
dataset = data.frame(DATA$SPY..Adj.Close.,DATA$NVDA..Adj.Close.)
View(dataset)
RSS=function(b0,b1,y,x)
{
  yhat=b0+b1*x;
  err=yhat-y;
  return(sum(err^2));
}

beta0=seq(from=0,to=14,by=.005);
N0=length(beta0);
beta1=seq(from=.02, to=.08,by=.0005);
N1=length(beta1);
z=matrix(NA,N0,N1);
for(i in 1:N0)
{
  for(j in 1:N1)
  {
    z[i,j]=RSS(beta0[i],beta1[j],dataset$DATA.SPY..Adj.Close.,dataset$DATA.NVDA..Adj.Close.);
  }
}

which.min(z)
which.min(z)%%N0
ceiling(which.min(z)/N0)
which(z==min(z), arr.ind = TRUE)
min(z)
z[ 2801 ,121]

```

## Compare the output of your function to that of the lm command in R.

```{r}
#insert r code here
model_1<-lm(dataset$DATA.SPY..Adj.Close.~dataset$DATA.NVDA..Adj.Close.)
names(model_1)
summary(model_1)

```


# Question 5

As you have probably seen in this homework, just simply looking at the close prices and trying to run models on the variables is not terribly interesting.  You've begun to see what types of techniques we will be studying in this class.  Here is an exerpt from the final project/homework:

"In this assignment, you will be required to find a set of data to run regression on.  This data set should be financial in nature, and of a type that will work with the models we have discussed this semester (hint: we didn't look at time series)  You may not use any of the data sets in the ISLR package that we have been looking at all semester.  Your data set that you choose should have both qualitative and quantitative variables. (or has variables that you can transform)

Provide a description of the data below, where you obtained it, what the variable names are and what it is describing."

You don't have to actually create the data set at this time, but what sort of problem are you looking to solve?  What data set would you need to answer this question?  Please provide what you are looking into and how you could approach the problem below.

#At this point I want find two models that could work long term in the market , regardless of what state the market is in.
#There are fundamentals to the companies that are listed on the stock market. These are hardcore facts ( mostly reliable) that show case the cash flow , asset base, business model, company history , information on board of directors etc. 
#I want to figure out a model that could act as a screener based on these facts sampling the the listed companies on the market. The variable I would as my "y" variable that would predict a companies status/ public perception could be its credit evaluation/rating.

#Second model I want to work on would include the companies i select from the screener and their portfolio and try and predict their beta with respect to the broader market. Basically I would try and create a efficient portfolio of these fundamentally strong companies , to see if they could outperform the benchmarks in both a bull and a bear run.
