---
output: pdf_document
---
# Stevens Institute of Technology
# FA590.  Assignment #4.
# This content is protected and may not be shared, uploaded, or distributed
#"1/2/17"-4/25/22

## Enter Your Name Here, or "Anonymous" if you want to remain anonymous..
## `r format(Sys.time(), "%Y-%m-%d")`


I pledge on my honor that I have not given or received any unauthorized assistance on this assignment/examination. I further pledge that I have not copied any material from a book, article, the Internet or any other source except where I have expressly cited the source.

By filling out the following fields, you are signing this pledge.  No assignment will get credit without being pledged.

Name:Darsh Kachhara

CWID:10474181

Date:5/4/22

# Instructions


When you have completed the assignment, knit the document into a PDF file, and upload _both_ the .pdf and .Rmd files to Canvas.

Note that you must have LaTeX installed in order to knit the equations below.  If you do not have it installed, simply delete the questions below.
```{r}
CWID = 10474181 #Place here your Campus wide ID number, this will personalize
#your results, but still maintain the reproducible nature of using seeds.
#If you ever need to reset the seed in this assignment, use this as your seed
#Papers that use -1 as this CWID variable will earn 0's so make sure you change
#this value before you submit your work.
personal = CWID %% 10000
set.seed(personal)
library(readr)
library(leaps)
library(splines)
library(gam)
library(MASS)
library(tree)
library(randomForest)
library(boot)
library(gbm)
library(class)
library(e1071)
library(glmnet)
```
# Question 1:
In this assignment, you will be required to find a set of data to run regression on.  This data set should be financial in nature, and of a type that will work with the models we have discussed this semester (hint: we didn't look at time series)  You may not use any of the data sets in the ISLR package that we have been looking at all semester.  Your data set that you choose should have both qualitative and quantitative variables. (or has variables that you can transform)

Provide a description of the data below, where you obtained it, what the variable names are and what it is describing.

The project will determine the relationship between the Indian StockMarket Index NIFty 50 and other sectors of the same stock market by their Indexes.
    
The data I have used for this project includes NIFTY50 (The largest and most recognized Index of National Stock Exchange India.) It is made of a cumulative of 50 stocks(BLUE CHIP) ranging from all sectors and is considered a proxy for the entire Indian stock market.The other 10 Indexes represent their respectve sectors.The data has been downloaded from Indian Data website "Trendlyne."

The Varaibles are 
Explanatory varaibles :
NIFTYAUTO	: Represents Auto sector
NIFTYBANK 	:Represents Banking sector
NIFTYENERGY : Represents Energy sector
NIFTYFMCG: Represents FMCG sector
NIFTYIT	:Represents IT sector
NIFTYMEDIA 	: Represents Media sector
NIFTYMETAL	:Represents Metal sector
NIFTYPHARMA	:Represents Pharma sector
NIFTYREALTY : Represents Reality sector	
NIFTYINFRA 	: Represents Infrastructure sector
Explained
NIFTY50: CNX composite Nifty 50 index
```{r}
library(readxl)
MAIN_DATA <- read_excel("DARSHp4data/MAIN_DATA.xlsx")
all_data <- MAIN_DATA
all_data <- log(all_data[c(2:nrow(all_data)),]/all_data[c(1:nrow(all_data)-1),]) 
all_data <- cbind(all_data[c(1:dim(all_data)[1]-1),
c(1:dim(all_data)[2]-1)], all_data[c(2:dim(all_data)[1]),
dim(all_data)[2]]) 
colnames(all_data)[ncol(all_data)] <- c("NIFTY50")
dim(all_data)
summary(all_data)
print(head(all_data))
print(tail(all_data))
cor(all_data)
```
Categorizing Nifty as Bullish for positive return on Nifty and Bearish for negative return.
```{r}
clas_data <- all_data
NIFTY50_der <- rep(1,nrow(clas_data)) 
NIFTY50_der[which(clas_data$NIFTY50>=0)] = "Bullish"
NIFTY50_der[which(clas_data$NIFTY50<0)] ="Bearish"
NIFTY50_der <- as.factor(NIFTY50_der)
clas_data$NIFTY50 <- NIFTY50_der
clas_train <- sample(nrow(clas_data),floor(nrow(clas_data)/2)) 
clas_trainset <- clas_data[clas_train,]
clas_testset <- clas_data[-clas_train,]
```

```{r}
set.seed(1)
train <- sample(nrow(all_data),floor(nrow(all_data)/2))
trainset <- all_data[train,]
testset <- all_data[-train,]
```
# Question 2:
Pick a quantitative variable and fit at least four different models in order to predict that variable using the other predictors.  Determine which of the models is the best fit.  You will need to provide strong reasons as to why the particular model you chose is the best one.  You will need to confirm the model you have selected provides the best fit and that you have obtained the best version of that particular model (i.e. subset selection or validation for example).  You need to convince the grader that you have chosen the best model.

To conduct regression on the quantitative variable, we use 4 method to do it. 
1. Simple Linear Regression

2. Multiple Linear Regression with subset selection and lasso/ridge modification.

3,Support Vector Regression

4,Random Forest Regression:

##Simple Regression

Being in the Indian market, I know first hand that NIFTYBANK and NIFTY 50 are often traded as a pair, so lets fit a simple model and see if it works out
```{r}
model1 <- lm(NIFTY50 ~NIFTYBANK, data = trainset)
summary(model1)
```
The explanatory variable NIFTYBANK is significant and the Adj. R-Squared is 0.03384  respectively.Now, let us test this model using the test dataset.
```{r}
p1 <- predict(model1, newdata=testset)
e1 <- mean((testset$NIFTY50 - p1)^2)
e1
```
So, the Mean Square Error of the model1 is 0.0001293893, which seems very less.So, the model might be a good model.

## Multiple Linear Regression
Now we perform the multiple linear regression. First of all, we do the subset selection to determine which variable is more likely to be included in the model.

```{r}
subsets=regsubsets(NIFTY50~.,data=trainset,method="exhaustive",nvmax=30)
summary(subsets)
```
In order to determin the number of variables, we plot several indicators against the number of variables to find the best one.Finding the best model with the help of Mallow Cp.
```{r}
c <- summary(subsets)$cp
plot(c ,type='b',xlab="No. of Predictors",ylab=expression("Mallows C"[P]),
col="blue")
points(which.min(c),c[which.min(c)],pch=20,
col="red")
```
The plots suggest here we select 4 variables. The 4 varaibles contain first 3 indices NIFTYBANK NIFTYIT NIFTYFMCG NIFTYREALTY. After regression, we give the summary of the model and the test MSE.

```{r}
fit1 = lm(NIFTY50~NIFTYIT+NIFTYBANK+NIFTYFMCG+NIFTYREALTY,data = trainset)
summary((fit1))
prediction1 = predict(fit1,testset)
mse1 = mean((prediction1-testset$NIFTY50)^2)
mse1
```
As, we see in the summary of model2 the variable  NIFTY FMCG and NIFTY REALTY are not significant. So we don't take that variable as part of our model.
```{r}
fit2 = lm(NIFTY50~NIFTYIT+NIFTYBANK,data = trainset)
summary((fit2))
prediction2 = predict(fit2,testset)
mse2 = mean((prediction2-testset$NIFTY50)^2)
mse2
```
So, two variables are significant. The Adj. R-squared value of the model2 is 0.04103  which is greater than model1. So, the Mean Square Error of the model2 is  0.0001287693, which seems very less  but is more than model1's MSE. 

## Support Vector Regression

Support vector regression is a generalization of the support vector machine to the regression problem. It is a fast and accurate way of interpolating data sets. It is useful when you have an expensive function you want to approximate over a known domain. It learns quickly and is systematically improvable. For building a model using SVR we use same variables obtained earlier based on Mallow Cp.
```{r}
set.seed(1)
model3 <- svm(NIFTY50~NIFTYIT+NIFTYBANK+NIFTYFMCG+NIFTYREALTY,data = trainset,
                  type = "eps-regression")

```
Now, let us test this model using the test dataset.
```{r}
set.seed(1)
p3 <- predict(model3, newdata=testset)
e3 <- mean((testset$NIFTY50 - p3)^2)
e3
```
So, the Mean Square Error of the model2 is 0.000128034, which seems very less.So, the model might be a good model as well.

##(4) Random Forest Regression:

```{r}
set.seed(1)
model4 <-randomForest(x = trainset[,c("NIFTYBANK","NIFTYIT",
                                      "NIFTYFMCG","NIFTYREALTY")],
                       y = trainset$NIFTY50, ntree = 501)
```

Now, let us test this model using the test dataset.
```{r}
p4 <- predict(model4, newdata=testset)
e4 <- mean((testset$NIFTY50 - p4)^2)
e4
```
 So, the Mean Square Error of the model2 is  0.0001296344, which seems very less.
    So, the model might be a good model as well.
    
     Comparison table of MSE of all four models:
```{r}
data.frame("MSE"=c("model1"=e1,"model2"=mse2,"model3"=e3,"model4"=e4))
```
Based on Mean Square Error (MSE) we can say that model 3 i.e. Support Vector Regression is the best model.

#Question 3:

Do the same approach as in question 2, but this time for a qualitative variable.
#(1) Logistic Regression Model:
```{r}
set.seed(1)
logfit=glm(NIFTY50~.,data=clas_trainset,family=binomial) 
result1 = predict(logfit,clas_testset,type = "response") 
test_predict1 = rep(1,nrow(clas_testset)) 
test_predict1[which(result1>=0.5)] <- "Bullish"
test_predict1[which(result1<0.5)] <- "Bearish"
res_table1 = table(test_predict1,clas_testset$NIFTY50) 
acc1 = (res_table1[1,1]+res_table1[2,2])/sum(res_table1) 
print(res_table1)
print(acc1)
```
The accuracy of this regression on the test set is 0.5236641. Note that we include all the variable into the model.
##LDA
```{r}
set.seed(1)
ldafit=lda(NIFTY50~.,data=clas_trainset)
result2 = predict(ldafit,clas_testset,type = "response")$class 
res_table2 = table(result2,clas_testset$NIFTY50)
acc21 = (res_table2[1,1]+res_table2[2,2])/sum(res_table2) 
print(res_table2)
print(acc21)
```
Using the LDA method we can get the accuracy of 0.5251908
###QDA

```{r}
set.seed(1)
qdafit = qda(NIFTY50~.,data=clas_trainset)
result3 = predict(qdafit,clas_testset,type = "response")$class 
res_table3 = table(result3,clas_testset$NIFTY50)
acc23 = (res_table3[1,1]+res_table3[2,2])/sum(res_table3) 
print(res_table3)
print(acc23)
```
Using the QDA method we can get the accuracy of 0.5099237 on the test set which is not better than the LDA method.
# SVM Linear
```{r}
set.seed(1)
tune.out = tune(svm,NIFTY50~.,data = clas_trainset,kernel="linear",
ranges = list(cost=c(0.001,0.01,0.1,1.5,10,100)))

bestmod41 = tune.out$best.model
result41 = predict(bestmod41,clas_testset)
res_table61 = table(result41,clas_testset$NIFTY50)
acc41 = (res_table61[1,1]+res_table61[2,2])/sum(res_table61)
print(acc41)
```

```{r}
conclusion2<- data.frame(c(acc1,acc21,acc23,acc41),
                         row.names=c("logistic regression","LDA","QDA","SVM"))
colnames(conclusion2) = "test accuracy"
print(conclusion2)
```
So the SVM will give us the highest test accuracy result.
#Question 4:

In this problem, you will use support vector approaches in order to predict the direction of your ETFs in your data set from homework 2.  

##(a)
Create two different data frames, one for each ETF.  Each data frame should include the log returns of your assets as well as a binary classifier for the direction of each ETF. 
```{r}
REQ<-read_csv("DARSHp4data/REQ.csv")
SPYframe <- data.frame(REQ$SPYr,REQ$CSPY)
colnames(SPYframe)<-c("SPYreturn","BCSPY")
head(SPYframe)
QQQframe<- data.frame(REQ$QQQr,REQ$CQQQ)
colnames(QQQframe)<-c("QQQreturn","BCQQQ")
head(QQQframe)
```
##(b)
Fit a support vector classifier to the data using linear kernels.  You should use the tune function to determine an optimal cost for each SVM.  What do you see in these results?  Is one ETF more accurately predicted over the other?
```{r}
set.seed(personal)
tune.out1 = tune(svm, SPYframe$BCSPY~SPYframe$SPYreturn, 
                 data = SPYframe, kernel = "linear", ranges =
list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out1)

tune.out2 = tune(svm, QQQframe$BCQQQ~QQQframe$QQQreturn, 
                 data = QQQframe, kernel = "linear", ranges =
list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out2)
```
For both, when the cost equals to 0.01, the error reaches the lowest level.
##(c)
Now repeat (b), this time using SVMs with radial and polynomial basis kernels,with different values of gamma and degree and cost. Comment on your results.
```{r}
set.seed(personal)
tune.out3 = tune(svm,SPYframe$BCSPY~SPYframe$SPYreturn,
                 data = SPYframe,kernel="radial",
                 ranges = list(cost=c(0.01,0.1,1,5,10,100),
                               gamma = c(0.01,0.1,1,3,5,10,100)))
summary(tune.out3)

set.seed(personal)
tune.out4 = tune(svm,QQQframe$BCQQQ~QQQframe$QQQreturn, 
                 data = QQQframe,kernel="radial",
                 ranges = list(cost=c(0.01,0.1,1,5,10,100),
                               gamma = c(0.01,0.1,1,3,5,10,100)))
summary(tune.out4)

```
For radial kernel,for both, when cost=100 and gamma = 100, the error will get the lowest level.
```{r}
set.seed(personal)
tune.out4 = tune(svm,SPYframe$BCSPY~SPYframe$SPYreturn, data = SPYframe,kernel="polynomial",ranges = list(cost=c(0.01,0.1),degree = c(2,3)))
summary(tune.out4)

set.seed(personal)
tune.out5 = tune(svm,QQQframe$BCQQQ~QQQframe$QQQreturn, data = QQQframe,kernel="polynomial",ranges = list(cost=c(0.01,0.1,10),degree = c(2,3)))
summary(tune.out5)
```
When using the polynomial basis kernel, for both
the cv error reaches its lowest level for cost equals to 
0.1 and dgree equals to 2.
