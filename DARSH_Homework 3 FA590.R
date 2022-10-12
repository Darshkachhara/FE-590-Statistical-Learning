---
output: pdf_document
---

# FE590.  Assignment #3.
# This content is protected and may not be shared, uploaded,
# or distributed

## Enter Your Name Here, or "Anonymous" if you want to remain anonymous..
## `r format(Sys.time(), "%Y-%m-%d")`


# Instructions

In this assignment, you should use R markdown to answer the questions below.  Simply type your R code into embedded chunks as shown above.

When you have completed the assignment, knit the document into a PDF file, and upload _both_ the .pdf and .Rmd files to Canvas.

Note that you must have LaTeX installed in order to knit the equations below.  If you do not have it installed, simply delete the questions below.

# Question 1 (based on JWHT Chapter 5, Problem 8)

In this problem, you will perform cross-validation on a simulated data set.

You will use this personalized simulated data set for this problem:
```{r}
library(leaps)
library(boot)
library(MASS)
library(ISLR)

CWID = 10474181 #Place here your Campus wide ID number, this will personalize
#your results, but still maintain the reproduceable nature of using seeds.
#If you ever need to reset the seed in this assignment, use this as your seed
#Papers that use -1 as this CWID variable will earn 0's so make sure you change
#this value before you submit your work.
personal = CWID %% 10000
set.seed(personal)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
```

   (a) In this data set, what is _n_ and what is _p_?
   (b) Create a scatterplot of _x_ against _y_. Comment on what you find.
   (c) Compute the LOOCV errors that result from fitting the following four models using least squares:
      1.  $Y = \beta_0 + \beta_1 X + \epsilon$
      2.  $Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \epsilon$
      3.  $Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \beta_3 X^3 + \epsilon$
      4.  $Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \beta_3 X^3 + \beta_4 X^4 + \epsilon$
   (d) Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.
   (e) Comment on the statistical significance of the coefficient estimates that results from fitting each of the models in (c) using least squares. Do these results agree with the conclusions drawnbased on the cross-validation results?


```{r}
# Enter your R code here!

#(a)
# n showcases the number of observations. Here n = 100
# p is the number of  predictors. Here p = 2 i.e. x and x^2.
# #(b)
plot(x=x, y=y, main="Plot: x against y", xlab = "x", ylab = "y", col = "darkblue")
#The relation between x and y in not linear. It quadratic relationship.
# as we define y as x - 2*x^2 + epsilon which is quadratic.
df_1 <- data.frame(x,y)
#Model 1 
modelA <- glm(y ~ x)
modelA1 <- cv.glm(df_1, modelA)
modelA1$delta 
#Model 2 
modelB <- glm(y ~ poly(x, 2))
modelB2 <- cv.glm(df_1, modelB)
modelB2$delta 
#Model 3
modelC <- glm(y ~ poly(x, 3))
modelC3 <- cv.glm(df_1, modelC)
modelC3$delta 
#Model 4 
modelD <- glm(y ~ poly(x, 4))
modelD4 <- cv.glm(df_1, modelD)
modelD4$delta 
# (d)
#  modelB has the smallest LOOCV error. It was
#expected that modelB should have least LOOCV error as we defined y as x - 2*x^2
# + epsilon.
#When we see the delta vector we get two values. The values are
# identical upto two decimal place.
#(e)
summary(modelA)
summary(modelB)
summary(modelC)
summary(modelD)

# modelB i.e. x and x^2 (linear and quadratic
# part) are significant. 
#  modelC the variable x^3 is not significant. 
# modelD the variable x^3 and x^4 are not significant. 
# modelB has least LOOCV error. 
#Thus the statistical significance of the coefficient
# estimates agree with the conclusions drawnbased 
#on the cross-validation results.



```
# Question 2 (based on JWTH Chapter 7, Problem 10)

The question refers to the 'College' data set

(a) Split the data into a training set and a test set.  Using out-of-state tuition as the response and the other variables as the predictors, perform subset selection (your choice on how) in order to identify a satisfactory model that uses just a subset of the predictors (if your approach suggests using all of the predictors, then follow your results and use them all).
(b) Fit a GAM on the training data, using out-of-state tuition as the response and the features selected in the previous step as the predictors, using splines of each feature with 5 df.
(c) Evaluate the model obtained on the test set, and explain the results obtained
(d) For which variables, if any, is there evidence of a non-linear relationship with the response?  Which are probably linear?  Justify your answers.

```{r}
# Enter your R code here!

#(a)
set.seed(personal)
attach(College)
length(College)
xvar <- sample(length(Apps), as.integer(length(Apps)*0.85))
college_Train_set <- College[xvar,]
college_Test_set <- College[-xvar,]
s_college <- regsubsets(Outstate ~ ., data = college_Train_set, method = "exhaustive", nvmax
= 18)

summary(s_college)[7]
c_p <- summary(s_college)$cp
plot(c_p , type='b', xlab="No. of Predictors",
 ylab=expression("Mallows C"[P]), col="blue")
points(which.min(c_p), c_p[which.min(c_p)], pch=22,
 col="red")

which.min(c_p)

# Model with 14 variables is the best: has the minimum Mallows Cp.
# The 14 coefficients are as follow:

coef(s_college,14)

#(b)

library(gam)
colnames(College)

gamModel <- gam(Outstate ~ Private + s(Apps,5) + s(Accept,5) +
 s(Enroll,5) + s(Top10perc,5) + s(Room.Board,5) +s(S.F.Ratio ,5)
 +s(Personal,5) + s(PhD,5) + s(Terminal,5) +s(perc.alumni,5)+
 s(perc.alumni,5) + s(Expend,5) + s(Grad.Rate,5),data =college_Train_set)

#(c)

mean((college_Test_set$Outstate - predict(gamModel, college_Test_set))^2)

#The MSE is very large. Thus, we can say that the model does not fit well

summary(gamModel)

# Variables Expend and Accept have lower p-value:non-linear relationship 
#with the response variable.
# Variables Top10perc and perc.alumni have higher p-values:
# linear relationship with the response variable.


```

# Question 3 (based on JWHT Chapter 7, Problem 6)

In this exercise, you will further analyze the `Wage` data set.

(a) Perform polynomial regression to predict `wage` using `age.` Use cross-validation to select the optimal degree d for the polynomial. What degree was chosen? Make a plot of the resulting polynomial fit to the data.
(b) Fit a step function to predict `wage` using `age`, and perform cross-validation to choose the optimal number of cuts. Make a plot of the fit obtained.

```{r}
# Enter your R code here!
#(a)
attach(Wage)
summary(Wage)

summary(Wage$age)
summary(Wage$wage)

d <- NULL
for(i in 1:10)
{
 Wage.Model <- glm(formula = wage ~ poly(age, i), data = Wage)
 d[i] <- cv.glm(Wage, Wage.Model, K=10)$delta[2]
}
d

plot(x=c(1:10), y=d, type = "l",xlab = "degrees",
 ylab = "Cross Validation error", col="blue")
points(d, col="red")

# The optimal degree can be d = 3.

fit1 = lm(wage~poly(age, 1), data=Wage)
fit2 = lm(wage~poly(age, 2), data=Wage)
fit3 = lm(wage~poly(age, 3), data=Wage)
fit4 = lm(wage~poly(age, 4), data=Wage)
fit5 = lm(wage~poly(age, 5), data=Wage)
fit6 = lm(wage~poly(age, 6), data=Wage)
fit7 = lm(wage~poly(age, 7), data=Wage)
fit8 = lm(wage~poly(age, 8), data=Wage)
fit9 = lm(wage~poly(age, 9), data=Wage)
fit10 = lm(wage~poly(age, 10), data=Wage)
anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)

# According to Anova polynomials above degree 2 are insignificant.
# Hence, we take degree 2 as the optimal degree.

plot(wage~age, data=Wage, col="blue")
age.grid <- seq(18,80)
lm.fit <- fit2
lm.pred <- predict(lm.fit, data.frame(age=age.grid))
lines(age.grid, lm.pred, col="red")

#(b)

errors <- NULL
for(i in 2:10){
 Wage$ageCuts <- cut(age,i)
 Wage.Model_1 <- glm(wage ~ ageCuts,data = Wage)
 errors[i] <- cv.glm(Wage,Wage.Model_1,K=10)$delta[2]
}
error_1 <- errors[-1]
plot(2:10, error_1, type = "l", lwd = 2, col = "dark blue",
 xlab = "cuts", ylab = "Cross Validation error")
points(errors, col="red")

#Minimum error gives optimal number of cuts.

min <- which.min(errors)
min

#Therefore, optimal number of cuts is equal to 8.

model <- glm(wage ~ cut(age, min),data = Wage)
prediction <- predict(model,
 newdata = data.frame(age = range(age)[1]:range(age)[2]))
plot(wage ~ age, data=Wage, col= "blue")
lines(18:80, prediction, col="red")

```

# Question 4 (based on JWHT Chapter 8, Problem 8)

In the lab, a classification tree was applied to the `Carseats` data set after converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable.

(a) Split the data set into a training set and a test set.
(b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?
(c) Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?
(d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the `importance()` function to determine which variables are most important.

```{r}
# Enter your R code here!
#(a)
attach(Carseats)
summary(Carseats)
set.seed(personal)
samples <- sample(1:400, 320)
training_set <- Carseats[samples,]
testing_set <- Carseats[-samples,]
#(b)
library(tree)
library(rpart)
treeCarseats_model <- tree(Sales ~ ., data = training_set,
 method = "recursive.partition",
 split = c("deviance", "gini"),
 model = TRUE)
summary(treeCarseats_model)
plot(treeCarseats_model)
text(treeCarseats_model, cex=0.51)
predCarseats <- predict(treeCarseats_model, testing_set) 

#Mean Square Error(MSE):
mean((testing_set$Sales - predCarseats)^2)
#(c)
treeCV <- cv.tree(treeCarseats_model, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(treeCV$size, treeCV$dev, type = "o", col = "blue",
 lwd = 2, xlab = "Size", ylab = "Deviance")
plot(treeCV$k, treeCV$dev, type = "o", col = "red",
 lwd = 2,xlab = "Complexity of cost",ylab = "Deviance")
names(treeCV)

#We check the deviance in treeCV

treeCV$dev
m <- which.min(treeCV$dev)
m

# we choose 1 due to minimum deviance.
# cheacking size at 1st position

treeCV$size[1]
prunedCarseats = prune.tree(treeCarseats_model, best = 7)
par(mfrow = c(1, 1))
plot(prunedCarseats)
text(prunedCarseats, pretty = 0)
prediction <- predict(prunedCarseats, testing_set)

#Pruning Tree Mean Square Error(MSE). 

mean((testing_set$Sales - prediction)^2)

#no improvement 

mean((testing_set$Sales - prediction)^2) #MSE with pruning
mean((testing_set$Sales - predCarseats)^2) #Test MSE 

#(d)
library(randomForest)
set.seed(personal)
bagCarseats <- randomForest(Sales ~ ., data = training_set, mtry = 10, ntree = 500,
importance = T)
bagPred <- predict(bagCarseats, testing_set)
mean((testing_set$Sales - bagPred)^2)

#Bagging improves the test MSE to 2.772075. 

importance(bagCarseats)

#Price, ShelveLoc and CompPrice are three most important predictors of Sale.



```
# Question 5 (based on JWTH Chapter 8, Problem 10)

Use boosting (and bagging) to predict Salary in the Hitters data set

(a) Remove the observations for which salary is unknown, and then log-transform the salaries
(b) Split the data into training and testing sets for cross validation purposes.
(c) Perform boosting on the training set with 1000 trees for a range of values of the shrinkage parameter $\lambda$.  Produce a plot with different shrinkage parameters on the x-axis and the corresponding training set MSE on the y-axis
(d) Produce a plot similar to the last one, but this time using the test set MSE
(e) Fit the model using two other regression techniques (from previous classes) and compare the MSE of those techniques to the results of these boosted trees.
(f) Reproduce (c) and (d), but this time use bagging instead of boosting and compare to the boosted MSE's and the MSE's from (e)

```{r}
# Enter your R code here!
#for this code, I had to import the data set from the internet since the one that is Installed in R is not showing any values

#(a)
#library(ISLR2)
library(readr)

Hitters <- read_csv("Hitters.csv")
summary(Hitters)
sum(is.na(Hitters$Salary))
Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))
Hitters$Salary = log(Hitters$Salary)

#(b)
train_val = 1:200
Hitters.train_set = Hitters[train_val, ]
Hitters.test_set = Hitters[-train_val, ]

#(c)
library(gbm)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.e = rep(NA, length.lambdas)
test.e = rep(NA, length.lambdas)
for (i in 1:length.lambdas) 
   {
 boost.hitters = gbm(Salary ~ ., data = Hitters.train_set, distribution ="gaussian", n.trees = 1000, shrinkage = lambdas[i])
 train.p = predict(boost.hitters, Hitters.train_set, n.trees = 1000)
 test.p = predict(boost.hitters, Hitters.test_set, n.trees = 1000)
 train.e[i] = mean((Hitters.train_set$Salary - train.p)^2)
 test.e[i] = mean((Hitters.test_set$Salary - test.p)^2)
    }
plot(lambdas, train.e, type = "b", xlab = "Shrinkage", ylab = "Train MSE", col = "blue", pch = 20)

#(d)
plot(lambdas, test.e, type = "b", xlab = "Shrinkage", ylab = "Test MSE",
 col = "red", pch = 20)
min(test.e)
lambdas[which.min(test.e)]

#(e)
lm.fit_5 = lm(Salary ~ ., data = Hitters.train_set)
lm.pred_5 = predict(lm.fit_5, Hitters.test_set)
mean((Hitters.test_set$Salary - lm.pred_5)^2)
library(glmnet)
x = model.matrix(Salary ~ ., data = Hitters.train_set)
y = Hitters.train_set$Salary
x.test = model.matrix(Salary ~ ., data = Hitters.test_set)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test_set$Salary - lasso.pred)^2)

#Both linear model and regularization like Lasso have higher test MSE than
#boosting.

#(f)                     

library(randomForest)
bag.hitters <- randomForest(Salary ~ ., data = Hitters.train_set)
mean((Hitters.test_set$Salary - predict(bag.hitters, Hitters.test_set))^2)

#The MSE of bagging is lower than boosting's

```
