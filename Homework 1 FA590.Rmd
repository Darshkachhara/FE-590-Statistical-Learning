---
output: pdf_document

---

```{r global_options, include=FALSE}
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
```



# FE590.  Assignment #1.


## `r format(Sys.time(), "%Y-%m-%d")`

##I pledge on my honor that I have not given or received any unauthorized assistance on this assignment/examination. I further pledge that I have not copied any material from a book, article, the Internet or any other source except where I have expressly cited the source.

#By filling out the following fields, you are signing this pledge.  No assignment will get credit without being pledged.

#Name: Darsh Kachhara

##CWID:10474181

##Date:02/11/2022

# Question 1

## Question 1.1
```{r}
CWID = 10474181 #Place here your Campus wide ID number, this will personalize
#your results, but still maintain the reproduceable nature of using seeds.
#If you ever need to reset the seed in this assignment, use this as your seed
#Papers that use -1 as this CWID variable will earn 0's so make sure you change
#this value before you submit your work.
personal = CWID %% 10000
set.seed(personal)
```
Generate a vector `x` containing 10,000 realizations of a random normal variable with mean 1.0 and standard deviation 3.0, and plot a histogram of `x` using 100 bins.

##(Note that the following two fields can be added wherever you desire to show a solution.  You can use the first for a written response, and the second for showing R code and its output.  Some questions will require just one, and some both.  I will not always provide you with these, but you can add them at your discretion whereven necessary.  If it makes sense to do the R code first then thats fine.  If you want to include multiple of each, thats ok too.  Do what you feel is necessary to answer the question fully.)

\textcolor{red}{Solution:} 

```{r}
#response
#1.1

x <-rnorm(personal,1.0,3.0)
hist(x,100)
```


## Question 1.2
Calculate the mean and standard deviation of these 10000 values.  Do your answers make sense?

## \textcolor{red}{Solution:}

```{r}
# my response
mean(x)
sd(x)

# The answer make sense as they have been calculated keeping in mind the sample mean and std formula
```





## Question 1.3
Using the `sample` function, take out 10 random samples of 500 observations each (with replacement).  Create a vector of the means of each sample.  Calculate the mean of the sample means and the standard deviation of the sample means.  What do you observe about these results?

## \textcolor{red}{Solution:}

```{r}
# Enter your R code here!
sample_1<-sample(x,500, replace = TRUE, prob = NULL)
sample_2<-sample(x,500, replace = TRUE, prob = NULL)
sample_3<-sample(x,500, replace = TRUE, prob = NULL)
sample_4<-sample(x,500, replace = TRUE, prob = NULL)
sample_5<-sample(x,500, replace = TRUE, prob = NULL)
sample_6<-sample(x,500, replace = TRUE, prob = NULL)
sample_7<-sample(x,500, replace = TRUE, prob = NULL)
sample_8<-sample(x,500, replace = TRUE, prob = NULL)
sample_9<-sample(x,500, replace = TRUE, prob = NULL)
sample_10<-sample(x,500, replace = TRUE, prob = NULL)
mean_1<-mean(sample_1)
mean_2<-mean(sample_2)
mean_3<-mean(sample_3)
mean_4<-mean(sample_4)
mean_5<-mean(sample_5)
mean_6<-mean(sample_5)
mean_7<-mean(sample_6)
mean_8<-mean(sample_7)
mean_9<-mean(sample_8)
mean_10<-mean(sample_10)
mean_vector <- c(mean_1,mean_2,mean_3,mean_4,mean_5,mean_6,mean_7,mean_8,mean_9,mean_10)
mean_vector
mean_sample_means <- mean(mean_vector)
mean_sample_means
sd(mean_vector)

# if there were more samples , by the law of large numbers the mean of samples would = to the mean of 
# population in this solution however , that is not the case.
```
#Question 2

##Question 2.1

Create a script that creates a vector of the values from 1 to 100 using a for loop.

## \textcolor{red}{Solution:}

```{r}
# Enter your R code here!
fx<- 0
length(fx) <- 100
for(i in 0:length(fx))
{
  fx[i] <- i
}
fx

```

##Question 2.2

Create a script that creates a vector of the first 20 Fibbonaci numbers.

## \textcolor{red}{Solution:}

```{r}
# Enter your R code here!
fx1<- numeric(20)
fx1[1]<-fx1[2]<-1
for(i in 3:20)
{
  fx1[i]<-fx1[i-2]+fx1[i-1]
}

fx1
    
```






# Question 3
Download the FinDataAdjCl.csv data set from the Canvas shell, as you'll need it for this assignment

## Question 3.1

Load the csv file and use the summary command to analyze three stocks from this data set (your choice which 3)



```{r}
# Enter your R code here!
library(readr)
FinDataAdjCl <- read_csv("FinDataAdjCl.csv")
stocks_data <- FinDataAdjCl
stocks_data$ROKU=NULL
stocks_data
summary(stocks_data$NKE)
summary(stocks_data$MSFT) 
summary(stocks_data$GE)
```



## Question 3.2

Split the data into two pieces, one for the dates from 2015 and earlier and the other for the dates after 2015.  Use these subsets to rerun your summary command for each group for the same stocks you used in 3.1

## \textcolor{red}{Solution:}

```{r}
# Enter your R code here!
stocks_data_split_1 <- stocks_data[1:468,]
stocks_data_split_2 <- stocks_data[469:1258,]
a<-summary(stocks_data_split_1$NKE)
b<-summary(stocks_data_split_1$MSFT) 
c<-summary(stocks_data_split_1$GE)
d<-summary(stocks_data_split_2$NKE)
e<-summary(stocks_data_split_2$MSFT) 
f<-summary(stocks_data_split_2$GE)
```

## Question 3.3

What do you see in these results? Are the company's prices changing?  Do you think this is just inflation or is it more noticable in one or the other?

## \textcolor{red}{Solution:}

```{r}
# Enter your R code here!
diff_in_mean <- mean(stocks_data_split_2$NKE)-mean(stocks_data_split_1$NKE)
(diff_in_mean/mean(stocks_data_split_1$NKE)*100)
#The mean of the two data sets vary by 34.1% overall. In the USA , inflation has between 2-3% 
#yoy pre-covid, so there is definitely some positive movement for NKE.
diff_in_mean_1 <- mean(stocks_data_split_2$MSFT)-mean(stocks_data_split_1$MSFT)
(diff_in_mean_1/mean(stocks_data_split_1$MSFT)*100)
#The mean of the two data sets vary by 83.09% overall. In the USA , inflation has between 2-3% 
#yoy pre-covid, so there is definitely some positive movement for NKE.
diff_in_mean_2 <- mean(stocks_data_split_2$GE)-mean(stocks_data_split_1$GE)
(diff_in_mean_2/mean(stocks_data_split_2$GE)*100)
# There is 9.09% fall seen in the stock, this could be due to various reasons, inflation isn't one of them

```



#Question 4

##Question 4.1

Download and use the "fakedataeasy.csv" file found under Modules section of Canvas.  Using this data set, find the mean and standard deviation of each of the variables in this data set.  

## \textcolor{red}{Solution:}

```{r}
# Enter your R code here!
fake_data_frame <- read_csv("fakedataeasy.csv")
fake_data_frame
head(fake_data_frame)
mean_of_all <- c(mean(fake_data_frame$y),mean(fake_data_frame$x5),mean(fake_data_frame$x6),mean(fake_data_frame$x7), mean(fake_data_frame$x8))
names(mean_of_all) <- c("y","x5","x6","x7","x8")
std_for_all <- c(sd(fake_data_frame$y),sd(fake_data_frame$x5),sd(fake_data_frame$x6),sd(fake_data_frame$x7), sd(fake_data_frame$x8))
info_all <- matrix(c(mean_of_all,std_for_all),nrow=2,ncol=5)
rownames(info_all) <- c("mean","std")
colnames(info_all) <- c("y","x5","x6","x7","x8")
info_all

```

