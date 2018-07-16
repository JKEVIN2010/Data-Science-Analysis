Predictive Analytics
================
Kevin Mekulu
July 14, 2018

Predictive Analysis of The Dataset:
===================================

Introduction:
=============

Our main goal in this project will be to predict wether or not a person makes

over $50,000 a year. Since we are dealing with a classification problem with

binary outcome(a person can make either over 50,000 a year or less than 50,000 a year),

we will utilize logistic regression which is the machine learning algorithm of

choice for these types of problems.

Data Preprocessing:
-------------------

In this step, we will perform some data preprocessing in order to identify

significant variables that will be useful for modeling purposes. We will be using histograms to quickly visualize the distribution of different variables

``` r
#Univariate Analysis
r1 <- ggplot(dat.train,aes(age)) + geom_histogram(binwidth = 0.7) #The distribution seems to be left skewed

r2 <- ggplot(dat.train,aes(log(fnlwgt))) + geom_histogram(binwidth = 0.5) #Use log transformation to normalize data

r3 <- ggplot(dat.train, aes(log(capital.gain + 1))) + geom_histogram(binwidth = 0.5) #MOstly 0's not significant

r4 <- ggplot(dat.train, aes(log(capital.loss + 1))) + geom_histogram(binwidth = 0.5) #Mostly 0's not significant feature

r5 <- ggplot(dat.train, aes(hours.per.week)) + geom_histogram(binwidth = 0.5) 

r6 <- ggplot(dat.train, aes(education.num)) + geom_histogram(binwidth = 0.5) #Assume it corresponds to number of years in school

grid.arrange(r1, r2, r3, r4, r5, r6, ncol=3)
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-3-1.png)

We notice that Capital gain and capital loss have very narrow

distributions(heavily skewed) and over 90% of observations are 0's therefore they will be excluded from the analysis.

Analysis of the categorical variables Work Class, Education and occupation
--------------------------------------------------------------------------

Let's take a closer look at these variables using bar charts which are visualization tools used by statisticians to analyze the distribution of categorical variables and see if we should exclude any variable to improve our final model.

``` r
h1 <- ggplot(dat.train, aes(x=workclass)) + ggtitle("Work Class") + xlab("Work Class") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(dat.train$workclass)))

h2 <- ggplot(dat.train, aes(x=education)) + ggtitle("Education") + xlab("Education") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +
  scale_x_discrete(limits = rev(levels(dat.train$education)))

h3 <- ggplot(dat.train, aes(x=occupation)) + ggtitle("Occupation") + xlab("Occupation") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +
  scale_x_discrete(limits = rev(levels(dat.train$occupation)))

#h4 <- ggplot(dat.train, aes(x=native.country)) + ggtitle("Native Country") + xlab("Native Country") +
  #geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() + 
  #scale_x_discrete(limits = rev(levels(dat.train$native.country))) 
  
grid.arrange(h1, h2, h3, ncol=2)
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-4-1.png)

We take a more detailed look at the variable native country.

``` r
h5 <- ggplot(dat.train, aes(x=native.country)) + ggtitle("Native Country") + xlab("Native Country") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(dat.train$native.country))) 

h5
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-5-1.png)

We immediately notice that distribution of the variable native country is very narrow.

Most observations are from the United States therefore the variable will be

excluded from our analysis.

``` r
#We excluded all three variables from the analysis
dat.train$capital.gain <- NULL
dat.train$capital.loss <- NULL
dat.train$native.country <- NULL
```

Distribution Analysis of Other Categorical Variables:
-----------------------------------------------------

We will analyze other categorical variables to see if they should be included

in our analysis. This time, we will be using pie charts to further analyze

different distributions.

``` r
p1 <- ggplot(dat.train, aes(x=factor(1), fill=marital.status)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) + 
  xlab("") + ylab("") + ggtitle("Marital Status") 

p2 <- ggplot(dat.train, aes(x=factor(1), fill=relationship)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) + 
  xlab("") + ylab("") + ggtitle("Relationship") 

p3 <- ggplot(dat.train, aes(x=factor(1), fill=race)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) + 
  xlab("") + ylab("") + ggtitle("Race")

p4 <- ggplot(dat.train, aes(x=factor(1), fill=sex)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) + 
  xlab("") + ylab("") + ggtitle("Sex")
grid.arrange(p1, p2, p3, p4, ncol=2)
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-7-1.png) We notice that distributions of these categorical variables don't display any

extreme skewness therefore will be conserved for our regression analysis.

Correlation Analysis of Numerical Variables:
--------------------------------------------

It's always a good practice to identify highly correlated variables if any that

could potentially negatively affect our final model. Let's see if there are any

correlated numerical variables in our dataset.

``` r
#Correlation Analysis Between Numerical Variables:
numeric.var <- sapply(dat.train, is.numeric) # Find numerical variables

corr.matrix <- cor(dat.train[,numeric.var])  # Calculate the correlation matrix

corrplot(corr.matrix, main="\n Correlation Plot for Numerical Variables")  ## Correlation plot
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
par(mar=c(5.1,4.1,4.1,2.1))  # Restore plot margins
```

There seems to be no visible correlation between numerical variables in the dataset.

Correlation Analysis Between Categorical Variables:
---------------------------------------------------

Let's perform the same task for categorical variables. We will be using boxplots for this specific task.

``` r
par(mfrow=c(2,2)) #Arrange plots in a grid
boxplot(dat.train$age~dat.train$class, main="Age vs. Income Class", 
        xlab="Income Class", ylab="Age")

boxplot(dat.train$education.num~dat.train$class, main="Years of Education vs. Income Class", 
        xlab="Income Class", ylab="Years of Education")

boxplot(dat.train$fnlwgt~dat.train$class,log = "y", main="log(Weight) vs. Income Class", 
        xlab="Income Class", ylab="log(Weight)")

boxplot(dat.train$hours.per.week~dat.train$class, main="Hours per Week vs. Income Class", 
        xlab="Income Class", ylab="Hours per Week")
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
par(mfrow=c(1,1))
```

We notice that income class and weight have nearly identical distribution.

``` r
par(mfrow=c(3,2))
par(las=1)  # horizontal axis labels

plot(table(dat.train$class, dat.train$workclass), main="Work Class vs. Income Class", cex=1.5)

plot(table(dat.train$class, dat.train$marital.status), main="Marital Status vs. Income Class", cex=1.5)

plot(table(dat.train$class, dat.train$occupation), main="Occupation vs. Income Class", cex=1.5)

plot(table(dat.train$class, dat.train$relationship), main="Relationship vs. Income Class", cex=1.5)

plot(table(dat.train$class, dat.train$race), main="Race vs. Income Class", cex=1.5)

plot(table(dat.train$class, dat.train$sex), main="Sex vs. Income Class", cex=1.5)
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
par(las=0)  # parallel axis labels
```

There is no significant correlation between these categorical variables

therefore, we will consider all these variables.

Predictive Analytics:
=====================

We will use logistic regression as planned to train our model on the training set and use it on unseen data (the test set.)

1- Logistic Regression
----------------------

Logistic regression is modeling the probability that an individual makes more than $50,000 annually. In other words, a response closer to 1 indicates higher chance of making over $50,000, while a response closer to 0 indicates a higher chance of making less than 50,000. Therefore, a threshold of 0.5 is used to determine whether an individual is predicted to make more than $50,000 annually or not. A confusion matrix and a ROC curve are presented to evaluate how well the model predicts income.

The metrics used will be AUROC, accuracy and a confusion matrix.

``` r
set.seed(1234) #Used to ensure the reproducibility of results

fit1 <- glm(class ~ ., data = dat.train, family = binomial('logit'), na.action = na.omit) #Logistic Regression Model

#Confusion Matrix:
prob <- predict(fit1, dat.test, type = 'response')

pred <- rep('<=50K', length(prob))

pred[prob>=.5] <- '>50K'

tb <- table(pred, dat.test$class)

tb  #Displays confusion Matrix
```

    ##        
    ## pred     <=50K.  >50K.
    ##   <=50K   11465   1701
    ##   >50K      970   2145

``` r
accuracy <- sum(diag(tb)) / sum(tb)  #Compute the model's accuracy

accuracy # Display Accuracy
```

    ## [1] 0.8359437

The model produced an accuracy of about 83.5% Logistic Regression Inference: The model gives higher accuracy on unseen data when it has all the predictors included. The model's accuracy decreases when some of the predictors are removed.

### ROC Curve Plot and The Area Under The Curve (AUC)

The ROC curve is a plot of sensitivity (the ability of the model to predict an

event correctly) versus 1-specificity for the possible cut-off classification

probability values.

The ROC curve is more informative than the classification table (Confusion

Matrix) since it summarizes the predictive power for all possible probability

values.

``` r
pr <- prediction(prob, dat.test$class)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)
```

![](Predictive_Aunalytics_files/figure-markdown_github/unnamed-chunk-12-1.png)

### AUC Computation:

The AUC is basically just tells us how frequently a random draw from our

predicted response probabilities on your 1-labeled data (&gt; 50k) will be greater

than a random draw from your predicted response probabilities on your 0-labeled

data (&lt;= 50k).

``` r
#AUC 
auc <- performance(pr, measure = "auc")

auc <- auc@y.values[[1]]

auc #Display AUC
```

    ## [1] 0.8854446

The area under the curve is about 0.88 which signals a very strong predictive

ability of our model.

Conclusion:
===========

We have realized that logistic regression has the best auc and therefore is the most appropriate algorithm for this problem.

A good way to improve our model's accuracy would be to optimize parameters contained in different models.

Further feature engineering could also be explored in order to improve the predictive power of our models.

While there are others very powerful and sophisticated machine learning algorithms out there,

The very traditional logistic regression model seems to be the most efficient

and very easy to explain to people without a technical background.
