Segmentation Analysis
================
Kevin Mekulu
July 13, 2018

Segmentation Study of the dataset
=================================

Introduction:
=============

In this segmentation study, we will try to retrieve actionable insights from the

given datasets by performing an exploratory data analysis using various

visualization tools and libraries contained in R, and coming up with pertinent

questions throughout the process to guide our analysis.

Exploratory Data Analysis:
==========================

Age vs Income: How does a person's age affect his/her income potential ?
------------------------------------------------------------------------

We will start by analyzing the potential relationship between people's age and

their income potential, especially wether or not they make over $50,000 per year. Since we are dealing two continuous variables in this case, we will use a

histogram to accurately answer the question.

``` r
setwd("C:/Users/jkevi_000/Documents") 

rm(list=ls(all=TRUE)) # Clear memory

#Load Visualization Tools
library(ggplot2)

library(gridExtra)

library(plyr)


#Load Datasets

dat.train <- read.csv("au_train.csv")

dat.test <- read.csv("au_test.csv")

dat.full <- rbind(dat.train,dat.test) #Merge both datasets

# Age vs Income class
ggplot(dat.train) + aes(x=as.numeric(age), group= class, fill= class) + 
  geom_histogram(binwidth=1, color='black')
```

![](Segmentation_Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#Age vs Sex
ggplot(dat.full) + aes(x=as.numeric(age), group=sex, fill=sex) + 
geom_histogram(binwidth=1, color='black')
```

![](Segmentation_Analysis_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
boxplot (age ~ class, data = dat.train, 
         main = "Age distribution for different income levels",
         xlab = "Income Levels", ylab = "Age", col = "blue")
```

![](Segmentation_Analysis_files/figure-markdown_github/unnamed-chunk-1-3.png)

From the graphs, we notice that The majority of participants make less or equal

to $50,000. The people that make over 50k a year are between 30 and 55 years

old. We can assume that they are mostly mid-career professionals with a few

years of experience under their belt.So the more time spent working, the more

likely a person will make over 50k per year.

More experience in the workplace leads to a higher salary.

We also notice that females are very underepresented in the dataset which might

signal a possible bias during the survey.

Occupation vs Income: Does a highly specialized job guarantee a higher income?
------------------------------------------------------------------------------

In this case, we take a closer look at the relationship between occupation and

income potential.

``` r
qplot(class, data = dat.train, fill = occupation) + facet_grid (. ~ occupation)
```

![](Segmentation_Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

It's pretty clear that highly specialized jobs do not guarantee a higher income

In fact, we can see a lot people with less specialized jobs making over 50k per

year and vice versa. As expected, executives and managers have the largest

amount of people making over 50k, closely followed by professors.

Income vs Education: Does higher education help earn more money?
----------------------------------------------------------------

We will explore this very popular question using our usual visualization

tools(histograms and bar charts) to explain the not-so-obvious relationship

between income and education.

``` r
qplot(class, data = dat.train, fill = education) + facet_grid (. ~ education)
```

![](Segmentation_Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

We observe that the majority of people making over 50,000 a year have at

Bachelor's degree closely followed by a high school degree.

We also notice that they are higher in number when compared to master's and phD degree holders which is expected because a typical class of graduates will be

about 90% bachelor's degree holders, 30% master's and 10 % PhD.

Therefore we can imply that education level for the most part affects a person's income potential.

Income vs Relationship vs Race:
-------------------------------

Let's analyze how relationship and race are connected a person's income

potential.

``` r
qplot(class, data = dat.train, fill = relationship) + facet_grid (. ~ race)
```

![](Segmentation_Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

We notice that the majority of the people making over 50k a year are white male

married workers

closely followed by black married workers and pacific islanders.
