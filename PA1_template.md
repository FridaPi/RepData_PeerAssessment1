---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    self_contained: no
---

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
activity$day<-weekdays(activity$date)
```

## What is the mean total number of steps taken per day?

```r
library(ggplot2)
activityAgg <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
qplot(steps, data = activityAgg, binwidth = 1200)+
        xlab("Steps Taken") +
        ylab("Count")+ 
        theme_bw()
```

![](PA1_template_files/figure-html/Steps per Day-1.png)<!-- -->

```r
mean<-mean(activityAgg$steps)
median<-median(activityAgg$steps)
```

The mean and median number of steps taken each day is $1.0766189\times 10^{4}$ and $1.0765\times 10^{4}$ respectively.  


## What is the average daily activity pattern?

```r
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
        geom_line() +
        xlab("5-Minute Interval") +
        ylab("Average Number of Steps Taken")+ 
        theme_bw()
```

![](PA1_template_files/figure-html/Average Daily Pattern-1.png)<!-- -->

```r
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```

The 5-minute interval, on average across all the days in the dataset, with the maximum number of steps is $8:35$.

## Imputing missing values

```r
num_na<-sum(is.na(activity))
```

The number of N/A in the data is $2304$.  


### Impute missing steps

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
imputeSteps <- activity %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

###Histogram with immputed Data

```r
imputed_total_steps <- imputeSteps %>% group_by(date) %>% summarise(daily_steps = sum(steps))

ggplot(imputed_total_steps, aes(daily_steps)) + 
  geom_histogram(binwidth = 2000) + 
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")
```

![](PA1_template_files/figure-html/Historgram with Imputed Data-1.png)<!-- -->

```r
imputed_mean = mean(imputed_total_steps$daily_steps, na.rm=TRUE)
imputed_median = median(imputed_total_steps$daily_steps, na.rm=TRUE)
```

The new mean and median number of steps taken each day is $1.0766189\times 10^{4}$ and $1.0766189\times 10^{4}$ respectively.  



## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
day_of_week <- imputeSteps%>%
  mutate(
    date = ymd(date),
    weekday_or_weekend = case_when(wday(date) %in% 2:6 ~ "Weekday",
                                   wday(date) %in% c(1,7) ~ "Weekend")
  ) %>% select(-date) %>%
  group_by(interval, weekday_or_weekend) %>%
  summarise(
    steps = mean(steps)
  )
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
ggplot(day_of_week, aes(interval, steps)) + 
  geom_line() + 
  facet_wrap(~weekday_or_weekend, nrow = 2) +
  xlab("5-Minute Intervals") + 
  ylab("Average Number of Steps")
```

![](PA1_template_files/figure-html/Weekdays and Weekends-1.png)<!-- -->





