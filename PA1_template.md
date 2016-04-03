---
title: "Reproducible Research"
author: "Pratap Parne"
date: "April 2, 2016"
output: html_document
---


```r
##knitr::opts_chunk$set(echo = TRUE)

## R Markdown


## Read the csv file data ```
Activity <- read.csv("activity.csv")
## Excluding NA from the data
Activity$date <- as.Date(Activity$date)
ActivityExclNA <- na.omit(Activity)

##Calculate the total number of steps taken per day
TotalStepsPerDay <- tapply(ActivityExclNA$steps,ActivityExclNA$date,sum)

## Histogram from Total Steps per Day

hist(TotalStepsPerDay, breaks =10)
```

![plot of chunk setup](figure/setup-1.png)

```r
## Mean of the Total No of Steps per day
mean(TotalStepsPerDay)
```

```
## [1] 10766.19
```

```r
## Media of the Total No of Steps per day
median(TotalStepsPerDay)
```

```
## [1] 10765
```

```r
## Calculating Average number of Steps by interval
AvgStepsPerInterval <- tapply(ActivityExclNA$steps,ActivityExclNA$interval,mean)
StepsPerInterval <- data.frame(unique(ActivityExclNA$interval),AvgStepsPerInterval)
colnames(StepsPerInterval) <- c("Interval","AvgSteps")

## Plotting the graph for Average steps by Interval
plot(StepsPerInterval$Interval,StepsPerInterval$AvgSteps,type="l",xlab ="Interval", ylab ="AvgStepsByInterval",col="red")
```

![plot of chunk setup](figure/setup-2.png)

```r
##Finding the interval with maximum number of Steps
subset(StepsPerInterval,StepsPerInterval$AvgSteps == max(StepsPerInterval$AvgSteps))
```

```
##     Interval AvgSteps
## 835      835 206.1698
```

```r
## Displaying the number of missing values in DataSet
sapply(Activity,function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

```r
## Filling missing data with the average number of steps per interval
ActivityWithNAFill <- Activity
ActivityWithNAFill$steps[is.na(ActivityWithNAFill$steps)] <- StepsPerInterval[match(ActivityWithNAFill$interval,StepsPerInterval$Interval),2]
```

```
## Warning in ActivityWithNAFill$steps[is.na(ActivityWithNAFill$steps)] <-
## StepsPerInterval[match(ActivityWithNAFill$interval, : number of items to
## replace is not a multiple of replacement length
```

```r
##Calculate the total number of steps taken per day with missing data filled in
TotalStepsPerDaywithNAFill <- tapply(ActivityWithNAFill$steps,ActivityWithNAFill$date,sum)

## Histogram from Total Steps per Day with missing data filled in

hist(TotalStepsPerDaywithNAFill, breaks =10)
```

![plot of chunk setup](figure/setup-3.png)

```r
## Mean of the Total No of Steps per day with missing data filled in
mean(TotalStepsPerDaywithNAFill)
```

```
## [1] 10766.19
```

```r
## Media of the Total No of Steps per day with missing data filled in
median(TotalStepsPerDaywithNAFill)
```

```
## [1] 10766.19
```

```r
## There is no big difference. Mean and Median are same once we fill in the missing data and it matches with the mean of the vector
## we eliminated the NA's from  whereas median is slightly less

## Adding a day column to the dataframe to identify weekdays vs weekend
ActivityWithNAFill$day <- weekdays(ActivityWithNAFill$date)
ActivityWithNAFill$day <-as.factor(ActivityWithNAFill$day)
levels(ActivityWithNAFill$day) <- list( Weekend  = c("Saturday","Sunday"),Weekday =c("Monday","Tuesday","Wednesday","Thursday","Friday"))

## Calculating Average number of Steps by interval by Weekday and Weekend
WD <- subset(ActivityWithNAFill,ActivityWithNAFill$day =="Weekday")
AvgStepsPerIntervalByWD <- tapply(WD$steps,WD$interval,mean)
WDStepsPerInterval <- data.frame(unique(WD$interval),AvgStepsPerIntervalByWD)
colnames(WDStepsPerInterval) <- c("Interval","AvgSteps")

WN <- subset(ActivityWithNAFill,ActivityWithNAFill$day =="Weekend")
AvgStepsPerIntervalByWN <- tapply(WN$steps,WN$interval,mean)
WNStepsPerInterval <- data.frame(unique(WN$interval),AvgStepsPerIntervalByWN)
colnames(WNStepsPerInterval) <- c("Interval","AvgSteps")


## Plotting the graph for Average steps by Interval
par(mfrow =c(2,1))

plot(WDStepsPerInterval$Interval,WDStepsPerInterval$AvgSteps,type="l",xlab ="Interval", ylab ="AvgStepsByInterval",col="red",main ="Weekdays")

plot(WNStepsPerInterval$Interval,WNStepsPerInterval$AvgSteps,type="l",xlab ="Interval", ylab ="AvgStepsByInterval",col="red",main ="Weekend")
```

![plot of chunk setup](figure/setup-4.png)
