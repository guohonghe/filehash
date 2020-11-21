---
title: "PA1_Template"
author: "Guohong He"
date: "11/20/2020"
output: 
  html_document: 
    keep_md: yes
---




```r
library(dplyr)
library(lubridate)
```

## Reproducible Research Course Project 1

Download the data from URL and store it to ./data folder

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!dir.exists("./data")){dir.create("./data")}
if(!file.exists("./data/data.zip")){
    download.file(url=url, destfile="./data/data.zip", method="curl")
}
```
\

#### 1 loading and processing the data

```r
unzip("./data/data.zip", list=TRUE)
```

```
##           Name Length                Date
## 1 activity.csv 350829 2014-02-11 10:08:00
```

```r
activity <- read.csv(unzip("./data/data.zip", "activity.csv"))
head(activity, 2)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
```

Convert integer format of variable *__interval__* to H:Min format

```r
activity$interval <- hm(paste(as.character(activity$interval%/%100),
            ":", as.character(activity$interval%%100), sep=""))
head(activity, 2)
```

```
##   steps       date interval
## 1    NA 2012-10-01       0S
## 2    NA 2012-10-01    5M 0S
```
\

#### 2 What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```r
day.steps <- activity %>% filter(!is.na(steps))%>% group_by(date) %>% 
    summarise(steps=sum(steps))
```

Make a histogram of the total number of steps taken per day

```r
hist(day.steps$steps, breaks=length(day.steps$date), 
     main="Histogram of Steps per Day", xlab= "Steps/Day", ylab="Frequency (Days)")
box()
```

<img src="PA1_ReproducibleResearch_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Calculate the mean and median of the total number of steps taken per day

```r
day.steps.mean <- as.integer(mean(day.steps$steps))
day.steps.median <- as.integer(median(day.steps$steps))
```

The mean of total steps taken per day is *__10766__*.
Its median is *__10765__*.

\

#### 3 What is the average daily activity pattern?

Calculate average steps taken in 5-minute interval each day

```r
act.pattern <- activity %>% filter(!is.na(steps))%>% group_by(interval) %>% 
    summarise(steps=mean(steps))
```

Make a time series plot of the average steps taken each day based on 5-minute interval

```r
plot(as.numeric(act.pattern$interval)/3600, act.pattern$steps, type="l", 
     col="red", main="Average Steps in 5-Minute Interval Each Day (0-24 Hours)", 
     xlab="Hours in a Day", ylab="Steps", xaxt="none")
grid()
axis(1,seq(0,24,3))
```

<img src="PA1_ReproducibleResearch_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

Calculate which 5-minute interval contains the maximum number of steps.

```r
act.pattern.max <- act.pattern[act.pattern$steps==max(act.pattern$steps),]
time <- paste(act.pattern.max$interval@hour,"H:",
              (act.pattern.max$interval@minute-5), "Min to ", 
              act.pattern.max$interval@hour,"H:",
              (act.pattern.max$interval@minute), "Min", sep="") 
steps <- as.integer(act.pattern.max$steps) 
```

Averagingly, at this time period *__8H:30Min to 8H:35Min__* each day, the activity has the maximum number of steps. It is *__206__* steps.

\

#### 4 Imputing missing values

Calculate the total number of missing values in the dataset

```r
number.NA <- activity %>% filter(is.na(steps))%>% nrow()
```

The total number of missing value is *__2304__*.

Create new dataset and filling in all missing values with the mean of steps in that 5-minute interval.

```r
act.modified <- activity
for (i in 1:nrow(act.modified)) {
    if(is.na(act.modified$steps[i])) {
        act.modified$steps[i] <- 
        mean(act.modified$steps[act.modified$interval==act.modified$interval[i]], 
            na.rm = TRUE)
    }
}
```

Re-calculate the total number of steps taken per day (averaged over two months)

```r
day.steps.2 <- act.modified %>% group_by(date) %>% summarise(steps=sum(steps))
```

Make another histogram of the total number of steps taken each day based on modified data

```r
hist(day.steps.2$steps, breaks=length(day.steps.2$date), 
     main="Histogram of Steps per Day based on Modified Data", 
     xlab= "Steps/Day", ylab="Frequency (Days)")
box()
```

<img src="PA1_ReproducibleResearch_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

Re-calculate the mean and median of the total number of steps taken per day

```r
day.steps.2.mean=as.integer(mean(day.steps.2$steps))
day.steps.2.median=as.integer(median(day.steps.2$steps))
```

Since we used the average steps of 5-minutes interval in a day to replace those missing data in the same time period, the mean of steps per day doesn't change (*__10766__* from modified dataset vs *__10766__* from original dataset), but its median from modified dataset will be the same of its current mean (*__10766__* from modified dataset vs *__10765__* from original dataset).

\

#### 5 Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable *__Weekdays__* in the dataset with two levels – “weekday” and “weekend” 

```r
wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
we <- c("Saturday", "Sunday")
act.modified <- cbind(act.modified, weekdays=act.modified$date)
act.modified$weekdays[weekdays(as.Date(act.modified$date)) %in% we]<-"weekend"
act.modified$weekdays[weekdays(as.Date(act.modified$date)) %in% wd]<-"weekday"
```

Calculate the average number of steps taken, averaged across all weekday days or weekend days

```r
act.modified <- act.modified %>% group_by(interval, weekdays) %>% 
  summarise(steps=mean(steps))
```

Make a time series plot of steps based on the 5-minute interval

```r
plot(as.numeric(act.modified$interval[act.modified$weekdays=="weekday"])/3600, 
     act.modified$steps[act.modified$weekdays=="weekday"], type="l", 
     col="red", main="Average Steps in 5-Minute Interval per Day (0-24 Hours)", 
     xlab="Hours", ylab="Steps", xaxt="none")
grid()
axis(1,seq(0,24,3))
par(new=TRUE)
lines(as.numeric(act.modified$interval[act.modified$weekdays=="weekend"])/3600, 
      act.modified$steps[act.modified$weekdays=="weekend"], type="l", 
      col="blue")
legend("topright",legend=c("Average Steps in a Weekday", "Average Steps in a Weekend"), 
       lwd=1, col=c("red", "blue"), bty="n", xjust=1)
```

<img src="PA1_ReproducibleResearch_files/figure-html/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />
