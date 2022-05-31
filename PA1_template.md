---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
measuredData <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))
```

As the following 2 questions ask to ignore the missing/NA values, removing NA values and converting the list to a data.table


```r
library(data.table)
setDT(measuredData)
nonImputedData <- measuredData[!is.na(measuredData$steps),]
```

## What is mean total number of steps taken per day?

```r
totalStepsDaily <- nonImputedData[ ,list(daily_steps=sum(steps)), by=date]
cat(" Mean of total steps per day =", mean(totalStepsDaily[["daily_steps"]]), "\n",
    "Median of total steps per day =", median(totalStepsDaily[["daily_steps"]]) )
```

```
##  Mean of total steps per day = 10766.19 
##  Median of total steps per day = 10765
```


## What is the average daily activity pattern?

```r
avgStepsDaily <- nonImputedData[ ,list(daily_steps=mean(steps)), by=interval]
print(avgStepsDaily)
```

```
##      interval daily_steps
##   1:        0   1.7169811
##   2:        5   0.3396226
##   3:       10   0.1320755
##   4:       15   0.1509434
##   5:       20   0.0754717
##  ---                     
## 284:     2335   4.6981132
## 285:     2340   3.3018868
## 286:     2345   0.6415094
## 287:     2350   0.2264151
## 288:     2355   1.0754717
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
