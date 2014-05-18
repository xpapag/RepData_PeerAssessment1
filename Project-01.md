Processing and interpreting the personal activity monitoring Data
=================================================================

## Introduction

Loading and preprocessing the data; Read the data in the csv and do the right transformations

```r
ActivityData <- read.table("./activity.csv", sep = ",", header = TRUE, na.strings = NA)
ActivityData$date <- as.Date(ActivityData$date)
head(ActivityData, n = 10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```


## What is mean total number of steps taken per day?

We do the following:

1. Summarize the steps per day by using the plyr package. 

2. Plot the histogram of the sums 

3. Calculate and plot the mean and the median

```r
library(plyr)
df <- ddply(ActivityData, .(date), summarize, sumSteps = sum(steps, na.rm = TRUE))
hist(df$sumSteps, col = "green", breaks = 10, xlab = "Number of steps per day", 
    main = "Histogram of the total number of steps taken each day")
stepsMean <- mean(df$sumSteps, na.rm = TRUE)
stepsMedian <- median(df$sumSteps, na.rm = TRUE)
abline(v = mean(df$sumSteps, na.rm = TRUE), col = "red", lwd = 4)
abline(v = median(df$sumSteps, na.rm = TRUE), col = "magenta", lwd = 4)
```

![plot of chunk histogram](figure/histogram.png) 


The mean value of the total number of steps taken per day is **9354.2295** and the median value of the total number of steps taken per day is **10395**.

### What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(plyr)
library(ggplot2)
df2 <- ddply(ActivityData, .(interval), summarize, avgSteps = mean(steps, na.rm = TRUE))
g <- ggplot(df2, aes(x = df2$interval, y = df2$avgSteps))
g + geom_line() + theme(panel.background = element_rect(colour = "pink")) + 
    labs(x = "Interval") + labs(y = "Average Steps per interval") + labs(title = "Time series plot of the 5-minute interval and the avg num of steps")
```

![plot of chunk timesSeries](figure/timesSeries.png) 


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
df2 <- ddply(ActivityData, .(interval), summarize, avgSteps = mean(steps, na.rm = TRUE))
maxAvgSteps <- max(df2$avgSteps, na.rm = TRUE)
tmp <- df2[df2$avgSteps == maxAvgSteps, ]
maxInterval <- tmp$interval
```

The **835** 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.

### Imputing missing values

```r
totalNumOfNAs <- sum(is.na(ActivityData$steps))
```

There are **2304** missing values in the dataset (i.e. the total number of rows with NAs).

### Filling in all of the missing values in the dataset with 0
What is mean total number of steps taken per day with the missing values filled in with zeros?

```r
library(plyr)
newActivityData <- ActivityData
newActivityData[is.na(newActivityData)] <- 0
df3 <- ddply(newActivityData, .(date), summarize, sumSteps = sum(steps))
hist(df3$sumSteps, col = "green", breaks = 10, xlab = "Number of steps per day", 
    main = "Histogram of the total number of steps taken each day")
nstepsMean <- mean(df3$sumSteps, na.rm = TRUE)
nstepsMedian <- median(df3$sumSteps, na.rm = TRUE)
abline(v = mean(df3$sumSteps, na.rm = TRUE), col = "red", lwd = 4)
abline(v = median(df3$sumSteps, na.rm = TRUE), col = "magenta", lwd = 4)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


The mean value of the total number of steps taken per day is **9354.2295** and the median value of the total number of steps taken per day is **1.0395 &times; 10<sup>4</sup>**.
