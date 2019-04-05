Peer-graded Assignment: Course Project 1
================
Liberty Zhang

Loading and preprocessing the data
----------------------------------

``` r
activityData <- read.csv("activity.csv")
```

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Make a histogram of the total number of steps taken each day

``` r
library(ggplot2)
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm = TRUE)
qplot(steps, data = stepsPerDay, binwidth = 1500) + xlab("Steps") + ylab("Frequency") + ggtitle("Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

1.  Calculate and report the mean and median total number of steps taken per day

``` r
#Mean
mean(stepsPerDay$steps)
```

    ## [1] 10766.19

``` r
#Median
median(stepsPerDay$steps)
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
stepsByInterval <- aggregate(steps ~ interval, activityData, mean, na.rm = TRUE)
plot(stepsByInterval$interval, stepsByInterval$steps, type = "l", main = "Average Number of Steps Taken Across All Day", xlab = "Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maxSteps <- which.max(stepsByInterval$steps)

stepsByInterval[maxSteps, ]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(activityData))
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. **I will impute the missing data with the mean of the interval that the missing data belongs to.**

2.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
activityDataNew <- activityData
activityDataNew$steps <- impute(activityData$steps, mean)
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
stepsPerDayNew <- tapply(activityDataNew$steps, activityDataNew$date, sum)
qplot(stepsPerDayNew, binwidth = 1500) + xlab("Steps") + ylab("Frequency") + ggtitle("Total Number of Steps Taken Each Day (Imputed)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
mean(stepsPerDayNew)
```

    ## [1] 10766.19

``` r
median(stepsPerDayNew)
```

    ## [1] 10766.19

**Based on the calculation, the mean does not change and the median varies just a little from the first part.**

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
activityDataNew$dateType <-  ifelse(as.POSIXlt(activityDataNew$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

``` r
finalData <- aggregate(steps ~ interval + dateType, data=activityDataNew, mean)
qplot(interval, steps, data = finalData, geom=c("line"), xlab = "Interval", ylab = "Steps") +
  facet_wrap(~ dateType, ncol = 1)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)
