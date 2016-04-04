# Reproducible Research: Peer Assessment 1
hapstermeister  


## Loading and preprocessing the data

```r
activities <- read.csv("activity.csv")
activities$date <- as.Date(activities$date)
```

## What is mean total number of steps taken per day?

### Make a histogram of the total number of steps taken each day

```r
totStepsByDay <- aggregate(steps ~ date, data = activities, sum)
png("figure/Histogram_Total_Number_of_Steps.png", width = 480, height = 480)
hist(x = totStepsByDay$steps,
     breaks = 15,
     main = "Total number of steps taken each day",
     xlab = "Total Steps",
     ylab = "Number of Days"
)
dev.off()
```

```
## png 
##   2
```
![plot of chunk Histogram_Total_Number_of_Steps](figure/Histogram_Total_Number_of_Steps.png)

### Calculate and report the mean and median total number of steps taken per day

```r
meanSteps <- mean(totStepsByDay$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps <- median(totStepsByDay$steps)
medianSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanStepsByInterval <- aggregate(steps ~ interval, data = activities, mean)
png("figure/Plot_Avg_Steps_By_Interval.png", width = 480, height = 480)
plot(x = meanStepsByInterval$interval,
     y = meanStepsByInterval$steps,
     type = "l",
     main = "Average number of steps taken, averaged across all days",
     xlab = "Interval",
     ylab = "Average number of steps"
)
dev.off()
```

```
## png 
##   2
```
![plot of chunk Plot_Avg_Steps_By_Interval](figure/Plot_Avg_Steps_By_Interval.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meanStepsByInterval[which.max(meanStepsByInterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activities$steps))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filledActivities <- activities
fillNA <- function(x) {
   output <- meanStepsByInterval[which(meanStepsByInterval == x),]$steps
   return(output)
}
filledActivities$steps <- ifelse(is.na(activities$steps),
                               fillNA(activities$interval),
                               activities$steps)
```

### Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totFilledStepsByDay <- aggregate(steps ~ date, data = filledActivities, sum)
png("figure/Histogram_Total_Number_of_Filled_Steps.png", width = 480, height = 480)
hist(x = totFilledStepsByDay$steps,
     breaks = 15,
     main = "Total number of steps taken each day",
     xlab = "Total Steps",
     ylab = "Number of Days"
)
dev.off()
```

```
## png 
##   2
```
![plot of chunk Histogram_Total_Number_of_Filled_Steps](figure/Histogram_Total_Number_of_Filled_Steps.png)


```r
meanFilledSteps <- mean(totFilledStepsByDay$steps)
meanFilledSteps
```

```
## [1] 10766.19
```

```r
medianFilledSteps <- median(totFilledStepsByDay$steps)
medianFilledSteps
```

```
## [1] 10766.19
```

```r
meanFilledSteps - meanSteps
```

```
## [1] 0
```

```r
medianFilledSteps - medianSteps
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
filledActivities$weekindicator <- ifelse(weekdays(filledActivities$date) %in% c('Saturday','Sunday'),
                                             'weekend',
                                             'weekday')
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
weekdaysData <- subset(aggregate(steps ~ interval + weekindicator, 
                                 data = filledActivities, 
                                 mean), 
                       weekindicator == 'weekday')
weekendsData <- subset(aggregate(steps ~ interval + weekindicator, 
                                  data = filledActivities, 
                                  mean), 
                       weekindicator == 'weekend')
png("figure/Plot_Total_Number_of_Steps_By_Interval_And_Week_Indicator.png", width = 480, height = 480)
par(mfcol = c(2,1), mar = c(2,2,2,2))
plot(x = weekendsData$interval, 
     y = weekendsData$steps, 
     type = "l", 
     ylab = "Average Steps",
     main = "Average Number of Steps Taken during the Weekend")
plot(x = weekdaysData$interval, 
     y = weekdaysData$steps, 
     type = "l",
     xlab = "Interval", 
     ylab = "Average Steps",
     main = "Average Number of Steps Taken during the Weekdays")
dev.off()
```

```
## png 
##   2
```
![plot of chunk Plot_Total_Number_of_Steps_By_Interval_And_Week_Indicator](figure/Plot_Total_Number_of_Steps_By_Interval_And_Week_Indicator.png)
