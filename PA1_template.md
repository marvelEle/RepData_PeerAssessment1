# Reproducible Research: Peer Assessment 1

# Question 1
## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your
analysis

```r
        library(plyr)
        library(ggplot2)
        library(knitr)
        library(reshape2)
        
        activity <- read.csv("./activity/activity.csv")
```


# Question 2
## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
        activity_data <- melt(data = activity, id.vars ="date", measure.vars = "steps", na.rm = TRUE)

        steps_byday <- dcast(activity_data, date ~ variable, sum)
        
        plot(steps_byday, type= "h", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/mean total number of steps per day-1.png) 

2. Calculate and report the mean and median total number of steps taken
per day

```r
        mean_stepsperday <- mean(steps_byday$steps)
        
        cat("Mean total number of step taken per day = ",mean_stepsperday)
```

```
## Mean total number of step taken per day =  10766.19
```

```r
        median_stepsperday <- median(steps_byday$steps)
        
        cat("Median total number of step taken per day = ",median_stepsperday)
```

```
## Median total number of step taken per day =  10765
```



# Question 3
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)


```r
        activity_interval_data <- melt(data = activity, id.vars ="interval", measure.vars = "steps", na.rm = TRUE)

        interval_byday <- dcast(activity_interval_data, interval ~ variable, mean)
        
        plot(interval_byday, type = "l")
        
        abline(h=mean(interval_byday$steps))
```

![](PA1_template_files/figure-html/average daily activity-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

```r
        max_perinterval <- max(interval_byday$steps)
        
        cat('Maximum number of steps by average of 5 minutes interval',max_perinterval)
```

```
## Maximum number of steps by average of 5 minutes interval 206.1698
```



# Question 4
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)


```r
        NO_NA_data <- activity

        total_missingval <- sum(is.na(NO_NA_data$steps))
        
        cat("Total number of missing values = ",total_missingval)
```

```
## Total number of missing values =  2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.

```r
        NA_index <- which(is.na(NO_NA_data$steps))

        data_NO_na <- melt(data = activity,id.vars="interval", measure.vars="steps", na.rm= TRUE)
        
        interval_data <- dcast(data_NO_na, interval ~ variable, mean)
        
        head(interval_data)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
        for (counter in NA_index) {
    
                step_temp <- NO_NA_data$interval[counter]
        
                index <- which(interval_data$interval == step_temp)
        
                NO_NA_data$steps[counter] <- interval_data$steps[index]
        
        }
        
        head(NO_NA_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
        total_missval <- sum(is.na(NO_NA_data$steps))
        cat("Total missing data = ",total_missval)
```

```
## Total missing data =  0
```


4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. 

```r
        activity_data_2 <- melt(data = NO_NA_data,id.vars="date", measure.vars="steps")
        
        steps_byday_2 <- dcast(activity_data_2, date ~ variable, sum)
        
        plot(steps_byday_2)

        abline(h=mean(steps_byday_2$steps))
```

![](PA1_template_files/figure-html/imputing missing values-1.png) 

Do these values differ from the estimates from the first part of the assignment?

```r
        mean_stepsperday_2 <- mean(steps_byday_2$steps)
        
        cat("New data mean :",mean_stepsperday_2)
```

```
## New data mean : 10766.19
```

```r
        cat("previous data mean :",mean_stepsperday)
```

```
## previous data mean : 10766.19
```

```r
        median_stepsperday_2 <- median(steps_byday_2$steps)
        
        cat("New data median :",median_stepsperday_2)
```

```
## New data median : 10766.19
```

```r
        cat("previous median :",median_stepsperday)
```

```
## previous median : 10765
```


What is the impact of imputing missing data on the estimates of the total daily number of steps?
** There's not much different either imputing missing data or exclude the missing data. Just the median value have a bit different.**

# Question 5
## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.

```r
        data_week <- NO_NA_data
        data_week$weektime <- as.factor(ifelse(weekdays(as.POSIXlt(NO_NA_data$date)) %in% c("Saturday","Sunday"),"weekend", "weekday"))

        Weekday_data <- subset(data_week, weektime=="weekday")
        Weekend_data <- subset(data_week, weektime=="weekend")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).
        

```r
        Weekday_p <- melt(Weekday_data, id.vars="interval", measure.vars="steps")
        Weekend_p <- melt(Weekend_data, id.vars="interval", measure.vars="steps")
        Weekday_new <- dcast(Weekday_p, interval ~ variable, mean)
        Weekend_New <- dcast(Weekend_p, interval ~ variable, mean)
        par(mfrow=c(2,1)) 
        plot(Weekday_new, type="l", main="weekdays")

        plot(Weekend_New, type="l", main="weekend")
```

![](PA1_template_files/figure-html/different between weekdays and weekends-1.png) 
