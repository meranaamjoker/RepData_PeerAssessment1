# Reproducible Research: Peer Assessment 1

## Assumptions
* Current working directory is set to current project
* File name is *activity.csv* and is present in the current working directory

## Libraries used

```r
library(dplyr)
library(ggplot2)
library(lubridate)
```

## Loading and preprocessing the data
>Show any code that is needed to
>
>Load the data (i.e. read.csv())
>Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv(file = "activity.csv", header = TRUE, colClasses = c("numeric", "character", "integer"))

# Convert the date to correct format
activity$date <- ymd(activity$date)

# quickly check the data
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
# subset data frame to values without na for later use
activity_nona <- activity[complete.cases(activity),]
```

## What is mean total number of steps taken per day?
> For this part of the assignment, you can ignore the missing values in the dataset.
>
> Calculate the total number of steps taken per day
>
> If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
>
> Calculate and report the mean and median of the total number of steps taken per day


```r
# Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, activity_nona, sum)

head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
# draw the histogram
hist(steps_per_day$steps, col = "blue", xlab = "Steps per day", main = "Total Number of Steps per day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day

# mean
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
# median
median(steps_per_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
> What is the average daily activity pattern?
>
>Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
>
>Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_data <- aggregate(steps ~ interval, activity_nona, mean)
names(interval_data)[2] <- "mean_steps"

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(
  x = interval_data$interval,
  y = interval_data$mean_steps,
  type = "l",
  main = "Time Series Plot of the 5-Minute Interval Vs Average Steps Taken",
  xlab = "5-Minute Interval",
  ylab = "Average Number of Steps Taken (averaged across all days)"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
interval_data[interval_data$mean_steps==max(interval_data$mean_steps),]
```

```
##     interval mean_steps
## 104      835   206.1698
```

## Imputing missing values
> Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
>
> Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingdata <- is.na(activity$steps)
sum(missingdata)
```

```
## [1] 2304
```

> Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
>
> Create a new dataset that is equal to the original dataset but with the missing data filled in.

Strategy to fill the missing value is to put the mean of all non-NA values of that interval (across all days)

```r
filled_data <- activity
avg_interval <- tapply(filled_data$steps, filled_data$interval, mean, na.rm=TRUE, simplify=TRUE)
filled_data$steps[missingdata] <- avg_interval[as.character(filled_data$interval[missingdata])]

# Check if all NA are filled
sum(is.na(filled_data))
```

```
## [1] 0
```

> Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# steps per day, including missing values
steps_per_day_nona <- aggregate(steps ~ date, filled_data, sum)
hist(steps_per_day_nona$steps, col = "blue", xlab = "Steps per day", main = "Total Number of Steps per day (including missing values)", breaks=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

## Are there differences in activity patterns between weekdays and weekends?
> For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
>
> Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
filled_data <- mutate(filled_data, weektype = ifelse(weekdays(filled_data$date) == "Saturday" | weekdays(filled_data$date) == "Sunday", "weekend", "weekday"))
```

> Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_per_day_weektype <- aggregate(steps ~ interval + weektype, filled_data, mean)

s <- ggplot(steps_per_day_weektype, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +  facet_wrap(~weektype, ncol = 1, nrow=2)

print(s)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

From the two plots it seems that there is more activity earlier in the day during weekdays compared to weekends, but more activity throughout the weekends compared with weekdays
