#library
library(dplyr)
library(ggplot2)
library(lubridate)

# Loading and preprocessing the data

activity <- read.csv(file = "activity.csv", header = TRUE, colClasses = c("numeric", "character", "integer"))
activity$date <- ymd(activity$date)

names(activity)
str(activity)
head(activity)

#Process/transform the data (if necessary) into a format suitable for your analysis

# subset data frame to values without na for later use
activity_nona <- activity[complete.cases(activity),]

# What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, activity_nona, sum)

# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps, col = "blue", xlab = "Steps per day", main = "Total Number of Steps per day", breaks = 20)
head(steps_per_day)

# Calculate and report the mean and median of the total number of steps taken per day
mean(steps_per_day$steps)
median(steps_per_day$steps)

# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
interval_data <- aggregate(steps ~ interval, activity_nona, mean)
names(interval_data)[2] <- "mean_steps"

plot(
  x = interval_data$interval,
  y = interval_data$mean_steps,
  type = "l",
  main = "Time Series Plot of the 5-Minute Interval Vs Average Steps Taken",
  xlab = "5-Minute Interval",
  ylab = "Average Number of Steps Taken (averaged across all days)"
)


interval_data[interval_data$mean_steps==max(interval_data$mean_steps),]

# Imputing missing values
missingdata <- is.na(activity$steps)
sum(missingdata)


filled_data <- activity
avg_interval <- tapply(filled_data$steps, filled_data$interval, mean, na.rm=TRUE, simplify=TRUE)
filled_data$steps[missingdata] <- avg_interval[as.character(filled_data$interval[missingdata])]

sum(is.na(filled_data))

# steps per day, including missing values
steps_per_day_nona <- aggregate(steps ~ date, filled_data, sum)
hist(steps_per_day_nona$steps, col = "blue", xlab = "Steps per day", main = "Total Number of Steps per day (including missing values)", breaks=20)

filled_data <- mutate(filled_data, weektype = ifelse(weekdays(filled_data$date) == "Saturday" | weekdays(filled_data$date) == "Sunday", "weekend", "weekday"))

steps_per_day_weektype <- aggregate(steps ~ interval + weektype, filled_data, mean)

s <- ggplot(steps_per_day_weektype, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +  facet_wrap(~weektype, ncol = 1, nrow=2)

print(s)