Reproducible Research, Peer Assessment 1

Load and Process Data:

unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

Mean total number of steps taken per day:

library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

Average daily activity pattern:

library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")

Five-minute interval containing the maximum number of steps On average across all the days in the dataset: 

averages[which.max(averages$steps),]

Fill in missing values:

Remove NA and replace with five-minute mean calculated above:

missing <- is.na(data$steps)
table(missing)

All of the missing values are filled in with mean value for that 5-minute interval:

fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

Create histogram of the total number of steps taken each day, then calculate the mean and median total number of steps:

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)

The original data contained NA; there were replaced by five-minute mean.  For NA days, set total number of steps to zero. Zeros are removed from the histogram.

To calculate the differences in activity patterns between weekdays and weekends, determine the day of the week for each measurement in the dataset using the filled-in values:

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

Make a panel plot containing plots of average number of steps taken on weekdays and weekends:

averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
