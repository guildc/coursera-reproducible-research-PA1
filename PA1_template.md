
# Reproducible Research: Peer Assessment 1



## 1. Loading and preprocessing the data


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')}
activity <- read.csv("activity.csv", header = TRUE)
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
# Convert date variable from factor to date class
activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity$date)
```

```
##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
```

## 2. What is the mean total number of steps taken per day


```r
library(dplyr)
# First, group and summarize steps by date
StepsPerDay <- group_by(activity, date) %>% summarize(total.steps=sum(steps), .groups='drop') %>% as.data.frame
# Table of total steps by date
head(StepsPerDay)
```

```
##         date total.steps
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

## Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
# First, remove the 8 rows containing NAs
StepsPerDay <- StepsPerDay[complete.cases(StepsPerDay),]
# Create histogram
p <- ggplot() + geom_histogram(data = StepsPerDay, aes(x = total.steps),boundary=0,binwidth=2500, fill = "pink", col = "black") +
    labs(x = "Steps", y = "Frequency") + ggtitle("Histogram of Total Steps Per Day") + theme(plot.title = element_text(face="bold", hjust=0.5))+
    scale_x_continuous(breaks = seq(0,25000,2500)) + scale_y_continuous(breaks=seq(0,18,2))
p 
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->


```r
steps.mean <- mean(StepsPerDay$total.steps, na.rm=TRUE)
print(steps.mean)
```

```
## [1] 10766.19
```


```r
steps.median <-median(StepsPerDay$total.steps, na.rm = TRUE)
print(steps.median)
```

```
## [1] 10765
```

## 3. What is the average daily activity pattern

```r
# Make a time series plot(i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# First, group by time interval and compute mean steps
group_data <- group_by(activity, interval) %>% summarize(mu.steps = mean(steps, na.rm=TRUE), .groups='drop') %>% as.data.frame
# create two vectors - one for the time interval and the other for the average number of steps
x <- group_data$interval
y <- group_data$mu.steps
# Create Time series plot of 5-minute interval and the average number of steps taken
plot(x, y, type = "l", col = "blue", xlab = "Time Interval", ylab = "Average Number of Steps", main = "Average Daily Activity")
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->


```r
# Find the max mean total steps
max.mu.steps <- max(group_data$mu.steps)
print(max.mu.steps)
```

```
## [1] 206.1698
```

```r
# Find the time interval at the max mean total steps
maxTime <- group_data[group_data$mu.steps == max.mu.steps, ]
# Extract and print time interval
maxInterval <- maxTime$interval
print(maxInterval)
```

```
## [1] 835
```

## 4. Imputing missing values

```r
library(imputeMissings)
# Calculate and report missing values in the dataset
sum(is.na(activity$steps)) # returns the total number of missing values in the variable 'steps'
```

```
## [1] 2304
```

```r
# Impute (fill in) the missing values based on the median value per date in the data set
imputed_data <- group_by(activity, date) %>% impute(object = NULL, method = "median/mode", flag = TRUE) %>% as.data.frame
# Check that there are no missing values in the new data
sum(is.na(imputed_data))
```

```
## [1] 0
```

```r
# Imputed data set grouped by date
StepsPerDay2 <- group_by(imputed_data, date) %>% summarize(total.steps=sum(steps), .groups='drop') %>% as.data.frame
```


```r
# Make the histogram
p <- ggplot() + geom_histogram(data = StepsPerDay2, aes(x = total.steps),boundary=0,binwidth=2500, fill = "purple", col = "black") +
    labs(x = "Steps", y = "Frequency") + ggtitle("Histogram of Total Steps Per Day") + theme(plot.title = element_text(face="bold", hjust =0.5))+
    scale_x_continuous(breaks = seq(0,25000,250000)) + scale_y_continuous(breaks=seq(0,18,2))
p 
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->


```r
# calculate the mean and median of the new, imputed data
mean.steps <- mean(StepsPerDay2$total.steps)
print(mean.steps)
```

```
## [1] 9354.23
```

```r
median.steps <- median(StepsPerDay2$total.steps)
print(median.steps)
```

```
## [1] 10395
```

```
The resulting mean and median estimates of the imputed data are lower than that of the original data set in which the missing values were removed.
```

## 5. Are there differences in activity patterns between weekdays and weekends

```r
# Convert date variable from factor to date class
imputed_data$day <- as.Date(imputed_data$date, "%Y-%m-%d")
# Create a new column for day of the week
imputed_data$weekday <- weekdays(imputed_data$day)
# check data
# str(imputed_data)
# Create a new variable indicating either weekday or weekend values and convert column to a factor
imputed_data$weekday.group <- as.factor(ifelse(imputed_data$weekday == "Saturday" |     imputed_data$weekday == "Sunday","weekend", "weekday"))
# Check that the class of weekday.group is in fact a factor
# class(imputed_data$weekday.group)
# Look at the distribution of new variable
# table(imputed_data$weekday.group)

# Create weekday.group by interval stratum
interval.weekday <- group_by(imputed_data, interval, weekday.group)

grouped.interval.weekday <- summarize(interval.weekday, mean.steps = mean(steps), .groups='drop')
```



```r
# Create the time series panel plot
library(lattice)
xyplot(mean.steps ~ interval | weekday.group , grouped.interval.weekday, type = "l",
       layout = c(1,2), xlab = "Time Interval", ylab = "Average Number of Steps", main="Average Steps Per Time Interval by Weekday Type")
```

![](PA1_template_files/figure-html/panel-1.png)<!-- -->

```
The step activity trends are different based on whether the activity occurs on a weekend versus 
a weekday. The average number of steps trends higher on the weekends than the weekdays.
```
