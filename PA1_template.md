---
title: "PA1_template"
author: "Choong-Hoon Hyun"
date: "5/7/2022"
output: html_document
---
# Peer-graded Assignment: Reproducible Research Week 2 Course Proejct 1

## Loading and preprocessing the data 

### Download file, load the dataset to R, and process/transforam the data into a format suitable for the analysis

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
f <- file.path(getwd(), "activity.zip")
download.file(fileUrl, f, curl = "method")
unzip(zipfile = "activity.zip")

activity_monitoring_data <- read.csv("activity.csv")
activity_monitoring_data$date <- as.Date(activity_monitoring_data$date, "%Y-%m-%d")
head(activity_monitoring_data)
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

## What is mean total number of steps taken per day?

###  1. Calculate the total number of steps taken per day
#### Load ggplot2

```r
library(ggplot2)
```


```r
steps_per_day <- split(activity_monitoring_data, activity_monitoring_data$date)
total_steps <- sapply(steps_per_day, function(x) {
  sum(x[, "steps"])
})
total_steps_df <- as.data.frame(total_steps)
head(total_steps_df)
```

```
##            total_steps
## 2012-10-01          NA
## 2012-10-02         126
## 2012-10-03       11352
## 2012-10-04       12116
## 2012-10-05       13294
## 2012-10-06       15420
```
### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
#### Plot a histogram of the total number of steps taken each day

```r
ggplot(total_steps_df, aes(total_steps)) + geom_histogram(binwidth = 1000, fill = "white", col = "blue") +
  labs(x= "Steps", y = "Frequency", title = "Total Number of Steps Taken each Day (Histogram)")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk Histogram of the total number of steps taken each day](<figure/Histogram of the total number of steps taken each day-1.png>)

### 3. Calculate and report the mean and median of the total number of steps taken per day
#### Mean of the total number of steps taken per day

```r
mean(total_steps_df$total_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

#### Median of the total number of steps taken per day

```r
median(total_steps_df$total_steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
#### Load dplyr package

```r
library(dplyr)
```

#### Make dataset to show the average steps per intervals

```r
avg_daily_steps <- activity_monitoring_data %>% group_by(interval) %>%
  summarize(avg_steps = mean(steps, na.rm = TRUE))
head(avg_daily_steps)
```

```
## # A tibble: 6 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1        0    1.72  
## 2        5    0.340 
## 3       10    0.132 
## 4       15    0.151 
## 5       20    0.0755
## 6       25    2.09
```

#### Make a time series plot

```r
ggplot(avg_daily_steps, aes(interval, avg_steps)) + geom_line(col = "orange") + 
  labs(x = "5-minute intervals", y = "Average Number of Steps", title = "Average Daily Steps Across All Days")
```

![plot of chunk Plot a time series plot](<figure/Plot a time series plot-1.png>)
  
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
subset(avg_daily_steps, avg_steps == max(avg_steps))
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

## Imputing missing values
### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
total_number_missing_values <- subset(activity_monitoring_data, is.na(activity_monitoring_data))
nrow(total_number_missing_values)
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

#### *Strategy for missing values*: 
* The average of the associated intervals will be used to fill in all of the missing values in the dataset. Fill the NA values with the average steps dataset obtained from "avg_daily_steps".

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_monitoring_data_no_NA <- left_join(activity_monitoring_data, avg_daily_steps, by = "interval") %>%
  transmute(steps = coalesce(steps, avg_steps), date, interval) 
head(activity_monitoring_data_no_NA)
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

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### Calculate the total number of steps taken each day with "activity_monitoring_data_no_NA"

```r
steps_per_day_no_NA <- split(activity_monitoring_data_no_NA, activity_monitoring_data_no_NA$date)
total_steps_no_NA <- sapply(steps_per_day_no_NA, function(x) {
  sum(x[, "steps"])
})
total_steps_no_NA_df <- as.data.frame(total_steps_no_NA)
names(total_steps_no_NA_df) <- c("steps_no_NA")
total_steps_no_NA_df$steps_no_NA <- as.integer(total_steps_no_NA_df$steps_no_NA)
head(total_steps_no_NA_df)
```

```
##            steps_no_NA
## 2012-10-01       10766
## 2012-10-02         126
## 2012-10-03       11352
## 2012-10-04       12116
## 2012-10-05       13294
## 2012-10-06       15420
```

#### Plot a histogram of the total number of steps taken each day

```r
ggplot(total_steps_no_NA_df, aes(steps_no_NA)) + geom_histogram(binwidth = 1000, fill = "white", col = "red") +
  labs(x = "Steps", y = "Frequency", title = "Total Number of Steps Taken each Day w/o NAs (Histogram)")
```

![plot of chunk Histogram of the total number of steps taken each day with no NA dataset](<figure/Histogram of the total number of steps taken each day with no NA dataset-1.png>)

#### Calculate and report mean and median total number of steps taken per day

```r
mean(total_steps_no_NA_df$steps_no_NA)
```

```
## [1] 10766.16
```

```r
median(total_steps_no_NA_df$steps_no_NA)
```

```
## [1] 10766
```

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

* We have the new values for mean and median. The new mean is 10766.16 and the old one is 10766.19. 
* The new median is 10766 and the old one is 10765. 
* It is notable that the missing values were removed with "na.rm = TRUE" when the values are calculated from the first part of the assignment. We filled out the NAs with average of associated intervals, the new means slightly decreased while the new median increased a little bit. The presence of missing days may introduce bias into some calculations and they are compensated by filling out the NAs with the average.

```r
new_mean <- mean(total_steps_no_NA_df$steps_no_NA)
old_mean <- mean(total_steps_df$total_steps, na.rm = TRUE)
new_median <- median(total_steps_no_NA_df$steps_no_NA)
old_median <- median(total_steps_df$total_steps, na.rm = TRUE)

comparison <- data.frame(mean = c(new_mean, old_mean), median = c(new_median, old_median)) 
rownames(comparison) <- c("new value", "old value")
print(comparison)
```

```
##               mean median
## new value 10766.16  10766
## old value 10766.19  10765
```

## Are there differences in activity patterns between weekdays and weekends? For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_DoW <- activity_monitoring_data_no_NA
activity_DoW$day_of_week <- weekdays(activity_DoW$date)
activity_DoW$weekday_weekend <- ifelse(activity_DoW$day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_DoW$weekday_weekend <- as.factor(activity_DoW$weekday_weekend)
head(activity_DoW)
```

```
##       steps       date interval day_of_week weekday_weekend
## 1 1.7169811 2012-10-01        0      Monday         weekday
## 2 0.3396226 2012-10-01        5      Monday         weekday
## 3 0.1320755 2012-10-01       10      Monday         weekday
## 4 0.1509434 2012-10-01       15      Monday         weekday
## 5 0.0754717 2012-10-01       20      Monday         weekday
## 6 2.0943396 2012-10-01       25      Monday         weekday
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days(y-axis). 

```r
avg_daily_steps_wkday <- activity_DoW %>% filter(weekday_weekend == "weekday") %>% group_by(interval) %>%
  summarize(average_steps = mean(steps)) 
avg_daily_steps_wkday$day <- "weekday"

avg_daily_steps_wkend <- activity_DoW %>% filter(weekday_weekend == "weekend") %>% group_by(interval) %>%
  summarize(average_steps = mean(steps))
avg_daily_steps_wkend$day <- "weekend"

avg_daily_steps_total <- rbind(avg_daily_steps_wkday, avg_daily_steps_wkend)
head(avg_daily_steps_total)
```

```
## # A tibble: 6 × 3
##   interval average_steps day    
##      <int>         <dbl> <chr>  
## 1        0        2.25   weekday
## 2        5        0.445  weekday
## 3       10        0.173  weekday
## 4       15        0.198  weekday
## 5       20        0.0990 weekday
## 6       25        1.59   weekday
```

```r
ggplot(avg_daily_steps_total, aes(interval, average_steps)) + geom_line(aes(color = day)) + facet_grid(rows = vars(day)) + 
  labs(x = "5-minute intervals", y = "Average Number of Steps", title = "Average Daily Steps Across All weekdays or weekends")
```

![plot of chunk plot of average_daily_steps across all weekdays and weekends](<figure/plot of average_daily_steps across all weekdays and weekends-1.png>)
