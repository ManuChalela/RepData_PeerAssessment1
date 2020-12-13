---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Unzip data to obtain a csv file.


```r
library(data.table)
library(fs)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
##     yday, year
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
path <- file.path(getwd(), "data/")
fs::dir_create(path = path)
fileUrl <-
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,
              destfile = file.path(path, 'dataFiles.zip'),
              method = "curl")
unzip(file.path(path, "dataFiles.zip"), exdir = path)
activity <-
  data.table::fread(input = file.path(path, "activity.csv"))
```



## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
Total_Steps <-
  activity[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)]
head(Total_Steps, 10)
```

```
##           date steps
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps Median_Steps
## 1:   10766.19        10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
Interval <- activity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(Interval, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Interval[steps == max(steps), .(max_interval = interval)]
```

```
##    max_interval
## 1:          835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
activity[is.na(steps), .N ]
```

```
## [1] 2304
```

```r
# alternative solution
nrow(activity[is.na(steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Filling in missing values with median of dataset. 
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.table::fwrite(x = activity, file = file.path(path, "tidyData.csv"), quote = FALSE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
Total_Steps <- activity[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

```
##    Mean_Steps Median_Steps
## 1:    9354.23        10395
```

```r
ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# Just recreating activityDT from scratch then making the new factor variable. (No need to, just want to be clear on what the entire process is.) 
activity <- data.table::fread(input = "data/activity.csv")
activity[, date := as.POSIXct(date, format = "%Y-%m-%d")]
Sys.setlocale("LC_TIME","en_US")
```

```
## Warning in Sys.setlocale("LC_TIME", "en_US"): OS reports request to set locale
## to "en_US" cannot be honored
```

```
## [1] ""
```

```r
activity[, `Day of Week`:= lubridate::wday(x = date, label = TRUE, abbr = FALSE, locale = Sys.getlocale("LC_TIME"))]
activity$`weekday or weekend` <- ifelse(activity$`Day of Week` %in% c('sábado', 'domingo'), "weekend", "weekday")
#activity[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activity$`Day of Week` <- case_when(
  activity$`Day of Week` == "lunes" ~ "monday",
  activity$`Day of Week` == "martes" ~ "tuesday",
  activity$`Day of Week` == "miércoles" ~ "wednesday",
  activity$`Day of Week` == "jueves" ~ "thursday",
  activity$`Day of Week` == "viernes" ~ "friday",
  activity$`Day of Week` == "sábado" ~ "saturday",
  activity$`Day of Week` == "domingo" ~ "sunday")
activity[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activity[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activity, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:    NA 2012-10-01        0      monday            weekday
##  2:    NA 2012-10-01        5      monday            weekday
##  3:    NA 2012-10-01       10      monday            weekday
##  4:    NA 2012-10-01       15      monday            weekday
##  5:    NA 2012-10-01       20      monday            weekday
##  6:    NA 2012-10-01       25      monday            weekday
##  7:    NA 2012-10-01       30      monday            weekday
##  8:    NA 2012-10-01       35      monday            weekday
##  9:    NA 2012-10-01       40      monday            weekday
## 10:    NA 2012-10-01       45      monday            weekday
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activity[is.na(steps), "steps"] <-
  activity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
Interval <-
  activity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)]
ggplot(data = Interval ,
       aes(x = interval , y = steps, color = `weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap( ~
                                                                                                                                                                                            `weekday or weekend` , ncol = 1, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
