---
title: "coursera_assignment"
author: "msluszniak"
date: "5/1/2020"
output:
  html_document:
    df_print: paged
---



## Report

Firstly, load the data and process it. Then calculate the total number of steps taken per day

```r
#load data from working directory
setwd("/home/mateusz/Desktop/Working_directory_R/data/luzne_csv_lub_txt")
dataset = read.csv("activity.csv")
#data now is in data frame

library(dplyr)
#consider only cases in which NA doesnt occur
dataset = dataset[complete.cases(dataset), ]
dataset = group_by(dataset, date) 
review_sum = summarise(dataset, sum = sum(steps))
str(review_sum)
```

```
## tibble [53 × 2] (S3: tbl_df/tbl/data.frame)
##  $ date: Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ sum : int [1:53] 126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

Histogram of the total number of steps taken each day

```r
#plot histogram
with(review_sum, hist(sum, col = "red", xlab = "number of steps per day", breaks = 10, ylim = c(0,25)))
```

![plot 1](figure/unnamed-chunk-4-1.png)

The mean and median of the total number of steps taken per day

```r
#calculate mean and median of steps for each day
#NAs have been already removed
mean_step = mean(review_sum$sum)
median_step = median(review_sum$sum)
print(mean_step)
```

```
## [1] 10766.19
```

```r
print(median_step)
```

```
## [1] 10765
```

A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
#series plot
setwd("/home/mateusz/Desktop/Working_directory_R/data/luzne_csv_lub_txt")
dataset = read.csv("activity.csv")
dataset = dataset[complete.cases(dataset), ]
dataset = group_by(dataset, interval)
review_mean = summarise(dataset, average = mean(steps, na.rm = TRUE))
with(review_mean, plot(x = interval, y = average, 
                  type = "l", lwd = 2, ylab = "Average num of steps", xlab = "Intervals", 
                  main = "Steps per interval"))
```

![plot 2](figure/unnamed-chunk-6-1.png)


The 5-minute interval that, on average, contains the maximum number of steps

```r
max_interval_pos = which.max(review_mean$average)
max_interval = review_mean[max_interval_pos, "interval"]
print(as.numeric(max_interval))
```

```
## [1] 835
```


dealing with missing values
number of missing values

```r
setwd("/home/mateusz/Desktop/Working_directory_R/data/luzne_csv_lub_txt")
dataset = read.csv("activity.csv")
NA_num = sum(!complete.cases(dataset))
print(NA_num)
```

```
## [1] 2304
```

I decided to take mean for 5-minute interval rounded to the nearest natural number in order to replace NAs. This is the plot with proceed NAs

```r
#as earlier create data frame containing average number of steps per interval
setwd("/home/mateusz/Desktop/Working_directory_R/data/luzne_csv_lub_txt")
dataset1 = read.csv("activity.csv")
dataset_mean = group_by(dataset1, interval)
review_mean1 = summarise(dataset_mean, average = mean(steps, na.rm = TRUE))

#we need to replace NAs
for(i in 1:length(dataset1$interval)){
  #if there is NA
  if(is.na(dataset1[i, "steps"])){
    #decompose interval into number of hours and minutes
    n = dataset1[i, "interval"]
    n_hour = n %/% 100
    n_min = (n %% 100)
    #the index in review_mean1 which we need is the number of 5-minute periods
    #in hour:minute so what we should do is to take 12*n_hour because every hour
    #is 1hour = 12 * 5min and minutes/5 which is obvious and add 1 because we start
    #in from 0*5min (0 is the first interval)
    dataset1[i, "steps"] = round(review_mean1[12*n_hour+n_min/5 + 1, "average"])
  }
}

dataset2 = dataset1
dataset1 = group_by(dataset1, date) 
review_sum1 = summarise(dataset1, sum = sum(steps))
with(review_sum1, hist(sum, col = "red",
                       xlab = "number of steps per day", breaks = 10, ylim = c(0,25)))
```

![plot 3](figure/unnamed-chunk-9-1.png)

The mean and median of the total number of steps taken per day

```r
mean_step = mean(review_sum1$sum)
median_step = median(review_sum1$sum)
print(mean_step)
```

```
## [1] 10765.64
```

```r
print(median_step)
```

```
## [1] 10762
```

A panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
dataset2$date = as.Date(dataset2$date)
final = cbind(dataset2[1], dataset2[3], lapply(dataset2[2], weekdays))

days = (final$date == "sobota" | final$date == "niedziela")
final = mutate(final, days = days)
xyz = split(final, days)
res1 = group_by(xyz[[1]], interval) # week
res2 = group_by(xyz[[2]], interval) # weekend
review_mean1 = summarise(res1, average = mean(steps, na.rm = TRUE))
review_mean2 = summarise(res2, average = mean(steps, na.rm = TRUE))
par(mfrow = c(1,2))

with(review_mean1, plot(x = interval, y = average,
                       type = "l", lwd = 2, ylab = "Average num of steps", xlab = "Intervals", 
                       main = "Steps per interval in week"))
with(review_mean2, plot(x = interval, y = average,
                        type = "l", lwd = 2, ylab = "Average num of steps", xlab = "Intervals", 
                        main = "Steps per interval in weekend"))
```

![plot 4](figure/unnamed-chunk-11-1.png)
