---
title: "Reproducible Research - Peer Assignment 1"
author: "Zoltan Novak"
date: "Friday, February 13, 2015"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
---

Downloading, unzipping, loading and preprocessing the data


```r
#Downloading the raw data zip if does not exist
if (!file.exists("repdata-data-activity.zip")){
        download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                      destfile="repdata-data-activity.zip",
                      method="auto", 
                      quiet = FALSE, 
                      mode = "wb")
}        

#Unzipping the raw data zip if csv does not exist
if (!file.exists("repdata-data-activity.zip") || !file.exists("Source_Classification_Code.rds")){
        unzip(zipfile = "repdata-data-activity.zip", 
              overwrite = TRUE,
              unzip = "internal",
              setTimes = FALSE)
}

rawData = read.csv("activity.csv")
print("Raw Data Summary:")
```

```
## [1] "Raw Data Summary:"
```

```r
str(rawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


What is mean total number of steps taken per day?

```r
stepsPerDay = aggregate(rawData$steps ~ rawData$date, rawData, FUN=sum)
names(stepsPerDay) <- c("date", "steps")

## displaying the histogram
library(ggplot2)
qplot(steps, data = stepsPerDay, binwidth = diff(range(stepsPerDay$steps))/30) + geom_histogram(binwidth = diff(range(stepsPerDay$steps))/30) + scale_y_continuous(breaks=c(1,3,7,10)) + ggtitle("Total steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```

What is the average daily activity pattern?

```r
stepsPerInt = aggregate(rawData$steps ~ rawData$interval, rawData, FUN=mean)
names(stepsPerInt) <- c("interval", "steps")
qplot(interval, steps, data=stepsPerInt, type="1") + geom_line() + ggtitle("Average steps in time intervals")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# print time interval with max average step count
stepsPerInt[stepsPerInt$steps==max(stepsPerInt$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

Inputing missing values

```r
# estimating missing (NA) steps based on the mean of the steps in the corresponging 5 minute interval from the other days
cleanData <-rawData
for (i in 1:nrow(cleanData)){
  if (is.na(cleanData[i, 1])){
    tmp <- cleanData[i, 3]
    cleanData[i, 1] <- stepsPerInt[stepsPerInt$interval == tmp, 2]
  }
}
# joining data frames with missing and estimated step valus for easier display
rawData <- cbind(rawData, rep("With missing values", nrow(rawData)))
cleanData <- cbind(cleanData, rep("Estimated missing values", nrow(cleanData)))
names(rawData) <-c("steps", "date", "interval", "type")
names(cleanData) <-c("steps", "date", "interval", "type")
allData <- rbind(rawData, cleanData)

allStepsPerDay = aggregate(allData$steps ~ allData$date + allData$type, allData, FUN=sum)
names(allStepsPerDay) <- c("date", "type", "steps")


# plotting total steps per day / data sets with missing and estimated values are displayed in differend facets
ggplot(allStepsPerDay, aes(x=date, y=steps)) +  geom_bar(stat="identity") + facet_grid(type ~ .) 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
print("Comparing total number of steps taken each day")
```

```
## [1] "Comparing total number of steps taken each day"
```

```r
print("Total number of steps taken each day (mean, meadian) where data has missing values")
```

```
## [1] "Total number of steps taken each day (mean, meadian) where data has missing values"
```

```r
mean(allStepsPerDay[allStepsPerDay$type =="With missing values", ][, 3])
```

```
## [1] 10766.19
```

```r
median(allStepsPerDay[allStepsPerDay$type =="With missing values", ][, 3])
```

```
## [1] 10765
```

```r
print("Total number of steps taken each day (mean, meadian) where missing values vere replaced with the average for the corresponding time intervals from other days")
```

```
## [1] "Total number of steps taken each day (mean, meadian) where missing values vere replaced with the average for the corresponding time intervals from other days"
```

```r
mean(allStepsPerDay[allStepsPerDay$type =="Estimated missing values", ][, 3])
```

```
## [1] 10766.19
```

```r
median(allStepsPerDay[allStepsPerDay$type =="Estimated missing values", ][, 3])
```

```
## [1] 10766.19
```

Are there differences in activity patterns between weekdays and weekends?

```r
dayType <- weekdays(as.Date(allData$date, "%Y-%m-%d"), abbreviate=TRUE)
dayType <- sub("H|K|Sze|Cs|P", "weekday", dayType)
dayType <- sub("Szo|V", "weekend", dayType)
allData <- cbind(allData, dayType)

cleanData <- allData[allData$type == "Estimated missing values", ]
meanStepsPerInt = aggregate(cleanData$steps ~ cleanData$interval + cleanData$dayType, cleanData, FUN=mean)
names(meanStepsPerInt) <- c("interval", "dayType", "steps")

ggplot(meanStepsPerInt, aes(x=interval, y=steps)) +  geom_line(type="1") + facet_grid(dayType ~ .)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


