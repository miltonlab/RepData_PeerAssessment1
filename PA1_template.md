# Reproducible Research: Peer Assessment 1 - Report
Milton Labanda  
13/02/2015  

### Requriments


```r
library(plyr)
library(ggplot2)
```


## Loading and preprocessing the data

```r
#setwd('~/datasciencecoursera//repdata//RepData_PeerAssessment1')
df <- read.table(unzip('activity.zip','activity.csv'), sep=',', header=T)
summary(df)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?


```r
byDay <- ddply(df, ~date, summarise, totalSteps=sum(steps))
byDay <- byDay[complete.cases(byDay),]
# hist(activities$totalSteps, 
#     col = "red", 
#     main = "Total Steps each Day", 
#     xlab="Total Steps by Day", 
#     breaks = nrow(totalsteps))
#head(byDay)
ggplot(byDay, aes(totalSteps)) + geom_histogram(binwidth=500, fill="steelblue") + labs(title="Total steps by Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean totalSteps by day is **1.0766189\times 10^{4}**  
The median totalSteps by day is **10765**  

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
byInterval <- ddply(df, ~interval, summarize, averaged=mean(steps, na.rm=T))
max_averaged = max(byInterval$averaged)
max_interval = byInterval[byInterval$averaged==max_averaged,]$interval
ggplot(byInterval, aes(interval, averaged)) + geom_line(colour="steelblue") + labs(title="Mean steps by Interval") + geom_vline(xintercept=max_interval, colour="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The interval that contain the maximun number of steps by day is **835** with an average of 206.1698113 steps.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 


```r
missing<-df[!complete.cases(df),]
rows_na = nrow(missing)
```

The total number of rows with NAs is:  2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filledDF <- df
filledDF[!complete.cases(filledDF),"steps"]<- byInterval$averaged
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
byDay <- ddply(filledDF, ~date, summarise, totalSteps=sum(steps))
ggplot(byDay, aes(totalSteps)) + geom_histogram(binwidth=500, fill="steelblue") + labs(title="Total steps by Day cleaned")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The mean totalSteps by day in cleaned data is **1.0766189\times 10^{4}**  
The median totalSteps by day in cleaned data is **1.0766189\times 10^{4}**  

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
wkDF <- transform(filledDF, 
                      dayOfWeek = ifelse (weekdays(as.Date(filledDF$date)) %in% c("saturday", "sunday", "sábado","domingo"), "weekend", "weekday")
                      )
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
groupedWeek <- ddply(wkDF, .(interval, dayOfWeek), summarise, averagedSteps = mean(steps))
graph = ggplot(groupedWeek, aes(interval, averagedSteps, group=dayOfWeek)) + geom_line(colour="steelblue") + facet_wrap(~dayOfWeek, nrow=2)
print(graph)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

