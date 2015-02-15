# Reproducible Research: Peer Assessment 1 - Report
Milton Labanda  
13/02/2015  


## Loading and preprocessing the data

```r
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
library(plyr)
library(ggplot2)
library(scales)
byDay <- ddply(df, ~date, summarise, totalSteps=sum(steps))
clean <- byDay[complete.cases(byDay),]
#head(byDay)
ggplot(clean, aes(x=as.Date(date))) + geom_histogram(binwidth=.5,alpha=.5,colour="black", position="identity") + scale_y_continuous(labels = percent_format())
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean totalSteps by day is **1.0766189\times 10^{4}**  
The median totalSteps by day is **10765**  

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
