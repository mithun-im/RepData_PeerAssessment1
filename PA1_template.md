# Reproducible Research: Peer Assessment 1
Mithun IM  


## Loading and preprocessing the data


```r
data <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?


```r
Mean_Steps <- mean(data[,1],na.rm= TRUE,echo=TRUE)
```
## What is the average daily activity pattern?


```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Imputing missing values

```r
Total_missing <- sum(is.na(data[,1]))
```
## Are there differences in activity patterns between weekdays and weekends?
