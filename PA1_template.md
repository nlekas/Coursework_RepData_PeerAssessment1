# Reproducible Research: Peer Assessment 1

---------------------

## Loading and preprocessing the data    

```r
    require(plyr) 
```

```
## Loading required package: plyr
```

```r
    require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
    x <- read.csv("data/activity.csv")
    x$date <- strptime(x$date, "%Y-%m-%d")
    #x <- subset(x, select=c(steps, interval, pDate))
```

---------------------

## What is mean & median total number of steps taken per day?
   1. Calculate the total number of steps taken per day:

```r
tspd <- ddply(x, .(date), summarise, t.steps.day=sum(steps, na.rm=TRUE))
tspd
```

```
##          date t.steps.day
## 1  2012-10-01           0
## 2  2012-10-02         126
## 3  2012-10-03       11352
## 4  2012-10-04       12116
## 5  2012-10-05       13294
## 6  2012-10-06       15420
## 7  2012-10-07       11015
## 8  2012-10-08           0
## 9  2012-10-09       12811
## 10 2012-10-10        9900
## 11 2012-10-11       10304
## 12 2012-10-12       17382
## 13 2012-10-13       12426
## 14 2012-10-14       15098
## 15 2012-10-15       10139
## 16 2012-10-16       15084
## 17 2012-10-17       13452
## 18 2012-10-18       10056
## 19 2012-10-19       11829
## 20 2012-10-20       10395
## 21 2012-10-21        8821
## 22 2012-10-22       13460
## 23 2012-10-23        8918
## 24 2012-10-24        8355
## 25 2012-10-25        2492
## 26 2012-10-26        6778
## 27 2012-10-27       10119
## 28 2012-10-28       11458
## 29 2012-10-29        5018
## 30 2012-10-30        9819
## 31 2012-10-31       15414
## 32 2012-11-01           0
## 33 2012-11-02       10600
## 34 2012-11-03       10571
## 35 2012-11-04           0
## 36 2012-11-05       10439
## 37 2012-11-06        8334
## 38 2012-11-07       12883
## 39 2012-11-08        3219
## 40 2012-11-09           0
## 41 2012-11-10           0
## 42 2012-11-11       12608
## 43 2012-11-12       10765
## 44 2012-11-13        7336
## 45 2012-11-14           0
## 46 2012-11-15          41
## 47 2012-11-16        5441
## 48 2012-11-17       14339
## 49 2012-11-18       15110
## 50 2012-11-19        8841
## 51 2012-11-20        4472
## 52 2012-11-21       12787
## 53 2012-11-22       20427
## 54 2012-11-23       21194
## 55 2012-11-24       14478
## 56 2012-11-25       11834
## 57 2012-11-26       11162
## 58 2012-11-27       13646
## 59 2012-11-28       10183
## 60 2012-11-29        7047
## 61 2012-11-30           0
```
   2. Histogram of the total number of steps taken per day:

```r
hist(tspd$t.steps.day, xlab="Steps Per Day", 
     ylab="Days Occured", main="Total Steps Taken")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

   3. Mean & Median Calculation:
*Note that NA values were removed. More on the missing values later.*

```r
mymean <- round(mean(tspd$t.steps.day, na.rm=TRUE), digits=2)
mymedian <- round(median(tspd$t.steps.day, na.rm=TRUE), digits=2)
```
 - **Mean**: 9354.23  
 - **Median**: 1.0395 &times; 10<sup>4</sup> 

## What is the average daily activity pattern?
   
   1. Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
int <- ddply(x, .(interval), summarise, mean_interval=mean(steps, na.rm=TRUE))
plot(int$interval, int$mean_interval, type="l", xlab="5-Minute Interval", ylab="Average Number Of Steps Across All Days" )
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

   2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps <- max(int$mean_interval)
DFmaxinterval <- subset(int, mean_interval==maxsteps, select=c(interval, mean_interval))
maxinterval <- DFmaxinterval[,1]
```
The interval 835 contains the maxium number of steps on average across all days.


## Imputing missing values

   1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
bad <- is.na(x$steps)
missingvalues <- nrow(subset(x, bad))
```
   - There are 2304 number of NAs in the dataset.

   2. Devise a strategy for filling in all of the missing values in the dataset. Then create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
xNAs <- x
for(i in 1:nrow(xNAs)){
    if(is.na(xNAs$steps[i])){
        xNAs$steps[i] <- int$mean_interval[which(xNAs$interval[i]==int$interval)]
    }
}
```

   3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
tspd2 <- ddply(xNAs, .(date), summarise, t.steps.day=sum(steps, na.rm=TRUE))
hist(tspd2$t.steps.day, xlab="Steps Per Day", ylab="Days Occured", main="Number of Steps Taken Per Day\n With NAs Filled In With Average Steps For Interval")
```

![plot of chunk xnasmean](./PA1_template_files/figure-html/xnasmean.png) 


```r
mymean2 <- round(mean(tspd2$t.steps.day, na.rm=TRUE), digits=2)
mymedian2 <- round(median(tspd2$t.steps.day, na.rm=TRUE), digits=2)
meanchange <- round(mymean-mymean2, digits=2)
medianchange <- round(mymedian - mymedian2, digits=2)
```

 - **Mean**: 1.0766 &times; 10<sup>4</sup>  
 - **Median**: 1.0766 &times; 10<sup>4</sup> 
 - The impact of filling in the NAs with the mean of all steps for that particular interval from all day is a decrease in mean by -1411.96 and a decrease in median by -371.19.


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

   1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
   

```r
daf <- function(y){
    output <- vector()
    for(i in 1:length(y)){
        if(y[i] %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
            output[i] <- "Weekday"
        }else if(y[i] %in% c("Saturday", "Sunday")){
            output[i] <- "Weekend"
        }
    }
    output
}
xNAs <- mutate(xNAs, day=weekdays(date))
dayofweek <- factor(daf(xNAs$day))
xNAs <- cbind(xNAs, dayofweek)
```

   2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

**Average Number of Steps Across All Weekend and Weekdays**

```r
xNAs.wend <- subset(xNAs, dayofweek=="Weekend")
xNAs.wday <- subset(xNAs, dayofweek=="Weekday")

wend <- ddply(xNAs.wend, .(interval), summarise, mean_interval=mean(steps, na.rm=TRUE))
wday <- ddply(xNAs.wday, .(interval), summarise, mean_interval=mean(steps, na.rm=TRUE))

par(mfrow=c(1,2))

plot(wend$interval, wend$mean_interval, type="l", xlab="5-Minute Interval", ylab="Avg. Steps", main="Weekends" )
plot(wday$interval, wday$mean_interval, type="l", xlab="5-Minute Interval", ylab="Avg. Steps", main="Weekdays" )
```

![plot of chunk panelplot](./PA1_template_files/figure-html/panelplot.png) 


