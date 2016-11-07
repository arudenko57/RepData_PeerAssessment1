Activity Monitoring Analysis
============================

1. Loading and preprocessing the data
-------------------------------------

    knitr::opts_chunk$set(echo = TRUE)
    raw_activity <- read.csv("activity.csv")
    all_activity <- raw_activity[!is.na(raw_activity$steps),]
    activity <- all_activity[all_activity$steps>0,]
    str(activity)

    ## 'data.frame':    4250 obs. of  3 variables:
    ##  $ steps   : int  117 9 4 36 25 90 411 413 415 519 ...
    ##  $ date    : Factor w/ 61 levels "10/1/2012","10/10/2012",..: 12 12 23 23 23 23 23 23 23 23 ...
    ##  $ interval: int  2210 2215 410 430 535 550 555 600 605 610 ...

    #agg <- tapply(activity$steps,activity$date,sum)

2. Total number of steps taken per day, their mean and median
-------------------------------------------------------------

    # steps_agg <- tapply(activity$steps,activity$date,sum)
    daily_steps <- aggregate(steps ~ date, activity, sum)
    hist(daily_steps$steps, main="Histogram of the total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    steps_mean <- mean(daily_steps$steps, na.rm = TRUE)
    steps_median <- median(daily_steps$steps, na.rm = TRUE)

Mean of number of steps taken each day is 1.076618910^{4}

Median number of steps taken each day is 10765

3. Average number of steps taken
--------------------------------

    library(ggplot2)
    steps_avg_per_interval <- aggregate(steps ~ interval, activity,mean)
    plot(steps_avg_per_interval$interval,steps_avg_per_interval$steps, type="l",xlab = "5-min intervals", ylab = "Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ms <- max(steps_avg_per_interval$steps)
    x <- which.max(steps_avg_per_interval$steps)
    intN <- steps_avg_per_interval$interval[x]

Interval with maximum average number of steps (352.483871) is 835

4. Imputing missing values
--------------------------

    tot_NA <- sum(!complete.cases(raw_activity))

Total number of missing values is 2304

### Replacing missing steps values with average for that interval

    imputed <- raw_activity
    avg <- aggregate(steps ~ interval, all_activity,mean) 
    for(i in 1:length(imputed$steps)) {
      if (is.na(imputed$steps[i])) {
        x <- which(avg$interval == imputed$interval[i])
        imputed$steps[i] <- avg$steps[x]
      } 
    }
    imp_NA <- sum(!complete.cases(imputed))

Total number of missing values is 0

    daily_steps <- aggregate(steps ~ date, imputed, sum)
    hist(daily_steps$steps, main="Total number of steps taken each day after imputing mising values")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    new_mean <- mean(daily_steps$steps, na.rm = TRUE)
    new_median <- median(daily_steps$steps, na.rm = TRUE)
    d_mean <- new_mean - steps_mean
    d_median <- new_median - steps_median

After the missing values where imputing:

Mean of number of steps taken each day: 1.076618910^{4} (changed by 0)

Median number of steps taken each day: 1.076618910^{4} (change by
1.1886792)

Differences in activity patterns between weekdays and weekends
--------------------------------------------------------------

    dtw <- as.POSIXlt(as.Date(imputed$date,"%m/%d/%Y"))$wday
    dt_type <- ifelse(dtw %in% c(1:5), "weekday", "weekend")
    imputed$daytype <- factor(dt_type, labels = c("weekday", "weekend"))

    # Creating a plot
    library(lattice)
    avg <- aggregate(steps ~ interval + daytype, imputed, mean)
    y <- avg$steps
    x <- avg$interval
    f <- avg$daytype
    xyplot(y ~ x | f, layout= c(1,2), type="l", xlab = "5-min intervals", ylab="Average number od steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

END
---
