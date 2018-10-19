Activity Dataset
========================================================
Load the 'Activity Dataset' and necessary libraries

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
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
activity<-read.csv('activity.csv')
```
group by date

```r
comp_activity<-na.omit(activity)
dailystep<-group_by(comp_activity,date)%>%summarize(step=sum(steps))
dailystep
```

```
## # A tibble: 53 x 2
##    date        step
##    <fct>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```
plot histogram

```r
qplot(step, bins=30, data=dailystep)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


mean of steps taken per day


```r
mean(dailystep$step)
```

```
## [1] 10766.19
```

median of steps taken per day

```r
median(dailystep$step)
```

```
## [1] 10765
```
group steps by interval


```r
mean_step_per_interval<-group_by(comp_activity, interval)%>%summarize(step=mean(steps))
```

Time series plot of interval against step


```r
plot<-ggplot(mean_step_per_interval, aes(y=step,x=interval))+geom_line()+labs(title='Interval againt time')
plot
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
5-minute interval with maximum steps


```r
mean_step_per_interval$interval[which.max(mean_step_per_interval$step)]
```

```
## [1] 835
```
##IMPUTING MISSING VALUES##
=============================

total missing values(NA)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

Replace missing values with the mean steps for that 5-mins Interval



```r
act.copy<-activity
for (i in seq(nrow(act.copy))) {
  if (is.na(act.copy[i,1])) {
    intvl<-act.copy[i,3]
    act.copy[i,1]<-round(subset(mean_step_per_interval,interval==intvl,step),3)
  }
}
sum(is.na(act.copy))
```

```
## [1] 0
```

```r
dim(act.copy)
```

```
## [1] 17568     3
```

Histogram of total steps taken after imputation


```r
steps.day<-group_by(act.copy,date)%>%summarize(steps.per.day=sum(steps))
head(steps.day,8)
```

```
## # A tibble: 8 x 2
##   date       steps.per.day
##   <fct>              <dbl>
## 1 2012-10-01        10766.
## 2 2012-10-02          126 
## 3 2012-10-03        11352 
## 4 2012-10-04        12116 
## 5 2012-10-05        13294 
## 6 2012-10-06        15420 
## 7 2012-10-07        11015 
## 8 2012-10-08        10766.
```

```r
qplot(steps.per.day,  data=steps.day)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

##Mean Steps per day after imputation##

```r
mean(steps.day$steps.per.day)
```

```
## [1] 10766.19
```

##median steps per day after imputation##

```r
median(steps.day$steps.per.day)
```

```
## [1] 10766.19
```

**Convert date variable using as.Date function

```r
act.copy$day<-weekdays(as.Date(act.copy$date,'%Y-%m-%d'))
head(act.copy)
```

```
##   steps       date interval    day
## 1 1.717 2012-10-01        0 Monday
## 2 0.340 2012-10-01        5 Monday
## 3 0.132 2012-10-01       10 Monday
## 4 0.151 2012-10-01       15 Monday
## 5 0.075 2012-10-01       20 Monday
## 6 2.094 2012-10-01       25 Monday
```

**Group days into weekdays and weekends**


```r
act.copy$weektype<-ifelse(act.copy$day %in% c('Saturday','Sunday'),'weekend','weekday')
act.copy$weektype<-as.factor(act.copy$weektype)
head(act.copy,10)
```

```
##    steps       date interval    day weektype
## 1  1.717 2012-10-01        0 Monday  weekday
## 2  0.340 2012-10-01        5 Monday  weekday
## 3  0.132 2012-10-01       10 Monday  weekday
## 4  0.151 2012-10-01       15 Monday  weekday
## 5  0.075 2012-10-01       20 Monday  weekday
## 6  2.094 2012-10-01       25 Monday  weekday
## 7  0.528 2012-10-01       30 Monday  weekday
## 8  0.868 2012-10-01       35 Monday  weekday
## 9  0.000 2012-10-01       40 Monday  weekday
## 10 1.472 2012-10-01       45 Monday  weekday
```

**Calculate mean daily step per 5-min interrval for all days**


```r
r<-act.copy%>%group_by(interval,weektype)%>%summarize(mean.step=mean(steps))
class(act.copy$weektype)
```

```
## [1] "factor"
```

```r
head(r)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval weektype mean.step
##      <int> <fct>        <dbl>
## 1        0 weekday     2.25  
## 2        0 weekend     0.215 
## 3        5 weekday     0.445 
## 4        5 weekend     0.0425
## 5       10 weekday     0.173 
## 6       10 weekend     0.0165
```

*PANEL PLOT OF WEEKEND AND WEEKDAY MEAN STEPS*


```r
p<-ggplot(r, aes(x=interval,y=mean.step))+geom_line()+facet_grid(.~weektype)
p
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)




