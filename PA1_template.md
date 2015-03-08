
# Reproducible Research: Peer Assessment 1

## Loading the data 

```{r,echo=TRUE}
activityData<-read.csv("activity.csv",sep=",",head=TRUE)
activityData$date<-as.Date(activityData$date)
```


## What is mean total number of steps taken per day?
### A histogram of the total number of steps taken each day
```{r}
totalStepsPerDay<-tapply(activityData$steps,activityData$date,sum)
hist(totalStepsPerDay,xlab="Total number steps taken per day",main="Histogram of the total number of steps taken each day")
```

### mean total number of steps taken per day
```{r}
mean(totalStepsPerDay,na.rm=TRUE)
```
### median total number of steps taken per day
```{r}
median(totalStepsPerDay,na.rm=TRUE)
```

## What is the average daily activity pattern?
### a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
meanStepsInterval<-tapply(activityData$steps,activityData$interval,mean,na.rm=TRUE)
intervals<-unique(activityData$interval)
plot(intervals,meanStepsInterval,type="l",xlab="Interval",ylab="Nums of steps")
```

### Which h 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
intervals[which.max(meanStepsInterval)]
```

## Imputing missing values
### the total number of missing values in the dataset
```{r}
nrow(activityData)-sum(complete.cases(activityData))
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r}
#strategy for filling misising values: mean steps for that intervals
meanStepsIntervalsData<-data.frame(meanStepsInterval,intervals)
names(meanStepsIntervalsData)<-c("meanStep","interval")
# copy a new dataset and  filling the missing  value
newActivityData<-activityData
for (i in 1:nrow(newActivityData))
  if (is.na(newActivityData$step[i]))
    newActivityData$steps[i]<-meanStepsInterval[meanStepsIntervalsData$interval==newActivityData$interval[i]]
head(newActivityData)
```
### a histogram of the total number of steps taken each day 
```{r}
newTotalStepsPerDay<-tapply(newActivityData$steps,newActivityData$date,sum,na.rm=TRUE)
hist(newTotalStepsPerDay,xlab="Total number steps taken per day",main="Histogram of the total number of steps taken each day")
```
### mean total number steps taken per day
```{r}
newMeanTotalStepsPerDay<-mean(newTotalStepsPerDay,na.rm=TRUE)
```
### median total number steps taken per day
```{r}
newMedianTotalStepsPerDay<-median(newTotalStepsPerDay,na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
weekdayLevel<-weekdays(newActivityData$date)
weekdayLevel[weekdayLevel=="星期一"|weekdayLevel=="星期二"|weekdayLevel=="星期三"|weekdayLevel=="星期四"|weekdayLevel=="星期五"]<-"weekday"
weekdayLevel[weekdayLevel=="星期六"|weekdayLevel=="星期日"]<-"weekend"
newActivityData<-cbind(newActivityData,weekdayLevel)
head(newActivityData)
```
### a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
```{r}
newActivityData<-cbind(newActivityData,weekdayLevel)
weekdayData<-newActivityData[newActivityData$weekdayLevel=="weekday",]
meanWeekday<-tapply(weekdayData$steps,weekdayData$interval,mean,na.rm=TRUE)
weekendData<-newActivityData[newActivityData$weekdayLevel=="weekend",]  
meanWeekend<-tapply(weekendData$steps,weekendData$interval,mean,na.rm=TRUE)
# meanDayData<-data.frame(meanWeekday,meanWeekend)
par(mfrow=c(1,2))
plot(intervals,meanWeekday,type="l",xlab="Interval",ylab="Mean Steps",main="weekday")
plot(intervals,meanWeekend,type="l",xlab="Interval",ylab="Mean Steps",main="weekend")
```