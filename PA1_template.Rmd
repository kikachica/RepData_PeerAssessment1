---
title: "Reproducible Research - First Peer Assignment"
output: html_document 
keep_md: true

---
###Data  

The data for this assignment can be downloaded from the course web site:  

Dataset: [Activity monitoring data][1] [52K]

The variables included in this dataset are:  

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date**: The date on which the measurement was taken in YYYY-MM-DD format

**interval**: Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.  

###Loading and preprocessing the data  

Let's load the data first and set locale to English.

```{r, echo=TRUE}
setwd("~/Coursera/Reproducible research/RepData_PeerAssessment1")
activity <- read.csv("~/Coursera/Reproducible research/RepData_PeerAssessment1/activity.csv", stringsAsFactors=FALSE, row.names=NULL)
Sys.setlocale("LC_TIME", "English")
```

First, let's transform *date* column to POSIXct object and remove NAs.  
For this, I will use the **lubridate** package.

```{r, results='hide'}
library(lubridate)
activity$date<-ymd(activity$date)
activity_no_NA<-na.omit(activity)
row.names(activity_no_NA)<-NULL
```

###What is mean total number of steps taken per day?
  
1. Calculate the total number of steps taken per day 

2. Make a histogram of the total number of steps taken each day  

The following code creates a dataframe of total steps taken for each day and creates a histogram.

```{r}
activity_summary <- aggregate(steps ~ date, activity_no_NA, sum)
hist(activity_summary$steps, col="blue", main="Histogram of total number of steps taken per day",  
     xlab="Total number of steps in a day")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(activity_summary$steps)
median(activity_summary$steps)
```
The mean is **10766.19** and the median is **10765**.  

###What is the average daily activity pattern?  
  
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```{r}
activity_summary2 <- aggregate(steps ~ interval, activity_no_NA, mean, row.names=NULL)
with(activity_summary2, plot(steps~interval, type="l"))

```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```{r}
max(activity_summary2[,2])
activity_summary2$interval[which.max(activity_summary2[,2])]
```

Interval **835** contains maximum number of steps, which is **206.1698** steps.  

###Imputing missing values  
  
1. Calculate and report the total number of missing values in the dataset
  
```{r}
tot_NA=sum(is.na(activity$steps))
```
  
In the dataset there are **2304** missing values. 

2. Devise a strategy for filling in all of the missing values in the dataset. 
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

I have filled the missing values by taking average steps per interval.  
The new dataset is called *activity2*

```{r}
activity2<-activity
int<-0
step<-0
for(i in 1:nrow(activity2)){
        
        if(is.na(activity2[i,1])){
                int[i]<-activity2[i,3]
                
                for(j in 1:nrow(activity_summary2)){
                        
                        if(activity_summary2[j,1]==int[i]){
                                step[i]<-activity_summary2[j,2]
                                activity2[i,1]<-step[i]
                        }
                }                    
        }               
}
```
  
  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  
Do these values differ from the estimates from the first part of the assignment?  
What is the impact of imputing missing data on the estimates of the total daily number of steps?  

First, I draw a histogram of the new dataset.  

```{r}
activity_summary3 <- aggregate(steps ~ date, activity2, sum)
hist(activity_summary3$steps, col="red", main="Histogram of total number of steps taken per day",  
     xlab="Total number of steps in a day")
```

    
Then, I calculate the mean and the median of total number of steps taken per day.  
  
 ```{r}
mean(activity_summary3$steps)
median(activity_summary3$steps)
``` 
  
Both the mean and median are **10766.19**.    

We can notice that the mean of the new dataset is the same as the old one without NAs and the median differes a little. That means that adding missing values the way we added them do not impact a lot the total daily number of steps.    

###Are there differences in activity patterns between weekdays and weekends?    

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

 ```{r}
activity2$weekday<-as.factor(weekdays(activity2$date))
activity2$day<-0
for(i in 1:nrow(activity2)){
if(activity2[i,4]=="Saturday" || activity2[i,4]=="Sunday" )
        activity2[i,5]<-"weekend"
        else
        activity2[i,5]<-"weekday"
}
activity2$weekday<-NULL
activity2$day<-as.factor(activity2$day)
``` 
  
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  
  
```{r cache=T, fig.show='asis'}  
steps_per_WD <- aggregate(steps ~ interval+day, activity2, mean)
library(ggplot2)
p<-qplot(interval, steps, data=steps_per_WD, geom=c("line"), xlab="interval", ylab="Number of steps") + facet_wrap(~day, ncol=1)
print(p)
```
  
In the end, we remove all variables to free memory  
  

```{r}  
rm(activity, activity2, activity_summary, activity_summary2, activity_summary3, activity_no_NA, steps_per_WD)
```
    
[1]: "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"