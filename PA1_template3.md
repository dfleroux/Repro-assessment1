---
title: "Reproducible Research Assignment 1" 
author: "Denis Leroux"
date: "29/10/2019"
output: 
  html_document:
    keep_md: true
---



# COURSERA Reproducible Research Peer Assessment Project 1
### Orignal submission on Nov. 16th, resubmitted on Nov. 29th

# INTRODUCTION

This assignment makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day. The data
consists of two months of data from an anonymous individual collected during
the months of October and November, 2012 and include the number of steps
taken in 5 minute intervals each day.
Data
The data for this assignment can be downloaded from the course web site:
• Dataset: Activity monitoring data [52K]
The variables included in this dataset are:
• steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA)
• date: The date on which the measurement was taken in YYYY-MM-DD
format
• interval: Identifier for the 5-minute interval in which measurement was
taken
The dataset is stored in a comma-separated-value (CSV) file and there are a
total of 17,568 observations in this dataset.

# DATA DOWNLOAD

Data downloaded from [website] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 
Activity data downloaded into MasterDataFile "MDF"
Process/transform the data from 'factor' to 'date'

## Instruction for reviewers: please make sure that the "activity.csv" file is in the same  directory as the markdown rmd file when loading data.


```r
# Activity data downloaded into MasterDataFile "MDF"
MDF = read.csv(file = "activity.csv", header=TRUE, sep=",")

#Process/transform the data from 'factor' to 'date'
MDF$date = as.Date(MDF$date,"%Y-%m-%d")
```

## PART A:  Analysis without NA replacement
### Total steps per day (histogram)

```r
table=table(MDF$date)
SPD=NULL
for (i in 1:length(table)) {
   SPD[i] = sum(MDF[MDF$date==names(table[i]),][,1],na.rm=T)    
}

hist(SPD, main = "Total number of steps taken per day",xlab = "steps")
```

![](PA1_template3_files/figure-html/total steps per day-1.png)<!-- -->

### Mean and median total number of steps taken per day====


```r
Mean_per_day=NULL; Median_per_day=NULL;Summary=NULL;Summary2=NULL

for (i in 1:length(table)) {
   Summary[[i]]= summary(MDF[MDF$date==names(table[i]),][,1], na.rm=T)
}

Mean_per_day = unlist(lapply(Summary,"[", 4)); names(Mean_per_day) = levels(MDF$date)
Median_per_day = unlist(lapply(Summary,"[", 3)); names(Median_per_day) = levels(MDF$date)

print("The MEAN total number of steps taken per day from day 1 to day 61 is ")
```

```
## [1] "The MEAN total number of steps taken per day from day 1 to day 61 is "
```

```r
print (round(Mean_per_day, 2))
```

```
##  [1]   NaN  0.44 39.42 42.07 46.16 53.54 38.25   NaN 44.48 34.38 35.78
## [12] 60.35 43.15 52.42 35.20 52.38 46.71 34.92 41.07 36.09 30.63 46.74
## [23] 30.97 29.01  8.65 23.53 35.14 39.78 17.42 34.09 53.52   NaN 36.81
## [34] 36.70   NaN 36.25 28.94 44.73 11.18   NaN   NaN 43.78 37.38 25.47
## [45]   NaN  0.14 18.89 49.79 52.47 30.70 15.53 44.40 70.93 73.59 50.27
## [56] 41.09 38.76 47.38 35.36 24.47   NaN
```

```r
print("The MEDIAN total number of steps taken per day from day 1 to day 61 is ")
```

```
## [1] "The MEDIAN total number of steps taken per day from day 1 to day 61 is "
```

```r
print(round(Median_per_day, 2))
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [24]  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0
## [47]  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA
```


###  Average daily activity pattern
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
step_int=as.numeric(levels(as.factor(MDF$interval)))

for (i in 1:length(step_int)) {
  Summary2[[i]]= summary(MDF[MDF$interval == step_int[i],] [,1], na.rm=T)
}

Mean_per_day.interval = unlist(lapply(Summary2,"[", 4))
names(Mean_per_day.interval) = as.character(step_int)
plot(x = step_int, y = Mean_per_day.interval , type="l", xlab = "Interval", ylab = "Mean number of steps per day & interval", main = "Mean nb. of steps par interval ovar all days")
```

![](PA1_template3_files/figure-html/av. daily activity pattern-1.png)<!-- -->

```r
step_max=which(Mean_per_day.interval == max(Mean_per_day.interval))
print(paste("Max activity at interval = ", MDF$interval[step_max] , " interval, which corresponds to ", round(step_max/12, 1), "hours", sep=""))
```

```
## [1] "Max activity at interval = 835 interval, which corresponds to 8.7hours"
```

## PART B:  filling in all of the missing values in the dataset by replacing NAs by mean
## INSTRUCTIONS
Imputing missing values
Note that there are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do
these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total
daily number of steps?



### B1. Calculate and report the total number of missing values in the dataset

```r
NA_MDF = sum(is.na(MDF))
print(paste("total number of missing values in the dataset", NA_MDF))
```

```
## [1] "total number of missing values in the dataset 2304"
```


### B2 STRATEGY TO REPLACE NAs: Filling in all of the missing values in the dataset

### B2.1. Replacing NA values in summary by zero if total nb of steps is zero for a given date

```r
SummaryB=NULL
for (i in 1:length(Summary)) {
   SummaryB[[i]] = ifelse(is.na(Summary[[i]]), 0, Summary[[i]])
}
```
  
   
### B2.2. Replacing NA values by mean of that day
### B3. Create a new dataset  with missing data filled in.

```r
MDFB=MDF
for (i in 1:length(SummaryB)) {
   MDFB$steps=ifelse(is.na(MDFB$steps) & MDFB$date == levels(as.factor(MDFB$date))[i], SummaryB[[i]] [4], MDFB$steps)
}
```
  

### B4.1. Make a histogram of the total number of steps taken each day

```r
tableB=table(MDFB$date)
SPDB=NULL
for (i in 1:length(tableB)) {
        SPDB[i] = sum(MDFB[MDFB$date==names(tableB[i]),][,1],na.rm=T)    
}
par(mfrow=c(1,2))
hist(SPD, main = "Total number of steps taken per day (with NAs)", xlab = "steps", cex.main = 0.8)
hist(SPDB, main = "Total number of steps taken per day \n(NA replaced by mean at a given day)", xlab = "steps", cex.main = 0.8)
```

![](PA1_template3_files/figure-html/hist compare-1.png)<!-- -->

```r
SPD == SPDB
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [43] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [57] TRUE TRUE TRUE TRUE TRUE
```
The logical test above shows no difference before /after NA replacement

### B4.2.Mean total number of steps taken per day after replacing NAs per mean 


```r
Mean_per_dayB=NULL; Median_per_dayB=NULL;SummaryB=NULL;SummaryB2=NULL
 for (i in 1:length(tableB)) {
     Mean_per_dayB[i] = round(mean(MDFB[MDFB$date==names(tableB[i]),][,1], na.rm=T),2) 
     Median_per_dayB[i] = round(median(MDFB[MDFB$date==names(tableB[i]),][,1], na.rm=TRUE),2) 
    SummaryB[[i]]= summary(MDFB[MDFB$date==names(tableB[i]),][,1], na.rm=T)
 }
Mean_per_dayB = unlist(lapply(SummaryB,"[", 4)); names(Mean_per_dayB) = levels(MDFB$date)
Median_per_dayB = unlist(lapply(SummaryB,"[", 3)); names(Median_per_dayB) = levels(MDFB$date)

print("Mean_per_day with NAs replaced by mean")
```

```
## [1] "Mean_per_day with NAs replaced by mean"
```

```r
print(round(Mean_per_dayB), 2)
```

```
##  [1]  0  0 39 42 46 54 38  0 44 34 36 60 43 52 35 52 47 35 41 36 31 47 31
## [24] 29  9 24 35 40 17 34 54  0 37 37  0 36 29 45 11  0  0 44 37 25  0  0
## [47] 19 50 52 31 16 44 71 74 50 41 39 47 35 24  0
```

```r
print("Median_per_day with  NAs replaced by mean")
```

```
## [1] "Median_per_day with  NAs replaced by mean"
```

```r
print(round(Median_per_dayB), 2)
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [36] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

### Compare Means and Median with and whithout NAs being replaced by means on a given day: are they equal ?

```r
print("Are Mean_per_day equal for each of the 61 days?")
```

```
## [1] "Are Mean_per_day equal for each of the 61 days?"
```

```r
Mean_per_dayB == Mean_per_day
```

```
##  [1]   NA TRUE TRUE TRUE TRUE TRUE TRUE   NA TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [29] TRUE TRUE TRUE   NA TRUE TRUE   NA TRUE TRUE TRUE TRUE   NA   NA TRUE
## [43] TRUE TRUE   NA TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [57] TRUE TRUE TRUE TRUE   NA
```

```r
print("Are Medium_per_day equal for each of the 61 days?")
```

```
## [1] "Are Medium_per_day equal for each of the 61 days?"
```

```r
Median_per_dayB == Median_per_day
```

```
##  [1]   NA TRUE TRUE TRUE TRUE TRUE TRUE   NA TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [29] TRUE TRUE TRUE   NA TRUE TRUE   NA TRUE TRUE TRUE TRUE   NA   NA TRUE
## [43] TRUE TRUE   NA TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [57] TRUE TRUE TRUE TRUE   NA
```


The logical tests above shows that they are equal. Indeed all NAs originate from unique days, in which case the total nb of steps is zero and there is no added steps in fine when replacing by the NULL mean. 

## C. Are there differences in activity patterns between weekdays and weekends?

### C1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend”


```r
   # assign days to master data file
MDFW=cbind(MDFB, weekdays(MDF$date))
colnames(MDFW) [4] = "day"
   #split into two dataframes for weekdays (MDFD) & for weekends (MDFE))
MDFE=MDFW[(MDFW$day == "Saturday") | (MDFW$day == "Sunday"),]
MDFD=MDFW[!(MDFW$day == "Saturday") & (!MDFW$day == "Sunday"),]
```


### C2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
SummaryB2_E=NULL; SummaryB2_D = NULL 
for (i in 1:length(step_int)) {
   SummaryB2_E[[i]]= summary(MDFE[MDFE$interval == step_int[i],] [,1], na.rm=T)
   SummaryB2_D[[i]]= summary(MDFD[MDFD$interval == step_int[i],] [,1], na.rm=T)
}

Mean_per_day.interval_E = unlist(lapply(SummaryB2_E,"[", 4))
names(Mean_per_day.interval_E) = as.character(step_int)
Mean_per_day.interval_D = unlist(lapply(SummaryB2_D,"[", 4))
names(Mean_per_day.interval_D) = as.character(step_int)

par(mfrow=c(2,1))
plot(x = step_int, y = Mean_per_day.interval_E , type="l", xlab = "Interval", ylab = "Mean number of steps", main = "Mean number of steps per day & interval on Weekends",cex.main=0.6, cex.lab= 0.6, ylim=c(0,200))
plot(x = step_int, y = Mean_per_day.interval_D , type="l", xlab = "Interval", ylab = "Mean number of steps", main = "Mean nb. of steps par interval over all days on weekdays",cex.main=0.6, cex.lab= 0.6,  ylim=c(0,200))
```

![](PA1_template3_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
step_max_B=which(Mean_per_day.interval == max(Mean_per_day.interval))
```

### CONCLUSION: Activity is overall higher on Weekends. The strong activity peak happens around 2 PM  on weekdays and is well marked unlike on weeg ends where activity is spreadout over the day from approx 10 AM to 7 PM


