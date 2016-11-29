---
title: "The Stepping Habits of Anonymous"
author: "kb"
date: "November 27, 2016"
output: html_document
---

This report analyzes data from Anonymous's walking habits.

First, we'll need to load the data from our working directory:


```r
setwd("/home/thea/Desktop/R working directory/ReproResearch/Wk2")
ourfile <- read.csv("activity.csv", header=TRUE)
```

# 1) What is the mean total number of steps taken per day?

Our first question is how many total steps Anonymous walks per day. What are the mean and median steps per day?

We can generate three lists to show us this information:


```r
sumsteps <- aggregate(ourfile$steps~ourfile$date, ourfile, sum)
meansteps <- aggregate(ourfile$steps~ourfile$date, ourfile, mean)
mediansteps <- aggregate(ourfile$steps~ourfile$date, ourfile, median)

colnames(sumsteps) <- c("Date", "TotalSteps")
meansteps[,2] <- as.integer(meansteps[,2])

stepswnas <- data.frame(sumsteps,meansteps[,2],mediansteps[,2])
colnames(stepswnas) <- c("Date", "Total Steps", "Average Steps", 
                         "Median Steps")
```

Here are the first 20 rows of data compiled into one table. Notice rows with missing data have been omitted:


```
##          Date Total Steps Average Steps Median Steps
## 1  2012-10-02         126             0            0
## 2  2012-10-03       11352            39            0
## 3  2012-10-04       12116            42            0
## 4  2012-10-05       13294            46            0
## 5  2012-10-06       15420            53            0
## 6  2012-10-07       11015            38            0
## 7  2012-10-09       12811            44            0
## 8  2012-10-10        9900            34            0
## 9  2012-10-11       10304            35            0
## 10 2012-10-12       17382            60            0
## 11 2012-10-13       12426            43            0
## 12 2012-10-14       15098            52            0
## 13 2012-10-15       10139            35            0
## 14 2012-10-16       15084            52            0
## 15 2012-10-17       13452            46            0
## 16 2012-10-18       10056            34            0
## 17 2012-10-19       11829            41            0
## 18 2012-10-20       10395            36            0
## 19 2012-10-21        8821            30            0
## 20 2012-10-22       13460            46            0
```

Here are the Total Steps per day depicted in a histogram:


```r
barplot(sumsteps[,2], names.arg=sumsteps[,1],
        main="Total Steps per Day, excluding NAs",
        xlab="Date", ylab="Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

# 2) What is the average daily activity pattern?

Next, let's study the average number of steps taken per 5-minute interval. This will show at which times of day Anonymous is most active, on average.


```r
stepsperint <- aggregate(ourfile$steps~ourfile$interval, ourfile, mean)
colnames(stepsperint) <- c("interval","AvgSteps")
```

Here is a time series plot of when Anonymous is most active:


```r
plot(stepsperint$interval, stepsperint$AvgSteps, type="l", 
     xlab="5-min interval", ylab="Steps taken", 
     main="Time lapse of average steps over 24 hours")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

At which time of day is Anonymous most active, on average?


```r
maxstep <- which(stepsperint[,2] == max(stepsperint[,2])) #placeholder of max steps
int <- stepsperint[maxstep,1]
timehr <- paste(substr(int,1,1),":",substr(int,2,3)," am", sep="")
```

Thus, Anonymous takes the most steps, on average, around **8:35 am**.

# 3) Inputing missing values

Now we'd like to address missing data points. First, let's identify all missing values in our data.


```r
nas <- ourfile[is.na(ourfile$steps),] #data frame with all na values
totalnas <- length(nas[,1])
percentnas <- round(totalnas/length(ourfile[,1])*100,2)
```

It looks like there are 2304 missing rows in our data, or 13.11% of all the rows. We can fill these in with the average steps per 5-min interval that Anonymous usually walks on other days.


```r
#1 create big vector w "answer" column adjacent to Steps + reorder
answerkey <- merge(ourfile,stepsperint)
answerkey <- answerkey[with(answerkey, order(date,interval)),]

#2 if steps = na, replace w answerkey
fullsteps <- with(answerkey,ifelse(is.na(steps),AvgSteps,steps))

#3 stitch "fullsteps" back into full data frame
complete <- data.frame(answerkey[,3],answerkey[,1],fullsteps)
complete[,3] <- as.integer(complete[,3])
colnames(complete) <- c("Date", "Interval", "Steps")
```

The results is a "filled in" data set, where N/A values have been replaced by the average value for that interval. Here is a sample of the top 20 rows:


```
##          Date Interval Steps
## 1  2012-10-01        0     1
## 2  2012-10-01        5     0
## 3  2012-10-01       10     0
## 4  2012-10-01       15     0
## 5  2012-10-01       20     0
## 6  2012-10-01       25     2
## 7  2012-10-01       30     0
## 8  2012-10-01       35     0
## 9  2012-10-01       40     0
## 10 2012-10-01       45     1
## 11 2012-10-01       50     0
## 12 2012-10-01       55     0
## 13 2012-10-01      100     0
## 14 2012-10-01      105     0
## 15 2012-10-01      110     0
## 16 2012-10-01      115     0
## 17 2012-10-01      120     0
## 18 2012-10-01      125     1
## 19 2012-10-01      130     1
## 20 2012-10-01      135     0
```

Now, let's calculate the Total, Average, and Median steps taken per day of this complete data set:


```r
sumfullsteps <- aggregate(complete$Steps~complete$Date, complete, sum)
meanfullsteps <- aggregate(complete$Steps~complete$Date, complete, mean)
medianfullsteps <- aggregate(complete$Steps~complete$Date, complete, median)

sumfullsteps[,2] <- sapply(sumfullsteps[,2],as.integer)
meanfullsteps[,2] <- sapply(meanfullsteps[,2],as.integer)
medianfullsteps[,2] <- sapply(medianfullsteps[,2],as.integer)

colnames(sumfullsteps) <- c("Date","TotSteps")
colnames(meanfullsteps) <- c("Date", "AvgSteps")
colnames(medianfullsteps) <- c("Date","MedSteps")
```

We can compare the "adjusted" data to the original data set with NAs.


```r
#1 clean data
colnames(stepswnas) <- c("Date", "TotStepsNA", "AvgStepsNA", "MedStepsNA")
stepswnas[,3] <- as.integer(stepswnas[,3])

#2 merge all data frames
merge1 <- merge(stepswnas, sumfullsteps, all=T)
merge2 <- merge(meanfullsteps, medianfullsteps)
allstats <- merge(merge1,merge2, all=T)

#3 reorder columns for clarity
allstats <- allstats[,c(1,2,5,3,6,4,7)]
```

Which gives us the following data frame, where headers including 'NA' are the original, unadjusted data. Here is an excerpt of the top 20 rows:


```
##          Date TotStepsNA TotSteps AvgStepsNA AvgSteps MedStepsNA MedSteps
## 1  2012-10-01         NA    10641         NA       36         NA       33
## 2  2012-10-02        126      126          0        0          0        0
## 3  2012-10-03      11352    11352         39       39          0        0
## 4  2012-10-04      12116    12116         42       42          0        0
## 5  2012-10-05      13294    13294         46       46          0        0
## 6  2012-10-06      15420    15420         53       53          0        0
## 7  2012-10-07      11015    11015         38       38          0        0
## 8  2012-10-08         NA    10641         NA       36         NA       33
## 9  2012-10-09      12811    12811         44       44          0        0
## 10 2012-10-10       9900     9900         34       34          0        0
## 11 2012-10-11      10304    10304         35       35          0        0
## 12 2012-10-12      17382    17382         60       60          0        0
## 13 2012-10-13      12426    12426         43       43          0        0
## 14 2012-10-14      15098    15098         52       52          0        0
## 15 2012-10-15      10139    10139         35       35          0        0
## 16 2012-10-16      15084    15084         52       52          0        0
## 17 2012-10-17      13452    13452         46       46          0        0
## 18 2012-10-18      10056    10056         34       34          0        0
## 19 2012-10-19      11829    11829         41       41          0        0
## 20 2012-10-20      10395    10395         36       36          0        0
```



Notice the Total Steps taken across the course of the study increase from 570608 with NAs included, to 655736 with NA values estimated from remaining data. This amounts to an average of 9354 steps per day with NAs included, and 10749 steps per day with "filled in" data.

Finally, let's examine a histogram of the Total Steps per day.


```r
with(sumfullsteps,{
    barplot(TotSteps, names.arg=Date, 
            col=adjustcolor("blue",alpha.f=.5),
            main="Total Steps per Day",
            xlab="Date", ylab="Steps")
})
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

# 4) Are there differences in activity patterns between weekdays and weekends?

First, let's add a column to our "filled in" data frame indicating whether each date is a weekday or weekend.


```r
#1 column for which day of week each date is
complete[,4] <- sapply(as.Date(complete[,1]),weekdays)
colnames(complete)[4] <- "DayofWeek"
complete[,3] <- as.integer(complete[,3])

#2 column: weekday or weekend?
complete[,5] <- ifelse(complete[,4]=="Saturday" | 
                       complete[,4]=="Sunday", "weekend", 
                       "weekday")
colnames(complete)[5] <- "DayType"
```

Now we can prepare our data for visualization by aggregating it by weekday vs weekend and 5-min interval:


```r
wklysteps <- aggregate(complete$Steps~complete$Interval+DayType, complete, mean)
wklysteps[,3] <- as.integer(wklysteps[,3])
colnames(wklysteps) <- c("Interval", "DayType", "Steps")
```

Here is the code to plot a time series of when Anonymous is most active, segregated by weekday vs weekend:


```r
library(ggplot2)
wkdayplot <- 
    with(wklysteps,{
        qplot(Interval, Steps, facets = DayType~., geom="line",
              main="Average Steps Taken by Weekday/Weekend")
    })
```

And the plot itself:

![plot of chunk wkdayplot](figure/wkdayplot-1.png)

# THE END

