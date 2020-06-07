---
title: "Reproducible_Project"
author: "Lakshmi Kovvuri"
date: "6/7/2020"
output: html_document
---
# Download the data file

temp<-tempfile()

fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download.file(fileUrl, temp, mode="wb")

# Unzip the file and read the csv file

unzip(temp, "activity.csv")

activity<-read.csv("activity.csv")

# Loading libraries

library(ggplot2)
library(dplyr)

# Task1: Calculate the total number of steps taken per day.

totalSteps<-aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)

# Task2: Make a Histogram of the total number of steps taken per day.

 g<-hist(totalSteps$steps, main="Total steps taken per day", xlab="Steps", col="deeppink", breaks=20)
 
 print(g)
 
 png(file="g.png")

# Task3: Mean and Median of the total number of steps

 MeanWithNA<-mean(totalSteps$steps, na.rm=TRUE)

 MedianWithNA<-median(totalSteps$steps, na.rm=TRUE)

# Task4: Time series plot of the average steps taken 

 averageSteps<-aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)

  # creating plot for the average steps

   g1<-ggplot(averageSteps, aes(interval, steps))

  # Setting shape and color for the plot

   g1<-g1+geom_line(col="blue", lwd=1.2)

  # Setting titles for the plot

 g1<-g1+ggtitle("Average daily activity pattern")+ xlab ("Time Interval") + ylab("Steps")

print(g1) 
png(file="g1.png")


 # Task 5: Which 5-min interval contains the maximum number of steps

 maxSteps<-averageSteps$interval [which.max (averageSteps$steps) ]

# Task 6: Calculate the total number of missing values in the dataset

 missingSteps<-sum(is.na(activity$steps))

# Devise a strategey for filling all missing values with mean

  matchSteps<-round(averageSteps$steps[match(activity$interval, averageSteps$interval)],0)

imputeSteps<- ifelse(is.na(activity$steps), matchSteps, activity$steps)

# create a new data frame with the missing data filled in

revisedActivity<-data.frame(steps=imputeSteps, interval=activity$interval, date=activity$date)

# Task 7: Histogram of the steps taken after missing values are imputed 

 # Grouping imputed steps with date

ImputedSteps<-aggregate(revisedActivity$steps, list(revisedActivity$date), FUN=sum)

 # setting column names for the table

colnames(ImputedSteps)<-c("Date", "Steps")

# Creating histogram for the imputed steps

g2<-ggplot(ImputedSteps, aes(Steps))

# setting shape and colors for the plot

g2<-g2+geom_histogram(boundary=0, binwidth=2000, col="chartreuse4", fill="chartreuse3")

# setting titles for the plot

g2<-g2+ggtitle("Histogram of the steps with missing data filled")+xlab("Steps")+ylab("Interval")

print(g2)
png(file="g2.png")

# Mean and median of the steps taken after missing data filled

MeanWithoutNA<-mean(ImputedSteps$Steps)

MedianWithoutNA<-median(ImputedSteps$Steps)

# Difference of 'Steps taken per day' and
#   'Steps taken per day after imputing missing data'

 MeanWithNA-MeanWithoutNA

MedianWithNA-MedianWithoutNA

# Impact of imputing missing data on the estimates of the steps

summary(totalSteps)

summary(ImputedSteps)

# Task 8: Panel plot comparing the average number of steps 
#         across weekdays and weekends

#Converting date to date type

revisedActivity$date<-as.Date(revisedActivity$date, format="%Y-%m-%d")

  # function to check whether the given date is weekend of weekday

TypeOfweek<-sapply(revisedActivity$date, 
               function(x)
               {
                 if(weekdays(x)=="Saturday"|weekdays(x)=="Sunday")
                  {
                   y<-"Weekend"
                  }
                 else
                  {
                    y<-"Weekday"
                   }
                 y
                })

  # Adding the field 'TypeOfweek' to the dataset

Activity2<-mutate(revisedActivity, TypeOfweek )

# Calculating average for the steps taken across weekends & weekdays

weeksActivity<-aggregate(steps~interval, data=Activity2, FUN=mean, na.rm=TRUE)

# Creating plot for the average daily steps across weekends & weekdays

g3<-ggplot(Activity2, aes(x=interval, y=steps, color=TypeOfweek))

# Setting shape for the plot

g3<-g3+geom_line()+ facet_wrap(~TypeOfweek, ncol=1,nrow=2)

# Setting names for the plot

g3<-g3+labs(title="Average steps for Weekdays & Weekends", x="Interval", y="Steps")

print(g3)
png(file="g3.png")

message("Cheers...'echo=TRUE'...")


         

