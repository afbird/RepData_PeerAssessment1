# Course 5 - Project 2 

## initialize environment

setwd("C:/Users/afbir/Documents/Coursera/Course 5 - Data Analysis/Project 1")

library(dplyr)
library(ggplot2)
library(gridExtra)

## Section 1 - Loading and Preprocessing Data

### load data

dfActivity <- read.csv("activity.csv")

### remove NAs

dfActivityNoNAs <- dfActivity[complete.cases(dfActivity),]

### convert date to class Date

dfActivityNoNAs$date <- as.Date(dfActivityNoNAs$date)

## Section 2 - What is mean total number of steps taken per day?

### Calculate and plot the total number of steps taken per day

dfDailyTotal <- dfActivityNoNAs %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise(steps_daily_total = sum(steps)) %>%
  na.omit()

png("ggHistogram1.png")

ggHistogram1 <- ggplot(dfDailyTotal, aes(x=steps_daily_total)) + geom_histogram()

plot(ggHistogram1)

dev.off()

### Calculate and report the mean and median of the total number of steps taken per day

dfTotalMeanMedian <- dfDailyTotal %>%
  select(steps_daily_total) %>%
  summarise(steps_daily_mean = mean(steps_daily_total), steps_daily_median = median(steps_daily_total))

print(paste("Daily Mean = ", format(dfTotalMeanMedian$steps_daily_mean, nsmall=2)))
print(paste("Daily Median = ", dfTotalMeanMedian$steps_daily_median))

## Section 3 - What is the average daily activity pattern?

dfIntervalMeanNoNAs <- dfActivityNoNAs %>%
  select(interval, steps) %>%
  group_by(interval) %>%
  summarise(steps_interval_mean = mean(steps)) %>%
  na.omit()

png("ggTimeSeries1.png")

ggTimeSeries1 <- ggplot(dfIntervalMeanNoNAs, aes(x=interval, y=steps_interval_mean)) + geom_line()

plot(ggTimeSeries1)

dev.off()

dfIntervalMaxMeanNoNAs <- arrange(dfIntervalMeanNoNAs, desc(steps_interval_mean))[1,]

intIntervalNum <- as.character(dfIntervalMaxMeanNoNAs[1,1])
numIntervalMax <- as.character(round(dfIntervalMaxMeanNoNAs[1,2], digits = 2))

print(paste("Time step with maximum mean number of steps = ", intIntervalNum))
print(paste("Mean number of steps in interval = ", numIntervalMax))

## Section 4 - Imputing missing values

### calculate number of NAs

intNAs <- nrow(dfActivity) -  nrow(dfActivityNoNAs)

print(paste("Number of NAs = ", as.character(intNAs)))

### impute interval mean to NAs and create new dataset

dfActivityNew1 <- merge(x=dfActivity, y=dfIntervalMeanNoNAs)

dfActivityNew1$steps[is.na(dfActivityNew1$steps)] <- 
  dfActivityNew1$steps_interval_mean[is.na(dfActivityNew1$steps)]

dfActivityNew2 <- dfActivityNew1 %>% select( interval, steps, date)

### Calculate and plot the total number of steps taken per day

dfDailyTotal2 <- dfActivityNew2 %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise(steps_daily_total = sum(steps)) %>%
  na.omit()

png("ggHistogram2.png")

ggHistogram2 <- ggplot(dfDailyTotal2, aes(x=steps_daily_total)) + geom_histogram()

plot(ggHistogram2)

dev.off()

### Calculate and report the mean and median of the total number of steps taken per day

dfTotalMeanMedian2 <- dfDailyTotal2 %>%
  select(steps_daily_total) %>%
  summarise(steps_daily_mean = mean(steps_daily_total), steps_daily_median = median(steps_daily_total))

print(paste("Daily Mean with Imputed Values = ", format(dfTotalMeanMedian2$steps_daily_mean, nsmall=2)))
print(paste("Daily Median with Imputed Values = ", dfTotalMeanMedian2$steps_daily_median))

## Section 3 - What is the average daily activity pattern?

dfActivityNew3 <- dfActivityNew2

dfActivityNew3$date = as.Date(dfActivityNew3$date)

dfActivityNew3$weekdayorweekend[!weekdays(dfActivityNew3$date) %in% c("Saturday", "Sunday")] <- "weekday"
dfActivityNew3$weekdayorweekend[weekdays(dfActivityNew3$date) %in% c("Saturday", "Sunday")] <- "weekend"

dfActivityNew3WeekDays <- filter(dfActivityNew3, weekdayorweekend == "weekday")
dfActivityNew3WeekEnds <- filter(dfActivityNew3, weekdayorweekend == "weekend")

dfIntervalMeanWeekEnd <- dfActivityNew3WeekEnds %>%
  select(interval, steps) %>%
  group_by(interval) %>%
  summarise(steps_interval_mean = mean(steps)) %>%
  na.omit()

dfIntervalMeanWeekDay <- dfActivityNew3WeekDays %>%
  select(interval, steps) %>%
  group_by(interval) %>%
  summarise(steps_interval_mean = mean(steps)) %>%
  na.omit()

png("ggTimeSeries3-4.png")

ggTimeSeries3 <- ggplot(dfIntervalMeanWeekEnd, aes(x=interval, y=steps_interval_mean)) + 
  geom_line() + 
  ggtitle("weekends") + 
  labs(x = "Interval", y = "Number of Steps") + 
  theme(plot.title = element_text(hjust=0.5))
  
ggTimeSeries4 <- ggplot(dfIntervalMeanWeekDay, aes(x=interval, y=steps_interval_mean)) + 
  geom_line() + 
  ggtitle("weekdays") + 
  labs(x = "Interval", y = "Number of Steps") + 
  theme(plot.title = element_text(hjust=0.5))

grid.arrange(ggTimeSeries3, ggTimeSeries4, nrow=2)

dev.off()
  