# sp500.features.R
# 2014-6-15
# R.Sloan
#
# This R code is deisgned to run in R Studio, development environment for R includes plot screen
#  R Studio is required for interactive graph using library manipulate
#
# Translating to R the ideas of Dan Bikle
# Based on his class Mr Stock Market meet Mr Data Scientist
#  http://www.spy611.com/blog/preso
# Starting with Standards and Poors 500 (S&P500) data downloaded from Yahoo Finance using wget 
#  as described in slide 1 http://www.spy611.com/blog/demo_wget
#  Using the file GSPC.csv this comma separated value file contains S&P500 stock data back 50 years

# This code is to create a feature set as defined in slide 7 http://www.spy611.com/blog/demo_features

# The code is written by James (Xinghai) Hu. It uses the features proposed by Robert Sloan.
# 5/31/2014 
# Robert Sloan: slight improvements and save feature file as featuresSP500.Rdata
# Sanjay Patel: added Feature sales 'Volume' for SP500
# Zvi Eintracht: install only new package
# 6/15/2014
# Peter Li: Simplified concept for calculating the maximum number of Rows (Stock days)
#        for output feature data frame possible given the raw data set and the features desires
# Robert Sloan: moved lag feature creation to function
# 6/18/2014 R. Sloan created functions for lead, lag and moving average.  
#           Modify lag.days, lead.days and moving.avg.days to select which features desired
#           To simplify the use of this data by prediction algorithms use only
#           only one lead.day prediction Target

rm(list = ls()) # remove any existing list objects

# set the working directory to where you place the GSPC.csv data file
setwd("/Users/Robert/Dropbox/Class/Mr Stock Market meet Mr Data Scientist/data")
#read in some price data
sp500 <- read.table(file = "GSPC.csv", header=TRUE, sep=",")
head(sp500)   # show the first few dates from the data showing the column lables

# since the most recent date is first plots of the data would appear backwards
#  showing the high stock market getting lower over time
# i+1 is in the past
# to reverse this so it looks more usual
index <- nrow(sp500):1

# here's price data in left to right chronological starting low and growing
plot(sp500$Close[index], pch=".")  

# Create an interactive plot with a start and size for the data
# install packages as needed. Packages only need to be installed once
#packages <- c("manipulate")
#install.packages(setdiff(packages, rownames(installed.packages())))
#library(manipulate)   
# manipulate(
#  plot(sp500$Close[index][x.min: (x.min+x.size)], type = "l", pch="."),  # type l is line
#  x.min = slider(0,nrow(sp500), initial = 10000),
#  x.size = slider(1,nrow(sp500), initial = 1000)
#)

# use attach to not have to write the data frame name over and over
attach(sp500) # now sp500 data.frame is assumed
#plot(Date,Close, pch=".")   # Can plot vs Date but this takes a while. 
                              #  This plots Close (y-axis) vs Date (x-axis)
# Using the past to predict the future.  For each date in the past we create a new data.frame
# of features which includes differences in prices for dates in the past
#  and changes for dates in the future
# We create lags (differences) from today to 1 day ago, 2 days ago, 1 week ago, 2 weeks ago, 1 month ago and 2 months ago
# A feature of the 100 day moving average (ma100) and Volume of stock traded
# And features of how the market did (lead) 1 day in the future, 2 days and 1 week

# We are looking at creating this from the S&P500 data.
# There is probably a windowing approach which would be better
#   |<-     100     ->|                                             |<-   6  ->|
#   ----------------------------------------------------------------------------
#   |xxxxxxxxxxxxxxxxx|                                             |xxxxxxxxxx|
#   ----------------------------------------------------------------------------
#                     ^                                             ^
#                     |___ oldest date in file                      |___ most recent date
# Not all of the data can be used as our difference calculations would go off the ends of the data
# i.e. for example we cannot look past the last date in our data as it requires dates before the last 
#  value we have
# we need to skip the first 6 days for lead features and the last 100 days for lag features

## Parameters ##

# NOTE: The following are stock market days. Stock market is open only 5 days/week
#  excluding holidays.  So 5 stock market days = 1 calendar week.  
#  20 stock market days = ~ 1 month (assuming a 4 week month)

# select a cut-off date.  Create features only for data after this cut off date
cutOffDate<-"2009-02-01"
sp500$Date<-as.Date(sp500$Date)  # change the Date column to be of class Date
sp500<-sp500[(sp500$Date>cutOffDate),]

# The following vectors determine which lag, lead, moving average features will be created
lag.days <- c(1, 2, 5, 10, 20, 40)  # 1 day, 2 days, 1 week, 2 weeks, 1 month, 2 months
lead.days <- c(1) # 1 day, 2 days, 5 days = 1 week
moving.avg.days <- c(10, 30, 50, 100)  # NOTE this is stock market days 
                        # for which  100 is actually multiplied by 7/5 to yield 140 calendar days
# Calculate the maximum range of the stock history we can use
sp500$idx <- 1:nrow(sp500)  # add an index row
first.obs <- sp500[1, "idx"] + max(lead.days)
last.obs <- sp500[nrow(sp500), "idx"] - max(c(lag.days, moving.avg.days))
calcRows <- last.obs - first.obs+1

# Copy some of the raw data directly
columnsToCopy =  c("Date", "Close","Volume")
# Subset for "complete" features lags, leads and moving avg.
sp <- sp500[sp500$idx >= first.obs & sp500$idx <= last.obs, columnsToCopy]

# this function creates a new vector of lag differences of stock market observations
#  a log of 1 is a vector of differences from current index to one day ago
#  a lag of 5 is a vector of differences from current indes to one calendar week ago
lagVector <- function(rawCloseData, lag, startingRow, numberOfRows){
    rawCloseData[startingRow:(numberOfRows+startingRow-1)] - rawCloseData[(startingRow+lag):(numberOfRows+startingRow+lag-1)]   
}

# Add the columns for the lag features
for (i in lag.days){
    columnName <-paste("lag_",i,"d",sep="")
    newColumn <- lagVector(sp500$Close, i, first.obs, calcRows)
    sp[, columnName] <- newColumn
}


# This function creates the Moving Average data to be generated
MovingAverageVector <- function(rawCloseData, movingAverage, startingRow){
#    rawCloseData[(startingRow-lead):(numberOfRows+startingRow-lead-1)] - rawCloseData[startingRow:(numberOfRows+startingRow-1)]    
    tempavg <- rep(0, (calcRows))  # a temporary vector used for computing moving average (average of 100 prices)
    tempavg[1] <- sum(sp500$Close[first.obs:(first.obs+movingAverage-1)])
    for (i in 2: length(tempavg) )
    {
        tempavg[i] <- tempavg[i-1] - sp500$Close[i+first.obs-2] + sp500$Close[i+first.obs+movingAverage-2]
    }
    tempavg/movingAverage
}

# Add the columns for the Moving Average features
for (i in moving.avg.days){
    columnName <-paste("ma",i,sep="")
    newColumn <- MovingAverageVector(sp500$Close, i, first.obs)
    sp[, columnName] <- newColumn
}

# This function creates the prediction target lead data
leadVector <- function(rawCloseData, lead, startingRow, numberOfRows){
    rawCloseData[(startingRow-lead):(numberOfRows+startingRow-lead-1)] - rawCloseData[startingRow:(numberOfRows+startingRow-1)]    
}

# Add the columns for the lead features
for (i in lead.days){
    columnName <-paste("lead_",i,"d",sep="")
    newColumn <- leadVector(sp500$Close, i, first.obs, calcRows)
    sp[, columnName] <- newColumn
}


write.table(sp,  file="featuresSP500.csv", sep=",",row.names=FALSE)

