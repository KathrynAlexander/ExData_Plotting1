## Course 4 Week 1 Project 1 Plot 2 Line Chart
## Data at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

setwd("C:\\Users\\Kathy\\Desktop\\RRR\\datasciencecoursera\\Course4Week1") ## Set working directory

if (!file.exists("data")) {
        dir.create("data")
}  ## Create directory of data if it does not exist

## Download file
temp <- tempfile()
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileURL,destfile = "./data/temp")
powerdata <- read.table(unz("./data/temp", "household_power_consumption.txt"),header = TRUE, sep = ";")
unlink(temp)

##List file and retrieve date downloaded
list.files("./data")
dateDownloaded <- date()
dateDownloaded

## exlore data
is.data.frame(powerdata)
ncol(powerdata)
nrow(powerdata)
row.names(powerdata)
str(powerdata)
head(powerdata)
tail(powerdata)

## Put variables in correct formats
powerdata$Date <-as.Date(powerdata$Date, format="%d/%m/%Y")
powerdata$Time <- format(strptime(powerdata$Time, "%H:%M:%S"),"%H:%M:%S")
powerdata$Global_active_power <- as.numeric(as.character(powerdata$Global_active_power))

## Calculate and add Day of Week
powerdata$Day <- weekdays(as.Date(powerdata$Date))

## Create Date-Time variable   "2006-12-16 17:24:00"
powerdata$DateTime <- paste(powerdata$Date,powerdata$Time)
powerdata$DateTime <-strptime(powerdata$DateTime,"%Y-%m-%d %H:%M:%S")
powerdata$DateTime <- as.POSIXct(powerdata$DateTime)

## filter on the dates of interest and combine them in one dataframe
library(dplyr)
powerdata2 <- select(filter(powerdata, Date == "2007-02-01"),c(Date,Time,Global_active_power,DateTime))
powerdata3 <- select(filter(powerdata, Date == "2007-02-02"),c(Date,Time,Global_active_power,DateTime))
powerdata4 <- rbind(powerdata2, powerdata3)
powerdata5 <- na.omit(powerdata4)
head(powerdata5)

## Create plot on screen
x<-powerdata5$DateTime
y<-powerdata5$Global_active_power
xmax <- max(powerdata5$DateTime)
xmin <- min(powerdata5$DateTime)
ymax <- max(powerdata5$Global_active_power)

plot(x,y,
        ylab="Global Active Power (kilowatts)",
        xlab=" ",
        xlim=c(xmin,xmax),
        ylim=c(0,ymax),
        type="l"
)

#### create plot in png file
png(file = "plot2.png", width = 480, height = 480, units = "px")  ## Open device; create file in working directory
plot(x,y,
     ylab="Global Active Power (kilowatts)",
     xlab=" ",
     xlim=c(xmin,xmax),
     ylim=c(0,ymax),
     type="l"
)
dev.off() ## Close the  file device
