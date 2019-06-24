## Course 4 Week 1 Project 1 Plot 3 Line Chart Multi COlor
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

## Put date in date format
powerdata$Date <-as.Date(powerdata$Date, format="%d/%m/%Y")

## Calculate and add Day of Week
powerdata$Day <- weekdays(as.Date(powerdata$Date))

## Put time in time format
powerdata$Time <- format(strptime(powerdata$Time, "%H:%M:%S"),"%H:%M:%S")

## Create Date-Time variable   "2006-12-16 17:24:00"
powerdata$DateTime <- paste(powerdata$Date,powerdata$Time)
powerdata$DateTime <-strptime(powerdata$DateTime,"%Y-%m-%d %H:%M:%S")
powerdata$DateTime <- as.POSIXct(powerdata$DateTime)

## Put Sub_metering in the correct format. 
powerdata$Sub_metering_1 <- as.numeric(as.character(powerdata$Sub_metering_1))
powerdata$Sub_metering_2 <- as.numeric(as.character(powerdata$Sub_metering_2))


## filter on the dates of interest and combine them in one dataframe
library(dplyr)
powerdata2 <- select(filter(powerdata, Date == "2007-02-01"),c(Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,DateTime))
powerdata3 <- select(filter(powerdata, Date == "2007-02-02"),c(Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,DateTime))
powerdata4 <- rbind(powerdata2, powerdata3)
powerdata5 <- na.omit(powerdata4)

## Create plot on screen
x<-powerdata5$DateTime
y1<-powerdata5$Sub_metering_1
y2<-powerdata5$Sub_metering_2
y3<-powerdata5$Sub_metering_3
xmax <- max(powerdata5$DateTime)
xmin <- min(powerdata5$DateTime)

plot(x,y1,
        ylab="Energy sub metering",
        xlab=" ",
        xlim=c(xmin,xmax),
        ylim=range(y1,y2,y3),
        type="l",
        col="black"
)

lines(x,y2, col="red")
lines(x,y3, col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"), lwd=c(1,1,1))


#### create plot in png file
png(file = "plot3.png", width = 480, height = 480, units = "px")  ## Open device; create file in working directory
plot(x,y1,
     ylab="Energy sub metering",
     xlab=" ",
     xlim=c(xmin,xmax),
     ylim=range(y1,y2,y3),
     type="l",
     col="black"
)

lines(x,y2, col="red")
lines(x,y3, col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col=c("black","red","blue"), lwd=c(1,1,1))
dev.off() ## Close the  file device
