## Course 4 Week 1 Project 1 Plot 1 Histogram
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
powerdata$Date <- as.Date(powerdata$Date, format="%d/%m/%Y")

## Put time in time format
powerdata$Time <- format(strptime(powerdata$Time, "%H:%M:%S"),"%H:%M:%S")

## Put Global Active Power in correct format
powerdata$Global_active_power <- as.numeric(as.character(powerdata$Global_active_power))

## filter on the dates of interest and combine them in one dataframe
library(dplyr)
powerdata2 <- select(filter(powerdata, ReadingDate == "2007-02-01"),c(Date,Time,Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3))
powerdata3 <- select(filter(powerdata, ReadingDate == "2007-02-02"),c(Date,Time,Global_active_power,Global_reactive_power,Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3))
powerdata4 <- rbind(powerdata2,powerdata3)

## Create histogram and plot on screen
hist(powerdata4$Global_active_power,  main="Global Active Power",xlab="Global Active Power (kilowatts)", col = "red")

## create histogram and plot to png file
png(file = "plot1.png", width = 480, height = 480, units = "px")  ## Open device; create file in working directory
hist(powerdata4$Global_active_power,  main="Global Active Power",xlab="Global Active Power (kilowatts)", col = "red") ## Create plot
dev.off() ## Close the  file device












