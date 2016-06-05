###################################################################
# Project for Exploratory Data Analysis Course
# Assignment: To do 4 Plots to explore data
# File: Plot4.R
###################################################################

###################################################################
#
# DataSet: Electric power consumption
# Description: Measurements of electric power consumption in one 
#              household with a one-minute sampling rate over a 
#              period of almost 4 years. Different electrical 
#              quantities and some sub-metering values are available
#
####
#
# Dataset has 2,075,259 rows and 9 columns
# Variables:
# 1. Date: Date in format dd/mm/yyyy
# 2. Time: time informat hh:mm:ss
# 3. Global_active_power: household global minute-avergaed active power (in KW)
# 4. Global_reactive_pwer: household global minute-avergaed reactive power (in KW)
# 5. Voltage: minute-averaged voltage (in volt)
# 6. Global_intensity:  household global minute averaged current intensity (in ampere)
# 7. Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It 
#         corresponds to the kitchen, containing mainlyy a dishwasher, an oven and a 
#         microwave (hot plates are not electric but gas powered)
# 8.Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It 
#         corresponds to the laundry room, containing a washing-machine, a tumble 
#         drier, a refrigerator and a light
# 9.Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy).It
#         corresponds to an electric water-heater and an air-conditioner
#
###################################################################

library(dplyr)

###################################################################
#
# Function to draw plot of Global_active_power across the days of interest
#
###################################################################
draw_plot1 <- function(neededFebData)
{  
  # Get the y axis range
  Global_active_power_range <- range(0, neededFebData$Global_active_power)
  
  # Plot the line without the axes and annotations
  plot(neededFebData$Global_active_power,type="l",ylim=Global_active_power_range,
       axes=FALSE, ann=FALSE)
  
  # Make x axis using Thu-Sat labels
  # Divide the 2880 observations into c(1,1440,2880) to label the x-axis
  axis(1, at=c(1,1440,2880), labels=c("Thu","Fri","Sat"))
  
  # Make y axis for the Global_active_power_range at intervals of 2
  axis(2, at=2*0:Global_active_power_range[2])
  
  # Draw a box around the plot
  box()
  
  # Set the title for y-axis
  title(ylab="Global Active Power", col.lab=rgb(0,0,0))
}

###################################################################
#
# Function to draw plot of Voltage across the days of interest
#
###################################################################

draw_plot2 <- function(neededFebData)
{  
  # Get the y axis range
  Voltage_range <- range(ceiling(min(neededFebData$Voltage)), 
                         ceiling(max(neededFebData$Voltage)))
  
  # Plot the line without the axes and annotations
  plot(neededFebData$Voltage,type="l",ylim=Voltage_range,
       axes=FALSE, ann=FALSE)
  
  # Make x axis using Thu-Sat labels
  # Divide the 2880 observations into c(1,1440,2880) to label the x-axis
  axis(1, at=c(1,1440,2880), labels=c("Thu","Fri","Sat"))
  
  # Make y axis for the Global_active_power_range at intervals of 2
  axis(2, at=seq(Voltage_range[1], Voltage_range[2], by=2))
  
  # Draw a box around the plot
  box()
  
  # Set the title for y-axis
  title(ylab="Voltage", col.lab=rgb(0,0,0))
  # Set the title for x-axis
  title(xlab="datetime", col.lab=rgb(0,0,0))
}

###################################################################
#
# Function to draw plot(s) of Energy sub metering across the days of interest
#
###################################################################

draw_plot3 <- function(neededFebData) {
  
  # Get the y axis range
  Sub_metering_range <- range(0, neededFebData$Sub_metering_1,
                              neededFebData$Sub_metering_2,
                              neededFebData$Sub_metering_3)
  
  # Plot the line without the axes and annotations
  plot(neededFebData$Sub_metering_1,type="l",ylim=Sub_metering_range,
       axes=FALSE, ann=FALSE)
  
  # Make x axis using Thu-Sat labels
  # Divide the 2880 observations into c(1,1440,2880) to label the x-axis
  axis(1, at=c(1,1440,2880), labels=c("Thu","Fri","Sat"))
  
  # Make y axis for the Sub_metering_range at intervals of 10
  axis(2, at=10*0:Sub_metering_range[2])
  
  # Draw a box around the plot
  box()
  
  # Set the title for y-axis
  title(ylab="Energy sub metering", col.lab=rgb(0,0,0))
  
  lines(neededFebData$Sub_metering_2, type="l", col="red")
  
  lines(neededFebData$Sub_metering_3, type="l", col="blue")
  
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         cex=0.8, col=c("black", "red", "blue"), bty="n", lty=c(1,1,1), 
         x.intersp=0.5, y.intersp=0.7)
}

###################################################################
#
# Function to draw plot of Global_reactive_power across the days of interest
#
###################################################################

draw_plot4 <- function(neededFebData)
{  
  # Get the y axis range
  Global_reactive_power_range <- seq(0, max(neededFebData$Global_reactive_power), by=0.1)
  
  # Plot the line without the axes and annotations
  plot(neededFebData$Global_reactive_power,type="l", #ylim=Global_reactive_power_range,
       axes=FALSE, ann=FALSE)
  
  # Make x axis using Thu-Sat labels
  # Divide the 2880 observations into c(1,1440,2880) to label the x-axis
  axis(1, at=c(1,1440,2880), labels=c("Thu","Fri","Sat"))
  
  # Make y axis for the Global_active_power_range at intervals of 2
  axis(2, at=Global_reactive_power_range)
  
  # Draw a box around the plot
  box()
  
  # Set the title for y-axis
  title(ylab="Global_reactive_power", col.lab=rgb(0,0,0))
  # Set the title for x-axis
  title(xlab="datetime", col.lab=rgb(0,0,0))
}

###################################################################
#
# Find file size:
#
###################################################################
file_name <- "household_power_consumption.txt"
cat("The Original File size of ", file_name, " is ", 
    round(file.info(file_name)$size/1024^2), " MB\n", sep="")

### skipValue is to skip the rows that are already read
### RowBlock is the number of rows being read at one time
skipValue <- 0
RowBlock <- 100000

### Date1 and Date2 are the dates of interest
Date1 <- as.Date('1/2/2007',"%d/%m/%Y")
Date2 <- as.Date('2/2/2007',"%d/%m/%Y")

### Set the mgp such that the title comes closer to the axis
par("mgp", c(2, 1, 0))
### classesVec to facilitate reading in and converting to needed data types
classesVec <- c("character","character","character","character","character","character",
                "character","character","character")

### ******* ASSUMPTION ************
### An assumption made is that the observations are in increasing chronological order
### So, if the first row read in is greater that 2/2/2007, we can safely exit

### Read the first chunk from the file
smallChunk <- read.csv2(file_name, nrows=RowBlock, skip=skipValue, colClasses = classesVec)

### Convert the date into date format
smallChunk <- mutate(smallChunk,Date=as.Date(Date,"%d/%m/%Y"))

### Keep the names so that we can use in the subsequent reads
colNames <- names(smallChunk)

### Remove unknown data

### Get the observations for the desired dates - 1/1/2007 and 1/2/2007 (d/m/Y)
neededFebData <- subset(smallChunk, Date == Date1 | Date == Date2)

### Now loop until the first row has observation date beyond 1/2/2007
### This is possible due to the assumption made and inspection of data

while(nrow(smallChunk) > 0) 
{
  ### Increment skipValue as a read has occurred and we need to skip
  skipValue <- skipValue + RowBlock
  smallChunk <- read.csv2(file_name, nrows = RowBlock, header=FALSE, 
                          col.names=colNames, skip=skipValue, colClasses=classesVec) 
  smallChunk <- mutate(smallChunk,Date=as.Date(Date,"%d/%m/%Y"))
  
  ### This makes the read operation efficient (Relying on ASSUMPTION)
  if(smallChunk$Date[1] > Date2)
    break
  
  ### See if we have any data for the dates of interest
  febData <- subset(smallChunk, Date == Date1 | Date == Date2)
  
  ### If we have data in this read, add it to the target dataset - neededFebData
  if(nrow(febData) > 0) 
  {
    tmpFebData <- neededFebData
    neededFebData <- rbind(tmpFebData, febData)
  }
}

### Change the format of the columns to facilitate exploratory data analysis
neededFebData <- mutate(neededFebData,Global_active_power=as.numeric(Global_active_power),
                        Global_reactive_power=as.numeric(Global_reactive_power),
                        Voltage=as.numeric(Voltage),Day=weekdays(Date,abbreviate=TRUE),
                        Sub_metering_1=as.numeric(Sub_metering_1),
                        Sub_metering_2=as.numeric(Sub_metering_2),
                        Sub_metering_3=as.numeric(Sub_metering_3),
                        TimeDiff=difftime(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),
                                          Date1,tz="EST",units="days"))
                          
cat("The size of the in-memory data object is ", object.size(neededFebData), " bytes\n", sep="")

png(file="Plot4.png")

# Set multiple plots on one page
par(mfrow=c(2,2))

# Call function draw_plot1 to draw the first plot
# Global_active_power across the days

draw_plot1(neededFebData)

draw_plot2(neededFebData)

draw_plot3(neededFebData)

draw_plot4(neededFebData)

# Close the file device
dev.off()


