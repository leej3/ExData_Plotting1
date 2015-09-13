plot3 <- function() {
# Function plots the energy used over the course of two days as measured by
# 3 different meters:
  #Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
    

# Data of electricity consumption as reported in the UC Irvine power consumption dataset. 1st and 2nd of February 2007 were assessed
#Data downloaded manually from :
#https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# on at  Wed Sep 9th 17:23:38 2015

# Load data: 
library(data.table)
house_power<- fread(input = "household_power_consumption.txt",
                    na.strings = c("NA","?","","N/A"))

# Convert date values 
house_power[,Date := as.Date(Date,format = "%d/%m/%Y")]
# extract data for relevant dates 
house_power<- house_power[Date == "2007-02-02" | Date == "2007-02-01"]

# Create a column for time in POSIXct format
house_power[, POSIXct_time := 
                as.POSIXct(paste(house_power[,Date],house_power[,Time]),
                format = "%Y-%m-%d %H:%M:%S")
                ]
# Convert other columns to numeric
house_power[ , names(house_power)[3:9] :=
                 lapply(.SD, function(x) as.numeric(x) ) , .SDcols=3:9]

#Create plot showing energy use measured over the two days by 3 sub_meters.

plot(house_power[!is.na(Sub_metering_1), POSIXct_time],
    house_power[!is.na(Sub_metering_1), Sub_metering_1],
    col = "black",
    type = "l",
    xlab = "",
    ylab = "Energy sub metering")

lines(house_power[!is.na(Sub_metering_2), POSIXct_time],
      house_power[!is.na(Sub_metering_2), Sub_metering_2],
      col = "red"    )

lines(house_power[!is.na(Sub_metering_3), POSIXct_time],
      house_power[!is.na(Sub_metering_3), Sub_metering_3],
      col = "blue"    )

legend("topright", 
       col = c("black","red","blue"),
       pch = 20,
       cex = 0.8,
       legend = c("Sub_metering 1", "Sub_metering 2", "Sub_metering 3"))

#Generate png file
dev.copy(png, file = "plot3.png")
dev.off()
}
