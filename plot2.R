plot2 <- function() {
# Function plots the global active power used over the course of two days in 
# February 2007
#Data downloaded manually from :
#https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# on Sep 15th 2015

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

#Create plot showing Global active power use over the two days.
plot(house_power[!is.na(Global_active_power), POSIXct_time],
    house_power[!is.na(Global_active_power), Global_active_power],
    col = "black",
    type = "l",
    xlab = "",
     ylab = "Global active power (kilowatts)")

#Generate png file
dev.copy(png, file = "plot2.png")
dev.off()
}
