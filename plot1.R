plot1 <- function() {
# Function plots a histogram of the global active power

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

#Create histogram for Global active power over the two days.
par(cex = 0.8)
hist(house_power[!is.na(Global_active_power), Global_active_power],
     col = "red",
     xlab = "Global active power (kilowatts)",
     main = "Global active power")

#Generate png file
dev.copy(png, file = "plot1.png")
dev.off()
}
