

#
# The read_data function downloads the household_power_consumption dataset 
# if the file does not exist. In addition it will return a data frame with
# the appropriate column classes.
#
read_data <- function() {
        if(!file.exists("household_power_consumption.txt")) {
                url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                
                download.file( url,"power.zip")
                unzip("power.zip")
        }
        colClasses <- c("character", "character", rep("numeric",7))
        
        dtbl <- read.table("household_power_consumption.txt", sep=";", 
                           nrows = 2075260, colClasses = colClasses, 
                           header = TRUE, na.strings = "?")
        dtbl$Date <- as.Date(dtbl$Date, "%d/%m/%Y")
        return(dtbl)
        
}

#
# The interestingTimes function uses a data frame argument and returns a 
# subset of the data from 02-01-2007 to 02-02-2007
#
interestingTimes <- function(df) {
        rtn <- df[df$Date == "2007-02-01"|df$Date == "2007-02-02",]
        
        rtn$Time <- strptime(paste(rtn$Date, 
                                   rtn$Time), "%Y-%m-%d %H:%M:%S")
        return(rtn)
}


#
# plot_four will plot a 4 line graphs of:
#               Energy Sub Meters (1,2,3) vs Time
#               Global Active Power vs Time
#               Voltage vs Time
#               Global Reactive Power vs time
# and save the graph as plot4.png
#       There is an optional data frame argument
#
plot_four <- function(tbf=NA) {
        if(is.na(tbf)) {
                tbf <- read_data()
        }
        
        interesting <- interestingTimes(tbf)
        
        par(mfrow = c(2,2))
        plot(interesting$Time, interesting$Global_active_power, type = "l", 
                xlab = " ", ylab = "Global Active Power") 

        plot(interesting$Time, interesting$Voltage, type = "l", 
                xlab = " " , ylab = "Voltage") 

        plot(interesting$Time, interesting$Sub_metering_1, type = "l", 
                ylab = "Energy sub metering" , xlab = " ") 
        lines(interesting$Time, interesting$Sub_metering_2, col = "red") 
        lines(interesting$Time, interesting$Sub_metering_3, col = "blue")

        plot(interesting$Time, interesting$Global_reactive_power, type = "l", 
                xlab = "datetime", ylab = "Global_reactive_power")
        
        dev.copy(png, file="plot4.png", width=504, height=504)
        dev.off()

}

plot_four()
