
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
# plot_three will plot a line graph of the Energy Sub Meters (1,2,3) vs Time 
# and save the graph as plot3.png
#       There is an optional data frame argument
#
plot_three <- function(tbf = NA) {
        
        if(is.na(tbf)) {
                tbf <- read_data()
        }
        
        
        interesting <- interestingTimes(tbf)
        
        par(mfrow = c(1,1))
        plot(interesting$Time, interesting$Sub_metering_1, type = "l", 
             ylab = "Energy sub metering" , xlab = " ")
        lines(interesting$Time, interesting$Sub_metering_2, col = "red")
        lines(interesting$Time, interesting$Sub_metering_3, col = "blue")
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),  lty = 1,
               col=c("black", "red", "blue"), cex=0.8, inset = c(0.1,0))
        
        dev.copy(png, file="plot3.png", width=504, height=504)
        dev.off()
}

plot_three()

