
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
# plot_one will plot a histogram of the Global Active Power and 
# save the graph as plot1.png
#       There is an optional data frame argument
#
plot_one <- function(tbf = NA) {
        
        if(is.na(tbf)) {
                tbf <- read_data()
        }
        
        interesting <- interestingTimes(tbf)

        par(mfrow = c(1,1))
        hist(interesting$Global_active_power, main = "Global Active Power", 
                 xlab = "Global Active Power (kilowatts)", col = "red")
        
        dev.copy(png, file="plot1.png", width=504, height=504)
        dev.off()

}
plot_one()

