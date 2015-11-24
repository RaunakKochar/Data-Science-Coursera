plot4 <- function() {
    
    data_full <- read.csv("power/household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                          nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
    data_full$Date <- as.Date(data_full$Date, format="%d/%m/%Y")
    
    ## Subsetting the data
    data <- subset(data_full, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
    rm(data_full)
    
    ## Converting dates
    datetime <- paste(as.Date(data$Date), data$Time)
    data$Datetime <- as.POSIXct(datetime)
    
    #Plotting
    par(mfrow = c(2,2))
    
    plot(data$Datetime, data$Global_active_power, ylab = "Global_active_power", xlab = "", type = 'l')
    
    
    plot(data$Datetime, data$Voltage, ylab = "Voltage", xlab = "datetime", type = 'l')
    

    with(data, {
        plot(Sub_metering_1~Datetime, type="l",
             ylab="Energy sub metering", xlab="")
        lines(Sub_metering_2~Datetime,col='Red')
        lines(Sub_metering_3~Datetime,col='Blue')
    })
    legend("topright", col = c("black", "red", "blue"), lwd = 2, cex = 0.5, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))    
    
    plot(data$Datetime, data$Global_reactive_power, ylab = "Global_active_power", xlab = "", type = 'l')
    dev.copy(png, file="plot4.png", height=480, width=480)
    dev.off()
    
}