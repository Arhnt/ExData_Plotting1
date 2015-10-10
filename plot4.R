library(dplyr);
# Create various charts for household consumputions 
# between 01-02-2007 and 02-02-2007
plot4 <- function() {
    data <- read.table("household_power_consumption.txt", stringsAsFactors = FALSE, sep = ";", header=TRUE) %>%
        filter(grepl("^(1|2)/2/2007$", Date)) %>%
        mutate(Sub_metering_1 = as.numeric(Sub_metering_1),
               Sub_metering_2 = as.numeric(Sub_metering_2),
               Sub_metering_3 = as.numeric(Sub_metering_3),
               Voltage = as.numeric(Voltage),
               Global_active_power = as.numeric(Global_active_power),
               Global_reactive_power = as.numeric(Global_reactive_power),
               Timestamp = as.POSIXct(strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S")));
    
    png(filename = "plot4.png", width = 480, height = 480);
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0,0,0,0));
    
    with(data, plot(Timestamp, Global_active_power, type="n", ylab = "Global Active Power (kilowatts)", xlab = ""))
    with(data, lines(Timestamp, Global_active_power))
    
    with(data, plot(Timestamp, Voltage, type="n", ylab = "Voltage"))
    with(data, lines(Timestamp, Voltage))

    with(data, plot(Timestamp, pmax(Sub_metering_1, Sub_metering_2, Sub_metering_3), type="n", ylab = "Energy sub meeting", xlab = ""));
    with(data, lines(Timestamp, Sub_metering_1, col = "Black"));
    with(data, lines(Timestamp, Sub_metering_2, col = "Red"));
    with(data, lines(Timestamp, Sub_metering_3, col = "Blue"));
    
    legend("topright", 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col=c("black","red","blue"), lwd=1, lty=c(1,1,1), 
           pch=c(NA,NA,NA), merge=FALSE );

    with(data, plot(Timestamp, Global_reactive_power, type="n", ylab = "Global Reactive Power"))
    with(data, lines(Timestamp, Global_reactive_power))
    
    dev.off();
}
