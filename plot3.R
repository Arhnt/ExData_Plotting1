library(dplyr);
# Create a chart to compare Sub Meterings for household consumputions 
# between 01-02-2007 and 02-02-2007
plot3 <- function() {
    data <- read.table("household_power_consumption.txt", stringsAsFactors = FALSE, sep = ";", header=TRUE) %>%
        filter(grepl("^(1|2)/2/2007$", Date)) %>%
        mutate(Sub_metering_1 = as.numeric(Sub_metering_1),
               Sub_metering_2 = as.numeric(Sub_metering_2),
               Sub_metering_3 = as.numeric(Sub_metering_3),
               Timestamp = as.POSIXct(strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S")));
    
    png(filename = "plot3.png", width = 480, height = 480);
   
    with(data, plot(Timestamp,
                    pmax(Sub_metering_1, Sub_metering_2, Sub_metering_3),
                    type="n",
                    ylab = "Energy sub meeting",
                    xlab = ""));
    with(data, lines(Timestamp, Sub_metering_1, col = "Black"));
    with(data, lines(Timestamp, Sub_metering_2, col = "Red"));
    with(data, lines(Timestamp, Sub_metering_3, col = "Blue"));
    
    legend("topright", 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col=c("black","red","blue"), lwd=1, lty=c(1,1,1), 
           pch=c(NA,NA,NA), merge=FALSE );
    
    dev.off();
}
