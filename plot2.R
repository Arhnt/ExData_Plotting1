library(dplyr);
# Create a chart for Global Active Power for household consumputions 
# between 01-02-2007 and 02-02-2007
plot2 <- function() {
    data <- read.table("household_power_consumption.txt", stringsAsFactors = FALSE, sep = ";", header=TRUE) %>%
        filter(grepl("^(1|2)/2/2007$", Date)) %>%
        mutate(Global_active_power = as.numeric(Global_active_power),
               Timestamp = as.POSIXct(strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S")));

    png(filename = "plot2.png", width = 500, height = 500);
    with(data, plot(Timestamp, Global_active_power, type="n", ylab = "Global Active Power (kilowatts)"))
    with(data, lines(Timestamp, Global_active_power))
    
    dev.off();
}
