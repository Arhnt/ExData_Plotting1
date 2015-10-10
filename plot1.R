library(dplyr);
# Create a histogram for Global Active Power for household consumputions 
# between 01-02-2007 and 02-02-2007
plot1 <- function() {
    data <- read.table("household_power_consumption.txt", stringsAsFactors = FALSE, sep = ";", header=TRUE) %>%
        filter(grepl("^(1|2)/2/2007$", Date)) %>%
        mutate(Global_active_power = as.numeric(Global_active_power));
    png(filename = "plot1.png", width = 500, height = 500);
    hist(data$Global_active_power,
         col = "Red",
         main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)");
    dev.off();
}
