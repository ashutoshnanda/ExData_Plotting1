plot4 <- function() {
    # Reading in the data
    url <- paste("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2F", 
             "household_power_consumption.zip", sep = "") 
    zip.file.name <- "electric-power-consumption.zip" 
    data.file.name <- "household_power_consumption.txt"
    if(!file.exists(data.file.name)) {
        if(!file.exists(zip.file.name)) {
            download.file(url, zip.file.name)
        }   
        unzip(zip.file.name)
    }
    powerusage <- read.csv(data.file.name, sep = ";", na.strings = "?")
    date.field <- "Date"
    time.field <- "Time"
    valid.dates <- c("2007-02-02", "2007-02-01")
    powerusage[[date.field]] <- strptime(powerusage[[date.field]], "%d/%m/%Y")
    powerusage <- 
        powerusage[is.element(powerusage[[date.field]], valid.dates), ]
    powerusage[[time.field]] <- strptime(
        paste(as.character(powerusage[[date.field]]), 
              powerusage[[time.field]]),
        "%Y-%m-%d %H:%M:%S"
        )
    
    #Plotting code
    globalactivepower.field <- "Global_active_power"
    sub.first.field <- "Sub_metering_1"
    sub.second.field <- "Sub_metering_2"
    sub.third.field <- "Sub_metering_3"
    voltage.field <- "Voltage"
    globalreactivepower.field <- "Global_reactive_power"
    png(filename = "plot4.png")
    par(mfcol = c(2, 2))
    plot(powerusage[[time.field]], powerusage[[globalactivepower.field]], 
         type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
    
    with(powerusage, plot(get(time.field), get(sub.first.field),
              ylim = range(cbind(get(sub.first.field), 
                                 get(sub.second.field), 
                                 get(sub.third.field))),
         type = "n", xlab = "", ylab = "Enegy sub metering"))
    lines(powerusage[[time.field]], powerusage[[sub.first.field]], 
          col = "black")
    lines(powerusage[[time.field]], powerusage[[sub.second.field]], 
          col = "red")
    lines(powerusage[[time.field]], powerusage[[sub.third.field]], 
          col = "blue")    
    legend("topright", c(sub.first.field, sub.second.field, sub.third.field), 
           lty = c(1,1,1), col = c("black", "red", "blue"))
    
    plot(powerusage[[time.field]], powerusage[[voltage.field]], 
         type = "l", xlab = "datetime", ylab = "Voltage")
    
    with(powerusage, plot(get(time.field), get(globalreactivepower.field), 
                          type = "l", xlab = "datetime", 
                          ylab = globalreactivepower.field))
    dev.off()
}