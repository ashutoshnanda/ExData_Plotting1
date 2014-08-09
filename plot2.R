plot2 <- function() {
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
    png(filename = "plot2.png")
    plot(powerusage[[time.field]], powerusage[[globalactivepower.field]], 
         type = "l", main = "Global Active Power", xlab = "", 
         ylab = "Global Active Power (kilowatts)")
    dev.off()
}