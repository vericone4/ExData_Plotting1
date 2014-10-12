plot4 <- function(){
        #This function plots the 4 graphs with the characterstics described
        # in the below code
        #
        #Functional description:
        #   1. Extracts the data from the file downloaded from URL
        #      (https://d396qusza40orc.cloudfront.net/
        #      exdata%2Fdata%2Fhousehold_power_consumption.zip))
        #   2. Reads the raw data from the file into a data table
        #   3. Subsets the raw data table based on date range
        #   4. Converts the date time char into date time object
        #   5  Plots the 4 graphs  
        #
        #Args:
        #  none
        #
        #Returns:
        #   none
        #
        
        # STEP 1 - DATA PROCESIING 
        # set working Directory
        setwd("~/Data/Venkata/Personal/Coursera/exdata/Working_Directory")
        
        # intialize the file name
        file.name <- "Data//exdata-data-household_power_consumption//household_power_consumption.txt"
        
        data.raw <- read.table(file.name, 
                               header     = TRUE,
                               sep        = ";" ,                              
                               colClasses = c("character","character",
                                              rep("numeric", 6)),
                               na.strings = "?" ,
                               row.names  = NULL)
        
        # subset the raw data table using the data range
        data.raw.subset <- subset(data.raw, (data.raw$Date =="1/2/2007") |
                                          ( data.raw$Date == "2/2/2007"))        
        # removing the raw dataset from memory 
        rm(data.raw)
        
        # merge the date and time column into one
        data.raw.subset$DateTime <- paste(data.raw.subset$Date, 
                                          data.raw.subset$Time)
        # convert the char date time col to date time object
        data.raw.subset$DateTime <- strptime(data.raw.subset$DateTime, 
                                             "%d/%m/%Y %H:%M:%S")
        
        # STEP 2 - PLOTTING 
        # Store in plot4.png
        png("Output//Graphs//plot4.png", width = 480, height = 480)
        
        # global settings -- 1 row and 1 column for output
        par(mfrow = c(2,2))
        # global settings - margin set by the following space
        par(mar = c(4,4,2,2))
        
        
        # plotting the line graph -1
        with(data.raw.subset, 
             plot(x        = DateTime,  # DateTime as X -axis
                  y        = Global_active_power,  # Global Power as Y -axis
                  type     = "l",  # line graph
                  xlab     =  "",
                  ylab     = "Global Active Power (kilowatts)"))
        
        # plotting the line graph -2
        with(data.raw.subset, 
             plot(x        = DateTime,  # DateTime as X -axis
                  y        = Voltage,  # Global Power as Y -axis
                  type     = "l",  # line graph
                  xlab     =  "datetime",
                  ylab     = "Voltage (kilowatts)"))
        
        # plotting the line graph -3
        with(data.raw.subset, 
             plot(x        = DateTime,  # DateTime as X -axis
                  y        = Sub_metering_1,  # Global active Power as Y -axis
                  type     = "n",  # no plots
                  xlab     =  "",
                  ylab     = "Energy sub metering"))
        
        # add line 1 -- sub metering 1
        with(data.raw.subset,
             lines(DateTime, Sub_metering_1, type = "l" , col ="grey30"))
        # add line 2 -- sub metering 2
        with(data.raw.subset,
             lines(DateTime, Sub_metering_2, type = "l" , col ="red"))
        # add line 3 -- sub metering 3
        with(data.raw.subset,
             lines(DateTime, Sub_metering_3, type = "l" , col ="blue"))
        
        # adding legend 
        legend("topright", 
               legend    = c("Sub_metering_1",
                             "Sub_metering_2", 
                             "Sub_metering_3"),
               pch       = c(rep(NA,3)), 
               col       = c("black", "red", "blue"), 
               lty       = c(1, 1, 1),
               lwd       = 1,
               ncol      = 1, 
               cex       =  1, 
               text.font = 1)
        
        
        # plotting the line graph -4
        with(data.raw.subset, 
             plot(x        = DateTime,  # DateTime as X -axis
                  y        = Global_reactive_power,  # Global reactive Power as Y -axis
                  type     = "l",  # line graph
                  xlab     =  "datetime",
                  ylab     = "Voltage (kilowatts)"))
        # close the graphics device
        dev.off()
}