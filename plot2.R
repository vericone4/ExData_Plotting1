plot2 <- function(){
        #This function plots the line graph between days of consumption and 
        # Global active  power for the given data set
        #
        #Functional description:
        #   1. Extracts the data from the file downloaded from URL
        #      (https://d396qusza40orc.cloudfront.net/
        #      exdata%2Fdata%2Fhousehold_power_consumption.zip)
        #   2. Reads the raw data from the file into a data table
        #   3. Subsets the raw data table based on date range
        #   4. Converts the date time char into date time object
        #   5  Plots the line graph for active power and days
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
        # Store in plot2.png
        png("Output//Graphs//plot2.png", width = 480, height = 480)
        
        # global settings -- 1 row and 1 column for output
        par(mfrow = c(1,1))
        # global settings - margin set by the following space
        par(mar = c(4,4,2,2))
        
        # plotting the line graph
        with(data.raw.subset, 
             plot(x        = DateTime,  # DateTime as X -axis
                  y        = Global_active_power,  # Global Power as Y -axis
                  type     = "l",  # line graph
                  xlab     =  "",
                  ylab     = "Global Active Power (kilowatts)"))
        
        
        # close the graphics device
        dev.off()
}