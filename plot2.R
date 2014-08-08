#
# This R script file implements task 2 for Project 1
#
# The structure of this file:
# - a function to extract needed rows from input file is presented first
# - the code that uses the extracted data frame and plots needed graphs 
#   is presented after the function

# Function reads the input file fileName and returns only data rows
# in CSV format that correspond to measurements from Feb 1 and 2
# of 2007. The function skips all 2006 rows, filters only those
# from 2007 which fall into the needed time interval, and then skips
# the rest. It reads input data line by line.
data2007 <-function(fileName)
{
  # Prepare a data frame
  d <-data.frame("Date" = character(0), 
                 "Time" = character(0), 
                 "Global_active_power"=numeric(0), 
                 "Global_reactive_power"=numeric(0), 
                 "Voltage"=numeric(0), 
                 "Global_intensity"=numeric(0), 
                 "Sub_metering_1"=numeric(0), 
                 "Sub_metering_2"=numeric(0), 
                 "Sub_metering_3"=numeric(0),
                 stringsAsFactors=FALSE)
  
  # Open a connection to an input file
  con <- file(fileName, "r")
  
  # Check connection
  if( isOpen(con ) )
  {
    # Connection has been opened
    
    # Some variables used later:
    # Count of rows to be processed
    N = 1
    # Flag to indicate that the reading of the input 
    # file reached needed data 
    dataBegin = FALSE
    
    # Now read input file line by line and extract only 
    # those for Feb 1-2 of 2007 and add them to d
    while( TRUE )
    {   
      input <- readLines(con, n = 1)
      
      # Check for 2006 year
      g = grep("2006", input, fixed=TRUE)
      if( length(g) > 0 )
      {
        # 2006 year, skip
        next
      }
      
      # Check for 2007 year
      g = grep("2007", input, fixed=TRUE)
      if( length(g) > 0 )
      {
        # We have data for 2007! 
        
        # Now find the begining of data - 2007-02-01 
        if( !dataBegin )
        {
          g <- grep("1/2/2007", input, fixed=TRUE)
          if( length(g) > 0 )
          {
            dataBegin = TRUE
          }else
          {
            # Not there yet, keep reading...
            next
          }
        }else
        {
          # We are in the needed date frame, check for the end
          g <- grep("3/2/2007", input, fixed=TRUE)
          if( length(g) > 0 )
          {
            # We are done!
            # Close input connection
            close(con)
            
            break
          }
        }
        # Split input string 
        c <- unlist(strsplit(input, ";"))
        # Add the next row to data.frame
        d[N,1] <- as.character(c[1])
        d[N,2] <- as.character(c[2])
        for(i in 3:9){ d[N,i]<-as.numeric(c[i])}
        
        # Update counter      
        N = N + 1
        
        # Continue
        next
      }
    }
  }else
  {
    message("Cannot open input file")
  }
  d
}

################ The main program starts here ################

# Set input data file name (it shall be in a current directory)
# ATTENTION: 
#    1) The txt file is a result of the ZIP archive extraction
#    2) The notation is for Linux! Please fix for Windows/Mac
f<-"./household_power_consumption.txt"

# Parse data to extract only needed rows
d<- data2007(f)

# Check if parsing was successful
if( nrow(d) > 2 )
{
  # Define 3 points for X axis to be at the beginning, middle of 2 days (=24hx60min)
  # and the end of 2 days ( in secomds, which is 2x24x60 = 2880 sec )
  xticks <- c(0, 1440, 2880)
  xlabels<-c("Thu", "Fri", "Sat")
  # Plot without axis, but in a box
  with(d, plot(d$Global_active_power,type="l",
       ylab="Global active power (kilowatts)",xlab="", axes=FALSE, frame.plot=TRUE ) )
  # Add default Y axis
  axis(2)
  # Add altered x acis - display days of the week
  axis(1, at=xticks, labels=xlabels)
  
  # Now save the graph in the PNG file ( 480x480 pixels )
  dev.copy(png,"plot2.png", width=480, height=480)
  dev.off()
}