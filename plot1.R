plot1<-function(){
      #Generate a line plot of the *TOTAL* PM25 emissions for the entire country for
      #1999,2002,2005, and 2008.
      #Simple line plots are useful to see trends (as stated by Joel Grus in "Data Science from Scratch").
      #Data is the EPA National Emissions Inventory
      #and can be obtained from their website for individual years:
      #www.epa.gov/ttn/chief/elinformation.html
      #Data was obtained via Coursera assignment page as one zipped file.

      #!!!!NOTE!!!! Plot MUST BE GENERATED using R base plotting.


      #Read in the data
      NEI<-readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/summarySCC_PM25.rds")

      #Subset the data by year, to calculate the total emissions for each year
      emissions1999 <- subset(NEI,NEI$year == "1999")
      emissions2002 <- subset(NEI,NEI$year == "2002")
      emissions2005 <- subset(NEI,NEI$year == "2005")
      emissions2008 <- subset(NEI,NEI$year == "2008")

      #Take a look at the data, by year...
      print("Summary 1999 National Emissions for all sources")
      print(summary(emissions1999$Emissions))
      print("Summary 2002 National Emissions for all sources")
      print(summary(emissions2002$Emissions))
      print("Summary 2005 National Emissions for all sources")
      print(summary(emissions2005$Emissions))
      print("Summary 2008 National Emissions for all sources")
      print(summary(emissions2008$Emissions))

      #Calculate the total emissions from all sources
      totalEmissions1999 <- sum(emissions1999$Emissions)
      totalEmissions2002 <- sum(emissions2002$Emissions)
      totalEmissions2005 <- sum(emissions2005$Emissions)
      totalEmissions2008 <- sum(emissions2008$Emissions)

      #Create vectors of the result
      years<-c(1999,2002,2005,2008)
      totals<-c(totalEmissions1999,totalEmissions2002,totalEmissions2005,totalEmissions2008)

      #Generate a simple line plot to see if there is a clear trend.
      #However, there are only four points so this may not provide a definitive
      #answer, so add a best fit line (linear regression line) to determine
      #if there is a trend.  A positive slope for the best fit line indicates
      #that emissions are increasing from 1998 to 2008, whereas a negative
      #slope indicates decreasing emissions.
      png(filename="plot1.png",width=480,height=480)
      plot(years,totals,pch=20,col="blue",main="Total PM25 Emissions for United States from all Sources",xlab="Year", ylab="Total PM 25 Emissions (tons)")
      abline(lm(totals~years),col="red")

      #Label the linear regression line, define the line type, use solid line (linetype=1),
      #and add the legend.
      legend(x="topright", legend="Linear Regression", lty=1,col="red",title="Best-fit line")
      dev.off()
}
