plot1<-function(){
      #Generate a plot of the *TOTAL* PM25 emissions for the entire country for
      #1999,2002,2005, and 2008.  Data is the EPA National Emissions Inventory
      #and can be obtained from their website for individual years:
      #www.epa.gov/ttn/chief/elinformation.html
      #Data was obtained via Coursera assignment page as one zipped file.

      #Read in the data
      NEI <- readRDS("/Users/Coursera/exdata_data_NEI_data/summarySCC_PM25.rds")
      #SCC <- readRDS("/Users/Coursera/exdata_data_NEI_data/Source_Classification_Code.rds")

      #Subset the data by year, to calculate the total emissions for each year
      emissions1999 <- subset(NEI,NEI$year=="1999")
      emissions2002 <- subset(NEI,NEI$year=="2002")
      emissions2005 <- subset(NEI,NEI$year=="2005")
      emissions2008 <- subset(NEI,NEI$year == "2008")

      #Calculate the total emissions from all sources
      totalEmissions1999 <- sum(emissions1999$Emissions)
      totalEmissions2002 <- sum(emissions2002$Emissions)
      totalEmissions2005 <- sum(emissions2005$Emissions)
      totalEmissions2008 <- sum(emissions2008$Emissions)

      #Create vectors of the result
      years<-c(1999,2002,2005,2008)
      totals<-c(totalEmissions1999,totalEmissions2002,totalEmissions2005,totalEmissions2008)

      #Generate a simple plot
      png(filename="totalpm.png",width=480,height=480)
      plot(years,totals,type="l",main="Total PM25 Emissions from all Sources",xlab="", ylab="Total PM 25 Emissions (tons)")
      dev.off()
}
