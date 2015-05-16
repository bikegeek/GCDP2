plot2<-function(){
      #Determine if *TOTAL* emissions (from all sources) for Baltimore City have increased or decreased.
      #Generate a simple line plot of only 4 points to see if a trend exists.
      #!!!!NOTE!!!:  The plots MUST BE GENERATED using R base plotting.

      NEI <- readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/summarySCC_PM25.rds")
      #SCC <- readRDS("/Users/Coursera/exdata_data_NEI_data/Source_Classification_Code.rds")

      #First, subset the data for Baltimore City, with fips=24150
      baltimore <- subset(NEI,NEI$fips=="24510")

      #Take a peek at the emissions for Baltimore City
      print("Baltimore City Emissions summary:")
      print(summary(baltimore$Emissions))

      #Subset the data by year
      emissions1999<-subset(baltimore,baltimore$year=="1999")
      emissions2002 <-subset(baltimore,baltimore$year=="2002")
      emissions2005 <- subset(baltimore,baltimore$year=="2005")
      emissions2008 <- subset(baltimore,baltimore$year == "2008")

      #Calculate the total emissions from all sources
      totalEmissions1999 <- sum(emissions1999$Emissions)
      totalEmissions2002 <- sum(emissions2002$Emissions)
      totalEmissions2005 <- sum(emissions2005$Emissions)
      totalEmissions2008 <- sum(emissions2008$Emissions)

      #Create vectors of the years and the totals for emissions.
      years<-c(1999,2002,2005,2008)
      totals<-c(totalEmissions1999,totalEmissions2002,totalEmissions2005,totalEmissions2008)

      #Look at Baltimore City data frame
      print("Summary of Baltimore total emissions from 1999-2008")
      print(summary(totals))

      #Generate a simple line plot of only four data point to see if a trend exists.
      png(filename="plot2.png",height=480,width=480)
      plot(years,totals,pch=19,main="Total PM25 Emissions- All Sources for Baltimore City",xlab="", ylab="Total PM 25 Emissions (tons)",col="black")

     #Since there's an increase from 2002 to 2005 (which defies the overarching trend of decreasing
     #emissions), let's add a best-fit line (linear regression/least squares fit)
     abline(lm(totals~years),col="red")

     #Label the linear regression line, define the line type, use solid line (linetype=1),
     #and add the legend.
     legend(x="topright", legend="Linear Regression", lty=1,col="red",title="Best-fit line")
     dev.off()
}

