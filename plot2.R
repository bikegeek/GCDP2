p2q2<-function(){
      NEI <- readRDS("/Users/Coursera/exdata_data_NEI_data/summarySCC_PM25.rds")
      #SCC <- readRDS("/Users/Coursera/exdata_data_NEI_data/Source_Classification_Code.rds")
      baltimore <- subset(NEI,NEI$fips=="24510")
      emissions1999<-subset(baltimore,baltimore$year=="1999")
      print(head(emissions1999,10))
      #plot(NEI$Pollutant,NEI$year,xlab=" ",ylab="total PM2.5 Emissions (tons)")
      #Subset the data by year
      emissions2002 <-subset(baltimore,baltimore$year=="2002")
      emissions2005 <- subset(baltimore,baltimore$year=="2005")
      emissions2008  <- subset(baltimore,baltimore$year == "2008")

      #Calculate the total emissions from all sources
      print(head(emissions1999$Emissions,10))
      totalEmissions1999 <- sum(emissions1999$Emissions)
      print(cat("total 1999: ",totalEmissions1999))
      totalEmissions2002 <- sum(emissions2002$Emissions)
      totalEmissions2005 <- sum(emissions2005$Emissions)
      totalEmissions2008 <- sum(emissions2008$Emissions)

      #Create vectors of the result
      years<-c(1999,2002,2005,2008)
      totals<-c(totalEmissions1999,totalEmissions2002,totalEmissions2005,totalEmissions2008)

      #Generate a simple plot
      png(filename="baltimore.png", width=480,height=480, bg="white")
      plot(years,totals,main="Total PM25 Emissions for Baltimore City (from all Sources)",xlab="", ylab="Total PM 25 Emissions (tons)")
      dev.off()
}

