baseplot3<-function(){
      NEI <- readRDS("/Users/Coursera/exdata_data_NEI_data/summarySCC_PM25.rds")
      #SCC <- readRDS("/Users/Coursera/exdata_data_NEI_data/Source_Classification_Code.rds")
      #Subset for Baltimore City
      baltimore <- subset(NEI,NEI$fips=="24510")
      #Now subset the Baltimore City data into each year
      emissions1999 <-subset(baltimore,baltimore$year=="1999")
      emissions2002 <-subset(baltimore,baltimore$year=="2002")
      emissions2005 <- subset(baltimore,baltimore$year=="2005")
      emissions2008 <- subset(baltimore,baltimore$year == "2008")
      #print(emissions2008)

      #Subset each year by source type: "point", "nonpoint", "onroad", "nonroad"
      point1999 <- subset(emissions1999, emissions1999$type=="POINT")

      nonpoint1999 <- subset(emissions1999, emissions1999$type=="NONPOINT")
      onroad1999 <- subset(emissions1999, emissions1999$type=="ON-ROAD")
      nonroad1999 <- subset(emissions1999, emissions1999$type=="NON-ROAD")

      point2002 <- subset(emissions2002, emissions2002$type=="POINT")
      print(head(point2002,5))
      nonpoint2002 <- subset(emissions2002, emissions2002$type=="NONPOINT")
      onroad2002 <- subset(emissions2002, emissions2002$type=="ON-ROAD")
      nonroad2002 <- subset(emissions2002, emissions2002$type=="NON-ROAD")

      point2005 <- subset(emissions2005, emissions2005$type=="POINT")
      print(head(point2005,5))
      nonpoint2005 <- subset(emissions2005, emissions2005$type=="NONPOINT")
      onroad2005 <- subset(emissions2005, emissions2005$type=="ON-ROAD")
      nonroad2005 <- subset(emissions2005, emissions2005$type=="NON-ROAD")

      point2008 <- subset(emissions2008, emissions2008$type=="POINT")
      nonpoint2008 <- subset(emissions2008, emissions2008$type=="NONPOINT")
      onroad2008 <- subset(emissions2008, emissions2008$type=="ON-ROAD")
      nonroad2008 <- subset(emissions2008, emissions2008$type=="NON-ROAD")

      #Calculate the total emissions from each source ()
      totalPoint1999 <- sum(point1999$Emissions)
      totalNonPoint1999 <- sum(nonpoint1999$Emissions)
      totalOnRoad1999 <- sum(onroad1999$Emissions)
      totalNonRoad1999 <- sum(nonroad1999$Emissions)


      totalPoint2002 <- sum(point2002$Emissions)
      totalNonPoint2002 <- sum(nonpoint2002$Emissions)
      totalOnRoad2002 <- sum(onroad2002$Emissions)
      totalNonRoad2002 <- sum(nonroad2002$Emissions)

      totalPoint2005 <- sum(point2005$Emissions)
      totalNonPoint2005 <- sum(nonpoint2005$Emissions)
      totalOnRoad2005 <- sum(onroad2005$Emissions)
      totalNonRoad2005 <- sum(nonroad2005$Emissions)

      totalPoint2008 <- sum(point2008$Emissions)
      totalNonPoint2008 <- sum(nonpoint2008$Emissions)
      totalOnRoad2008 <- sum(onroad2008$Emissions)
      totalNonRoad2008 <- sum(nonroad2008$Emissions)
      par(mfrow = c(1,4), mar=c(4,4,2,1), oma=c(0,0,2,0))

      #Create vectors of the result
      years<-c(1999,2002,2005,2008)
      points<-c(totalPoint1999,totalPoint2002,totalPoint2005,totalPoint2008)
      nonpoints <-c(totalNonPoint1999,totalPoint2002,totalPoint2005,totalPoint2008)
      onroad <- c(totalOnRoad1999,totalOnRoad2002, totalOnRoad2005,totalOnRoad2008)
      nonroad <- c(totalNonRoad1999, totalNonRoad2002, totalNonRoad2005, totalNonRoad2008)
      par(mfrow = c(4,1))
      with(baltimore,{
         plot(years,points,pch=19,main="Point Emissions",xlab="",ylab="Total PM25 Emissions")
         plot(years,nonpoints,pch=20,main="Non-Point Emissions",xlab="",ylab="Total PM25 Emissions")
         plot(years,onroad,pch=21,main="OnRoad Emissions",xlab="",ylab="Total PM25 Emissions")
         plot(years,nonroad,pch=22,main="Non-Road Emissions",xlab="",ylab="Total PM25 Emissions")
         mtext("Total PM25 Emissions for Baltimore City",outer=TRUE)
         model <-lm(years ~ points, baltimore)
         model <-lm(years ~ nonpoints, baltimore)
         model <-lm(years ~ onroad, baltimore)
         model <-lm(years ~ nonroad, baltimore)
      })

      #Generate a simple plot
      # png(filename="baltimore.png", width=480,height=480, bg="white")
      # plot(years,points,type="l",main="Total PM25 Point Emissions for Baltimore City",xlab="", ylab="Total PM 25 Emissions (tons)")
      # dev.off()

}

