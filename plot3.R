plot3<-function(){
      #Determine which source types (point, nonpoint, onroad, nonroad)
      #indicate an increase/decrease in emissions for Baltimore City.

      #Generate a panel of plots, one for each source type for
      #data for Baltimore City from 1999-2008 to see if there are any
      #outstanding trends.  A scatterplot of all the emission values with
      #a best fit line should indicate whether the emissions for that
      #source are increasing or decreasing.  A best fit line with a
      #positive slope will indicate an increasing trend, while a
      #best fit line with a negative slope indicates a decrease in
      #emissions.

      #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      # NOTE: YOU MUST USE ggplot2 to generate the plot.
      #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      library(ggplot2)

      #Load the data
      NEI <- readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/summarySCC_PM25.rds")

      #Subset the data for Baltimore City
      baltimore <- subset(NEI,NEI$fips=="24510")

      #For each source, calculate the total emissions for each year and type.
      #First, subset by year, then by type, and then get the total emissions.

      #1999 POINT, NONPOINT, ON-ROAD, and NON-ROAD
      balt1999 <- subset(baltimore, baltimore$year == "1999")
      point1999 <- subset(balt1999, balt1999$type =="POINT")
      totalPoint1999 <- sum(point1999$Emissions)
      nonpoint1999 <- subset(balt1999,balt1999$type =="NONPOINT")
      totalNonPt1999 <- sum(nonpoint1999$Emisssions)
      onroad1999 <- subset(balt1999,balt1999$type == "ON-ROAD")
      totalOnRd1999 <- sum(onroad1999$Emissions)
      nonroad1999 <- subset(balt1999,balt1999$type == "NON-ROAD")
      totalNonRd1999 <- sum(nonroad1999$Emissions)

      #2002 POINT, NONPOINT, ON-ROAD, and NON-ROAD
      balt2002 <- subset(baltimore, baltimore$year == "2002")
      point2002 <- subset(balt2002, balt2002$type == "POINT")
      totalPoint2002 <- sum(point2002$Emissions)
      nonpoint2002 <- subset(balt2002, balt2002$type == "NONPOINT")
      totalNonPt2002 <- sum(nonpoint2002$Emissions)
      onroad2002 <- subset(balt2002, balt2002$type == "ON-ROAD")
      totalOnRd2002 <- sum(onroad2002$Emissions)
      nonroad2002 <- subset(balt2002, balt2002$type == "NON-ROAD")
      totalNonRd2002 <-sum(nonroad2002$Emissions)

      #2005 POINT, NONPOINT, ON-ROAD, and NON-ROAD
      balt2005 <- subset(baltimore, baltimore$year == "2005")
      point2005 <- subset(balt2005, balt2005$type == "POINT")
      totalPoint2005 <- sum(point2005$Emissions)
      nonpoint2005 <- subset(balt2005, balt2005$type == "NONPOINT")
      totalNonPt2005 <- sum(nonpoint2005$Emissions)
      onroad2005 <- subset(balt2005, balt2005$type == "ON-ROAD")
      totalOnRd2005 <- sum(onroad2005$Emissions)
      nonroad2005 <- subset(balt2005, balt2005$type == "NON-ROAD")
      totalNonRd2005 <-sum(nonroad2005$Emissions)

      #2008 POINT, NONPOINT, ON-ROAD, and NON-ROAD
      balt2008 <- subset(baltimore, baltimore$year == "2008")
      point2008 <- subset(balt2008, balt2008$type == "POINT")
      totalPoint2008 <- sum(point2008$Emissions)
      nonpoint2008 <- subset(balt2008, balt2008$type == "NONPOINT")
      totalNonPt2008 <- sum(nonpoint2008$Emissions)
      onroad2008 <- subset(balt2008, balt2008$type == "ON-ROAD")
      totalOnRd2008 <- sum(onroad2008$Emissions)
      nonroad2008 <- subset(balt2008, balt2008$type == "NON-ROAD")
      totalNonRd2008 <-sum(nonroad2008$Emissions)


      #Create a new dataframe that can be used by ggplot
      #*kludgy way to do this, there is a better way to do this, but for now...

      #Total emissions for each year
      totalPoint <- c(totalPoint1999, totalPoint2002, totalPoint2005, totalPoint2008)
      totalNonPoint <-c(totalNonPt1999, totalNonPt2002, totalNonPt2005, totalNonPt2008)
      totalOnRoad <- c(totalOnRd1999, totalOnRd2002, totalOnRd2005, totalOnRd2008)
      totalNonRoad <- c(totalNonRd1999, totalNonRd2002, totalNonRd2005, totalNonRd2008)
      totalEmissions <- c(totalPoint, totalNonPoint, totalOnRoad, totalNonRoad)

      #All corresponding types
      types <- c("POINT","POINT","POINT","POINT","NONPOINT","NONPOINT",
                 "NONPOINT","NONPOINT","ON-ROAD","ON-ROAD", "ON-ROAD",
                 "ON-ROAD", "NON-ROAD","NON-ROAD","NON-ROAD", "NON-ROAD")
      #Corresponding year
      years <- c(1999,2002,2005,2008,
                 1999,2002,2005,2008,
                 1999,2002,2005,2008,
                 1999,2002,2005,2008)
      df <- data.frame("year" = years, "Emissions"=totalEmissions, "type"=types)
      print(summary(df))
      print(str(df))
      print(head(df,10))
      print(tail(df,10))




      #Generate a panel of plots of total Emissions vs year. The slope of the linear
      #regression lines gives an indication of whether the emissions are increasing
      #(positive slope), decreasing (negative slope), or unchanged
      #(slope of 1, i.e. a horizontal line)

      png(filename="plot3.png", width=480,height=480, bg="white")
      g <- ggplot(df,aes(year,Emissions))
      p <- g + geom_point() +
               labs(title="Baltimore City PM 25 Motor Vehicle Emissions") +
               ylab("Motor Vehicle Total Emissions (tons)") +
               geom_smooth(method="lm") +
               facet_grid(.~type)
      print(p)
      dev.off()

}
