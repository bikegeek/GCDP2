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

      #Generate a panel of plots of Emissions vs year. The slope of the linear
      #regression lines gives an indication of whether the emissions are increasing
      #(positive slope), decreasing (negative slope), or unchanged
      #(slope of 1, i.e. a horizontal line)
      png(filename="plot3.png", width=480,height=480, bg="white")
      g <- ggplot(baltimore,aes(year,Emissions))
      p <- g + geom_point() + facet_grid( . ~ type) + labs(title="Baltimore City PM 25 Emissions") + geom_smooth(method="lm")
      print(p)
      dev.off()

}
