plot3<-function(){
      library(ggplot2)
      NEI <- readRDS("/Users/Coursera/exdata_data_NEI_data/summarySCC_PM25.rds")
      #Subset for Baltimore City
      baltimore <- subset(NEI,NEI$fips=="24510")
      str(baltimore)
      g <- ggplot(baltimore,aes(year,Emissions))
      p <- g + geom_point() + facet_grid( . ~ type) + labs(title="Baltimore City PM 25 Emissions")
      summary(p)
      print(p)
      # png(filename="plot3.png", width=480,height=480, bg="white")
     # plot(years,totals,type="l",main="Total PM25 Emissions for Baltimore City (from all Sources)",xlab="", ylab="Total PM 25 Emissions (tons)")
    #  dev.off()

}
