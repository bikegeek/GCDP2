plot6 <- function(){
     
     #Addresses question 6: Compare motor vehicle emissions for Baltimore
     #City and Los Angeles to determine which city has the greatest changes from 1998-2008.

     #Approach: Generate simple line plots for the total motor vehicle emissions for both cities
     #and view side-by-side to see which has decreases/increases, determined by the slope
     #of the best-fit/linear regression line. A negative slope indicates an overall decrease
     #in emissions, a positive slope indicates an overall increase and a slope of 1 (horizontal
     #line indicates no overall change in emissions.
     #
     #The slope of the linear regression line will determine the degree of change- 
     #the larger the slope the greater the change).
     #Create facets to represent the Baltimore City and Los Angeles county plots.
    
     


     #Read in the data
     library(ggplot2)
     NEI <- readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/summarySCC_PM25.rds")
     SCC <- readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/Source_Classification_Code.rds")
     sector<- SCC$EI

     #Include off-road (fork-lifts,and other industrial equipment
     #and on-road (motorcycles, automobiles).  This will capture any
     #changes in construction/industrial activities in addition to 
     #transportation (personal, public, and otherwise). 
     motorvehicle <- subset(SCC, regexpr("vehicle|road", sector, ignore.case=TRUE) > 0)


     #Get the SCC values for motor vehicles (on-road and off-road, discussed above)
     mvId <- motorvehicle$SCC
     mvSCCs <- as.character(mvId)
     #print(mvSCCs)

     #Subset the NEI dataframe for Baltimore City and Los Angeles County motor vehicles
     #and create a new dataframe with the relevant information for ggplot2.
     #KLUDGY, but fortunately this is a small dataframe and dataset...
     allMV <- subset(NEI,NEI$SCC %in% mvSCCs )
     baltimoreMV <- subset(allMV, allMV$fips == "24510") 
     laCountyMV <- subset(allMV, allMV$fips == "06037")

     #Get the total emissions for each location: Baltimore City and Los Angeles County,
     #and corresponding year: 1999, 2002, 2005, and 2008.
     balt1999Data <- subset(baltimoreMV, baltimoreMV$year == "1999")
     balt2002Data <- subset(baltimoreMV, baltimoreMV$year == "2002")
     balt2005Data <- subset(baltimoreMV, baltimoreMV$year == "2005")
     balt2008Data <- subset(baltimoreMV, baltimoreMV$year == "2008")
     la1999Data <- subset(laCountyMV,laCountyMV$year == "1999")
     la2002Data <- subset(laCountyMV, laCountyMV$year == "2002")
     la2005Data <- subset(laCountyMV, laCountyMV$year == "2005")
     la2008Data <- subset(laCountyMV, laCountyMV$year == "2008")
     balt1999Totals <- sum(balt1999Data$Emissions)
     balt2002Totals <- sum(balt2002Data$Emissions)
     balt2005Totals <- sum(balt2005Data$Emissions)
     balt2008Totals <- sum(balt2008Data$Emissions)
     la1999Totals <- sum(la1999Data$Emissions)
     la2002Totals <- sum(la2002Data$Emissions)
     la2005Totals <- sum(la2005Data$Emissions)
     la2008Totals <- sum(la2008Data$Emissions)

     #Create the vectors that will comprise the columns of the new dataframe.
     totalEmissions <- c(balt1999Totals, balt2002Totals, balt2005Totals, balt2008Totals,
                         la1999Totals, la2002Totals, la2005Totals, la2008Totals)
     years <- c(1999,2002,2005,2008,
                1999,2002,2005,2008)
     #Add a new column, so we have a nice label for the facet.
     locations <- c("Baltimore City", "Baltimore City", "Baltimore City", "Baltimore City",
                   "Los Angeles County", "Los Angeles County", "Los Angeles County",
                   "Los Angeles County")
 
     df <- data.frame("year"=years, "Emission"=totalEmissions, "location"=locations)
     

     #Generate a simple plot with best fit line to determine if there is a 
     #trend.  A line with a negative slope indicates an overall decrease in
     #motor vehicle emissions, a line with a positive slope indicates an
     #overall increase in vehicle emissions. The plot with a regression line
     #with the largest slope (either negative or positive sloped) demonstrates
     #the greater changes in emissions.
     png(file="plot6.png",height=480,width=480)
     g <- ggplot(df, aes(year,Emission))
     p <- g + geom_point() +
          labs(title = "Motor Vehicle Emissions 1999-2008") +
          ylab("Total PM25 Emissions (tons)") + 
          xlab("Year") +
          geom_smooth(method="lm") + 
          facet_grid(.~location)
     print(p)
     dev.off()

}

