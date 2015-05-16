plot5<-function(){
     #Address question 5:
     #What is the trend in motor vehicle emissions for Baltimore City
     #from 1999-2008?
     #Determine the SCC values that correspond to both off-road and on-road
     #motor vehicles to account for all transportation (personal automobile,
     #public transportation, commercial transportation, etc.) and construction
     #and industrial motor vehicles (delivery trucks, cement trucks, tractors, 
     #etc.)
     #Use these SCC values from the Source_Classification_Code file to subset
     #the emission data for 1999-2008 from the summarySCC_PM25 data file.  
     #Make a simple plot with a best fit line to determine the trend.  A best
     #fit line with a positive slope indicates an overall increase in emissions      #while that with a negative slope indicates an overall decrease in 
     #emissions.  A horizontal line indicates no overall increase or 
     #decrease, i.e. emissions are constant. Use the total emissions for each
     #year, as the total amounts impact health and the environment.


     #Read in the data
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

     #Subset the NEI dataframe for Baltimore City motor vehicles
     MV <- subset(NEI,NEI$SCC %in% mvSCCs )
     baltimoreMV <- subset(MV, MV$fips == "24510")
     #Get the total emissions for each year: 1999, 2002, 2005, and 2008.
     mv1999Data <- subset(baltimoreMV, baltimoreMV$year == "1999")
     mv2002Data <- subset(baltimoreMV, baltimoreMV$year == "2002")
     mv2005Data <- subset(baltimoreMV, baltimoreMV$year == "2005")
     mv2008Data <- subset(baltimoreMV, baltimoreMV$year == "2008")
     mv1999Totals <- sum(mv1999Data$Emissions)
     mv2002Totals <- sum(mv2002Data$Emissions)
     mv2005Totals <- sum(mv2005Data$Emissions)
     mv2008Totals <- sum(mv2008Data$Emissions)
     totalEmissions <- c(mv1999Totals, mv2002Totals, mv2005Totals, mv2008Totals)
     years <- c(1999,2002,2005,2008)
     

     #Generate a simple plot with best fit line to determine if there is a 
     #trend.  A line with a negative slope indicates an overall decrease in
     #motor vehicle emissions, a line with a positive slope indicates an
     #overall increase in vehicle emissions.
     png(file="plot5.png",height=480,width=480)
     plot(years,totalEmissions,pch=19, xlab="Year",ylab="Emissions (tons)", main="Total motor vehicle emissions for Baltimore City 1999-2008")
     abline(lm(totalEmissions~years),col="red")
     legend(x="topright", legend="Linear Regression", lty=1,col="red",title="Best-fit line") 
     dev.off()
     
}
