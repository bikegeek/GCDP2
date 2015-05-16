plot4 <- function(){
        #Addresses question 4:
        #What is the trend for coal-combustion emissions for Baltimore City from
        #1999-2008?

        #Approach:  Determine the SCC values (from the Source_Classification_Code
        #that corresponds to any source that has any entry with "coal"
        #(case-insensitive).  Use these SCC values to subset the
        #summarySCC_PM25 data for Baltimore City (fips="24510") and calculate
        #the total emissions for each year: 1999, 2002,2005, and 2008.
        #Generate a simple plot with a best fit line to determine if there
        #is an overall trend.  A line with a negative slope will indicate
        #a decrease in emissions from 1999-2008, a line with a positive slope
        #indicates an increase in emissions.

        #if ggplot2 is used, uncomment
        #library(ggplot2)


        #Read in the data
        NEI <- readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/summarySCC_PM25.rds")
        SCC <- readRDS("/Users/MiniMe/Coursera/ExploratoryDataAnalysis/Project2/exdata_data_NEI_data/Source_Classification_Code.rds")

        #Subset based on coal in EI.Sector or Usage.Notes, and use the corresponding
        #SCC values on the summarySCC_PM25 data to see if there is a trend with the
        #coal-combustion sources.  Use regexpr to check for any variation of the
        #word 'coal' (case-insensitive) in the EI.Sector column.
        vec <- SCC$EI.Sector
        coal<-subset(SCC, regexpr("Coal", vec,ignore.case=TRUE) > 0 )
        coal$SCC <- as.character((coal$SCC))
        m<-coal$SCC

        #Subset the summarySCC_PM25 data based on the SCC ids for coal.
        NEI$SCC <- as.character(NEI$SCC)
        coalData <- subset(NEI,NEI$SCC %in% m)

        #Compute the total emissions for each year since it's the total
        #amount of emissions that affects health and climate.
        coal1999Data <- subset(coalData,coalData$year == "1999")
        coal2002Data <- subset(coalData,coalData$year == "2002")
        coal2005Data <- subset(coalData,coalData$year == "2005")
        coal2008Data <- subset(coalData,coalData$year == "2008")

        sum1999 <- sum(coal1999Data$Emissions)
        sum2002 <- sum(coal2002Data$Emissions)
        sum2005 <- sum(coal2005Data$Emissions)
        sum2008 <- sum(coal2008Data$Emissions)

        #Create a new data frame (in case we need to use ggplot2)
        Emissions <- c(sum1999, sum2002, sum2005, sum2008)
        year <- c(1999, 2002, 2005, 2008)
        df <- data.frame("year"= year, "Emissions" = Emissions)
        emissions <- df$Emission
        years <- df$year

        #Generate a simple plot with best fit line to determine overall trend.
        #A best fit line with a negative slope indicates a decrease in emissions,
        #whereas one with a positive slope indicated an increase in emission.
        #A slope of 1 (horizontal line) indicates no change in emission (ie no
        #increase or decrease).
        png(file="plot4.png", height=480,width=480)
        plot(years,emissions,pch=21, xlab="Year",
             ylab="Total Emissions from Coal-combustion (tons)",
             main="Total Coal-combustion Emissions for Baltimore City 199-2008",
             col="blue")
        abline(lm(emissions~years),col="red")
        legend(x="topright", legend="Linear Regression", lty=1,col="red",title="Best-fit line")

        #ggplot2 instructions for scatter plot, if base plot's simple line plot
        #doesn't look OK...
        #g <- ggplot(coalData,aes(x=year,y=Emissions))
        #p <- g  + labs(title="PM 25 Emissions from Coal") +
        #    geom_point(shape=1) + geom_smooth(method=lm)
        #print(p)

        dev.off()


}
