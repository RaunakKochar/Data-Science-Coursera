plot5 <- function(){
    library(ggplot2)
    
    NEI <- readRDS("exdata_2/summarySCC_PM25.rds")
    SCC <- readRDS("exdata_2/Source_Classification_Code.rds")
    
    #Subsetting SCC based on the terms MOBILE VEHICLES
    SCC <- SCC[grepl("mobile", SCC$EI.Sector, ignore.case = TRUE),]
    SCC <- SCC[grepl("vehicle", SCC$EI.Sector, ignore.case = TRUE),]
    
    #Subsetting NEI stepwise
    NEI <- NEI[NEI$fips == "24510",]
    NEI <- NEI[NEI$type == "ON-ROAD",]
    selected <- NEI$SCC %in% SCC$SCC
    NEI <- NEI[selected,]
    
    NEI <- aggregate(NEI[,c("Emissions")], by=list(NEI[,c("year")]), sum, na.rm = T)
    colnames(NEI) <- c("Year", "Emissions")
    
    p <- ggplot(data = NEI, aes(x = Year, y = Emissions)) + geom_bar(stat = "identity") + ylab("PM 2.5 Emission(1,00,000) in Baltimore") + ggtitle("Motor vehicular emissions in Baltimore city")

    #Saving the plot to a png device
    png(filename = "plot5.png", width = 480, height = 480)
    plot(p)
    dev.off()    
}