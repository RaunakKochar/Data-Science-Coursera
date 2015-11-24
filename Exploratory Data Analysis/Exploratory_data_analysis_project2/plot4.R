plot4 <- function(){
    library(ggplot2)
    
    NEI <- readRDS("exdata_2/summarySCC_PM25.rds")
    SCC <- readRDS("exdata_2/Source_Classification_Code.rds")
    
    #Subsetting SCC to include only coal combustion related sources
    SCC <- SCC[grepl("Combustion", SCC$SCC.Level.One, ignore.case = TRUE),] 
    SCC <- SCC[grepl("coal", SCC$SCC.Level.Three, ignore.case = TRUE),] 
    
    #Subsetting NEI to include only coal combustion related sources
    selected <- NEI[,2] %in% SCC[,1]
    NEI <- NEI[selected,]
    
    #Aggregating NEI by years
    NEI <- aggregate(NEI[,c(4)], by = list(NEI$year), sum, na.rm = T)
    
    colnames(NEI) <- c("Year", "Emission")
    
    #Creating a plot
    p <- ggplot(data = NEI, aes(x = Year, y = Emission)) + geom_bar(stat = "identity") + ylab("PM 2.5 Emission(1,00,000 Tons)")
    
    #Saving the plot to a png device
    png(filename = "plot4.png", width = 480, height = 480)
    plot(p)
    dev.off()
}