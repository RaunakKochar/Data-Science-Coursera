plot2 <- function(){
    setwd("C:/Users/RAUNAK KOCHAR/Documents/exdata_project2/")
    
    #Reading emission data for various counties
    NEI <- readRDS("exdata_2/summarySCC_PM25.rds")
    SCC <- readRDS("exdata_2/Source_Classification_Code.rds")
    
    #Subsetting the data frame and including rows for Baltimore county "fips - 24510"
    NEI <- NEI[NEI$fips == "24510",]
    
    total <- aggregate(NEI[,c(4)], by = list(NEI$year), sum, na.rm = TRUE)
    
    barplot(total$x, total$Group.1, xlab = "Year", ylab = "Emissions - Baltimore City",names.arg = c(1998,2002,2005,2008))
    
    dev.copy(png, file="plot2.png", height=480, width=480)
    dev.off()   
}