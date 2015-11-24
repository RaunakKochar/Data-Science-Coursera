plot1 <- function(){
    setwd("C:/Users/RAUNAK KOCHAR/Documents/exdata_project2/")
    
    NEI <- readRDS("exdata_2/summarySCC_PM25.rds")
    SCC <- readRDS("exdata_2/Source_Classification_Code.rds")
    
    total <- aggregate(NEI[,c(4)], by = list(NEI$year), sum, na.rm = TRUE)
    
    barplot(total$x, total$Group.1, xlab = "Year", ylab = "Emissions",names.arg = c(1998,2002,2005,2008))
    dev.copy(png, file="plot1.png", height=480, width=480)
    dev.off()   
}