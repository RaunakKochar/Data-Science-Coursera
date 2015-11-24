plot6 <- function(){
    library(ggplot2)
    
    NEI <- readRDS("exdata_2/summarySCC_PM25.rds")
    SCC <- readRDS("exdata_2/Source_Classification_Code.rds")
    
    #Subsetting SCC based on the terms MOBILE VEHICLES
    SCC <- SCC[grepl("mobile", SCC$EI.Sector, ignore.case = TRUE),]
    SCC <- SCC[grepl("vehicle", SCC$EI.Sector, ignore.case = TRUE),]
    
    #Subsetting NEI stepwise
    balt <- NEI[NEI$fips == "24510",]
    balt <- balt[balt$type == "ON-ROAD",]
    selected <- balt$SCC %in% SCC$SCC
    balt <- balt[selected,]
    
    la <- NEI[NEI$fips == "06037",]
    la <- la[la$type == "ON-ROAD",]
    selected <- la$SCC %in% SCC$SCC
    la <- la[selected,]
    
    #Coverting character to numeric
    balt[,1] <- as.numeric(as.character(balt[,1]))
    balt[,2] <- as.numeric(as.character(balt[,2]))
    la[,1] <- as.numeric(as.character(la[,1]))
    la[,2] <- as.numeric(as.character(la[,2]))
    
    balt <- aggregate(balt[,c("Emissions")], by=list(balt[,c("year")]), sum, na.rm = T)
    colnames(balt) <- c("Year", "Emissions")
    
    la <- aggregate(la[,c("Emissions")], by=list(la[,c("year")]), sum, na.rm = T)
    colnames(la) <- c("Year", "Emissions")
    
    #Changes over the years
    for(i in 1:3){
        balt[i,2] <- balt[i+1,2] - balt[i,2]
        la[i,2] <- la[i+1,2] - la[i,2]
    }
    
}