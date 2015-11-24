plot3 <- function(){
    library(ggplot2)
    
    NEI <- readRDS("exdata_2/summarySCC_PM25.rds")
    SCC <- readRDS("exdata_2/Source_Classification_Code.rds")
    
    #Subsetting the data frame and including rows for Baltimore county "fips - 24510"
    NEI <- NEI[NEI$fips == "24510",]
    
    #Creating a data frame
    df <- data.frame(Year = numeric(0),
                     Point = numeric(0), 
                     Non_point = numeric(0),
                     Road = numeric(0), 
                     Non_Road = numeric(0),
                     stringsAsFactors = F)
    
    #Aggregating the NEI by "type" group
    total1999 <- NEI[NEI$year == 1999,]
    total1999_p <- NEI[total1999$type == "POINT",]
    total1999_np <- NEI[total1999$type == "NONPOINT",]
    total1999_r <- NEI[total1999$type == "ON-ROAD",]
    total1999_nr <- NEI[total1999$type == "NON-ROAD",]
    
    total1999_sp <- sum(total1999_p$Emissions)
    total1999_snp <- sum(total1999_np$Emissions)
    total1999_sr <- sum(total1999_r$Emissions)
    total1999_snr <- sum(total1999_nr$Emissions)
    
    total2002 <- NEI[NEI$year == 2002,]
    total2002_p <- NEI[total2002$type == "POINT",]
    total2002_np <- NEI[total2002$type == "NONPOINT",]
    total2002_r <- NEI[total2002$type == "ON-ROAD",]
    total2002_nr <- NEI[total2002$type == "NON-ROAD",]
    
    total2002_sp <- sum(total2002_p$Emissions)
    total2002_snp <- sum(total2002_np$Emissions)
    total2002_sr <- sum(total2002_r$Emissions)
    total2002_snr <- sum(total2002_nr$Emissions)
    
    total2005 <- NEI[NEI$year == 2005,]
    total2005_p <- NEI[total2005$type == "POINT",]
    total2005_np <- NEI[total2005$type == "NONPOINT",]
    total2005_r <- NEI[total2005$type == "ON-ROAD",]
    total2005_nr <- NEI[total2005$type == "NON-ROAD",]
    
    total2005_sp <- sum(total2005_p$Emissions)
    total2005_snp <- sum(total2005_np$Emissions)
    total2005_sr <- sum(total2005_r$Emissions)
    total2005_snr <- sum(total2005_nr$Emissions)
    
    total2008 <- NEI[NEI$year == 2008,]
    total2008_p <- NEI[total2008$type == "POINT",]
    total2008_np <- NEI[total2008$type == "NONPOINT",]
    total2008_r <- NEI[total2008$type == "ON-ROAD",]
    total2008_nr <- NEI[total2008$type == "NON-ROAD",]
    
    total2008_sp <- sum(total2008_p$Emissions)
    total2008_snp <- sum(total2008_np$Emissions)
    total2008_sr <- sum(total2008_r$Emissions)
    total2008_snr <- sum(total2008_nr$Emissions)
    
    #Creating a new data frame
    
    df[nrow(df)+1,] <- c(as.character("1999"), total1999_sp, total1999_snp, total1999_sr, total1999_snr)
    df[nrow(df)+1,] <- c(as.character("2002"), total2002_sp, total2002_snp, total2002_sr, total2002_snr)
    df[nrow(df)+1,] <- c(as.character("2005"), total2005_sp, total2005_snp, total2005_sr, total2005_snr)
    df[nrow(df)+1,] <- c(as.character("2008"), total2008_sp, total2008_snp, total2008_sr, total2008_snr)
    
    #Converting the typeof cols from character to numeric 
    df$Year <- as.numeric(as.character(df$Year))
    df$Point <- as.numeric(as.character(df$Point))
    df$Non_point <- as.numeric(as.character(df$Non_point))
    df$Road <- as.numeric(as.character(df$Road))
    df$Non_Road <- as.numeric(as.character(df$Non_Road))
    
    #Plotting the data using ggplot    
    p <- ggplot(data = df, aes(x = Year)) + 
        geom_line(aes(y = Point, color = "Type")) +
        geom_line(aes(y = Non_point, color = "Non_point")) +
        geom_line(aes(y = Road, color = "Road")) +
        geom_line(aes(y = Non_Road, color = "Non_Road")) +
        xlab("Year") +
        ylab("Type")
    
    #Saving the plot to the png device    
    png(filename = "plot3.png", width = 480, height = 480)
    plot(p)
    dev.off()
}