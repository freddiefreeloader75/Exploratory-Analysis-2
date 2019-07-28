# Setting WorkSpace
setwd("~/Exploratory_Proj2")
# Library
library("data.table")
library(ggplot2)
# Download and unzip file
path <- getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
              , destfile = paste(path, "dataFiles.zip", sep = "/"))
unzip(zipfile = "dataFiles.zip")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Plot 1
# reading rds file (for information, already loaded in the loading script)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
## NEI Aggregate
globalNEI <- aggregate(Emissions~year,data = NEI,FUN = sum)
# plot file definition
png(filename = "plot1.png",width = 480, height = 480,units = "px")
# barplot definition
barplot(
        (globalNEI$Emissions)/10^6,
        names.arg = globalNEI$year,
        col = "blue",
        xlab = "Year",
        ylab = "PM2.5 Emissions",
        main = "Total PM2.5 Emissions"
)
 dev.off()
 
 # PLot 2
 # Blatimore NEI and aggregate
 baltimoreNEI <- (NEI[NEI$fips == "24510",])
 globalBaltimoreNEI <- aggregate(Emissions~year,data = baltimoreNEI,FUN = sum)
 # plot file definition
 png(filename = "plot2.png",width = 480, height = 480,units = "px")
 # barplot definition
 barplot(globalBaltimoreNEI$Emissions,
         names.arg = globalBaltimoreNEI$year,
         col = "blue",
         xlab = "Years",
         ylab = "PM2.5 Emissions (by Ton)",
         main = "Total PM2.5 Emissions for Baltimore"
 )
 dev.off()
 
 # Plot 3
 baltimoreNEI <- (NEI[NEI$fips == "24510",])
 # plot file definition
 png(filename = "plot3.png",width = 750, height = 602,units = "px",)
 # ggplot definition
 p3 <- ggplot(data = baltimoreNEI, aes(factor(year), Emissions, fill = type)) +
         geom_bar(stat = "identity") +
         facet_grid(facets = .~type,scales = "free",space = "free") +
         labs(x="Year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
         labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 / Source Type"))
 print(p3)
 dev.off()
 
 #plot 4
 # filter SCC with combution and coal related data
 combustion <- grepl(pattern = "combust",x = SCC$SCC.Level.One,ignore.case = TRUE)
 coal <- grepl(pattern = "coal",x = SCC$SCC.Level.Four,ignore.case = TRUE)
 combustioncoal <- (combustion & coal)
 combustionSCC <- SCC[combustioncoal,]$SCC
 combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]
 
 # plot file definition
 png(filename = "plot4.png",width = 480, height = 480,units = "px",)
 # ggplot definition
 p4 <- ggplot(data = combustionNEI, aes(factor(year), Emissions/10^5)) +
         geom_bar(stat = "identity",fill = "blue", width = 0.75) +
         theme_grey(base_size = 14,base_family = "") +
         labs(x="Year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
         labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions (USA 1999-2008)"))
 print(p4)
 dev.off()
 
 # Plot 5
 # Filter dataset with vehicle data
 vehicles <- grepl(pattern = "vehicle",x = SCC$SCC.Level.Two,ignore.case = TRUE)
 vehiclesSCC <- SCC[vehicles,]$SCC
 vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]
 # Baltimore data
 baltimorevehiclesNEI <- (vehiclesNEI[vehiclesNEI$fips == "24510",])
 # file plot definition
 png(filename = "plot5.png",width = 480, height = 480,units = "px",)
 # ggplot definition
 p5 <- ggplot(data = baltimorevehiclesNEI, aes(factor(year), Emissions)) +
         geom_bar(stat = "identity",fill = "blue", width = 0.75) +
         theme_grey(base_size = 14,base_family = "") +
         labs(x="Year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
         labs(title=expression("PM"[2.5]*" Vehicle Source Emissions - Baltimore"))
 print(p5)
 dev.off()
 
 #Plot 6
 # Filter dataset with Vehicle data
 vehicles <- grepl(pattern = "vehicle",x = SCC$SCC.Level.Two,ignore.case = TRUE)
 vehiclesSCC <- SCC[vehicles,]$SCC
 vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]
 
 ## filter vehicles from Baltimore and LA County, update Filter vehicles based Baltimore and LA County and update the observations name
 BaltLAvehicles <- (vehiclesNEI[vehiclesNEI$fips == "24510" | vehiclesNEI$fips == "06037",])
 BaltLAvehicles[,1] <- as.factor(x = BaltLAvehicles[,1])
 BaltLAvehicles <- BaltLAvehicles
 levels(BaltLAvehicles$fips)[levels(BaltLAvehicles$fips)=="06037"] <- "Los Angele County"
 levels(BaltLAvehicles$fips)[levels(BaltLAvehicles$fips)=="24510"] <- "Baltimore City"
 # file plot definition
 png(filename = "plot6.png",width = 480, height = 480,units = "px",)
 # ggplot definition
 p6 <- ggplot(data = BaltLAvehicles, aes(factor(year), Emissions)) +
         geom_bar(stat = "identity",fill = "blue", width = 0.75) + 
         facet_grid(facets = .~fips,scales = "free", space = "free") +
         theme_grey(base_size = 14,base_family = "") +
         labs(x="Year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
         labs(title=expression("PM"[2.5]*" Vehicle Emissions, LA County & Baltimore"))
 print(p6)
 dev.off()
