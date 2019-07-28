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