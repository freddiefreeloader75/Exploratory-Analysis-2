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