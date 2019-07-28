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