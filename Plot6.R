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