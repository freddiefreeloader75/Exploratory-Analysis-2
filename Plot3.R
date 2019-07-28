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