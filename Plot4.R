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