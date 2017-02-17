png("plots/africa.png", width = 1800, height = 1200, bg = "transparent")
plot(shp_africa, col=shp_africa$color, border="white", bg="transparent",
     main="Percent of malaria cases in Africa", cex.main=3, lwd=0.5)
pointLabel(coordinates(shp_africa), labels=shp_africa$Country, cex=1.5)
legend("bottom", fill=col_vec, cex=1.5, horiz=TRUE,
       legend=legend_txt,
       bty="n", x.intersp = 0.2, y.intersp = 0.2,
       border="darkgrey", text.col="black")
dev.off()
#install.packages("devEMF")
library(devEMF)
png("plots/spending.png", width=600, height=5, bg="transparent")
df_global_funding$Year <- as.numeric(as.character(df_global_funding$Year))
ggplot(data = df_global_funding, aes(x=Year, y=Amount, group=Source, colour=Source)) +
  geom_smooth(aes(group=Source), method="loess", se=FALSE, size=.9) +
  theme_bw() +
  ylab("Malaria funding (in million USD)") +
  xlab("Year") +
  theme(panel.background=element_rect(fill="transparent"),
        legend.background=element_rect(fill="transparent"),
        legend.box.background=element_rect(fill="transparent"),
        legend.key = element_rect(colour = NA, fill = NA),
        plot.background=element_rect(fill="transparent"))
dev.off()
