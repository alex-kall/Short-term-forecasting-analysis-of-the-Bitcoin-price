library(ggplot2)
library(ggcorrplot)
library(ggthemes)
library(ggseas)
library(data.table)
library(tools)
library(writexl)
library(gridExtra)
library(RColorBrewer)

##
## CSVs location
##
csvDirectory <- "C:/Users/alexandra/Desktop/ΔΙΠΛΩΜΑΤΙΚΗ/alexandra/rstudio"

df           <- read.csv(paste(csvDirectory, "all.csv", sep="/"), stringsAsFactors=F)
df_values    <- subset(df, select=-c(Date))

png(
  filename=paste(csvDirectory, "correlation.png", sep="/"),
  width=1024, height=768)

corr          <- round(cor(df_values, use="complete.obs"), 1)
ggcorrplot(corr, hc.order=TRUE,
           type="lower",
           lab=TRUE,
           lab_size=3,
           method="square",
           ggtheme=theme_bw,
           col=brewer.pal(n=3, name="Greens")) +
labs(title="Πίνακας Αλληλοσυσχέτισης Χαρακτηριστικών",
     subtitle="Συσχετίσεις μεταξύ αγαθών, δεικτών, μεγεθών του Bitcoin",
     caption="Πηγή: Yahoo Finance κ.ά.")
dev.off()

df            <- na.omit(df)
df$Date       <- as.Date(df$Date)

png(
  filename=paste(csvDirectory, "distribution.png", sep="/"),
  width=1024, height=768)
ggplot(df, aes(y=Bitcoin)) +
  geom_boxplot(fill="plum") +
  theme_gray() +
  labs(y="Τιμή Bitcoin (USD)",
       title="Κατανομή Τιμής Bitcoin",
       caption="Πηγή: Yahoo Finance") +
  ylim(-10000, 30000) + xlim(-2, 2)
dev.off()

png(
  filename=paste(csvDirectory, "decomposition.png", sep="/"),
  width=1024, height=768)
ggsdc(df,
      aes(x=Date, y=Bitcoin), method="stl", frequency=7, s.window="per",
      facet.titles=c(
        "Πραγματικές Παρατηρήσεις",
        "Τάση",
        "Εποχικότητα",
        "Τυχαιότητα")) +
  geom_line() +
  labs(title="Αποσύνθεση τιμής Bitcoin",
       subtitle="Ανάλυση της χρονοσειράς Bitcoin σε συνιστώσες",
       y="Τιμή Bitcoin (USD)") +
  xlab("Ημερομηνία")
dev.off()


---------------
png(
    filename=paste(csvDirectory, "cryptosplot.png", sep="/"),
    width=1024, height=768)
plot(
  ts(all_df$Bitcoin, frequency=365, start=c(2015,1)),
  col=2, type="l",
  ylab="Price", xlab="Date")

lines( ts(all_df$Ethereum, frequency=365, start= c(2015,1)), col=3);
lines( ts(all_df$Monero, frequency=365, start= c(2015,1)), col=4)   ;
lines( ts(all_df$Litecoin, frequency=365, start= c(2015,1)), col=5) ;
lines( ts(all_df$Dash, frequency=365, start= c(2015,1)), col=6)   ;
legend("topleft",legend = c("Bitcoin", "Ethereum", "Monero", "Litecoin", "Dash"), col=c(2:6), lty=1, cex=0.8)
dev.off()


