############################################################################################################################
### Code for plotting a number of custom derivative views
############################################################################################################################
library(RColorBrewer)
source("code_preprocess.R")

### Output directory
figdir <- "PDF_figures";
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)
dir.create("PNG_figures", recursive=TRUE, showWarnings=FALSE)

############################################################################################################################
### Custom regions

# Re-loading original data to undo some of the earlier "corrections" (see code_preprocess.R)
covid19_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
confirmed_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Confirmed.csv", sep="/"), sep=",", header=T, as.is=T)
deaths_all    <- read.delim(paste(covid19_dir, "time_series_19-covid-Deaths.csv",    sep="/"), sep=",", header=T, as.is=T)
recovered_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Recovered.csv", sep="/"), sep=",", header=T, as.is=T)
colnames(confirmed_all) <- colnames(deaths_all) <- colnames(recovered_all) <-
  c(colnames(confirmed_all)[1:4], as.character(as.Date(colnames(confirmed_all)[-c(1:4)], format="X%m.%d.%y")))

nam <- "Washington State"
population[nam] <- 7535591
idxs <- grep(", WA$", confirmed_all$Province.State);
confirmed[nam,] <- colSums(confirmed_all[idxs,-c(1:4)])
deaths[nam,] <- colSums(deaths_all[idxs,-c(1:4)])
recovered[nam,] <- colSums(recovered_all[idxs,-c(1:4)])

nam <- "King County, WA"
population[nam] <- 2190200
idxs <- grep("^King County, WA$", confirmed_all$Province.State);
confirmed[nam,] <- colSums(confirmed_all[idxs,-c(1:4)])
deaths[nam,] <- colSums(deaths_all[idxs,-c(1:4)])
recovered[nam,] <- colSums(recovered_all[idxs,-c(1:4)])

############################################################################################################################

### Percentage of population with confirmed cases / deaths / recoveries
confirmed_perc <- sweep(confirmed, 1, population, FUN="/") * 100
deaths_perc <- sweep(deaths, 1, population, FUN="/") * 100
recovered_perc <- sweep(recovered, 1, population, FUN="/") * 100

### Percentage of confirmed cases that have resulted in deaths / recoveries
confirmed_deaths_perc <- deaths / confirmed * 100
confirmed_recovered_perc <- recovered / confirmed * 100
confirmed_deaths_perc[confirmed < 100] <- NA # minimum of 100 cases required
confirmed_recovered_perc[confirmed < 100] <- NA # minimum of 100 cases required

### Construct date string for showing in figures.
string_date <- format(as.Date(tail(colnames(confirmed_perc), 1)), format="%B %d, %Y")

### Averages across all countries
confirmed_perc_mean <- colSums(confirmed) / sum(population) * 100
deaths_perc_mean    <- colSums(deaths)    / sum(population) * 100
recovered_perc_mean <- colSums(recovered) / sum(population) * 100

confirmed_deaths_perc_mean <- colSums(deaths) / colSums(confirmed) * 100
confirmed_recovered_perc_mean <- colSums(recovered) / colSums(confirmed) * 100

# Selection of countries
idxs <- which(rownames(confirmed_perc) %in% c("Italy", "King County, WA", "Washington State", "Netherlands"))
cols <- brewer.pal(9, "Set1")
cols <- cols[1:length(idxs)]

############################################################################################################################

fn <- "percentage_population_confirmed_custom"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,4,1,5), bg="white", cex=2)
# Confirmed cases
plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(confirmed_perc[idxs,])), 
     xlab="", ylab="", main=string_date, xlim=as.Date(c("2020-02-27", tail(colnames(confirmed_perc),1))))
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", rownames(confirmed_perc)[idxs], lwd=5, col=cols, inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

