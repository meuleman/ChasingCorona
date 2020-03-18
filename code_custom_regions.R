############################################################################################################################
### Code for plotting a number of custom figures based on selected regions of interest
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

### Note that these are currently (March 17, 2020) not properly being reported anymore
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
plot_population_percentages <- function(confirmed_perc, confirmed_perc_mean, idxs, cols, xlim=NULL, logscale=FALSE) {
  layout(matrix(1:2, ncol=2), width=c(7,3))
  par(oma=c(2,2,1,0), bg="white", cex=2, mar=c(0,0,0,4))
  if (is.null(xlim)) xlim <- range(as.Date(colnames(confirmed)))
  dat_to_plot <- confirmed_perc[idxs,]
  dat_to_plot[dat_to_plot==0] <- NA
  plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
       type="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", log=ifelse(logscale, "y", ""), 
       xlim=xlim, ylim=range(dat_to_plot[,as.Date(colnames(dat_to_plot)) %in% xlim], na.rm=T))
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc_mean, col="black", lwd=5)
  for (i in 1:length(idxs)) {
    lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
  }
  axis(1, labels=F, at=as.Date(colnames(confirmed_perc)), tcl=-0.25)
  axis(4, las=2)
  mtext("% of population", side=2, line=0.5, cex=2)
  legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
  par(mar=c(0,0,0,0))
  plot(0, type="n", axes=FALSE)
  labs <- c(rownames(confirmed_perc)[idxs], "World-wide")
  nums <- signif(c(apply(confirmed_perc[idxs,], 1, max, na.rm=T), tail(confirmed_perc_mean, 1)), 2)
  string_date <- format(as.Date(tail(colnames(confirmed_perc), 1)), format="%B %d, %Y")
  legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
  legend("topleft", "(x,y)", labs, lwd=5, cex=0.7, col=c(cols, "black"), inset=c(0.01, 0.1), bty="n")
  legend("topright", "(x,y)", paste(nums, "%", sep=""), cex=0.7, inset=c(0.01, 0.1), bty="n")
  par(new=T, xpd=T, oma=c(0,0,0,0))
  legend("bottomright", "(x,y)", "@nameluem\nwww.meuleman.org", text.col="grey", bty="n", cex=0.75)
}

fn <- "percentage_population_confirmed_custom"
xlim <- as.Date(c("2020-02-15", tail(colnames(confirmed_perc), 1)))
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
plot_population_percentages(confirmed_perc, confirmed_perc_mean, idxs, cols, xlim=xlim, logscale=FALSE)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_confirmed_custom_log"
xlim <- as.Date(c("2020-02-15", tail(colnames(confirmed_perc), 1)))
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
plot_population_percentages(confirmed_perc, confirmed_perc_mean, idxs, cols, xlim=xlim, logscale=TRUE)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}


