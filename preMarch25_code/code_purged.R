############################################################################################################################
### Code that was purged from the main analyses since they're either hard to interpret or plain wrong
############################################################################################################################
library(RColorBrewer)
library(gplots)
source("code_preprocess.R")

### Output directory
figdir <- "PDF_figures";
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)
dir.create("PNG_figures", recursive=TRUE, showWarnings=FALSE)

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
deaths_perc_mean <- colSums(deaths) / sum(population) * 100
recovered_perc_mean <- colSums(recovered) / sum(population) * 100

confirmed_deaths_perc_mean <- colSums(deaths) / colSums(confirmed) * 100
confirmed_recovered_perc_mean <- colSums(recovered) / colSums(confirmed) * 100

# Obtain "top-scoring" countries, in terms of percentage of population affected (min. 100 cases)
min_cases_100 <- which(apply(confirmed, 1, max, na.rm=T) > 100) # to accommodate NAs
min_pop_100 <- which(population > 100000)
#idxs <- head(order(-confirmed_perc[,ncol(confirmed_perc)]), 9)
#cols <- brewer.pal(9, "Set1")
idxs <- head(intersect(intersect(order(-apply(confirmed_perc, 1, max, na.rm=T)), min_cases_100), min_pop_100), 20)
cols <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
          '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', 
          '#808000', '#ffd8b1', '#000075', '#808080', '#000000')
#source: https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/

############################################################################################################################
# Esotheric plots, of percentage of population/cases resulting in death/recovery
# These are problematic to interpret, and erratic in nature too because of small numbers
############################################################################################################################
idxs <- head(intersect(intersect(order(-apply(confirmed_perc, 1, max, na.rm=T)), min_cases_100), min_pop_100), 20)

fn <- "percentage_population_deaths_recovered_top20_min100"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
string_date <- format(as.Date(tail(colnames(deaths_perc), 1)), format="%B %d, %Y")
# Deaths
plot(as.Date(colnames(deaths_perc)), rep(0, ncol(deaths_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(deaths_perc[idxs,], na.rm=T)), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(deaths_perc)), deaths_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(deaths_perc)), deaths_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Deaths", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
# Recovered
plot(as.Date(colnames(recovered_perc)), rep(0, ncol(recovered_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(recovered_perc[idxs,], na.rm=T)), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(recovered_perc)), recovered_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(recovered_perc)), recovered_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Recovered", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_cases_deaths_recovered_top20_min100"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
string_date <- format(as.Date(tail(colnames(deaths_perc), 1)), format="%B %d, %Y")
# Plot percentage of cases resulting in deaths
plot(as.Date(colnames(confirmed_deaths_perc)), rep(0, ncol(confirmed_deaths_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(confirmed_deaths_perc, na.rm=T)), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(confirmed_deaths_perc)), confirmed_deaths_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  idx <- rownames(confirmed_perc)[idxs[i]]
  lines(as.Date(colnames(confirmed_deaths_perc)), confirmed_deaths_perc[idx,], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(confirmed_deaths_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of confirmed cases", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Deaths", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
# Plot percentage of cases resulting in recovery
plot(as.Date(colnames(confirmed_recovered_perc)), rep(0, ncol(confirmed_recovered_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(confirmed_recovered_perc, na.rm=T)), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(confirmed_recovered_perc)), confirmed_recovered_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  idx <- rownames(confirmed_perc)[idxs[i]]
  lines(as.Date(colnames(confirmed_recovered_perc)), confirmed_recovered_perc[idx,], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(confirmed_recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of confirmed cases", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Recovered", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}


############################################################################################################################
# Plot percentage weekly change in confirmed cases, for each of (a new set of) top 20 countries
# This code will currently not work due to several missing variables -- but there's no point in fixing it
############################################################################################################################
fn <- "percentage_weekly_change_cases_confirmed_top20"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,3,1,4), bg="white", cex=2)
# Confirmed cases, percent change per week
confirmed_all_min100 <- confirmed_all[, -c(1:4)]
confirmed_all_min100[confirmed_all_min100 < 100] <- NA # a minimum of 10 cases per region/country
n <- ncol(confirmed_all_min100);
confirmed_all_perc_diff <- (t(apply(confirmed_all_min100, 1, diff, lag=7)) / confirmed_all_min100[,-c((n-6):n)]) * 100
colnames(confirmed_all_perc_diff) <- colnames(confirmed_all_min100[,-c(1:7)])
rownames(confirmed_all_perc_diff) <- gsub("^ - ", "", paste(confirmed_all$Province.State, confirmed_all$Country.Region, sep=" - "))
#idxs <- head(order(apply(confirmed_all_perc_diff, 1, median, na.rm=T), decreasing=TRUE), 9) # median weekly change
idxs <- head(order(confirmed_all_perc_diff[,ncol(confirmed_all_perc_diff)], decreasing=TRUE), 9) # current weekly change
cols <- brewer.pal(9, "Set1")
xlim <- as.Date(c("2020-03-01", tail(colnames(confirmed), 1)))
plot(as.Date(colnames(confirmed_all_perc_diff)), rep(1, ncol(confirmed_all_perc_diff)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", main=string_date,
     xlim=xlim, ylim=range(confirmed_all_perc_diff[idxs,], na.rm=T))
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed_all_perc_diff)), confirmed_all_perc_diff[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(confirmed_all_perc_diff)), tcl=-0.25)
axis(4, las=2)
mtext("weekly % change in confirmed cases", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", rownames(confirmed_all_perc_diff)[idxs], lwd=5, 
       col=cols, inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

# Plot percentage daily change in confirmed cases, for each of (a new set of) top 9 countries
fn <- "percentage_daily_change_cases_confirmed_top20"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,3,1,4), bg="white", cex=2)
# Confirmed cases, percent change per day
confirmed_all_min100 <- confirmed_all[, -c(1:4)]
confirmed_all_min100[confirmed_all_min100 < 100] <- NA # a minimum of 10 cases per region/country
n <- ncol(confirmed_all_min100);
confirmed_all_perc_diff <- (t(apply(confirmed_all_min100, 1, diff, lag=1)) / confirmed_all_min100[,-n]) * 100
colnames(confirmed_all_perc_diff) <- colnames(confirmed_all_min100[,-c(1:1)])
rownames(confirmed_all_perc_diff) <- gsub("^ - ", "", paste(confirmed_all$Province.State, confirmed_all$Country.Region, sep=" - "))
#idxs <- head(order(apply(confirmed_all_perc_diff, 1, median, na.rm=T), decreasing=TRUE), 9) # median daily change
idxs <- head(order(confirmed_all_perc_diff[,ncol(confirmed_all_perc_diff)], decreasing=TRUE), 9) # current daily change
cols <- brewer.pal(9, "Set1")
xlim <- as.Date(c("2020-03-01", tail(colnames(confirmed), 1)))
plot(as.Date(colnames(confirmed_all_perc_diff)), rep(1, ncol(confirmed_all_perc_diff)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", main=string_date,
     xlim=xlim, ylim=range(confirmed_all_perc_diff[idxs,], na.rm=T))
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed_all_perc_diff)), confirmed_all_perc_diff[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(confirmed_all_perc_diff)), tcl=-0.25)
axis(4, las=2)
mtext("daily % change in confirmed cases", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", rownames(confirmed_all_perc_diff)[idxs], lwd=5, 
       col=cols, inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

############################################################################################################################

# Other ideas:
# - plot exponent change (e.g. e-1) to predict going from exponential to sigmoidal
# - plot percentage of cases recovered: tipping point at 50%
# - plot weekly change in %
# - plot comparisons between countries: for similar shape/exponent, obtain shift in time


