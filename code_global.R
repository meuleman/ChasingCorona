############################################################################################################################
### Code for plotting COVID-19 case counts across countries
############################################################################################################################
library(RColorBrewer)
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
#min_pop_1000 <- which(population > 1000000)
#idxs <- head(order(-confirmed_perc[,ncol(confirmed_perc)]), 9)
#idxs <- head(intersect(intersect(order(-apply(confirmed_perc, 1, max, na.rm=T)), min_cases_100), min_pop_100), 9)
#cols <- brewer.pal(9, "Set1")
idxs <- head(intersect(intersect(order(-apply(confirmed_perc, 1, max, na.rm=T)), min_cases_100), min_pop_100), 20)
cols <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
          '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', 
          '#808000', '#ffd8b1', '#000075', '#808080', '#000000')
#source: https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/

options(scipen=2)

############################################################################################################################

# Plot absolute number of confirmed cases for each of the top 9 countries
fn <- "absolute_numbers_top9_min100"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
layout(matrix(1:2, ncol=2), width=c(7,3))
par(oma=c(2,2,1,0), bg="white", cex=2)
par(mar=c(0,0,0,4))
# Confirmed cases
dat_to_plot <- confirmed[idxs,]
dat_to_plot[dat_to_plot==0] <- NA
#xlim=as.Date(c("2020-02-15", tail(colnames(confirmed), 1)))
xlim=range(as.Date(colnames(confirmed)))
plot(as.Date(colnames(confirmed)), rep(1, ncol(confirmed)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", log="y", xlim=xlim,
     ylim=range(dat_to_plot[,as.Date(colnames(dat_to_plot)) %in% xlim], na.rm=T))
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed)), confirmed[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered)), tcl=-0.25)
axis(4, las=2)
mtext("# of confirmed cases", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
par(mar=c(0,0,0,0))
plot(0, type="n", axes=FALSE)
legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
labs <- rownames(confirmed)[idxs]
nums <- signif(confirmed[idxs,ncol(confirmed)], 2)
ord <- order(nums, decreasing=TRUE)
legend("topleft", "(x,y)", labs[ord], lwd=5, cex=0.7, col=cols[ord], inset=c(0.01, 0.1), bty="n")
legend("topright", "(x,y)", nums[ord], cex=0.7, inset=c(0.01, 0.1), bty="n")
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}


fn <- "percentage_population_confirmed_top9_min100"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
layout(matrix(1:2, ncol=2), width=c(7,3))
par(oma=c(2,2,1,0), bg="white", cex=2)
par(mar=c(0,0,0,4))
# Confirmed cases
dat_to_plot <- confirmed_perc[idxs,]
dat_to_plot[dat_to_plot==0] <- NA
xlim=as.Date(c("2020-02-15", tail(colnames(confirmed), 1)))
xlim=as.Date(c("2020-03-01", tail(colnames(confirmed), 1)))
plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", xlim=xlim,
     ylim=range(dat_to_plot[,as.Date(colnames(dat_to_plot)) %in% xlim], na.rm=T))
lines(as.Date(colnames(confirmed_perc)), confirmed_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
par(mar=c(0,0,0,0))
plot(0, type="n", axes=FALSE)
labs <- c(rownames(confirmed_perc)[idxs], "World-wide")
nums <- signif(c(confirmed_perc[idxs,ncol(confirmed_perc)], tail(confirmed_perc_mean, 1)), 2)
legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", labs, lwd=5, cex=0.7, col=c(cols, "black"), inset=c(0.01, 0.1), bty="n")
legend("topright", "(x,y)", paste(nums, "%", sep=""), cex=0.7, inset=c(0.01, 0.1), bty="n")
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_confirmed_top9_min100_log"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
layout(matrix(1:2, ncol=2), width=c(7,3))
par(oma=c(2,2,1,0), bg="white", cex=2)
par(mar=c(0,0,0,4))
# Confirmed cases
dat_to_plot <- confirmed_perc[idxs,]
dat_to_plot[dat_to_plot==0] <- NA
xlim=as.Date(c("2020-02-15", tail(colnames(confirmed), 1)))
xlim=as.Date(c("2020-03-01", tail(colnames(confirmed), 1)))
plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", xlab="", ylab="", log="y", xlim=xlim,
     ylim=range(dat_to_plot[,as.Date(colnames(dat_to_plot)) %in% xlim], na.rm=T))
lines(as.Date(colnames(confirmed_perc)), confirmed_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
par(mar=c(0,0,0,0))
plot(0, type="n", axes=FALSE)
labs <- c(rownames(confirmed_perc)[idxs], "World-wide")
nums <- signif(c(confirmed_perc[idxs,ncol(confirmed_perc)], tail(confirmed_perc_mean, 1)), 2)
legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", labs, lwd=5, cex=0.7, col=c(cols, "black"), inset=c(0.01, 0.1), bty="n")
legend("topright", "(x,y)", paste(nums, "%", sep=""), cex=0.7, inset=c(0.01, 0.1), bty="n")
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_deaths_recovered_top9_min100"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
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

fn <- "percentage_cases_deaths_recovered_top9_min100"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
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

