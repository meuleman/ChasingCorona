library(RColorBrewer)
source("code_preprocess.R")

### Output directory
figdir <- "PDF_figures";
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)

############################################################################################################################

### Percentage of population with confirmed cases / deaths / recoveries
confirmed_perc <- sweep(confirmed, 1, population, FUN="/") * 100
deaths_perc <- sweep(deaths, 1, population, FUN="/") * 100
recovered_perc <- sweep(recovered, 1, population, FUN="/") * 100

### Percentage of confirmed cases that have resulted in deaths / recoveries
confirmed_deaths_perc <- deaths / confirmed * 100
confirmed_recovered_perc <- recovered / confirmed * 100
confirmed_deaths_perc[confirmed < 50] <- NA # minimum of 50 cases required
confirmed_recovered_perc[confirmed < 50] <- NA # minimum of 50 cases required

### Averages across all countries
confirmed_perc_mean <- colSums(confirmed) / sum(population) * 100
deaths_perc_mean <- colSums(deaths) / sum(population) * 100
recovered_perc_mean <- colSums(recovered) / sum(population) * 100

confirmed_deaths_perc_mean <- colSums(deaths) / colSums(confirmed) * 100
confirmed_recovered_perc_mean <- colSums(recovered) / colSums(confirmed) * 100

# Obtain "top-scoring" countries, in terms of percentage of population affected (min. 50 cases)
min50 <- which(confirmed[,ncol(confirmed)] > 50)
#idxs <- head(order(-confirmed_perc[,ncol(confirmed_perc)]), 9)
idxs <- head(intersect(order(-confirmed_perc[,ncol(confirmed_perc)]), min50), 9)
cols <- brewer.pal(9, "Set1")

fn <- "percentage_population_confirmed_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,4,1,5), bg="white", cex=2)
# Confirmed cases
plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(confirmed_perc[idxs,])), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(confirmed_perc)), confirmed_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", c(rownames(confirmed_perc)[idxs], "World-wide"), lwd=5, 
       col=c(cols, "black"), inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_deaths_recovered_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
# Deaths
plot(as.Date(colnames(deaths_perc)), rep(0, ncol(deaths_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(deaths_perc[idxs,])), 
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
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(recovered_perc[idxs,])), 
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
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

fn <- "percentage_cases_deaths_recovered_top9_min50"
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
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

# Plot absolute number of confirmed cases for each of the top 9 countries
fn <- "absolute_numbers_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,3,1,4), bg="white", cex=2)
# Confirmed cases
plot(as.Date(colnames(confirmed)), rep(1, ncol(confirmed)), type="n", 
     log="y", yaxt="n", xaxs="i", yaxs="i", ylim=c(1, max(confirmed[idxs,], na.rm=T)), 
     xlab="", ylab="", main=string_date)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(confirmed)), confirmed[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(confirmed)), tcl=-0.25)
axis(4, las=2)
mtext("# of confirmed cases", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
#legend("topleft", "(x,y)", rownames(confirmed)[idxs], lwd=5, 
#       col=cols, inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}


