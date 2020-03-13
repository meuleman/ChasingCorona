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
### Test bed for studying daily/weekly change rate of cases

# Re-loading original data to undo some of the earlier "corrections" (see code_preprocess.R)
covid19_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
confirmed_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Confirmed.csv", sep="/"), sep=",", header=T, as.is=T)
deaths_all    <- read.delim(paste(covid19_dir, "time_series_19-covid-Deaths.csv",    sep="/"), sep=",", header=T, as.is=T)
recovered_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Recovered.csv", sep="/"), sep=",", header=T, as.is=T)
colnames(confirmed_all) <- colnames(deaths_all) <- colnames(recovered_all) <-
  c(colnames(confirmed_all)[1:4], as.character(as.Date(colnames(confirmed_all)[-c(1:4)], format="X%m.%d.%y")))


# Plot percentage weekly change in confirmed cases, for each of (a new set of) top 9 countries
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



# Other ideas:
# - plot exponent change (e.g. e-1) to predict going from exponential to sigmoidal
# - plot percentage of cases recovered: tipping point at 50%
# - plot weekly change in %
# - plot comparisons between countries: for similar shape/exponent, obtain shift in time
# - create heatmaps

#coeff_mat2=coeff_mat
#coeff_mat2[is.na(coeff_mat2)] = 0
#heatmap(as.matrix(coeff_mat2), Colv=NA, scale="row")


