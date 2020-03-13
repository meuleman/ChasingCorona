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




# Obtain "top-scoring" countries, in terms of percentage of population affected (min. 100 cases)
min_cases_100 <- which(apply(confirmed, 1, max, na.rm=T) > 100) # to accommodate NAs
min_pop_100 <- which(population > 100000)
idxs <- head(intersect(intersect(order(-apply(confirmed_perc, 1, max, na.rm=T)), min_cases_100), min_pop_100), 20)
#idxs <- intersect(intersect(order(-apply(confirmed_perc, 1, max, na.rm=T)), min_cases_100), min_pop_100)
cols <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6',
          '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3',
          '#808000', '#ffd8b1', '#000075', '#808080', '#000000')
#source: https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
nrange <- 10
offsets <- 49:0
coeff_mat <- matrix(NA, nrow=length(idxs), ncol=length(offsets), 
  dimnames=list(rownames(confirmed)[idxs], tail(colnames(confirmed), length(offsets))))
confint_arr <- array(NA, dim=c(length(idxs), length(offsets), 2),
  dimnames=list(rownames(confirmed)[idxs], tail(colnames(confirmed), length(offsets)), c("lo", "hi")))
#https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
for (i in 1:length(idxs)) {
  dat_prep <- as.numeric(confirmed[idxs[i],])
  dat_prep <- log10(dat_prep[which(dat_prep > 10)]) # data since there were 10 cases
  for (j in 1:length(offsets)) {
    if (length(dat_prep) < nrange+offsets[j]) {
      coeff_mat[i,j] <- NA
    } else {
      dat_to_fit <- head(tail(dat_prep, nrange+offsets[j]), nrange)
      fit <- lm(y~x, data.frame(x=1:length(dat_to_fit), y=dat_to_fit))
      coeff_mat[i,j] <- fit$coefficients[["x"]]
      confint_arr[i,j,] <- confint(fit)["x",]
      #lines(dat_to_fit, lwd=2, col=cols[i])
      #abline(fit, col=cols[i])
    }
  }
}
confint_lo <- (10^(confint_arr[,,1])-1)*100
confint_hi <- (10^(confint_arr[,,2])-1)*100
coeff_mat <- ((10^coeff_mat)-1)*100

fn <- "percentage_daily_change_10days_cases_confirmed_top20"
#plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
pdf(file=paste0(figdir, "/", fn, "_", id(), ".pdf"), width=14,height=8) # for alpha transparency
layout(matrix(1:2, ncol=2), width=c(7,3))
par(oma=c(2,2,1,0), bg="white", cex=2)
par(mar=c(0,0,0,4))
#xlim <- as.Date(c("2020-01-25", tail(colnames(confirmed), 1)))
xlim <- as.Date(c("2020-02-15", tail(colnames(confirmed), 1)))
#xlim <- as.Date(c("2020-03-01", tail(colnames(confirmed), 1)))
plot(as.Date(colnames(coeff_mat)), rep(0, ncol(coeff_mat)), type="n", yaxt="n", xaxs="i", 
     xlab="", ylab="", xlim=xlim, ylim=range(c(confint_lo, confint_hi), na.rm=T))
for (i in 1:length(idxs)) {
  polygon(x=c(as.Date(colnames(coeff_mat)), rev(as.Date(colnames(coeff_mat)))),
          y=c(confint_lo[i,], rev(confint_hi[i,])), border=FALSE, col=paste(cols[i], "33", sep=""))
}
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(coeff_mat)), coeff_mat[i,], col=cols[i], lwd=3)
}
axis(1, labels=F, at=as.Date(colnames(coeff_mat)), tcl=-0.25)
axis(4, las=2)
mtext("% daily increase", side=2, line=0.5, cex=2)
par(mar=c(0,0,0,0))
plot(0, type="n", axes=FALSE)
labs <- rownames(confirmed_perc)[idxs]
nums <- round(coeff_mat[,ncol(coeff_mat)])
vars <- round((confint_hi[,ncol(confint_hi)] - confint_lo[,ncol(confint_lo)])/2)
ord <- order(nums, decreasing=TRUE)
legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", labs[ord], lwd=5, cex=0.7, col=cols[ord], inset=c(0.01, 0.1), bty="n")
legend("topright", "(x,y)", paste(nums, ifelse(is.na(nums), "", paste("% (±", vars, "%)", sep="")), sep="")[ord], cex=0.7, inset=c(-0.3, 0.1), bty="n", adj=c(1,0.5))
par(new=T, xpd=T, oma=c(0,0,0,0))
legend("bottomright", "(x,y)", "@nameluem\nwww.meuleman.org", text.col="grey", bty="n", cex=0.75)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}


# Other ideas:
# - plot exponent change (e.g. e-1) to predict going from exponential to sigmoidal
# - plot percentage of cases recovered: tipping point at 50%
# - plot weekly change in %
# - plot comparisons between countries: for similar shape/exponent, obtain shift in time


