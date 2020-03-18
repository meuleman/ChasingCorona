############################################################################################################################
### Code for plotting COVID-19 case counts across countries
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
# Absolute number of confirmed cases for each of the top 20 countries
############################################################################################################################
plot_absolute_numbers <- function(confirmed, idxs, cols, xlim=NULL) {
  layout(matrix(1:3, ncol=3), width=c(7,7,3))
  par(oma=c(2,2,1,0), bg="white", cex=2, mar=c(0,0,0,4))
  if (is.null(xlim)) xlim <- range(as.Date(colnames(confirmed)))
  xlim_range <- which(as.Date(colnames(confirmed)) %in% xlim)
  dat_to_plot <- confirmed[idxs,]
  dat_to_plot[dat_to_plot==0] <- NA
  # linear
  plot(as.Date(colnames(confirmed)), rep(1, ncol(confirmed)),
       type="n", yaxt="n", xaxs="i", xlab="", ylab="", log="", 
       xlim=xlim, ylim=range(dat_to_plot[,xlim_range[1]:xlim_range[2]], na.rm=T))
  for (i in 1:length(idxs)) {
    lines(as.Date(colnames(confirmed)), confirmed[idxs[i],], col=cols[i], lwd=5)
  }
  axis(1, labels=F, at=as.Date(colnames(confirmed)), tcl=-0.25)
  axis(4, las=2)
  mtext("# of confirmed cases", side=2, line=0.5, cex=2)
  #legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
  legend("topleft", "(x,y)", "linear scale", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.col="grey")
  mtext("a", side=3, line=0.1, adj=0, cex=2, font=2)
  # logarithmic
  plot(as.Date(colnames(confirmed)), rep(1, ncol(confirmed)),
       type="n", yaxt="n", xaxs="i", xlab="", ylab="", log="y",
       xlim=xlim, ylim=range(dat_to_plot[,xlim_range[1]:xlim_range[2]], na.rm=T))
  for (i in 1:length(idxs)) {
    lines(as.Date(colnames(confirmed)), confirmed[idxs[i],], col=cols[i], lwd=5)
  }
  axis(1, labels=F, at=as.Date(colnames(confirmed)), tcl=-0.25)
  axis(4, las=2)
  #mtext("# of confirmed cases", side=2, line=0.5, cex=2)
  #legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
  legend("topleft", "(x,y)", "logarithmic scale", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.col="grey")
  mtext("b", side=3, line=0.1, adj=0, cex=2, font=2)

  par(mar=c(0,0,0,0))
  plot(0, type="n", axes=FALSE)
  string_date <- format(as.Date(tail(colnames(confirmed), 1)), format="%B %d, %Y")
  legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
  labs <- rownames(confirmed)[idxs]
  nums <- apply(confirmed[idxs,], 1, max, na.rm=T)
  ord <- order(nums, decreasing=TRUE)
  legend("topleft", "(x,y)", labs[ord], lwd=5, cex=0.7, col=cols[ord], inset=c(0.01, 0.1), bty="n")
  legend("topright", "(x,y)", nums[ord], cex=0.7, inset=c(0.01, 0.1), bty="n")
  par(new=T, xpd=T, oma=c(0,0,0,0))
  legend("bottomright", "(x,y)", "@nameluem\nwww.meuleman.org", text.col="grey", bty="n", cex=0.75)
}

fn <- "absolute_numbers_top20_min100_fromFeb15"
xlim <- as.Date(c("2020-02-15", tail(colnames(confirmed_perc), 1)))
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=22, height=8)
plot_absolute_numbers(confirmed, idxs, cols, xlim=xlim)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "absolute_numbers_top20_min100_fromMar01"
xlim <- as.Date(c("2020-03-01", tail(colnames(confirmed_perc), 1)))
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=22, height=8)
plot_absolute_numbers(confirmed, idxs, cols, xlim=xlim)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "absolute_numbers_top20_min100_fromBeginning"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=22, height=8)
plot_absolute_numbers(confirmed, idxs, cols)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

############################################################################################################################
# Percentage of per-country population that affected (confirmed cases), for each of the top 20 countries
############################################################################################################################
plot_population_percentages <- function(confirmed_perc, confirmed_perc_mean, idxs, cols, xlim=NULL) {
  layout(matrix(1:3, ncol=3), width=c(7,7,3))
  par(oma=c(2,2,1,0), bg="white", cex=2, mar=c(0,0,0,4))
  if (is.null(xlim)) xlim <- range(as.Date(colnames(confirmed_perc)))
  xlim_range <- which(as.Date(colnames(confirmed_perc)) %in% xlim)
  dat_to_plot <- confirmed_perc[idxs,]
  dat_to_plot[dat_to_plot==0] <- NA
  # linear scale
  plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
       type="n", yaxt="n", xaxs="i", xlab="", ylab="", log="", 
       xlim=xlim, ylim=range(dat_to_plot[,xlim_range[1]:xlim_range[2]], na.rm=T))
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc_mean, col="black", lwd=5)
  for (i in 1:length(idxs)) {
    lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
  }
  axis(1, labels=F, at=as.Date(colnames(confirmed_perc)), tcl=-0.25)
  axis(4, las=2)
  mtext("% of population", side=2, line=0.5, cex=2)
  #legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
  legend("topleft", "(x,y)", "linear scale", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.col="grey")
  mtext("a", side=3, line=0.1, adj=0, cex=2, font=2)
  # log scale
  plot(as.Date(colnames(confirmed_perc)), rep(0, ncol(confirmed_perc)), 
       type="n", yaxt="n", xaxs="i", xlab="", ylab="", log="y", 
       xlim=xlim, ylim=range(dat_to_plot[,xlim_range[1]:xlim_range[2]], na.rm=T))
  lines(as.Date(colnames(confirmed_perc)), confirmed_perc_mean, col="black", lwd=5)
  for (i in 1:length(idxs)) {
    lines(as.Date(colnames(confirmed_perc)), confirmed_perc[idxs[i],], col=cols[i], lwd=5)
  }
  axis(1, labels=F, at=as.Date(colnames(confirmed_perc)), tcl=-0.25)
  axis(4, las=2)
#  mtext("% of population", side=2, line=0.5, cex=2)
#  legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.font=4)
  legend("topleft", "(x,y)", "logarithmic scale", inset=c(-0.08,-0.02), bty="n", cex=1.25, text.col="grey")
  mtext("b", side=3, line=0.1, adj=0, cex=2, font=2)

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

fn <- "percentage_population_confirmed_top20_min100_fromFeb15"
xlim <- as.Date(c("2020-02-15", tail(colnames(confirmed_perc), 1)))
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=22, height=8)
plot_population_percentages(confirmed_perc, confirmed_perc_mean, idxs, cols, xlim=xlim)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_confirmed_top20_min100_fromMar01"
xlim <- as.Date(c("2020-03-01", tail(colnames(confirmed_perc), 1)))
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=22, height=8)
plot_population_percentages(confirmed_perc, confirmed_perc_mean, idxs, cols, xlim=xlim)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_confirmed_top20_min100_fromBeginning"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=22, height=8)
plot_population_percentages(confirmed_perc, confirmed_perc_mean, idxs, cols)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

############################################################################################################################
# Percentage daily increase in new confirmed cases, for each of the top 20 countries
# Estimated over 10 day intervals, no data shown for intervals with less than 10 cases at each day
# https://kenbenoit.net/assets/courses/ME104/logmodels2.pdf
# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
############################################################################################################################
### First compute the full matrix of coefficients, where possible
nrange <- 10 # length of estimation interval
offsets <- (ncol(confirmed):1)-1
coeff_mat <- matrix(NA, nrow=length(idxs), ncol=length(offsets), 
  dimnames=list(rownames(confirmed)[idxs], tail(colnames(confirmed), length(offsets))))
confint_arr <- array(NA, dim=c(length(idxs), length(offsets), 2),
  dimnames=list(rownames(confirmed)[idxs], tail(colnames(confirmed), length(offsets)), c("lo", "hi")))
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
# convert to percentages
confint_lo <- (10^(confint_arr[,,1])-1)*100
confint_hi <- (10^(confint_arr[,,2])-1)*100
coeff_mat <- ((10^coeff_mat)-1)*100

plot_percentage_daily_increase <- function(coeff_mat, confint_lo, confint_hi, cols, xlim=NULL) {
  layout(matrix(1:2, ncol=2), width=c(7,3))
  par(oma=c(2,2,1,0), bg="white", cex=2, mar=c(0,0,0,4))
  if (is.null(xlim)) xlim <- range(as.Date(colnames(coeff_mat)))
  xlim_range <- which(as.Date(colnames(coeff_mat)) %in% xlim)

  to_rm <- which(rowSums(is.na(coeff_mat[,xlim_range[1]:xlim_range[2]])) > 0)
  if (length(to_rm) == nrow(coeff_mat)) {
    return("No regions without missing data")
  } else if (length(to_rm) > 0) {
    coeff_mat <- coeff_mat[-to_rm,]
    confint_lo <- confint_lo[-to_rm,]
    confint_hi <- confint_hi[-to_rm,]
    cols <- cols[-to_rm]
  }

  plot(as.Date(colnames(coeff_mat)), rep(0, ncol(coeff_mat)), type="n", yaxt="n", xaxs="i", 
       xlab="", ylab="", xlim=xlim, ylim=range(c(confint_lo, confint_hi), na.rm=T))
  for (i in 1:nrow(coeff_mat)) { # Confidence Interval (CI) regions
    polygon(x=c(as.Date(colnames(coeff_mat)), rev(as.Date(colnames(coeff_mat)))),
            y=c(confint_lo[i,], rev(confint_hi[i,])), border=FALSE, col=paste(cols[i], "33", sep=""))
  }
  for (i in 1:nrow(coeff_mat)) { # plot lines well over shaded CI regions
    lines(as.Date(colnames(coeff_mat)), coeff_mat[i,], col=cols[i], lwd=3)
  }
  axis(1, labels=F, at=as.Date(colnames(coeff_mat)), tcl=-0.25)
  axis(4, las=2)
  mtext("% daily increase", side=2, line=0.5, cex=2)
  par(mar=c(0,0,0,0))
  plot(0, type="n", axes=FALSE)
  labs <- rownames(coeff_mat)
  nums <- round(coeff_mat[,ncol(coeff_mat)])
  vars <- round((confint_hi[,ncol(confint_hi)] - confint_lo[,ncol(confint_lo)])/2)
  ord <- order(nums, decreasing=TRUE)
  string_date <- format(as.Date(tail(colnames(confirmed_perc), 1)), format="%B %d, %Y")
  legend("topleft", "(x,y)", string_date, inset=c(-0.12,-0.02), bty="n", cex=1.25, text.font=4)
  legend("topleft", "(x,y)", labs[ord], lwd=5, cex=0.7, col=cols[ord], inset=c(0.01, 0.1), bty="n")
  numvars <- paste(nums, ifelse(is.na(nums), "", paste("% (±", vars, "%)", sep="")), sep="")
  legend("topright", "(x,y)", numvars[ord], cex=0.7, inset=c(-0.3, 0.1), bty="n", adj=c(1,0.5))
  par(new=T, xpd=T, oma=c(0,0,0,0))
  legend("bottomright", "(x,y)", "@nameluem\nwww.meuleman.org", text.col="grey", bty="n", cex=0.75)
}

fn <- "percentage_daily_change_10days_cases_confirmed_top20_fromMar01"
xlim <- as.Date(c("2020-03-01", tail(colnames(confirmed), 1)))
#plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
pdf(file=paste0(figdir, "/", fn, "_", id(), ".pdf"), width=14,height=8) # for alpha transparency
plot_percentage_daily_increase(coeff_mat, confint_lo, confint_hi, cols, xlim=xlim)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

fn <- "percentage_daily_change_10days_cases_confirmed_top20_fromFeb15"
xlim <- as.Date(c("2020-02-15", tail(colnames(confirmed), 1)))
#plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
pdf(file=paste0(figdir, "/", fn, "_", id(), ".pdf"), width=14,height=8) # for alpha transparency
plot_percentage_daily_increase(coeff_mat, confint_lo, confint_hi, cols, xlim=xlim)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 144 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}

############################################################################################################################
# Normalized numbers of current cases per country
############################################################################################################################

### Percentage of population with current cases
current <- confirmed - recovered - deaths
current_perc <- sweep(current, 1, population, FUN="/") * 100
idxs <- intersect(intersect(which(rowSums(current > 10) > 10), min_cases_100), min_pop_100)
#idxs <- which(rowSums(current > 10) > 10) # Select only regions/countries that have 10+ days with more than 10 current cases
current_mat <- as.matrix(current_perc[idxs,])
ord <- order(apply(current_mat, 1, which.max), -apply(current_mat, 1, max), decreasing=TRUE)

fn <- "percentage_population_current_cases_ALL_fromBeginning"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=13, height=8)
layout(matrix(1:2, ncol=2), widths=c(10,2))
par(mar=c(3,2,2,8), xpd=T, bg="white")
image(x=1:ncol(current_mat), y=1:nrow(current_mat), z=t(current_mat[ord,]), axes=FALSE, xlab="", ylab="",
      breaks=c(-1, seq(0, quantile(current_mat, prob=0.95), length.out=99), 100), 
      col=c("grey97", colorpanel(99, "grey90", "#e2ae79")))
wmax <- apply(current_mat, 1, which.max)
points(wmax[ord], 1:nrow(current_mat), pch=16, cex=0.2)
axis(4, at=1:nrow(current_mat), label=rownames(current_mat)[ord], las=2, tick=FALSE, cex.axis=0.6, line=-0.8)
text(x=1:ncol(current_mat), y=0, label=as.Date(colnames(current_mat)), srt=35, adj=c(1,1), cex=0.6)
mtext("Percentage of population reported as current COVID-19 cases", side=3, line=0.1, adj=0)
mtext("(current = all confirmed - recovered - deaths)", side=3, line=0.1, adj=1, cex=0.8, col="darkgrey")
legend("bottomright", "(x,y)", "@nameluem\nwww.meuleman.org", text.col="grey", bty="n", cex=0.75, inset=c(-0.17,-0.06))
par(mar=c(3,4,2,1), xpd=T, bg="white")
image(x=1:2, y=1:nrow(current_mat), z=t(cbind(apply(current_mat, 1, max), current_mat[,ncol(current_mat)])[ord,]), 
      axes=FALSE, xlab="", ylab="", breaks=c(-1, seq(0, quantile(current_mat, prob=0.95), length.out=99), 100), 
      col=c("grey97", colorpanel(99, "grey90", "#e2ae79")))
max_dates <- format(as.Date(colnames(current_mat)[wmax][ord]), format="%B %d, %Y")
axis(2, at=1:nrow(current_mat), label=max_dates, las=2, tick=FALSE, cex.axis=0.6, line=-0.8)
axis(2, at=nrow(current_mat)+1, label="Date of highest number", las=2, tick=FALSE, cex.axis=0.6, line=-0.8)
nums <- paste(signif(current_mat[cbind(1:nrow(current_mat),wmax)], 2), "%", sep="")
text(x=1, y=1:nrow(current_mat), nums[ord], cex=0.5, font=2)
nums <- paste(signif(current_mat[,ncol(current_mat)], 2), "%", sep="")
text(x=2, y=1:nrow(current_mat), nums[ord], cex=0.5, font=2)
string_date <- format(as.Date(tail(colnames(current_mat), 1)), format="%B %d")
text(x=1:2, y=0, label=c("Highest (·)", paste("Current\n(", string_date, ")", sep="")), srt=35, adj=c(1,1), cex=0.6, font=2)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  #system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
  system(paste("convert -density 300 -background white -alpha remove ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}



