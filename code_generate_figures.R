library(RColorBrewer)
#source("~/general.R")
#id <- function(x) "fixed"
#wideScreen()

############################################################################################################################
### Function for format-agnostic plotting -- always comes out looking the same!
plotfile <- function(filename="Rplot", type="png", width=7, height=7, device="bitmap", warn=TRUE) {
  format <- switch(type,
            pdf = "pdfwrite",
            png = "png16m",
            type);

  if (device == "bitmap") {
    bitmap(file=paste(filename, "_", id(), ".", type, sep=""),
           type=format, res=144, taa=ifelse(type=="png16m", 4, NA), width=width, height=height)
  } else {
    eval(parse(text=device))(file=paste(filename, "_", id(), ".", type, sep=""),
                             res=144, width=width, height=height)
  }

  if (warn) print("Do not forget to use 'dev.off()' afterwards!")
}

### Function for generating a initial + date string ("WM" hardcoded -- Wouter Meuleman)
id <- function() paste("WM", gsub("-", "", Sys.Date()), sep="")

figdir <- "PDF_figures";
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)

############################################################################################################################
### Load in population counts from the Worldbank (https://data.worldbank.org/indicator/SP.POP.TOTL)
pop_counts_file <- "WorldBank_data/API_SP.POP.TOTL_DS2_en_csv_v2_821007/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv";
pop_counts <- read.delim(pop_counts_file, skip=3, sep=",", header=T, as.is=T)

# Correct some labels for consistency between Worldbank and CSSE data
pop_counts[pop_counts$Country.Name=="Egypt, Arab Rep.","Country.Name"] <- "Egypt";
pop_counts[pop_counts$Country.Name=="Macao SAR, China","Country.Name"] <- "Macau";
pop_counts[pop_counts$Country.Name=="Hong Kong SAR, China","Country.Name"] <- "Hong Kong";
pop_counts[pop_counts$Country.Name=="Iran, Islamic Rep.","Country.Name"] <- "Iran";
pop_counts[pop_counts$Country.Name=="Russian Federation","Country.Name"] <- "Russia";
pop_counts[pop_counts$Country.Name=="Korea, Rep.","Country.Name"] <- "South Korea";
pop_counts[pop_counts$Country.Name=="China","Country.Name"] <- "Mainland China";
pop_counts[pop_counts$Country.Name=="United States","Country.Name"] <- "US";
pop_counts[pop_counts$Country.Name=="United Kingdom","Country.Name"] <- "UK";
pop_counts[pop_counts$Country.Name=="Slovak Republic","Country.Name"] <- "Slovakia";

# Simplify data to contain only latest population estimates
pop_counts_simple <- pop_counts[,c("Country.Name", "X2018")] #2019 is missing, so opting for 2018

############################################################################################################################
### Load in COVID-19 case data
covid19_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
covid19_confirmed <- read.delim(paste(covid19_dir, "time_series_19-covid-Confirmed.csv", sep="/"), sep=",", header=T, as.is=T)
covid19_deaths    <- read.delim(paste(covid19_dir, "time_series_19-covid-Deaths.csv",    sep="/"), sep=",", header=T, as.is=T)
covid19_recovered <- read.delim(paste(covid19_dir, "time_series_19-covid-Recovered.csv", sep="/"), sep=",", header=T, as.is=T)

# Need to pool Taiwan with Mainland China, as the Worldbank apparently does not recognize it.
covid19_confirmed[covid19_confirmed$Country.Region=="Taiwan","Country.Region"] <- "Mainland China";
covid19_deaths[covid19_deaths$Country.Region=="Taiwan","Country.Region"] <- "Mainland China";
covid19_recovered[covid19_recovered$Country.Region=="Taiwan","Country.Region"] <- "Mainland China";
# Same for Saint Barthelemy
covid19_confirmed[covid19_confirmed$Country.Region=="Saint Barthelemy","Country.Region"] <- "France"; 
covid19_deaths[covid19_deaths$Country.Region=="Saint Barthelemy","Country.Region"] <- "France"; 
covid19_recovered[covid19_recovered$Country.Region=="Saint Barthelemy","Country.Region"] <- "France"; 
# Same for Palestine
covid19_confirmed[covid19_confirmed$Country.Region=="Palestine","Country.Region"] <- "Israel"; 
covid19_deaths[covid19_deaths$Country.Region=="Palestine","Country.Region"] <- "Israel";
covid19_recovered[covid19_recovered$Country.Region=="Palestine","Country.Region"] <- "Israel"; 
# Same for Vatican City
covid19_confirmed[covid19_confirmed$Country.Region=="Vatican City","Country.Region"] <- "Italy";
covid19_deaths[covid19_deaths$Country.Region=="Vatican City","Country.Region"] <- "Italy";
covid19_recovered[covid19_recovered$Country.Region=="Vatican City","Country.Region"] <- "Italy";
# Same for Martinique
covid19_confirmed[covid19_confirmed$Country.Region=="Martinique","Country.Region"] <- "France";
covid19_deaths[covid19_deaths$Country.Region=="Martinique","Country.Region"] <- "France";
covid19_recovered[covid19_recovered$Country.Region=="Martinique","Country.Region"] <- "France";
# Same for French Guiana
covid19_confirmed[covid19_confirmed$Country.Region=="French Guiana","Country.Region"] <- "France";
covid19_deaths[covid19_deaths$Country.Region=="French Guiana","Country.Region"] <- "France";
covid19_recovered[covid19_recovered$Country.Region=="French Guiana","Country.Region"] <- "France";

# Aggregate by country, instead of region, since we only have country-level population data for now.
covid19_confirmed_simple <- aggregate(covid19_confirmed[,-c(1:4)], by=list(covid19_confirmed$Country.Region), FUN=sum)
covid19_deaths_simple <- aggregate(covid19_deaths[,-c(1:4)], by=list(covid19_deaths$Country.Region), FUN=sum)
covid19_recovered_simple <- aggregate(covid19_recovered[,-c(1:4)], by=list(covid19_recovered$Country.Region), FUN=sum)

# Match population data with COVID-19 data
match_res <- match(covid19_confirmed_simple[,1], pop_counts_simple$Country.Name)
covid19_confirmed_pop <- cbind(pop_counts_simple[match_res,], covid19_confirmed_simple)
covid19_deaths_pop <- cbind(pop_counts_simple[match_res,], covid19_deaths_simple)
covid19_recovered_pop <- cbind(pop_counts_simple[match_res,], covid19_recovered_simple)

## All but one country should have been matched -- the unmatched is the Diamond Princess cruise ship.
#covid19_confirmed_pop$Country.Name[which(covid19_confirmed_pop$Group.1 == "Others")] <- "Diamond Princess"
#covid19_deaths_pop$Country.Name[which(covid19_deaths_pop$Group.1 == "Others")] <- "Diamond Princess"
#covid19_recovered_pop$Country.Name[which(covid19_recovered_pop$Group.1 == "Others")] <- "Diamond Princess"
if (length(which(is.na(covid19_confirmed_pop$Country.Name))) > 1) stop("More than 1 unmatched: check it out")
covid19_confirmed_pop <- covid19_confirmed_pop[-which(is.na(covid19_confirmed_pop$Country.Name)),]
covid19_deaths_pop <- covid19_deaths_pop[-which(is.na(covid19_deaths_pop$Country.Name)),]
covid19_recovered_pop <- covid19_recovered_pop[-which(is.na(covid19_recovered_pop$Country.Name)),]

# Final simplification step
covid19_confirmed_pop <- covid19_confirmed_pop[,-3]
covid19_deaths_pop <- covid19_deaths_pop[,-3]
covid19_recovered_pop <- covid19_recovered_pop[,-3]

# Assign more meaningful column names
colnames(covid19_confirmed_pop) <- colnames(covid19_deaths_pop) <- colnames(covid19_recovered_pop) <- 
  c("country", "population", gsub("X", "", gsub("\\.", "/", colnames(covid19_confirmed_pop)[-c(1:2)])))

############################################################################################################################

### Percentage of population with confirmed cases / deaths / recoveries
covid19_confirmed_perc <- covid19_confirmed_pop[,-c(1:2)] / covid19_confirmed_pop$population * 100
covid19_deaths_perc <- covid19_deaths_pop[,-c(1:2)] / covid19_deaths_pop$population * 100
covid19_recovered_perc <- covid19_recovered_pop[,-c(1:2)] / covid19_recovered_pop$population * 100
rownames(covid19_confirmed_perc) <- rownames(covid19_deaths_perc) <- rownames(covid19_recovered_perc) <- 
  covid19_confirmed_pop$country;
colnames(covid19_confirmed_perc) <- colnames(covid19_deaths_perc) <- colnames(covid19_recovered_perc) <- 
  as.Date(colnames(covid19_recovered_perc), format="%m/%d/%y")

### Percentage of confirmed cases that have resulted in deaths / recoveries
covid19_confirmed_deaths_perc <- covid19_deaths_simple[,-1] / covid19_confirmed_simple[,-1] * 100
covid19_confirmed_recovered_perc <- covid19_recovered_simple[,-1] / covid19_confirmed_simple[,-1] * 100
rownames(covid19_confirmed_deaths_perc) <- rownames(covid19_confirmed_recovered_perc) <- covid19_deaths_simple[,1];
colnames(covid19_confirmed_deaths_perc) <- colnames(covid19_confirmed_recovered_perc) <- 
  as.Date(colnames(covid19_confirmed_pop)[-(1:2)], format="%m/%d/%y")
covid19_confirmed_deaths_perc[covid19_confirmed_simple[,-1] < 50] <- NA
covid19_confirmed_recovered_perc[covid19_confirmed_simple[,-1] < 50] <- NA

### Construct date string for showing in figures.
string_date <- format(as.Date(tail(colnames(covid19_confirmed_perc), 1)), format="%B %d, %Y")

### Averages across all countries
covid19_confirmed_perc_mean <- colSums(covid19_confirmed_pop[,-c(1:2)]) / sum(covid19_confirmed_pop[,2]) * 100
covid19_deaths_perc_mean <- colSums(covid19_deaths_pop[,-c(1:2)]) / sum(covid19_deaths_pop[,2]) * 100
covid19_recovered_perc_mean <- colSums(covid19_recovered_pop[,-c(1:2)]) / sum(covid19_recovered_pop[,2]) * 100

covid19_confirmed_deaths_perc_mean <- colSums(covid19_deaths_simple[,-1]) / colSums(covid19_confirmed_simple[,-1]) * 100
covid19_confirmed_recovered_perc_mean <- colSums(covid19_recovered_simple[,-1]) / colSums(covid19_confirmed_simple[,-1]) * 100

# Obtain "top-scoring" countries, in terms of percentage of population affected (min. 50)
min50 <- which(covid19_confirmed_pop[,ncol(covid19_confirmed_pop)] > 50)
#idxs <- head(order(-covid19_confirmed_perc[,ncol(covid19_confirmed_perc)]), 9)
idxs <- head(intersect(order(-covid19_confirmed_perc[,ncol(covid19_confirmed_perc)]), min50), 9)
cols <- brewer.pal(9, "Set1")

fn <- "percentage_population_confirmed_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,4,1,5), bg="white", cex=2)
# Confirmed cases
plot(as.Date(colnames(covid19_confirmed_perc)), rep(0, ncol(covid19_confirmed_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(covid19_confirmed_perc[idxs,])), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(covid19_confirmed_perc)), covid19_confirmed_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(covid19_confirmed_perc)), covid19_confirmed_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", c(rownames(covid19_confirmed_perc)[idxs], "World-wide"), lwd=5, 
       col=c(cols, "black"), inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

fn <- "percentage_population_deaths_recovered_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
# Deaths
plot(as.Date(colnames(covid19_deaths_perc)), rep(0, ncol(covid19_deaths_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(covid19_deaths_perc[idxs,])), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(covid19_deaths_perc)), covid19_deaths_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(covid19_deaths_perc)), covid19_deaths_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Deaths", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
# Recovered
plot(as.Date(colnames(covid19_recovered_perc)), rep(0, ncol(covid19_recovered_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(covid19_recovered_perc[idxs,])), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(covid19_recovered_perc)), covid19_recovered_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(covid19_recovered_perc)), covid19_recovered_perc[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of population", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Recovered", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

fn <- "percentage_cases_deaths_recovered_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=4)
par(mar=c(2,2,1,4), mfrow=c(1,2), cex=3, bg="white")
# Plot percentage of cases resulting in deaths
plot(as.Date(colnames(covid19_confirmed_deaths_perc)), rep(0, ncol(covid19_confirmed_deaths_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(covid19_confirmed_deaths_perc, na.rm=T)), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(covid19_confirmed_deaths_perc)), covid19_confirmed_deaths_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  idx <- rownames(covid19_confirmed_perc)[idxs[i]]
  lines(as.Date(colnames(covid19_confirmed_deaths_perc)), covid19_confirmed_deaths_perc[idx,], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_confirmed_deaths_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of confirmed cases", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Deaths", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
# Plot percentage of cases resulting in recovery
plot(as.Date(colnames(covid19_confirmed_recovered_perc)), rep(0, ncol(covid19_confirmed_recovered_perc)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=c(0, max(covid19_confirmed_recovered_perc, na.rm=T)), 
     xlab="", ylab="", main=string_date)
lines(as.Date(colnames(covid19_confirmed_recovered_perc)), covid19_confirmed_recovered_perc_mean, col="black", lwd=5)
for (i in 1:length(idxs)) {
  idx <- rownames(covid19_confirmed_perc)[idxs[i]]
  lines(as.Date(colnames(covid19_confirmed_recovered_perc)), covid19_confirmed_recovered_perc[idx,], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_confirmed_recovered_perc)), tcl=-0.25)
axis(4, las=2)
mtext("% of confirmed cases", side=2, line=0.5, cex=3)
legend("topleft", "(x,y)", "Recovered", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

# Plot absolute number of confirmed cases for each of the top 9 countries
fn <- "absolute_numbers_top9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,3,1,4), bg="white", cex=2)
# Confirmed cases
covid19_confirmed_abs <- covid19_confirmed_pop[,-c(1:2)]
rownames(covid19_confirmed_abs) <- covid19_confirmed_pop$country;
colnames(covid19_confirmed_abs) <- as.Date(colnames(covid19_confirmed_abs), format="%m/%d/%y")
covid19_confirmed_abs[covid19_confirmed_abs==0] <- NA
plot(as.Date(colnames(covid19_confirmed_abs)), rep(1, ncol(covid19_confirmed_abs)), type="n", 
     log="y", yaxt="n", xaxs="i", yaxs="i", ylim=c(1, max(covid19_confirmed_abs[idxs,], na.rm=T)), 
     xlab="", ylab="", main=string_date)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(covid19_confirmed_abs)), covid19_confirmed_abs[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_confirmed_abs)), tcl=-0.25)
axis(4, las=2)
mtext("# of confirmed cases", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
#legend("topleft", "(x,y)", rownames(covid19_confirmed_abs)[idxs], lwd=5, 
#       col=cols, inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}

# Plot percentage weekly increase in confirmed cases, for each of (a new set of) top 9 countries
fn <- "percentage_weekly_change_cases_confirmed_newtop9_min50"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=14, height=8)
par(mar=c(2,3,1,4), bg="white", cex=2)
# Confirmed cases, percent change per day
covid19_confirmed <- read.delim(paste(covid19_dir, "time_series_19-covid-Confirmed.csv", sep="/"), sep=",", header=T, as.is=T)
covid19_confirmed_min50 <- covid19_confirmed[, -c(1:4)]
covid19_confirmed_min50[covid19_confirmed_min50 < 50] <- NA # a minimum of 10 cases per region/country
n <- ncol(covid19_confirmed_min50);
#i <- which(covid19_confirmed$Country.Region=="XXX")
#covid19_confirmed_perc_diff <- t(apply(covid19_confirmed_min50, 1, diff, lag=1)) / covid19_confirmed_min50[,-n] * 100
covid19_confirmed_perc_diff <- (t(apply(covid19_confirmed_min50, 1, diff, lag=7)) / covid19_confirmed_min50[,-c((n-6):n)]) * 100
rownames(covid19_confirmed_perc_diff) <- gsub("^ - ", "", paste(covid19_confirmed$Province.State, covid19_confirmed$Country.Region, sep=" - "))
colnames(covid19_confirmed_perc_diff) <- as.Date(gsub("X", "", gsub("\\.", "/", tail(colnames(covid19_confirmed_min50), -7))), format="%m/%d/%y")
idxs <- head(order(apply(covid19_confirmed_perc_diff, 1, median, na.rm=T), decreasing=TRUE), 9)
plot(as.Date(colnames(covid19_confirmed_perc_diff)), rep(1, ncol(covid19_confirmed_perc_diff)), 
     type="n", yaxt="n", xaxs="i", yaxs="i", ylim=range(covid19_confirmed_perc_diff[idxs,], na.rm=T), 
     xlab="", ylab="", main=string_date)
for (i in 1:length(idxs)) {
  lines(as.Date(colnames(covid19_confirmed_perc_diff)), covid19_confirmed_perc_diff[idxs[i],], col=cols[i], lwd=5)
}
axis(1, labels=F, at=as.Date(colnames(covid19_confirmed_perc_diff)), tcl=-0.25)
axis(4, las=2)
mtext("weekly % increase in confirmed cases", side=2, line=0.5, cex=2)
legend("topleft", "(x,y)", "Confirmed cases", inset=c(-0.05,0.005), bty="n", cex=1.25, text.font=4)
legend("topleft", "(x,y)", rownames(covid19_confirmed_perc_diff)[idxs], lwd=5, 
       col=cols, inset=c(0.01, 0.1), bty="n")
box()
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf ", fn, "_latest.png", sep=""))
}





