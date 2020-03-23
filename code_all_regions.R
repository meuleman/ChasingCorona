############################################################################################################################
### Code for plotting a number of custom derivative views using as much regions as possible
############################################################################################################################
library(RColorBrewer)
library(gplots)
 
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

### Function to correct some region labels to prevent duplicates
fix_label <- function(lab_old, lab_new) {
  to_change <- which(rownames(confirmed) == lab_old)
  if (length(to_change) == 1) {
    rownames(confirmed)[to_change] <<- lab_new # global var, yikes!
    rownames(deaths)[to_change]    <<- lab_new
    rownames(recovered)[to_change] <<- lab_new
    print(paste("Changed region label", lab_old, "to", lab_new))
  } else {
    print(paste("Did NOT change region label", lab_old, "to", lab_new))
  }
}

############################################################################################################################

### Output directory
figdir <- "PDF_figures";
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)
dir.create("PNG_figures", recursive=TRUE, showWarnings=FALSE)

############################################################################################################################

# Re-loading original data to get the full dataset
covid19_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
confirmed_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Confirmed.csv", sep="/"), sep=",", header=T, as.is=T)
deaths_all    <- read.delim(paste(covid19_dir, "time_series_19-covid-Deaths.csv",    sep="/"), sep=",", header=T, as.is=T)
recovered_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Recovered.csv", sep="/"), sep=",", header=T, as.is=T)

metadata <- confirmed_all[,1:4];
confirmed <- confirmed_all[,-c(1:4)]
deaths <- deaths_all[,-c(1:4)]
recovered <- recovered_all[,-c(1:4)]

if(all(is.na(confirmed[,ncol(confirmed)]))) {
 confirmed <- confirmed[,-ncol(confirmed)]
 deaths <- deaths[,-ncol(deaths)]
 recovered <- recovered[,-ncol(recovered)]
}

metadata$Province.State <- ifelse(metadata$Province.State == metadata$Country.Region, "", metadata$Province.State)

colnames(confirmed) <- colnames(deaths) <- colnames(recovered) <- 
  as.character(as.Date(colnames(confirmed), format="X%m.%d.%y"))
rownames(confirmed) <- rownames(deaths) <- rownames(recovered) <- 
  gsub("^ - ", "", paste(metadata$Province.State, metadata$Country.Region, sep=" - "))

fix_label("Diamond Princess - Cruise Ship", "Cruise Ship")

to_rm <- which(rownames(confirmed) %in% c("Diamond Princess - US", "Grand Princess - US"))
if (!is.null(to_rm)) {
  confirmed <- confirmed[-to_rm,]
  deaths <- deaths[-to_rm,]
  recovered <- recovered[-to_rm,]
  metadata <- metadata[-to_rm,]
}

# Aggregate by country, and add as separate data points
confirmed_agg <- aggregate(confirmed, by=list(country=metadata$Country.Region), FUN=sum)
deaths_agg <- aggregate(deaths, by=list(country=metadata$Country.Region), FUN=sum)
recovered_agg <- aggregate(recovered, by=list(country=metadata$Country.Region), FUN=sum)

rownames(confirmed_agg) <- confirmed_agg[,1]; confirmed_agg <- confirmed_agg[,-1];
rownames(deaths_agg) <- deaths_agg[,1]; deaths_agg <- deaths_agg[,-1];
rownames(recovered_agg) <- recovered_agg[,1]; recovered_agg <- recovered_agg[,-1];

to_rm <- which(rownames(confirmed_agg) %in% rownames(confirmed))
confirmed <- rbind(confirmed, confirmed_agg[-to_rm,])
deaths <- rbind(deaths, deaths_agg[-to_rm,])
recovered <- rbind(recovered, recovered_agg[-to_rm,])

### Attempt to parse US state data to be able to include these as well
state_mapping <- c("Alabama"="AL", "Alaska"="AK", "Arizona"="AZ", "Arkansas"="AR", "California"="CA",
  "Colorado"="CO", "Connecticut"="CT", "Delaware"="DE", "District of Columbia"="DC", "Florida"="FL",
  "Georgia"="GA", "Hawaii"="HI", "Idaho"="ID", "Illinois"="IL", "Indiana"="IN", "Iowa"="IA",
  "Kansas"="KS", "Kentucky"="KY", "Louisiana"="LA", "Maine"="ME", "Montana"="MT", "Nebraska"="NE",
  "Nevada"="NV", "New Hampshire"="NH", "New Jersey"="NJ", "New Mexico"="NM", "New York"="NY",
  "North Carolina"="NC", "North Dakota"="ND", "Ohio"="OH", "Oklahoma"="OK", "Oregon"="OR",
  "Maryland"="MD", "Massachusetts"="MA", "Michigan"="MI", "Minnesota"="MN", "Mississippi"="MS",
  "Missouri"="MO", "Pennsylvania"="PA", "Rhode Island"="RI", "South Carolina"="SC",
  "South Dakota"="SD", "Tennessee"="TN", "Texas"="TX", "Utah"="UT", "Vermont"="VT", "Virginia"="VA",
  "Washington"="WA", "West Virginia"="WV", "Wisconsin"="WI", "Wyoming"="WY")

for (state in names(state_mapping)) {
  #print(state)
  state_lab <- paste(state, "- US")
  if (nrow(confirmed[state_lab,]) == 1) {
    state_counties <- grep(paste(",", state_mapping[state], "- US$"), rownames(confirmed))
    if (length(state_counties) > 0) {
      if (all(pmin(confirmed[state_lab,], colSums(confirmed[state_counties,], na.rm=T)) == 0)) {
        confirmed[paste(state, "- US"),] <- confirmed[state_lab,] + colSums(confirmed[state_counties,], na.rm=T)
        deaths[paste(state, "- US"),] <- deaths[state_lab,] + colSums(deaths[state_counties,], na.rm=T)
        recovered[paste(state, "- US"),] <- recovered[state_lab,] + colSums(recovered[state_counties,], na.rm=T)
      } else {
        warning(paste("Potential conflicting state-wide vs. county-wide data for", state, "state"))
      }
    } else {
      print(paste("No county data found for", state, "state"))
    }
  } else {
    print(paste("No data found for", state, "state"))
  }
}

idxs <- which(rowSums(confirmed > 10, na.rm=F) > 10) # Select only regions that have at least 10 days with more than 10 cases
# na.rm=F makes sure these are also regions where we have at least some data for ALL days (incl. current day).

############################################################################################################################

############################################################################################################################
# Percentage daily increase in new confirmed cases.
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

# remove entries with only non-positive values
to_rm <- which(rowSums(coeff_mat > 0, na.rm=T) == 0)
if (length(to_rm) > 0) {
  idxs <- idxs[-to_rm]
  coeff_mat <- coeff_mat[-to_rm,]
  confint_arr <- confint_arr[-to_rm,,]
} 

# convert to percentages
confint_lo <- (10^(confint_arr[,,1])-1)*100
confint_hi <- (10^(confint_arr[,,2])-1)*100
coeff_mat <- ((10^coeff_mat)-1)*100

coeff_mat_noNA <- as.matrix(coeff_mat)
coeff_mat_noNA[is.na(coeff_mat_noNA)] <- 0
coeff_mat_noNA <- coeff_mat_noNA[,colSums(coeff_mat_noNA) > 0]

ord <- order(apply(coeff_mat_noNA, 1, which.max), -apply(coeff_mat_noNA, 1, max), decreasing=TRUE)

fn <- "percentage_daily_change_10days_cases_confirmed_ALL_fromBeginning"
plotfile(paste(figdir, fn, sep="/"), type="pdf", width=16, height=10)
layout(matrix(1:2, ncol=2), widths=c(10,2))
par(mar=c(3,2,2,4), xpd=T, bg="white")
image(x=1:ncol(coeff_mat_noNA), y=1:nrow(coeff_mat_noNA), z=t(coeff_mat_noNA[ord,]), axes=FALSE, xlab="", ylab="",
      breaks=c(-1, seq(0.5, max(coeff_mat_noNA), length.out=99), 100), col=c("grey97", colorpanel(99, "grey90", "#e2ae79")))
wmax <- apply(coeff_mat_noNA, 1, which.max)
points(wmax[ord], 1:nrow(coeff_mat_noNA), pch=16, cex=0.2)
axis(4, at=1:nrow(coeff_mat_noNA), label=rownames(coeff_mat_noNA)[ord], las=2, tick=FALSE, cex.axis=0.4, line=-0.8)
text(x=1:ncol(coeff_mat_noNA), y=0, label=as.Date(colnames(coeff_mat_noNA)), srt=35, adj=c(1,1), cex=0.5)
mtext("Percent daily growth of number of confirmed cases", side=3, line=0.1, adj=0)
mtext(paste("(estimated across", nrange, "day intervals, requiring 10+ cases per day)"), side=3, line=0.1, adj=1, cex=0.8, col="darkgrey")
legend("bottomright", "(x,y)", "@nameluem\nwww.meuleman.org", text.col="grey", bty="n", cex=0.75, inset=c(-0.14,-0.06))
par(mar=c(3,4,2,1), xpd=T, bg="white")
image(x=1:2, y=1:nrow(coeff_mat_noNA), z=t(cbind(apply(coeff_mat_noNA, 1, max), coeff_mat_noNA[,ncol(coeff_mat_noNA)])[ord,]), 
      axes=FALSE, xlab="", ylab="",
      #breaks=c(-1, seq(0,max(coeff_mat_noNA), length.out=99), 100), col=colorpanel(100, "darkgrey", "#e2ae79"))
      breaks=c(-1, seq(0.5, max(coeff_mat_noNA), length.out=99), 100), col=c("grey97", colorpanel(99, "grey90", "#e2ae79")))
wmax <- apply(coeff_mat, 1, which.max)
max_dates <- format(as.Date(colnames(coeff_mat)[wmax][ord]), format="%B %d, %Y")
axis(2, at=1:nrow(coeff_mat_noNA), label=max_dates, las=2, tick=FALSE, cex.axis=0.4, line=-0.8)
axis(2, at=nrow(coeff_mat_noNA)+1.2, label="Date of highest growth", las=2, tick=FALSE, cex.axis=0.4, line=-0.8)
mtext("(95% confidence interval)", side=3, cex=0.4)
nums <- round(coeff_mat[cbind(1:nrow(coeff_mat),wmax)])
vars <- round((confint_hi[cbind(1:nrow(coeff_mat),wmax)] - confint_lo[cbind(1:nrow(coeff_mat),wmax)])/2)
numvars <- paste(nums, ifelse(is.na(nums), "", paste("% (±", vars, ")", sep="")), sep="")
text(x=1, y=1:nrow(coeff_mat_noNA), numvars[ord], cex=0.4, font=2)
nums <- round(coeff_mat_noNA[,ncol(coeff_mat_noNA)])
vars <- round((confint_hi[,ncol(confint_hi)] - confint_lo[,ncol(confint_lo)])/2)
numvars <- paste(nums, ifelse(is.na(nums), "", paste("% (±", vars, ")", sep="")), sep="")
text(x=2, y=1:nrow(coeff_mat_noNA), numvars[ord], cex=0.4, font=2)
string_date <- format(as.Date(tail(colnames(coeff_mat_noNA), 1)), format="%B %d")
text(x=1:2, y=0, label=c("Highest (·)", paste("Current\n(", string_date, ")", sep="")), srt=35, adj=c(1,1), cex=0.5, font=2)
dev.off()
if (file.exists(paste(figdir, "/", fn, "_", id(), ".pdf", sep=""))) {
  #system(paste("convert -density 300 ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
  system(paste("convert -density 300 -background white -alpha remove ", figdir, "/", fn, "_", id(), ".pdf PNG_figures/", fn, "_latest.png", sep=""))
}


#################

