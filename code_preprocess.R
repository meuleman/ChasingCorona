library(RColorBrewer)

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

############################################################################################################################
### Load in population counts from the Worldbank (https://data.worldbank.org/indicator/SP.POP.TOTL)
pop_counts_file <- "WorldBank_data/API_SP.POP.TOTL_DS2_en_csv_v2_821007/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv";
pop_counts <- read.delim(pop_counts_file, skip=3, sep=",", header=T, as.is=T)

# Simplify data to contain only latest population estimates
population <- pop_counts[,c("Country.Name", "X2018")] #2019 is missing, so opting for 2018
colnames(population) <- c("country_pop", "population")

# Correct some labels for consistency between Worldbank and CSSE data
population[population$country_pop=="Egypt, Arab Rep.","country_pop"] <- "Egypt";
population[population$country_pop=="Macao SAR, China","country_pop"] <- "Macau";
population[population$country_pop=="Hong Kong SAR, China","country_pop"] <- "Hong Kong";
population[population$country_pop=="Iran, Islamic Rep.","country_pop"] <- "Iran";
population[population$country_pop=="Russian Federation","country_pop"] <- "Russia";
population[population$country_pop=="Korea, Rep.","country_pop"] <- "South Korea";
population[population$country_pop=="China","country_pop"] <- "Mainland China";
population[population$country_pop=="United States","country_pop"] <- "US";
population[population$country_pop=="United Kingdom","country_pop"] <- "UK";
population[population$country_pop=="Slovak Republic","country_pop"] <- "Slovakia";

############################################################################################################################
### Load in COVID-19 case data
covid19_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
confirmed_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Confirmed.csv", sep="/"), sep=",", header=T, as.is=T)
deaths_all    <- read.delim(paste(covid19_dir, "time_series_19-covid-Deaths.csv",    sep="/"), sep=",", header=T, as.is=T)
recovered_all <- read.delim(paste(covid19_dir, "time_series_19-covid-Recovered.csv", sep="/"), sep=",", header=T, as.is=T)
colnames(confirmed_all) <- colnames(deaths_all) <- colnames(recovered_all) <- 
  c(colnames(confirmed_all)[1:4], as.character(as.Date(colnames(confirmed_all)[-c(1:4)], format="X%m.%d.%y")))

# Need to pool Taiwan with Mainland China, as the Worldbank apparently does not recognize it.
confirmed_all[confirmed_all$Country.Region=="Taiwan","Country.Region"] <- "Mainland China";
deaths_all[deaths_all$Country.Region=="Taiwan","Country.Region"] <- "Mainland China";
recovered_all[recovered_all$Country.Region=="Taiwan","Country.Region"] <- "Mainland China";
# Same for Saint Barthelemy
confirmed_all[confirmed_all$Country.Region=="Saint Barthelemy","Country.Region"] <- "France"; 
deaths_all[deaths_all$Country.Region=="Saint Barthelemy","Country.Region"] <- "France"; 
recovered_all[recovered_all$Country.Region=="Saint Barthelemy","Country.Region"] <- "France"; 
# Same for Palestine
confirmed_all[confirmed_all$Country.Region=="Palestine","Country.Region"] <- "Israel"; 
deaths_all[deaths_all$Country.Region=="Palestine","Country.Region"] <- "Israel";
recovered_all[recovered_all$Country.Region=="Palestine","Country.Region"] <- "Israel"; 
# Same for Vatican City
confirmed_all[confirmed_all$Country.Region=="Vatican City","Country.Region"] <- "Italy";
deaths_all[deaths_all$Country.Region=="Vatican City","Country.Region"] <- "Italy";
recovered_all[recovered_all$Country.Region=="Vatican City","Country.Region"] <- "Italy";
# Same for Martinique
confirmed_all[confirmed_all$Country.Region=="Martinique","Country.Region"] <- "France";
deaths_all[deaths_all$Country.Region=="Martinique","Country.Region"] <- "France";
recovered_all[recovered_all$Country.Region=="Martinique","Country.Region"] <- "France";
# Same for French Guiana
confirmed_all[confirmed_all$Country.Region=="French Guiana","Country.Region"] <- "France";
deaths_all[deaths_all$Country.Region=="French Guiana","Country.Region"] <- "France";
recovered_all[recovered_all$Country.Region=="French Guiana","Country.Region"] <- "France";
# Double-registered Ireland?
confirmed_all[confirmed_all$Country.Region=="Republic of Ireland","Country.Region"] <- "Ireland";
deaths_all[deaths_all$Country.Region=="Republic of Ireland","Country.Region"] <- "Ireland";
recovered_all[recovered_all$Country.Region=="Republic of Ireland","Country.Region"] <- "Ireland";

# Aggregate by country, instead of region, since we only have country-level population data for now.
confirmed_simple <- aggregate(confirmed_all[,-c(1:4)], by=list(country=confirmed_all$Country.Region), FUN=sum)
deaths_simple <- aggregate(deaths_all[,-c(1:4)], by=list(country=deaths_all$Country.Region), FUN=sum)
recovered_simple <- aggregate(recovered_all[,-c(1:4)], by=list(country=recovered_all$Country.Region), FUN=sum)

# Match population data with COVID-19 data
match_res <- match(confirmed_simple$country, population$country_pop)
confirmed_pop <- cbind(population[match_res,], confirmed_simple)
deaths_pop <- cbind(population[match_res,], deaths_simple)
recovered_pop <- cbind(population[match_res,], recovered_simple)

# All but one country should have been matched -- the unmatched is the Diamond Princess cruise ship.
to_rm <- which(is.na(confirmed_pop$country_pop))
if (length(to_rm) > 1) {
  warning("More than 1 unmatched: check it out")
  print(confirmed_pop[to_rm,1:3])
}
if (length(to_rm) > 0) {
  confirmed_pop <- confirmed_pop[-to_rm,]
  deaths_pop <- deaths_pop[-to_rm,]
  recovered_pop <- recovered_pop[-to_rm,]
}

# Final simplification step
population <- confirmed_pop[,2]
confirmed <- confirmed_pop[,-c(1:3)]
deaths <- deaths_pop[,-c(1:3)]
recovered <- recovered_pop[,-c(1:3)]
rownames(confirmed) <- rownames(deaths) <- rownames(recovered) <- 
  names(population) <- confirmed_pop[,1]

### Construct date string for showing in figures.
string_date <- format(as.Date(tail(colnames(confirmed), 1)), format="%B %d, %Y")

############################################################################################################################

