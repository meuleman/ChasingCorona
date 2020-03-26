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

### Function to correct some labels for consistency between Worldbank and CSSE data
fix_pop_label <- function(lab_old, lab_new) {
  idx <- which(population$country_pop==lab_old)
  if (length(idx) == 1) {
    population[idx,"country_pop"] <<- lab_new; # global var, yikes!
    print(paste("Changed Worldbank country label", lab_old, "to", lab_new))
  } else {
    print(paste("Did NOT change Worldbank country label", lab_old, "to", lab_new))
  }
}

### Function to correct some CSSE country labels so that we can use them with the population data
# For instance, because the Worldbank doesn't recognize certain areas as countries (e.g. Taiwan)
fix_csse_label <- function(lab_old, lab_new) {
  idx <- which(confirmed_all$Country.Region==lab_old)
  if (length(idx) == 1) {
    confirmed_all[idx,"Country.Region"] <<- lab_new; # global var, yikes!
    deaths_all[idx,"Country.Region"] <<- lab_new; # global var, yikes!
    #recovered_all[idx,"Country.Region"] <<- lab_new; # global var, yikes!
    print(paste("Changed CSSE country label", lab_old, "to", lab_new))
  } else {
    print(paste("Did NOT change CSSE country label", lab_old, "to", lab_new))
  }
}

############################################################################################################################
### Load in population counts from the Worldbank (https://data.worldbank.org/indicator/SP.POP.TOTL)
pop_counts_file <- "WorldBank_data/API_SP.POP.TOTL_DS2_en_csv_v2_821007/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv";
pop_counts <- read.delim(pop_counts_file, skip=3, sep=",", header=T, as.is=T)

# Simplify data to contain only latest population estimates
population <- pop_counts[,c("Country.Name", "X2018")] #2019 is missing, so opting for 2018
colnames(population) <- c("country_pop", "population")
to_rm <- which(is.na(population$population))
if (length(to_rm) > 0) population <- population[-to_rm,]

fix_pop_label("Egypt, Arab Rep.", "Egypt")
fix_pop_label("Macao SAR, China", "Macau")
fix_pop_label("Hong Kong SAR, China", "Hong Kong")
fix_pop_label("Iran, Islamic Rep.", "Iran")
fix_pop_label("Korea, Rep.", "Korea, South")
fix_pop_label("Russian Federation", "Russia")
#fix_pop_label("Moldova", "Republic of Moldova")
#fix_pop_label("China", "Mainland China")
fix_pop_label("United States", "US")
#fix_pop_label("United Kingdom", "UK")
fix_pop_label("Slovak Republic", "Slovakia")
fix_pop_label("Brunei Darussalam", "Brunei")
fix_pop_label("Czech Republic", "Czechia")
fix_pop_label("Venezuela, RB", "Venezuela")
fix_pop_label("St. Vincent and the Grenadines", "Saint Vincent and the Grenadines")
#fix_pop_label("Bahamas, The", "The Bahamas")
#fix_pop_label("Gambia, The", "The Gambia")
fix_pop_label("St. Lucia", "Saint Lucia")
fix_pop_label("Kyrgyz Republic", "Kyrgyzstan")

# Merge some labels
fix_pop_label("Congo, Rep.", "Congo")
fix_pop_label("Congo, Dem. Rep.", "Congo")

population <- aggregate(population$population, by=list(country_pop=population$country_pop), FUN=sum)
colnames(population) <- c("country_pop", "population")

############################################################################################################################
### Load in COVID-19 case data
covid19_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series/"
confirmed_all <- read.delim(paste(covid19_dir, "time_series_covid19_confirmed_global.csv", sep="/"), sep=",", header=T, as.is=T)
deaths_all    <- read.delim(paste(covid19_dir, "time_series_covid19_deaths_global.csv",    sep="/"), sep=",", header=T, as.is=T)
#recovered_all <- read.delim(paste(covid19_dir, "time_series_covid19_recovered_global.csv",    sep="/"), sep=",", header=T, as.is=T)
colnames(confirmed_all) <- colnames(deaths_all) <- #colnames(recovered_all) <- 
  c(colnames(confirmed_all)[1:4], as.character(as.Date(colnames(confirmed_all)[-c(1:4)], format="X%m.%d.%y")))

fix_csse_label("Taipei and environs", "China")
fix_csse_label("Macao SAR", "China")
fix_csse_label("Taiwan*", "China")
fix_csse_label("occupied Palestinian territory", "Israel")
fix_csse_label("Holy See", "Italy")
fix_csse_label("Saint Barthelemy", "France")
fix_csse_label("Saint Martin", "France")
fix_csse_label("Martinique", "France")
fix_csse_label("French Guiana", "France")
fix_csse_label("Reunion", "France")
fix_csse_label("Guadeloupe", "France")
fix_csse_label("Mayotte", "France")
fix_csse_label("Guernsey", "United Kingdom")
fix_csse_label("Jersey", "United Kingdom")
fix_csse_label("Iran (Islamic Republic of)", "Iran")
fix_csse_label("Viet Nam", "Vietnam")
fix_csse_label("Hong Kong SAR", "Hong Kong")
fix_csse_label("Congo (Kinshasa)", "Congo")
fix_csse_label("Congo (Brazzaville)", "Congo")
fix_csse_label("Republic of the Congo", "Congo")

# Aggregate by country, instead of region, since we only have country-level population data for now.
confirmed_simple <- aggregate(confirmed_all[,-c(1:4)], by=list(country=confirmed_all$Country.Region), FUN=sum)
deaths_simple    <- aggregate(deaths_all[,-c(1:4)], by=list(country=deaths_all$Country.Region), FUN=sum)
#recovered_simple    <- aggregate(recovered_all[,-c(1:4)], by=list(country=recovered_all$Country.Region), FUN=sum)

# Match population data with COVID-19 data
match_res     <- match(confirmed_simple$country, population$country_pop)
confirmed_pop <- cbind(population[match_res,], confirmed_simple)
deaths_pop    <- cbind(population[match_res,], deaths_simple)
#recovered_pop    <- cbind(population[match_res,], recovered_simple)

# All but one country should have been matched -- the unmatched is the Diamond Princess cruise ship.
to_rm <- which(is.na(confirmed_pop$country_pop))
if (length(to_rm) > 1) {
  warning("More than 1 unmatched: check it out")
  print(confirmed_pop[to_rm,1:3])
}
if (length(to_rm) > 0) {
  confirmed_pop <- confirmed_pop[-to_rm,]
  deaths_pop <- deaths_pop[-to_rm,]
  #recovered_pop <- recovered_pop[-to_rm,]
}

# Final simplification step
population <- confirmed_pop[,2]
confirmed <- confirmed_pop[,-c(1:3)]
deaths <- deaths_pop[,-c(1:3)]
#recovered <- recovered_pop[,-c(1:3)]
rownames(confirmed) <- rownames(deaths) <- #rownames(recovered) <-
  names(population) <- confirmed_pop[,1]

if(all(is.na(confirmed[,ncol(confirmed)]))) {
 confirmed <- confirmed[,-ncol(confirmed)]
 deaths <- deaths[,-ncol(deaths)]
 #recovered <- recovered[,-ncol(recovered)]
}

idx_undef <- which(is.na(confirmed[,ncol(confirmed)]))
if (length(idx_undef) > 0) {
  warning(paste(length(idx_undef), "regions with undefined amount of cases on last day"))
}

idx_zero <- which(confirmed[,ncol(confirmed)] == 0)
if (length(idx_zero) > 0) {
  warning(paste(length(idx_zero), "regions with zero cases on last day"))
}

### Construct date string for showing in figures.
#string_date <- format(as.Date(tail(colnames(confirmed), 1)), format="%B %d, %Y")

############################################################################################################################


