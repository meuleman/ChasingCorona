# ChasingCorona

### Visualizing COVID-19 coronavirus impact and spread

I was interested in seeing what percentage of the population is (confirmed to be) affected by the COVID-19 coronavirus,
and how these case numbers are growing over time for individual regions and countries.

Figures get updated daily as long as data remain available (h/t @JHUSystems).\
More information here: https://www.meuleman.org/project/chasingcorona/

**DISCLAIMER**: I am not a virologist or epidemiologist – I realize there are many caveats to interpreting these data.\
We do not nearly have enough data to make precise estimates, in particular because of the relatively small number of tests performed in most countries.

#### Percentage of population that are confirmed cases

This is essentially the Johns Hopkins University per-country [data](https://github.com/CSSEGISandData/COVID-19) for "confirmed cases", 
divided by the per-country 2018 population counts, as estimated and provided by the [World Bank](https://data.worldbank.org/indicator/SP.POP.TOTL).
Shown are the top 20 countries in terms of percent population confirmed to be COVID-19 cases.

![perc. of population confirmed](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_confirmed_top20_min100_fromMar01_latest.png "% of per-country population that are confirmed cases")

Data are shown on a linear and a logarithmic scale, because of the observed exponential growth of confirmed cases.

#### Daily growth of newly confirmed cases

The slope of the exponential curves tells us something about how fast the virus spreads.
These rates can be expressed in terms of daily percent new cases, based on the number of confirmed cases over the course of the 10 previous days:
![perc. daily confirmed new cases, examples](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_daily_change_10days_cases_confirmed_top20_fromFeb15_latest.png "% daily confirmed new cases")
Data shown for selected example countries from the top 20 list above.
Percentages on the right are 10-day average daily growth rates for the most recent date indicated and include 95% confidence interval ranges.

This growth over time can be further generalized across all countries and regions with sufficient data points, as such:
![perc. daily confirmed new cases, all](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_daily_change_10days_cases_confirmed_ALL_fromBeginning_latest.png "% daily confirmed new cases, all")

#### Percentage of population that has died of COVID-19

Shown are the top 20 countries in terms of percent of population died from COVID-19.

![perc. of population deaths](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_deaths_top20_min25_fromMar01_latest.png "% of per-country population that has died of COVID-19")

Data are shown on a linear and a logarithmic scale, because of the observed exponential growth of the number of deaths.


#### Additional figures

<!--
The following figures are even harder to interpret than the one shown above.
In particular, the percentage of confirmed cases resulting in death is heavily skewed upwards 
because of the lack of tests performed in the general population.\
That said, for the sake of completeness I include them here.

![perc. of population death or recovered](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_deaths_recovered_top20_min100_latest.png "% of per-country population that has died or recovered from COVID-19")

![perc. of cases death or recovered](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_cases_deaths_recovered_top20_min100_latest.png "% of per-country cases that has died or recovered from COVID-19")
-->

<!--
Population-normalized view for King County, Washington State (where I live) and the Netherlands (where my family lives):
![perc. of population confirmed, selection](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_confirmed_custom_latest.png "% of per-country population that are confirmed cases, selection")
-->

Number of confirmed cases (for the matching top20 %population confirmed cases countries shown above)
![number of confirmed cases per country](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/absolute_numbers_top20_min100_fromBeginning_latest.png "number of confirmed COVID-19 cases per country")

Number of deaths (for the matching top20 %population confirmed cases countries shown above)
![number of deaths per country](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/absolute_numbers_deaths_top20_min25_fromBeginning_latest.png "number of deaths due to COVID-19 per country")



