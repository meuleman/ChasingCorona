# ChasingCorona

### Expressing COVID-19 infection cases as a percentage of the population

I was interested in seeing what percentage of the population is (confirmed to be) affected by the COVID-19 coronavirus,
so I downloaded data on confirmed cases and normalized with per-country population estimates.

Figures get updated daily as long as data remain available (h/t @JHUSystems).\
More information here: https://www.meuleman.org/project/chasingcorona/

**DISCLAIMER**: I am not a virologist or epidemiologist – I realize there are many caveats to interpreting these data.\
We do not nearly have enough data to make precise estimates, in particular because of the relatively small number of tests performed in most countries.

![perc. of population confirmed](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_confirmed_top20_min100_latest.png "% of per-country population that are confirmed cases")

Because the growth of COVID-19 happens exponentially, it often makes more sense to show these numbers on a logarithmic scale:

![perc. of population confirmed log](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_confirmed_top20_min100_log_latest.png "% of per-country population that are confirmed cases")

### Daily procentual increase in confirmed COVID-19 cases

The slope of the exponential curves tells us something about how fast the virus spreads.
Here we have expressed these rates in terms of daily percent new cases, based on the number of confirmed cases over the course of the 10 previous days:

![perc. daily confirmed new cases](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_daily_change_10days_cases_confirmed_top20_latest.png "% daily confirmed new cases")

### Additional figures

<!--
The following figures are even harder to interpret than the one shown above.
In particular, the percentage of confirmed cases resulting in death is heavily skewed upwards 
because of the lack of tests performed in the general population.\
That said, for the sake of completeness I include them here.

![perc. of population death or recovered](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_deaths_recovered_top20_min100_latest.png "% of per-country population that has died or recovered from COVID-19")

![perc. of cases death or recovered](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_cases_deaths_recovered_top20_min100_latest.png "% of per-country cases that has died or recovered from COVID-19")
-->

Population-normalized view for King County, Washington State (where I live) and the Netherlands (where my family lives):
![perc. of population confirmed, selection](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/percentage_population_confirmed_custom_latest.png "% of per-country population that are confirmed cases, selection")

Number of confirmed cases on a logarithmic scale:
![number of confirmed cases per country](https://raw.githubusercontent.com/meuleman/ChasingCorona/master/PNG_figures/absolute_numbers_top20_min100_log_latest.png "number of confirmed COVID-19 cases per country")



