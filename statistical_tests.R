covid <- read.csv("covid19postvaxstatewidestats.csv")
missing <- is.na(covid)
byrow_missing <- apply(missing, MARGIN = 1, sum)
covid_clean <- covid[byrow_missing == 0, ]

##The code above reads in the csv, and removes all missing values. The next code chunk does the two proportion z test for vaccinated vs. unvaccinated cases.

get_zscore <- function(x1, x2, n1, n2) {
  x1 <- sum(x1)
  x2 <- sum(x2)
  
  n1 <- sum(n1)
  n2 <- sum(n2)
  
  p1 <- x1/n1
  p2 <- x2/n2
  
  p <- (x1+x2)/(n1+n2)
  
  SE <- (p*(1-p)*(1/n1+1/n2))^0.5
  
  z <- (p1-p2)/SE
  
  z
}

get_zscore(x1 = covid$vaccinated_cases, x2 = covid$unvaccinated_cases, n1 = covid$population_vaccinated, n2 = covid$population_unvaccinated)
get_zscore(x1 = covid$vaccinated_hosp, x2 = covid$unvaccinated_hosp, n1 = covid$population_vaccinated, n2 = covid$population_unvaccinated)
get_zscore(x1 = covid$vaccinated_deaths, x2 = covid$unvaccinated_deaths, n1 = covid$population_vaccinated, n2 = covid$population_unvaccinated)


##Using the standard normal distribution table, I found all three of the p-values to be less than 0.0001, effectively 
##showing that they are also below the significance level of alpha = 0.05. This means that all three of the null hypotheses 
##have been rejected, providing evidence supporting the alternative hypotheses that vaccinated individuals had lower rates of 
##COVID-19 cases, hospitalizations, and deaths.