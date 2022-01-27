# Before running this code for first time, if you don't already have a census API key, you ...
# ... need to get a census API key from http://api.census.gov/data/key_signup.html and ...
# install it into .Renviron file in environment variable CENSUS_API_KEY ...
# using tidycensus::census_api_key('YourKeyFromCensusDotGovGoesHere123abcdef', install=TRUE)
#
# For more info see help file for tidycensus::get_api_key ...
# ... and for even more info see
# ... https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_api_handbook_2020_ch02.pdf
#
# The tidycensus::get_estimates() function looks for the key in CENSUS_API_KEY
#
# The censusapi::getCensus() function by default looks for the key in CENSUS_KEY , but this R file ...
# ... has it looking instead in CENSUS_API_KEY (same place where tidycensus packages expects it) ...
# ... so if you already have a census key just in CENSUS_KEY then please also put it in CENSUS_API_KEY

library(magrittr)
library(data.table)
library(ggplot2)
library(tidycensus) # for get_estimates
library(censusapi) # for getCensus
library(RSocrata)
library(ggeffects)
library(scales)

# time window for covid deaths is from 'beginning of time' (really beginning of pandemic) through 2021feb, when age-based prioritization was active:
start.date <- -Inf # change to as.Date('2021-03-01') to analyze data from March 2021 forward
end.date <- as.Date('2021-02-28') # change to +Inf to analyze data to present

# naming conventions:
#   data.table objects end with .dt
#   melted data.table objects end with .m.dt

# Use US Census 1-year population estimates to find US-wide mean age for each age bin
pop.by.age1year.dt <- data.table(
  getCensus(
    name='timeseries/idb/1year', vars=c('FIPS', 'NAME', 'AGE', 'SEX', 'POP'),
    time=2019,
    key=Sys.getenv('CENSUS_API_KEY') # otherwise would look in CENSUS_KEY
  ))[FIPS=='US' & SEX==0]
pop.by.age1year.dt[ , `:=`(POP=as.numeric(POP), AGE=as.numeric(AGE)) ]
agebins.dt <- data.table(AGE=c(0, 5, 18, 30, 40, 50, 65, 75, 85)) # agebins.dt$AGE has closed lower bound of each age bin
agebins.dt[ , endAGE := c(tail(AGE, -1), Inf) ] # agebins.dt$endAGE has open upper bound of each age bin
agebins.dt <- agebins.dt[pop.by.age1year.dt, on='AGE', roll=+Inf][
  , .(
    startAGE=min(AGE), POP=sum(POP), meanAGE=sum(AGE*POP)/sum(POP)
  ),
  by='endAGE'
]
agebins.dt[endAGE == Inf, age.group := paste0('age.', startAGE, '.and.older')]
agebins.dt[endAGE < Inf, age.group := paste0('age.', startAGE, '.', endAGE-1)]

# Analysis of Just Age and plotting Fig 1
# see CDC documentation at https://dev.socrata.com/foundry/data.cdc.gov/9bhg-hcku

# Four more reads, two from CDC and two from Census Bureau:
#
# 
covid.by.age.dt <- data.table(read.socrata('https://data.cdc.gov/resource/9bhg-hcku.json'))[group=='By Month' & state=='United States' & sex=='All Sexes' & start.date < as.Date(end_date) & as.Date(end_date) <= end.date]
# Get vintage 2019 census data, because that's the latest available when code run:
pop.by.agegroup.us.dt <- data.table(get_estimates(geography='us', product='characteristics', breakdown='AGEGROUP', breakdown_labels=TRUE, year=2019))
# 
covid.by.age.race.eth.dt <- data.table(read.socrata('https://data.cdc.gov/resource/tpcp-uiv5.json'))[group=='By Month' & hhs_region=='United States' & start.date < as.Date(end_date) & as.Date(end_date) <= end.date]
# Get vintage 2019 census data, because that's the latest available when code run
pop.by.agegroup.race.eth.us.dt <- data.table(get_estimates(geography='us', product='characteristics', breakdown='AGEGROUP,RACE,HISP', breakdown_labels=TRUE, year=2019))

# Process the US Census data and bin age in a way to match up with CDC binning of age
pop.by.agegroup.us.cast.dt <- dcast(
  pop.by.agegroup.us.dt[ grepl('^Age|^[0-9]|^Under', AGEGROUP) & !grepl('^85', AGEGROUP)][ # have to filter out one of the two copies of 85+
    ,.(NAME, pop=value,
       agegroup=AGEGROUP %>% gsub('^Under ', 'under.', .) %>% gsub('Age |^', 'age.', .) %>% gsub(' to ', '.', .) %>% gsub(' years', '', .) %>% gsub(' and older', '.and.older', .) %>% gsub(' and over', '.and.older', .)
    )
  ],
  .~agegroup,
  value.var='pop'
)[ , .(
  age.0.4 = age.0.4,
  age.5.17 = age.5.13 + age.14.17,
  age.18.29 = age.18.24 + age.25.29,
  age.30.39 = age.30.34 + age.35.39,
  age.40.49 = age.40.44 + age.45.49,
  age.50.64 = age.50.54 + age.55.59 + age.60.64,
  age.65.74 = age.65.69 + age.70.74,
  age.75.84 = age.75.79 + age.80.84,
  age.85.and.older = age.85.and.older
  )]
sum(pop.by.agegroup.us.cast.dt) # sums to 328M and change (2019 US population)
pop.by.agegroup.us.m.dt <- melt(pop.by.agegroup.us.cast.dt, value.name='pop', variable.name = 'age.group') # generates harmless warning about id.vars and measure.vars both being NULL

# Process the CDC covid data according to the same age bins as we used to process the census data
covid.by.age.us.dt <- covid.by.age.dt[ ,
  .(covid_19_deaths=sum(as.numeric(covid_19_deaths))),
  by=age_group
]
covid.by.age.us.dt[age_group %in% c('0-17 years'), age.group := 'age.0.17']
covid.by.age.us.dt[age_group %in% c('Under 1 year', '1-4 years'), age.group := 'age.0.4']
covid.by.age.us.dt[age_group %in% c('18-29 years'), age.group := 'age.18.29']
covid.by.age.us.dt[age_group %in% c('30-39 years'), age.group := 'age.30.39']
covid.by.age.us.dt[age_group %in% c('40-49 years'), age.group := 'age.40.49']
covid.by.age.us.dt[age_group %in% c('50-64 years'), age.group := 'age.50.64']
covid.by.age.us.dt[age_group %in% c('65-74 years'), age.group := 'age.65.74']
covid.by.age.us.dt[age_group %in% c('75-84 years'), age.group := 'age.75.84']
covid.by.age.us.dt[age_group %in% c('85 years and over'), age.group := 'age.85.and.older']
covid.by.age.group.us.dt <- covid.by.age.us.dt[!is.na(age.group), .(covid_19_deaths = sum(covid_19_deaths)), by=age.group]
covid.by.age.group.us.dt <- rbindlist(list(
  data.table(
    age.group = 'age.5.17',
    covid_19_deaths = covid.by.age.us.dt[age.group=='age.0.17', covid_19_deaths] - covid.by.age.group.us.dt[age.group=='age.0.4', covid_19_deaths]
  ),
  covid.by.age.group.us.dt[age.group != 'age.0.17']
))

# Merge covid data with census data (putting a .dt file as the first argument in another .dt file's square brackets is the data.table way of doing a join)
covid.and.pop.by.age.group.us.dt <- covid.by.age.group.us.dt[pop.by.agegroup.us.m.dt, on='age.group'] # this is a join on age.group
covid.and.pop.by.age.group.us.per.100k.dt <- covid.and.pop.by.age.group.us.dt[ , .(covid_19_deaths_per_100k=1e5*covid_19_deaths/pop, number.of.covid.19.deaths=as.integer(covid_19_deaths), pop=as.integer(pop)), by='age.group']
covid.and.pop.by.age.group.us.per.100k.dt <- agebins.dt[ , .(age.group, meanAGE) ][covid.and.pop.by.age.group.us.per.100k.dt, on='age.group'] # this is a join on age.group

# Code run 2021sep16, at which time CDC data was updated 2021sep08 and had 538,627 covid-19 deaths through 2021feb28 (plus more after 2021feb28):
covid.and.pop.by.age.group.us.per.100k.dt[,sum(number.of.covid.19.deaths)]

# The fit that is presented in the paper is quasipoisson, but code is also presented for sensitivity checks (quasilogit and linear fits):
fit.quasipoisson <- glm(
  number.of.covid.19.deaths ~ offset(log(pop)) + I(meanAGE/10),
  family=quasipoisson,
  data=covid.and.pop.by.age.group.us.per.100k.dt
)
fit.quasilogit <- glm(
  cbind(number.of.covid.19.deaths, pop-number.of.covid.19.deaths) ~ I(meanAGE/10),
  family=quasibinomial,
  data=covid.and.pop.by.age.group.us.per.100k.dt
)
fit.linear <- lm(log10(covid_19_deaths_per_100k) ~ I(meanAGE/10), data=covid.and.pop.by.age.group.us.per.100k.dt)
summary(fit.quasipoisson)
exp(coef(fit.quasipoisson)) # risk ratio = 2.56-fold per decade
exp(coef(fit.quasilogit)) # odds ratio = 2.57-fold per decade
10^coef(fit.linear) # risk ratio = 2.84-fold per decade
# confidence intervals for the RRs and OR
exp(confint(fit.quasipoisson))
exp(confint(fit.quasilogit))
10^(confint(fit.linear))


fit.us.preds.quasipoisson <- as.data.table(ggemmeans(fit.quasipoisson, terms='meanAGE [0:100 by=10]', condition=c(pop=1e5)))

windows()
ggplot(
  covid.and.pop.by.age.group.us.per.100k.dt,
  aes(
    x=meanAGE,
    y=covid_19_deaths_per_100k)
  ) +
  xlab('Age (years)') +
  geom_ribbon(group=1, data=fit.us.preds.quasipoisson, aes(fill='fit', x=x, y=NULL, ymin=conf.low, ymax=conf.high), alpha=.4) +
  geom_line(group=1, size=1, data=fit.us.preds.quasipoisson, aes(color='fit', x=x, y=predicted)) +
  geom_point(size=4, aes(color='US CDC Data')) +
  scale_y_log10(minor_breaks=NULL) +
  annotation_logticks(sides='lr') +
  scale_fill_discrete(guide='none') +
  scale_x_continuous(breaks=agebins.dt[,startAGE], minor_breaks=NULL) +
  ylab('Covid-19 Deaths per 100k Population') +
  theme_gray(base_size=18) + theme(legend.position='none', legend.direction='vertical', legend.title=element_blank(), axis.ticks.y=element_blank())
ggsave('DDNJ-rev-Fig-1.png')
ggsave('DDNJ-rev-Fig-1.eps', device=cairo_ps)

###################

# Analysis of Age and Race-Ethnicity, plotting Figure 2

# use the four marginalized racial-ethnic groups of Bassett et al. (2020) in population calculations
pop.by.agegroup.raceeth4.us.dt <- copy(pop.by.agegroup.race.eth.us.dt)
pop.by.agegroup.raceeth4.us.dt[RACE=='Black alone' & HISP=='Non-Hispanic' , raceeth4 := 'Non-Hispanic Black']
pop.by.agegroup.raceeth4.us.dt[RACE=='All races' & HISP=='Hispanic', raceeth4 := 'Hispanic']
pop.by.agegroup.raceeth4.us.dt[RACE=='American Indian and Alaska Native alone' & HISP=='Non-Hispanic', raceeth4 := 'Non-Hispanic American Indian and Alaska Native']
pop.by.agegroup.raceeth4.us.dt[RACE=='Asian alone' & HISP=='Non-Hispanic', raceeth4 := 'Non-Hispanic Asian or Pacific Islander']
pop.by.agegroup.raceeth4.us.dt[RACE=='Native Hawaiian and Other Pacific Islander alone' & HISP=='Non-Hispanic', raceeth4 := 'Non-Hispanic Asian or Pacific Islander']
pop.by.agegroup.raceeth4.us.dt <- pop.by.agegroup.raceeth4.us.dt[!is.na(raceeth4) & AGEGROUP != 'Median age']
pop.by.agegroup.raceeth4.us.dt <- pop.by.agegroup.raceeth4.us.dt[ , .(value=sum(value)), by=.(AGEGROUP, raceeth4)] # Following Bassett et al., combine NH Asian with NH Pacific Islander

# use the same age groups in population table as are used in the CDC's covid dataset
pop.by.agegroup.raceeth4.us.cast.dt <- dcast(
  pop.by.agegroup.raceeth4.us.dt[ grepl('^Age|^[0-9]|^Under', AGEGROUP) & !grepl('^85', AGEGROUP)][ # have to filter out one of the two copies of 85+
    ,.(raceeth4, pop=value,
       agegroup=AGEGROUP %>% gsub('^Under ', 'under.', .) %>% gsub('Age |^', 'age.', .) %>% gsub(' to ', '.', .) %>% gsub(' years', '', .) %>% gsub(' and older', '.and.older', .) %>% gsub(' and over', '.and.older', .)
    )
  ],
  raceeth4~agegroup,
  value.var='pop'
)[ , .(raceeth4,
  age.0.4 = age.0.4,
  age.5.17 = age.5.13 + age.14.17,
  age.18.29 = age.18.24 + age.25.29,
  age.30.39 = age.30.34 + age.35.39,
  age.40.49 = age.40.44 + age.45.49,
  age.50.64 = age.50.54 + age.55.59 + age.60.64,
  age.65.74 = age.65.69 + age.70.74,
  age.75.84 = age.75.79 + age.80.84,
  age.85.and.older = age.85.and.older
  )]
pop.by.agegroup.raceeth4.us.m.dt <- melt(pop.by.agegroup.raceeth4.us.cast.dt, value.name='pop', variable.name='age.group') # generates harmless warning about id.vars and measure.vars both being NULL

# now use the same four margialized racial-ethnic groups of Bassett et al (2020) in processing the CDC data
covid.by.age.raceeth4.us.dt <- covid.by.age.race.eth.dt[
  hhs_region=='United States' & race_and_hispanic_origin %in% c('Non-Hispanic Black', 'Hispanic', 'Non-Hispanic American Indian or Alaska Native', 'Non-Hispanic Asian', 'Non-Hispanic Native Hawaiian or Other Pacific Islander'),
  .(race_and_hispanic_origin, covid_19_deaths),
  by=age_group
]
covid.by.age.raceeth4.us.dt[race_and_hispanic_origin=='Non-Hispanic Black', raceeth4 := 'Non-Hispanic Black']
covid.by.age.raceeth4.us.dt[race_and_hispanic_origin=='Hispanic', raceeth4 := 'Hispanic']
covid.by.age.raceeth4.us.dt[race_and_hispanic_origin=='Non-Hispanic American Indian or Alaska Native', raceeth4 := 'Non-Hispanic American Indian and Alaska Native']
covid.by.age.raceeth4.us.dt[race_and_hispanic_origin=='Non-Hispanic Asian', raceeth4 := 'Non-Hispanic Asian or Pacific Islander']
covid.by.age.raceeth4.us.dt[race_and_hispanic_origin=='Non-Hispanic Native Hawaiian or Other Pacific Islander', raceeth4 := 'Non-Hispanic Asian or Pacific Islander']
covid.by.age.raceeth4.us.dt <- covid.by.age.raceeth4.us.dt[ , .(covid_19_deaths=sum(as.numeric(covid_19_deaths))), by=.(age_group, raceeth4)]

covid.by.age.group.raceeth4.us.dt <- covid.by.age.raceeth4.us.dt[ , .(
  raceeth4,
  age.group = age_group %>% gsub('^', 'age.', .) %>% gsub('-', '.', .) %>% gsub(' years', '', .) %>% gsub(' and over', '.and.older', .),
  covid_19_deaths
) ]

# Merge covid data with census data
covid.and.pop.by.age.group.raceeth4.us.dt <- covid.by.age.group.raceeth4.us.dt[pop.by.agegroup.raceeth4.us.m.dt, on=c('raceeth4', 'age.group')] # this is a join on raceeth4 and age.group to merge the CDC and census data
covid.and.pop.by.age.group.raceeth4.us.per.100k.dt <- covid.and.pop.by.age.group.raceeth4.us.dt[ , .(covid_19_deaths_per_100k=1e5*covid_19_deaths/pop, number.of.covid.19.deaths=as.integer(covid_19_deaths), pop=as.integer(pop)), by=.(raceeth4, age.group)]
covid.and.pop.by.age.group.raceeth4.us.per.100k.dt <- agebins.dt[ , .(age.group, meanAGE) ][covid.and.pop.by.age.group.raceeth4.us.per.100k.dt, on='age.group'] # this is a join on age.group to bring meanAGE in

# combine covid-19 mortality by age group for the 4 marginalized racial-ethnic groups with covid-19 mortlity overall and calculate the ratio of covid-19 mortality rate for each combination of age group and racial-ethnic group
covid.and.pop.by.age.group.raceeth4.us.per.100k.vs.allraceeth.dt <- covid.and.pop.by.age.group.raceeth4.us.per.100k.dt[
  covid.and.pop.by.age.group.us.per.100k.dt[ , .(age.group, covid_19_deaths_per_100k.allracesandeth=covid_19_deaths_per_100k, number.of.covid.19.deaths.allracesandeth=number.of.covid.19.deaths, pop.allracesandeth=pop)],
  on='age.group'
][,.(raceeth4, age.group, meanAGE, covid_19_deaths_per_100k, covid_19_deaths_per_100k.allracesandeth, c19.death.elevation=covid_19_deaths_per_100k/covid_19_deaths_per_100k.allracesandeth, number.of.covid.19.deaths, number.of.covid.19.deaths.allracesandeth, pop, pop.allracesandeth)]

# model-free risk ratios and their CIs:
covid.and.pop.by.age.group.raceeth4.us.per.100k.vs.allraceeth.dt[ , `:=`(
  ln.rr.ci.half.width = # half-width of confidence interval for ln(risk ratio)
  qnorm(.975)*sqrt(
    ( (pop-number.of.covid.19.deaths)/number.of.covid.19.deaths / pop ) +
    ( (pop.allracesandeth-number.of.covid.19.deaths.allracesandeth)/number.of.covid.19.deaths.allracesandeth / pop.allracesandeth )
  )
)]
covid.and.pop.by.age.group.raceeth4.us.per.100k.vs.allraceeth.dt[ , `:=`(
  c19.death.elevation.lo=exp(log(c19.death.elevation)-ln.rr.ci.half.width), c19.death.elevation.hi=exp(log(c19.death.elevation)+ln.rr.ci.half.width)
)]
covid.and.pop.by.age.group.raceeth4.us.per.100k.vs.allraceeth.dt[c19.death.elevation==max(c19.death.elevation)] # pick out the joint combination of age.group and racial-ethnic group with the largest risk ratio relative to same age.group for overall population

raceeth5.levs <- c('All Races and Ethnicities', covid.and.pop.by.age.group.raceeth4.us.per.100k.vs.allraceeth.dt[,sort(unique(raceeth4))]) # raceeth5 includes "5th level" of overall (that actually includes more than just these four categories)
windows(10,12)
ggplot(covid.and.pop.by.age.group.raceeth4.us.per.100k.vs.allraceeth.dt, aes(meanAGE, covid_19_deaths_per_100k)) +
  facet_wrap(~paste(raceeth4, 'vs All Races and Ethnicities'), labeller=label_wrap_gen()) +
  geom_ribbon(group=1, data=fit.us.preds.quasipoisson[, cbind(.SD, raceeth5='All Races and Ethnicities')], aes(fill='fit', x=x, y=NULL, ymin=conf.low, ymax=conf.high), alpha=.4, show.legend=FALSE) +
  geom_line(group=1, size=1, data=fit.us.preds.quasipoisson[, cbind(.SD, raceeth5='All Races and Ethnicities')], aes(color='All Races and Ethnicities', shape='All Races and Ethnicities', x=x, y=predicted), show.legend=FALSE) +
  geom_point(size=3, aes(color='All Races and Ethnicities', shape='All Races and Ethnicities'), data=covid.and.pop.by.age.group.us.per.100k.dt) +
  geom_point(size=2, stroke=1.5, aes(color=raceeth4, shape=raceeth4)) +
  scale_color_discrete(name='group', labels=raceeth5.levs) +
  scale_shape_manual(name='group', labels=raceeth5.levs, values=c(16, 2:5)) +
  scale_y_log10(minor_breaks=NULL) +
  annotation_logticks(sides='lr') +
  scale_fill_discrete(guide='none') +
  scale_x_continuous(breaks=agebins.dt[,startAGE], minor_breaks=NULL) +
  ylab('Covid-19 Deaths per 100k Population') +
  xlab('Age (years)') +
  theme_gray(base_size=16) + theme(legend.position='bottom', legend.direction='vertical', legend.title=element_blank(), axis.ticks.y=element_blank())
ggsave('DDNJ-rev-Fig-2.png')
ggsave('DDNJ-rev-Fig-2.eps', device=cairo_ps)



# The fit that is presented in the paper is quasipoisson, but code is also presented for sensitivity checks (quasilogit and linear fits):
fit.raceeth5.quasipoisson <- glm(
  number.of.covid.19.deaths ~ offset(log(pop)) + I(meanAGE/10) + raceeth5,
  family=quasipoisson,
  data=rbindlist(list(
    covid.and.pop.by.age.group.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5='All Races and Ethnicities')],
    covid.and.pop.by.age.group.raceeth4.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5=raceeth4)]
  ))
)
fit.raceeth5.quasilogit <- glm(
  cbind(number.of.covid.19.deaths, pop-number.of.covid.19.deaths) ~ I(meanAGE/10) + raceeth5,
  family=quasibinomial,
  data=rbindlist(list(
    covid.and.pop.by.age.group.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5='All Races and Ethnicities')],
    covid.and.pop.by.age.group.raceeth4.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5=raceeth4)]
  ))
)
fit.raceeth5.linear <- lm(
  log10(number.of.covid.19.deaths) ~ I(meanAGE/10) + raceeth5,
  data=rbindlist(list(
    covid.and.pop.by.age.group.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5='All Races and Ethnicities')],
    covid.and.pop.by.age.group.raceeth4.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5=raceeth4)]
  ))
)
summary(fit.raceeth5.quasipoisson) # for t ratios etc. for Results section
exp(coef(fit.raceeth5.quasipoisson))
exp(confint(fit.raceeth5.quasipoisson))
exp(coef(fit.raceeth5.quasilogit))
exp(confint(fit.raceeth5.quasilogit))
10^coef(fit.raceeth5.linear)
10^confint(fit.raceeth5.linear)

################

summary.effects.quasipoisson.dt <- rbindlist(list(
  as.data.table(summary(fit.raceeth5.quasipoisson)$coefficients, keep.rownames = TRUE)[-(1:2) , .(group='Race & Ethnicity', label='Present Study', rn = rn %>% gsub('raceeth5', '', .), y=exp(Estimate), y.lo=exp(Estimate-qnorm(.975)*`Std. Error`), y.hi=exp(Estimate+qnorm(.975)*`Std. Error`))],
  data.table(
    group=c('Organ Transplant', 'Organ Transplant', 'Down Syndrome in Own Home, Family Home, or Shared Living', 'Intellectual or Developmental Disability in Group Home', 'Intellectual or Developmental Disability', 'Reduced Kidney Function (eGFR < 30)', 'Intellectual or Developmental Disability in Own Home, Family Home, or Shared Living', 'Intellectual or Developmental Disability in Group Home', 'Intellectual or Developmental Disability in ICF/DD-Habilitative', 'Intellectual or Developmental Disability in ICF/DD-Nursing', 'Intellectual or Developmental Disability in ICF/DD', 'Intellectual Disability'),
    label=c('Williamson et al. (hazard ratio)', 'Tartof et al.', 'Clift et al. (hazard ratio)', 'Landes et al. (2020)', 'Gleason et al. (odds ratio)', 'Williamson et al. (hazard ratio)', 'Williamson et al. (hazard ratio)', 'Landes et al. (2021)', 'Landes et al. (2021)', 'Landes et al. (2021)', 'Landes et al. (2021)', 'Landes et al. (2021)', 'Henderson et al.'),
    rn=c('Organ Transplant', 'Organ Transplant', 'Down Syndrome in Own Home, Family Home, or Shared Living', 'Intellectual or Developmental Disability in Group Home', 'Intellectual or Developmental Disability', 'Reduced Kidney Function (eGFR < 30)', 'Reduced Kidney Function (eGFR 30-60)', 'Intellectual or Developmental Disability in Own Home, Family Home, or Shared Living', 'Intellectual or Developmental Disability in Group Home', 'Intellectual or Developmental Disability in ICF/DD-Habilitative', 'Intellectual or Developmental Disability in ICF/DD-Nursing', 'Intellectual or Developmental Disability in ICF/DD', 'Intellectual Disability'),
    y=c(3.53, 6.54, 10.39, 7.781, 5.909, 2.52, 1.33, 0.37, 2.39, 8.56, 17.07, 22.09, 3.20),
    y.lo=c(2.77, 2.66, 7.08, 6.856, 5.277, 2.33, 1.28, 0.28, 1.59, 4.97, 10.29, 9.19, 2.16),
    y.hi=c(4.49, 16.12, 15.23, 8.832, 6.617, 2.72, 1.40, 0.49, 3.59, 14.74, 28.31, 53.08, 4.25)
  )
), use.names=TRUE, fill=TRUE)
rn.levels <- c('Intellectual or Developmental Disability in Own Home, Family Home, or Shared Living', summary.effects.quasipoisson.dt[group=='Race & Ethnicity', levels(reorder(rn, y))], 'Reduced Kidney Function (eGFR 30-60)', 'Reduced Kidney Function (eGFR < 30)', 'Organ Transplant', 'Intellectual or Developmental Disability in Group Home', 'Intellectual or Developmental Disability in ICF/DD-Habilitative', 'Intellectual or Developmental Disability in ICF/DD-Nursing', 'Intellectual or Developmental Disability in ICF/DD', 'Intellectual Disability', 'Intellectual or Developmental Disability', 'Down Syndrome in Own Home, Family Home, or Shared Living', 'Age (Risk Ratio per Decade of Life)')
label.levels <- c('Present Study', 'Clift et al. (hazard ratio)', 'Gleason et al. (odds ratio)', 'Henderson et al.', 'Landes et al. (2021)', 'Landes et al. (2020)', 'Tartof et al.', 'Williamson et al. (hazard ratio)')
decade.effect.quasipoisson.dt <- as.data.table(summary(fit.quasipoisson)$coefficients, keep.rownames = TRUE)[-1 , .(group=NULL, label='Present Study', rn ='Age (Risk Ratio per Decade of Life)', y=exp(Estimate), y.lo=exp(Estimate-qnorm(.975)*`Std. Error`), y.hi=exp(Estimate+qnorm(.975)*`Std. Error`))]
windows(14,4)
summary.and.decade.effects.quasipoisson.dt <- rbindlist(list(decade.effect.quasipoisson.dt, summary.effects.quasipoisson.dt), use.names=TRUE, fill=TRUE)
ggplot(summary.and.decade.effects.quasipoisson.dt, aes(y=y, ymin=y.lo, ymax=y.hi, x=rn, shape=factor(label, levels=label.levels), color=factor(label, levels=label.levels))) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_point(stroke=1, size=2, na.rm=TRUE, position=position_dodge(width=-.5)) +
  geom_linerange(size=1, show.legend=FALSE, na.rm=TRUE, position=position_dodge(width=-.5)) +
  scale_shape_manual(name='label', labels=label.levels, values=c(16, 1:10)) +
  scale_color_discrete(name='label', labels=label.levels) +
  coord_flip() +
  scale_x_discrete(limits=rn.levels) +
  scale_y_log10(minor_breaks=NULL, breaks=c(.1*(1:9), 1:9, 10*(1:9)), labels=c(.1, .2, .3, .4, '', .6, '', .8, '', 1:8, '', 10*(1:9))) +
  ylab('Covid-19 Mortality Risk Ratio') +
  theme_gray(base_size=12) + theme(axis.title.y=element_blank(), legend.title=element_blank())
ggsave('DDNJ-rev-Fig-3.png')
ggsave('DDNJ-rev-Fig-3.eps', device=cairo_ps)

# Table 1:

dcast(
  rbindlist(list(
    covid.and.pop.by.age.group.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5='All Races and Ethnicities')],
    covid.and.pop.by.age.group.raceeth4.us.per.100k.dt[, .(age.group, meanAGE, number.of.covid.19.deaths, pop, covid_19_deaths_per_100k, raceeth5=raceeth4)]
  )),
  raceeth5 + age.group + meanAGE ~ .,
  value.var=c('pop', 'number.of.covid.19.deaths', 'covid_19_deaths_per_100k')
)[ order(
  factor(raceeth5, levels=c('All Races and Ethnicities', 'Non-Hispanic American Indian and Alaska Native', 'Hispanic', 'Non-Hispanic Black', 'Non-Hispanic Asian or Pacific Islander')),
  factor(age.group, levels=c('age.0.4', 'age.5.17', 'age.18.29', 'age.30.39', 'age.40.49', 'age.50.64', 'age.65.74', 'age.75.84', 'age.85.and.older'))
)]
