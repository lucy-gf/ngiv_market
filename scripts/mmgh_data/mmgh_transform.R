#### MMGH data transform ####
#### ie transforming their data into our model age groups ####

model_age_groups <- c(0,5,20,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],100))

mmgh_cov <- data.table(read_xlsx(here::here('data','MMGH','cov_total.xlsx')))
mmgh_pop <- data.table(read_xlsx(here::here('data','MMGH','pops_total.xlsx')))

unique(mmgh_cov$pop_name)
unique(mmgh_pop$pop_name)

## total national + annual populations, which MMGH estimates are based on ##
library(wpp2022)
data(popprojAge1dt)
popprojAge1dt <- data.table(popprojAge1dt)

un_pop <- popprojAge1dt %>% select(country_code,name,year,age,pop) %>% 
  mutate(pop = 1000*pop) %>% 
  filter(year %in% start_year_of_analysis:(start_year_of_analysis+years_of_analysis)) %>% 
  mutate(iso3c = suppressWarnings(countrycode(country_code, origin='un', destination='iso3c')))

un_pop_ags <- un_pop %>% mutate(age_group = case_when(
  age %in% model_age_groups[1]:(model_age_groups[2] - 1) ~ 1,
  age %in% model_age_groups[2]:(model_age_groups[3] - 1) ~ 2,
  age %in% model_age_groups[3]:(model_age_groups[4] - 1) ~ 3,
  age %in% model_age_groups[4]:(max(un_pop$age)) ~ 4,
  T ~ NA
)) %>% group_by(iso3c, name, year, age_group) %>% summarise(pop = sum(pop), .groups = 'drop')

## check reasonable:
# sum(un_pop_ags %>% filter(year==2025,iso3c=='GBR') %>% select(pop))





