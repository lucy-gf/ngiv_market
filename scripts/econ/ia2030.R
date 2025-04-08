
# source(here::here('scripts','econ','national_health_econ_1.R'))
# source(here::here('next_gen_flu','functions','demography.R'))
# source(here::here('next_gen_flu','vacc_types.R'))
# source(here::here('next_gen_flu','flu_parallel.R'))

cond <- econ_cases_agg[vacc_type %in% c('0','no_vacc')]

cond <- cond[, c('simulation_index','iso3c','year','vacc_type','age_grp','infections','symptomatics','deaths','total_DALYs')]

doses_cond <- demand_input[vacc_scenario == '0']

doses_no_vacc <- copy(doses_cond)
doses_no_vacc[, doses := 0][,vacc_scenario := 'no_vacc']
doses_cond <- rbind(doses_cond, doses_no_vacc)

doses_cond <- doses_cond[, c('iso3c','WHO_region','vacc_scenario','year','doses','model_age_group')]
setnames(doses_cond, 'vacc_scenario','vacc_type')
setnames(doses_cond, 'model_age_group','age_grp')

cond <- cond[doses_cond, on = c('iso3c','vacc_type','year','age_grp')]

cond[vacc_type == '0', vacc_type := 'current']
cond[, age_grp := as.character(age_grp)]
cond[age_grp == '1', age_grp := '0-4']
cond[age_grp == '2', age_grp := '5-17']
cond[age_grp == '3', age_grp := '18-64']
cond[age_grp == '4', age_grp := '65+']

model_age_groups <- c(0,5,18,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))

start_year_of_analysis <- 2025
years_of_analysis <- 26

simulations <- 100
ageing <- T # are the populations being aged in the simulations?
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12 # number of weeks in vaccination program
country_itzs_names <- data.table(read_csv(here::here('next_gen_flu','data','country_itzs_names.csv')))
vaccine_variable <- c('doses','coverage')[1] # using MMGH doses or % coverage?
vacc_coverage <- c('YES','NO')[1] # switch 'no vaccination' simulations off/on,
same_cov_SA <- F # should NGIVs use the exact coverage of current vaccines?

i <- 1; pop_loop <- data.table()
for(iso in unique(cond$iso3c)){
  
  hemisphere_input <- demand_input[iso3c==iso, ]$hemisphere[1]
  
  ageing_date_in <- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
  vacc_calendar_start <- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])
  
  pop <- fcn_weekly_demog(iso,
                          ageing = T,
                          ageing_date = ageing_date_in,
                          dates_in = last_monday(seq.Date(as.Date('01-01-2025',format='%d-%m-%Y'),
                                       as.Date('31-12-2050',format='%d-%m-%Y'), by=7)),
                          demographic_start_year = 2025,
                          vaccine_used = demand_input[iso3c==iso & vacc_scenario=='0' & model_age_group==1]$vacc_used,
                          doses_dt = demand_input[iso3c==iso],
                          init_vaccinated = rep(0,4),
                          model_age_groups = c(0,5,18,65))
  
  pop <- pop[U == T & month(week) == (as.numeric(substr(ageing_date,4,5)) - 1) & day(week) %in% 1:7]
  pop[, year := year(week)]
  setnames(pop, 'total_as','population')
  
  pop_loop <- rbind(pop_loop, pop)
  
  print(i); i <- i + 1
  
}

setnames(pop_loop, 'country','iso3c')

cond <- cond[pop_loop[,c('iso3c','year','age_grp','population')], on=c('iso3c','year','age_grp')]

write_csv(cond, here::here('output','data','econ','ia2030','outputs.csv'))

cond_meds <- dt_to_meas(cond, c('iso3c','year','vacc_type','age_grp','WHO_region'), using50 = F)

write_csv(cond_meds, here::here('output','data','econ','ia2030','outputs_medians.csv'))

## for simon

cond_s <- copy(econ_cases_agg)

cond_s <- cond_s[, c('simulation_index','iso3c','year','vacc_type','age_grp','infections','symptomatics')]

doses_cond <- demand_input 
doses_no_vacc <- doses_cond[vacc_scenario=='0']
doses_no_vacc[, doses := 0][,vacc_scenario := 'no_vacc']
doses_cond <- rbind(doses_cond, doses_no_vacc)

doses_cond <- doses_cond[, c('iso3c','WHO_region','vacc_scenario','year','doses','model_age_group')]
setnames(doses_cond, 'vacc_scenario','vacc_type')
setnames(doses_cond, 'model_age_group','age_grp')

cond_s <- cond_s[doses_cond, on = c('iso3c','vacc_type','year','age_grp')]

cond_s[vacc_type == '0', vacc_type := 'current']
cond_s[, age_grp := as.character(age_grp)]
cond_s[age_grp == '1', age_grp := '0-4']
cond_s[age_grp == '2', age_grp := '5-17']
cond_s[age_grp == '3', age_grp := '18-64']
cond_s[age_grp == '4', age_grp := '65+']

cond_s <- cond_s[pop_loop[,c('iso3c','year','age_grp','population')], on=c('iso3c','year','age_grp')]

cond_s[, age_grp := NULL]
cond_s <- cond_s[, lapply(.SD, sum), by=c('simulation_index','iso3c', 'year', 'vacc_type','WHO_region')]

write_csv(cond_s, here::here('output','data','econ','ia2030','outputs_all_vts.csv'))





