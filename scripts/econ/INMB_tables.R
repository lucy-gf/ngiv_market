
### ECON PLOTS AND TABLES ###
options(scipen=1000000)

scenario_name <- 'base'
econ_folder_name <- '' # change this if looking at a sensitivity analysis

comparator <- c('no_vacc','0')[2] # which vaccine scenario is the comparator?
# no vaccination or current seasonal vaccines
comparator_name <- case_when(
  comparator == 'no_vacc' ~ 'no vaccination',
  comparator == '0' ~ 'current seasonal vaccines'
)

if(!dir.exists(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator))){
  dir.create(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator))
}
if(!dir.exists(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),comparator))){
  dir.create(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),comparator))
}

## TABLES ##

# n and % of countries with INMB > 0

n_countries <- econ_inmb_meds_w %>% group_by(vacc_type, WHOREGION) %>% 
  summarise(n_total = n())

tab1 <- econ_inmb_meds_w %>% filter(median > 0) %>% group_by(vacc_type, WHOREGION) %>% 
  summarise(n = n()) %>% right_join(n_countries, by = c('vacc_type', 'WHOREGION')) %>% 
  mutate(n = case_when(is.na(n) ~ 0, T ~ n)) %>% 
  mutate(percentage = round(100*n/n_total, 1))

tab1_global <- tab1 %>% select(vacc_type, n, n_total) %>% 
  group_by(vacc_type) %>% summarise(n = sum(n), n_total = sum(n_total)) %>% 
  mutate(percentage = round(100*n/n_total, 1), WHOREGION = 'Global') %>% 
  select(vacc_type, WHOREGION, n, n_total, percentage)

tab1_save <- tab1 %>% rbind(tab1_global) %>%  
  mutate(positive_INMB = paste0(percentage,'% (', n, '/', n_total, ')')) %>% 
  select(WHOREGION, vacc_type, positive_INMB) %>% arrange(WHOREGION, vacc_type)

write_csv(tab1_save, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table1_',comparator,'.csv')))

# total sum regional INMBs

regional_inmbs <- econ_inmb[WHO_regions, on='iso3c']
regional_inmbs <- regional_inmbs[, c('WHOREGION','vacc_type','simulation_index','inmb')]

global_inmbs <- regional_inmbs[, c('vacc_type','simulation_index','inmb')][, lapply(.SD, sum), by=c('vacc_type','simulation_index')]
global_inmbs[, WHOREGION := 'Global']

regional_inmbs <- regional_inmbs[, lapply(.SD, sum), by=c('WHOREGION','vacc_type','simulation_index')]
regional_inmbs <- rbind(regional_inmbs, global_inmbs)
regional_inmbs <- dt_to_meas(regional_inmbs, c('WHOREGION','vacc_type'))
regional_inmbs_w <- dcast(regional_inmbs, WHOREGION + vacc_type ~ measure, value.var = 'inmb')
regional_inmbs_w[, INMB_millions := paste0(round(median/1e6), ' (',
                                           round(eti95L/1e6), ', ',
                                           round(eti95U/1e6), ')')]

tab2_save <- regional_inmbs_w[, c('WHOREGION','vacc_type','INMB_millions')]

write_csv(tab2_save, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table2_',comparator,'.csv')))

# total sum regional INMBs **only in countries where median INMB > 0**

include_dt <- econ_inmb_meds_w[median>0]
include_dt <- arrange(include_dt, vacc_type, iso3c)

regional_inmbs <- econ_inmb[WHO_regions, on='iso3c']
for(vt in unique(regional_inmbs$vacc_type)){
  regional_inmbs <- regional_inmbs[! iso3c %notin% include_dt[vacc_type==vt]$iso3c]
}
regional_inmbs <- regional_inmbs[, c('WHOREGION','vacc_type','simulation_index','inmb')]

global_inmbs <- regional_inmbs[, c('vacc_type','simulation_index','inmb')][, lapply(.SD, sum), by=c('vacc_type','simulation_index')]
global_inmbs[, WHOREGION := 'Global']

regional_inmbs <- regional_inmbs[, lapply(.SD, sum), by=c('WHOREGION','vacc_type','simulation_index')]
regional_inmbs <- rbind(regional_inmbs, global_inmbs)
regional_inmbs <- dt_to_meas(regional_inmbs, c('WHOREGION','vacc_type'))
regional_inmbs_w <- dcast(regional_inmbs, WHOREGION + vacc_type ~ measure, value.var = 'inmb')
regional_inmbs_w[, INMB_millions := paste0(round(median/1e6), ' (',
                                           round(eti95L/1e6), ', ',
                                           round(eti95U/1e6), ')')]

tab3_save <- regional_inmbs_w[, c('WHOREGION','vacc_type','INMB_millions')]

write_csv(tab3_save, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table3_',comparator,'.csv')))

write_csv(include_dt, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table4_',comparator,'.csv')))



















