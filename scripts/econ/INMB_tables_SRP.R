
### ECON TABLES ###
options(scipen=1000000)

scenario_name <- 'base'
econ_folder_name <- paste0(ifelse(disease_modification, '_disease_mod',''),
                           ifelse(outp_include, '_outpatient',''),
                           ifelse((WTP_choice=='gdp'), '_gdp_',''),
                           ifelse((WTP_choice=='gdp'), WTP_GDP_ratio,''),
                           ifelse(discount_SA, '_discount0', ''),
                           ifelse(price_used != 'midpoint', paste0('_doseprice_',price_used), ''))

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

# total sum of regional and global INMBs

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

regional_inmbs2 <- econ_inmb[WHO_regions, on='iso3c']
regional_inmbs <- data.table()
for(vt in unique(regional_inmbs2$vacc_type)){
  regional_inmbs <- rbind(regional_inmbs,
                          regional_inmbs2[vacc_type == vt &
                                            iso3c %in% include_dt[vacc_type==vt]$iso3c])
}
regional_inmbs <- regional_inmbs[, c('WHOREGION','vacc_type','simulation_index','inmb')]

# fill in regions with no cost-effective countries
regional_inmbs <- regional_inmbs %>% 
  complete(WHOREGION = unique(tab2_save$WHOREGION[tab2_save$WHOREGION != 'Global']),
           vacc_type = unique(regional_inmbs$vacc_type),
           simulation_index = unique(regional_inmbs$simulation_index),
           fill = list(inmb = 0))
regional_inmbs <- data.table(regional_inmbs)

# regional_inmbs <- econ_inmb[WHO_regions, on='iso3c']
# for(vt in unique(regional_inmbs$vacc_type)){
#   regional_inmbs <- regional_inmbs[! iso3c %notin% include_dt[vacc_type==vt]$iso3c]
# }
# regional_inmbs <- regional_inmbs[, c('WHOREGION','vacc_type','simulation_index','inmb')]

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

# also save *which* countries/vaccine types have INMB > 0
write_csv(include_dt, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table4_',comparator,'.csv')))


# mean threshold prices, compared to Plos Med analysis, with intro years

format_price <- function(x){
  
  out_vec <- rep('', length(x))
  
  for(i in 1:length(x)){
    
    neg <- x[i] < 0
    
    a <- abs(x[i])
    if(a < 100){f <- round(a, 2)}else{
      f <- round(a)
    }
    
    if(0 <= f & f < 1 & nchar(f) == 3){f <- paste0(f, 0)}
    if(0 <= f & f < 1 & nchar(f) == 1){f <- paste0(f, '.00')}
    if(1 <= f & f < 10 & nchar(f) == 3){f <- paste0(f, 0)}
    if(1 <= f & f < 10 & nchar(f) == 1){f <- paste0(f, '.00')}
    if(10 <= f & f < 100 & nchar(f) == 4){f <- paste0(f, 0)}
    if(10 <= f & f < 100 & nchar(f) == 2){f <- paste0(f, '.00')}
    
    out_vec[i] <- paste0(ifelse(neg,'-',''), '$', f)
    
  }
  
  out_vec
  
}

mean_tp <- readRDS(here::here('output','data','econ','base_doseprice_lower', 'outputs','threshold_prices_meas_w_0.rds')) %>% 
  filter(vacc_type %in% c('A.1','C')) %>% 
  mutate(
    mean = format_price(mean),
    eti95L = format_price(eti95L),
    eti95U = format_price(eti95U)
  ) %>% 
  mutate(new_threshold = paste0(mean, ' (', eti95L, ' - ', eti95U, ')'))

load(here::here('output','data','econ','base_doseprice_lower', 'outputs','total_nat_sum'))
tp_plos_med <- total_nat_sum[vt %in% c(2,5)]
tp_plos_med[vt == 2, vacc_type := 'A.1']
tp_plos_med[vt == 5, vacc_type := 'C']
mean_tp_plos_med <- dt_to_meas(tp_plos_med[, c('country_code','vacc_type','ct_n','threshold_price')], cols = c('country_code','vacc_type','ct_n'), 
                               usingMean = T) %>% 
  pivot_wider(id_cols = c('country_code','vacc_type','ct_n'), names_from = measure, values_from = threshold_price) %>% 
  setnames('country_code','iso3c')
mean_tp_plos_med <- mean_tp_plos_med %>% filter(ct_n=='0-17, 65+') %>% 
  mutate(
    mean = format_price(mean),
    eti95L = format_price(eti95L),
    eti95U = format_price(eti95U)
  ) %>% 
  mutate(plos_threshold = paste0(mean, ' (', eti95L, ' - ', eti95U, ')')) %>% 
  filter(iso3c %in% mean_tp$iso3c)
    
country_specs <- data.table(read_xlsx(here::here('data','MMGH','country_specs.xlsx')))
intro_years <- data.table(read_csv(here::here('data','MMGH','intro_years.csv'), show_col_types = F))
intro_years[, income_g := case_when(
  grepl('High', income_g) ~ 'HIC',
  grepl('Upper', income_g) ~ 'UMIC',
  grepl('Lower', income_g) ~ 'LMIC',
  grepl('Low inc', income_g) ~ 'LIC',
)]
setnames(intro_years, 'vacc_scenario', 'vacc_type')
country_specs <- rbind(cbind(country_specs, vacc_type = 'A.1'),
                       cbind(country_specs, vacc_type = 'C')) %>% 
  left_join(intro_years %>% filter(vacc_type%in% c('A.1','C')), by = c('income_g','vacc_type'))

ov_table <- mean_tp %>% 
  select(iso3c, country, vacc_type, WHOREGION, new_threshold) %>% 
  left_join(mean_tp_plos_med %>% select(iso3c, vacc_type, plos_threshold), by = c('vacc_type','iso3c')) %>% 
  left_join(country_specs %>% select(iso3c, vacc_type, income_g, intro_year), by = c('vacc_type','iso3c')) %>% 
  select(country, WHOREGION, vacc_type, intro_year, new_threshold, plos_threshold)

colnames(ov_table) <- c('Country','WHO Region','Vaccine type','Introduction year','Threshold price','Previously found threshold price')

write.xlsx(ov_table, here::here('output','data','econ','threshold_comparison_table.xlsx'))












