
### ECON PLOTS AND TABLES ###
options(scipen=1000000)

scenario_name <- 'base'
econ_folder_name <- '' # change this if looking at a sensitivity analysis

comparator <- c('no_vacc','0')[1] # which vaccine scenario is the comparator?
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

## LOAD DATA ##
econ_cases_agg <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'econ_cases_agg.rds'))
doses <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'doses.rds'))

## NET MONETARY BENEFIT ##

econ_add <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','income_g','total_hosp_cost',
                               'discounted_epi_costs','total_DALYs','discounted_DALYs_cost')]
econ_add <- econ_add[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','income_g')]

doses_adding <- doses[, c('iso3c','simulation_index','vacc_scenario','doses','total_cost','discounted_doses_cost')]
setnames(doses_adding, 'vacc_scenario','vacc_type')
doses_adding <- doses_adding[, lapply(.SD, sum), by = c('iso3c','simulation_index','vacc_type')]

econ_nmb <- econ_add[doses_adding, on = c('iso3c','simulation_index','vacc_type')]
if(comparator=='0'){econ_nmb <- econ_nmb[!vacc_type=='no_vacc']}

econ_inmb <- econ_nmb[, c('vacc_type','iso3c','income_g','simulation_index','discounted_epi_costs','discounted_DALYs_cost','discounted_doses_cost')]

base_econ <- econ_inmb[vacc_type==comparator]
setnames(base_econ, 'discounted_epi_costs','discounted_epi_costs_base')
setnames(base_econ, 'discounted_DALYs_cost','discounted_DALYs_cost_base')
setnames(base_econ, 'discounted_doses_cost','discounted_doses_cost_base')
base_econ[, vacc_type := NULL]
econ_inmb <- econ_inmb[!vacc_type==comparator]

econ_inmb <- econ_inmb[base_econ, on=c('iso3c','income_g','simulation_index')]

econ_inmb[, incr_epi_cost := discounted_epi_costs_base - discounted_epi_costs] # saved costs of treatment 
econ_inmb[, incr_DALY_cost := discounted_DALYs_cost_base - discounted_DALYs_cost] # cost of DALYs averted
econ_inmb[, incr_doses_cost := discounted_doses_cost_base - discounted_doses_cost] # saved cost of vaccination (likely negative)
econ_inmb[, inmb := incr_epi_cost + incr_DALY_cost + incr_doses_cost]

econ_inmb[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]

econ_nmb2 <- copy(econ_nmb)
econ_nmb2[, total_cost := total_cost + total_hosp_cost]

econ_nmb2_base <- econ_nmb2[vacc_type==comparator]
setnames(econ_nmb2_base, 'total_DALYs','base_total_DALYs')
setnames(econ_nmb2_base, 'total_cost','base_total_cost')
econ_nmb2 <- econ_nmb2[econ_nmb2_base[,c('vacc_type','simulation_index','iso3c','income_g','base_total_DALYs','base_total_cost')], on = c('simulation_index','iso3c','income_g')]
econ_nmb2[, DALYs_averted := base_total_DALYs - total_DALYs]
econ_nmb2[, cost_saved := base_total_cost - total_cost]

econ_nmb2 <- econ_nmb2[, c('vacc_type','iso3c','income_g','total_cost','DALYs_averted','cost_saved')]
econ_nmb2[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]

econ_nmb_meds <- dt_to_meas(econ_nmb2, c('vacc_type','iso3c','names','income_g'))
econ_nmb_meds_w <- dcast(econ_nmb_meds,
                         vacc_type+iso3c+names+income_g~measure, 
                         value.var=c('cost_saved','DALYs_averted'))

econ_inmb_meds <- dt_to_meas(econ_inmb, c('vacc_type','iso3c','income_g','names'))
econ_inmb_meds_w <- dcast(econ_inmb_meds[, c('vacc_type','iso3c','income_g','inmb','measure')],
                          vacc_type+iso3c+income_g~measure, value.var='inmb')

econ_inmb_meds_w <- econ_inmb_meds_w[cost_predic_c[simulation_index==1&age_grp==1&iso3c%in%econ_inmb_meds_w$iso3c][,c('iso3c','gdpcap')], on='iso3c']

WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
setnames(WHO_regions, 'country_code','iso3c')
WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
econ_inmb_meds_w <- econ_inmb_meds_w[WHO_regions, on='iso3c']

write_rds(econ_inmb, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_inmb.rds'))
write_rds(econ_nmb2, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_nmb2.rds'))
write_rds(econ_nmb_meds_w, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_nmb_meds_w.rds'))
write_rds(econ_inmb_meds_w, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_inmb_meds_w.rds'))


## NNV (if comparator == 'no_vacc')

if(comparator=='no_vacc'){
  nnv <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c',
                            'year','infections','hospitalisations','deaths','total_DALYs')][, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','year')]
  nnv_doses <- doses[, c('vacc_scenario','vacc_used','simulation_index','iso3c',
                         'year','doses')][, lapply(.SD, sum), by=c('vacc_scenario','vacc_used','simulation_index','iso3c','year')]
  setnames(nnv_doses, 'vacc_scenario','vacc_type')
  nnv <- nnv[nnv_doses, on=c('vacc_type','simulation_index','iso3c','year')]
  nnv_base <- nnv[vacc_type=='no_vacc']
  nnv <- nnv[vacc_type==vacc_used]
  
  nnv[, combo := paste0(iso3c,'_',year)]
  nnv_base[, combo := paste0(iso3c,'_',year)]
  
  nnv_base <- nnv_base[combo %in% unique(nnv$combo)]
  
  setnames(nnv_base, 'infections', 'infections_base')
  setnames(nnv_base, 'hospitalisations', 'hospitalisations_base')
  setnames(nnv_base, 'deaths', 'deaths_base')
  
  nnv <- nnv[nnv_base[,c('simulation_index','combo','infections_base','hospitalisations_base','deaths_base')], on=c('simulation_index','combo')]
  nnv <- nnv[doses>0] # filter out years before introduction of current vaccines (i.e. infections)
  nnv[, combo := NULL]
  nnv_tot <- nnv[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','vacc_used')] # sum over years 
  nnv_tot[, year := NULL]
  
  nnv_tot[, nnv_inf := doses/(infections_base - infections)]
  nnv_tot[, nnv_hosp := doses/(hospitalisations_base - hospitalisations)]
  nnv_tot[, nnv_deaths := doses/(deaths_base - deaths)]
  
  global_nnv <- copy(nnv)
  global_nnv[, iso3c := NULL]
  global_nnv <- global_nnv[, lapply(.SD, sum), by=c('vacc_type','simulation_index','vacc_used')]
  global_nnv[, nnv_inf := doses/(infections_base - infections)]
  global_nnv[, nnv_hosp := doses/(hospitalisations_base - hospitalisations)]
  global_nnv[, nnv_deaths := doses/(deaths_base - deaths)]
    
  write_rds(nnv, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','nnv.rds'))
  write_rds(global_nnv, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','global_nnv.rds'))
}

## threshold prices

threshold_prices <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','year','discounted_epi_costs','discounted_DALYs_cost')]
threshold_prices <- threshold_prices[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','year')]

doses_tp <- doses[, c('iso3c','simulation_index','vacc_scenario','vacc_used','year','doses','price','delivery_cost','discount_rate')]
doses_tp[, discounted_doses := doses*discount_rate]
doses_tp[, discounted_delivery_cost_tot := discounted_doses*delivery_cost]
setnames(doses_tp, 'vacc_scenario', 'vacc_type')

threshold_prices <- threshold_prices[doses_tp[, c('iso3c','simulation_index','vacc_type','year','vacc_used','price','discounted_doses','discounted_delivery_cost_tot')],
                                     on=c('iso3c','simulation_index','vacc_type','year')]

threshold_prices_base <- threshold_prices[vacc_type == comparator]
threshold_prices <- threshold_prices[!vacc_type == comparator]
if(comparator == '0'){threshold_prices <- threshold_prices[!vacc_type == 'no_vacc']}
threshold_prices[, price := NULL]

threshold_prices_base[, discounted_doses_cost_total := discounted_delivery_cost_tot + price*discounted_doses]
setnames(threshold_prices_base, 'discounted_epi_costs', 'd_e_c_base')
setnames(threshold_prices_base, 'discounted_DALYs_cost', 'd_d_c_base')
setnames(threshold_prices_base, 'discounted_doses', 'd_d_base')
setnames(threshold_prices_base, 'discounted_doses_cost_total', 'd_doses_c_base')
threshold_prices_base[, price := NULL]
threshold_prices_base[, discounted_delivery_cost_tot := NULL]
threshold_prices_base[, vacc_type := NULL]
threshold_prices_base[, vacc_used := NULL]

if(comparator == 'no_vacc'){
  threshold_prices[, combo := paste0(iso3c,'_',year)]
  threshold_prices_base[, combo := paste0(iso3c,'_',year)]
  
  threshold_prices <- threshold_prices[vacc_type == vacc_used]
  threshold_prices_base <- threshold_prices_base[combo %in% threshold_prices$combo]
  
  threshold_prices[,combo := NULL]
  threshold_prices_base[,combo := NULL]
}

threshold_prices <- threshold_prices[threshold_prices_base, on=c('iso3c','simulation_index','year')]
threshold_prices[, vacc_used := NULL]

threshold_prices_tot <- threshold_prices[, lapply(.SD, sum), by=c('iso3c','simulation_index','vacc_type')]
threshold_prices_tot[, year := NULL]
threshold_prices_tot[, threshold_price := ((d_e_c_base - discounted_epi_costs) + 
                                             (d_d_c_base - discounted_DALYs_cost) + 
                                             (d_doses_c_base - discounted_delivery_cost_tot)
                                           )/discounted_doses]

write_rds(threshold_prices_tot, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('threshold_prices_',comparator,'.rds')))



