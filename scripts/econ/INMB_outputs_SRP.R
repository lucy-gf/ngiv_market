
### INMB OUTPUTS ###
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
if(!dir.exists(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'outputs'))){
  dir.create(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'outputs'))
}

## LOAD DATA (from national_health_econ_1.R) ##
econ_cases_agg <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'econ_cases_agg.rds'))
doses <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'doses.rds'))

## NET MONETARY BENEFIT ##

# merge costs and doses
econ_add <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','income_g','total_hosp_cost',
                               'discounted_epi_costs','total_DALYs','discounted_DALYs_cost')]
econ_add <- econ_add[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','income_g')]

doses_adding <- doses[, c('iso3c','simulation_index','vacc_scenario','doses','total_cost','discounted_doses_cost')]
setnames(doses_adding, 'vacc_scenario','vacc_type')
doses_adding <- doses_adding[, lapply(.SD, sum), by = c('iso3c','simulation_index','vacc_type')]

econ_nmb <- econ_add[doses_adding, on = c('iso3c','simulation_index','vacc_type')]

# make *incremental* wrt comparator
if(comparator=='0'){econ_nmb <- econ_nmb[!vacc_type=='no_vacc']}

econ_inmb <- econ_nmb[, c('vacc_type','iso3c','income_g','simulation_index','discounted_epi_costs','discounted_DALYs_cost','discounted_doses_cost')]

base_econ <- econ_inmb[vacc_type==comparator]
setnames(base_econ, 'discounted_epi_costs','discounted_epi_costs_base')
setnames(base_econ, 'discounted_DALYs_cost','discounted_DALYs_cost_base')
setnames(base_econ, 'discounted_doses_cost','discounted_doses_cost_base')
base_econ[, vacc_type := NULL]
econ_inmb <- econ_inmb[!vacc_type==comparator]

econ_inmb <- econ_inmb[base_econ, on=c('iso3c','income_g','simulation_index')]

# make incremental variables
econ_inmb[, incr_epi_cost := discounted_epi_costs_base - discounted_epi_costs] # saved costs of treatment 
econ_inmb[, incr_DALY_cost := discounted_DALYs_cost_base - discounted_DALYs_cost] # cost of DALYs averted
econ_inmb[, incr_doses_cost := discounted_doses_cost_base - discounted_doses_cost] # saved cost of vaccination (likely negative)
# make INMB
econ_inmb[, inmb := incr_epi_cost + incr_DALY_cost + incr_doses_cost] 

# add names for plot faceting
econ_inmb[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]

# data for cost-benefit planes
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

# make wide, for cost-benefit planes with intervals
econ_nmb_meds <- dt_to_meas(econ_nmb2, c('vacc_type','iso3c','names','income_g'))
econ_nmb_meds_w <- dcast(econ_nmb_meds,
                         vacc_type+iso3c+names+income_g~measure, 
                         value.var=c('cost_saved','DALYs_averted'))

# making INMB data wide, for plots against GDP per capita
econ_inmb_meds <- dt_to_meas(econ_inmb, c('vacc_type','iso3c','income_g','names'))
econ_inmb_meds_w <- dcast(econ_inmb_meds[, c('vacc_type','iso3c','income_g','inmb','measure')],
                          vacc_type+iso3c+income_g~measure, value.var='inmb')

econ_inmb_meds_w <- econ_inmb_meds_w[cost_predic_c[simulation_index==1&age_grp==1&iso3c%in%econ_inmb_meds_w$iso3c][,c('iso3c','gdpcap')], on='iso3c']

# adding WHO regions for color of geom_point
WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
setnames(WHO_regions, 'country_code','iso3c')
WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
econ_inmb_meds_w <- econ_inmb_meds_w[WHO_regions, on='iso3c']

# means
econ_inmb_mean <- dt_to_meas(econ_inmb, c('vacc_type','iso3c','income_g','names'), usingMean = T)
econ_inmb_mean_w <- dcast(econ_inmb_mean[, c('vacc_type','iso3c','income_g','inmb','measure')],
                          vacc_type+iso3c+income_g~measure, value.var='inmb')

econ_inmb_mean_w <- econ_inmb_mean_w[cost_predic_c[simulation_index==1&age_grp==1&iso3c%in%econ_inmb_meds_w$iso3c][,c('iso3c','gdpcap')], on='iso3c']
econ_inmb_mean_w <- econ_inmb_mean_w[WHO_regions, on='iso3c']

# saving data
write_rds(econ_inmb, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_inmb.rds'))
write_rds(econ_nmb2, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_nmb2.rds'))
write_rds(econ_nmb_meds_w, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_nmb_meds_w.rds'))
write_rds(econ_inmb_meds_w, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_inmb_meds_w.rds'))
write_rds(econ_inmb_mean_w, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','econ_inmb_mean_w.rds'))

### NNV AND THRESHOLD PRICES HERE ###
### THESE NEED CHECKING ###
### AND HAVE NO ACCOMPANYING PLOTS IN INMB_plots.R YET ###

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
  
  # uniquely identify countries+year which use NGIVs 
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
  
  # make NNV variables
  nnv_tot[, nnv_inf := doses/(infections_base - infections)]
  nnv_tot[, nnv_hosp := doses/(hospitalisations_base - hospitalisations)]
  nnv_tot[, nnv_deaths := doses/(deaths_base - deaths)]
  
  # make global (although the years used hugely differ between countries so not sure how valid this is)
  global_nnv <- copy(nnv)
  global_nnv[, iso3c := NULL]
  global_nnv <- global_nnv[, lapply(.SD, sum), by=c('vacc_type','simulation_index','vacc_used')]
  global_nnv[, nnv_inf := doses/(infections_base - infections)]
  global_nnv[, nnv_hosp := doses/(hospitalisations_base - hospitalisations)]
  global_nnv[, nnv_deaths := doses/(deaths_base - deaths)]
    
  # adding WHO regions for color of geom_point
  WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
  setnames(WHO_regions, 'country_code','iso3c')
  WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
  nnv <- nnv[WHO_regions, on='iso3c']
  nnv_tot <- nnv_tot[WHO_regions, on='iso3c']
  
  # save data
  write_rds(nnv, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','nnv.rds'))
  write_rds(nnv_tot, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','nnv_tot.rds'))
  write_rds(global_nnv, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs','global_nnv.rds'))
}

## threshold prices

threshold_prices <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','year','discounted_epi_costs','discounted_DALYs_cost','gdpcap')]
threshold_prices <- threshold_prices[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','year','gdpcap')]

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

# remove years where countries haven't yet introduced NGIVs
threshold_prices[, combo := paste0(iso3c,'_',year)]
threshold_prices_base[, combo := paste0(iso3c,'_',year)]

threshold_prices <- threshold_prices[vacc_type == vacc_used]
threshold_prices_base <- threshold_prices_base[combo %in% threshold_prices$combo]

threshold_prices[,combo := NULL]
threshold_prices_base[,combo := NULL]

threshold_prices <- threshold_prices[threshold_prices_base, on=c('iso3c','simulation_index','year')]
threshold_prices[, vacc_used := NULL]

threshold_prices_tot <- threshold_prices[, lapply(.SD, sum), by=c('iso3c','simulation_index','vacc_type','gdpcap')]
threshold_prices_tot[, year := NULL]
## this calculation needs checking: this looks correct noting that d_doses_c_base includes purchase price
threshold_prices_tot[, threshold_price := ((d_e_c_base - discounted_epi_costs) + 
                                             (d_d_c_base - discounted_DALYs_cost) + 
                                             (d_doses_c_base - discounted_delivery_cost_tot) # d_doses_c_base includes purchase price.
                                           )/discounted_doses]


# adding WHO regions for color of geom_point
WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
setnames(WHO_regions, 'country_code','iso3c')
WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
threshold_prices_tot <- threshold_prices_tot[WHO_regions, on='iso3c']

# save 

write_rds(threshold_prices_tot, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('threshold_prices_',comparator,'.rds')))

# median threhold price

threshold_prices_meas <- dt_to_meas(threshold_prices_tot, c('vacc_type','iso3c','country','WHOREGION','gdpcap'), usingMean = T)
threshold_prices_meas_w <- dcast(threshold_prices_meas,
                        vacc_type+iso3c+country+gdpcap+WHOREGION~measure, 
                        value.var=c('threshold_price'))
write_rds(threshold_prices_meas_w, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('threshold_prices_meas_w_',comparator,'.rds')))

## HEALTH OUTCOMES AVERTED ##

# by age group
epi_out <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','income_g','age_grp',
                               'infections','deaths','hospitalisations','YLLs','total_DALYs')]

epi_out_by_age <- epi_out[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','income_g','age_grp')] #combine across years
if(comparator==0){epi_out_by_age <- epi_out_by_age[vacc_type!="no_vacc",]}

epi_base_by_age <- epi_out_by_age[vacc_type==comparator]
epi_base_by_age[,vacc_type:=NULL]
epi_out_by_age <- epi_out_by_age[vacc_type!=comparator]
for (n in c('infections','deaths','hospitalisations','YLLs','total_DALYs')){setnames(epi_base_by_age,n,paste0('base_',n))}

epi_out_by_age <- epi_out_by_age[epi_base_by_age,,on=c('simulation_index','iso3c','income_g','age_grp')] 

epi_out_by_age[, incr_infections := base_infections - infections]
epi_out_by_age[, incr_deaths := base_deaths - deaths]
epi_out_by_age[, incr_hospitalisations := base_hospitalisations - hospitalisations]
epi_out_by_age[, incr_YLLs := base_YLLs - YLLs]
epi_out_by_age[, incr_total_DALYs := base_total_DALYs - total_DALYs]

epi_out_by_age_meas <- dt_to_meas(epi_out_by_age, c('vacc_type','iso3c','income_g','age_grp'), usingMean = T)
epi_out_by_age_meas_w <- dcast(epi_out_by_age_meas,
                         vacc_type+iso3c+income_g+age_grp~measure, 
                         value.var=c('infections','deaths','hospitalisations','YLLs','total_DALYs',
                                     'incr_infections','incr_deaths','incr_hospitalisations','incr_YLLs','incr_total_DALYs'))

# add names for plot faceting
epi_out[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]

WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
setnames(WHO_regions, 'country_code','iso3c')
WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
epi_out_by_age_meas_w  <- epi_out_by_age_meas_w [WHO_regions, on='iso3c']

write_rds(epi_out_by_age_meas_w , here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('incr_epi_outcomes_by_age',comparator,'.rds')))

# combining age groups
epi_out <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','income_g','gdpcap',
                              'infections','deaths','hospitalisations','YLLs','total_DALYs')]

epi_out <- epi_out[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','income_g','gdpcap')] #combine across years and age groups
if(comparator==0){epi_out <- epi_out[vacc_type!="no_vacc",]}

epi_base <- epi_out[vacc_type==comparator]
epi_base[,vacc_type:=NULL]
epi_out <- epi_out[vacc_type!=comparator]
for (n in c('infections','deaths','hospitalisations','YLLs','total_DALYs')){setnames(epi_base,n,paste0('base_',n))}

epi_out <- epi_out[epi_base,,on=c('simulation_index','iso3c','income_g')] 

epi_out[, incr_infections := base_infections - infections]
epi_out[, incr_deaths := base_deaths - deaths]
epi_out[, incr_hospitalisations := base_hospitalisations - hospitalisations]
epi_out[, incr_YLLs := base_YLLs - YLLs]
epi_out[, incr_total_DALYs := base_total_DALYs - total_DALYs]

epi_out_meas <- dt_to_meas(epi_out, c('vacc_type','iso3c','income_g','gdpcap'), usingMean = T)
epi_out_meas_w <- dcast(epi_out_meas,
                               vacc_type+iso3c+income_g+gdpcap~measure, 
                               value.var=c('infections','deaths','hospitalisations','YLLs','total_DALYs',
                                           'incr_infections','incr_deaths','incr_hospitalisations','incr_YLLs','incr_total_DALYs'))

# add names for plot faceting
epi_out[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]

WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
setnames(WHO_regions, 'country_code','iso3c')
WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
epi_out_meas_w  <- epi_out_meas_w [WHO_regions, on='iso3c']

write_rds(epi_out_meas_w , here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('incr_epi_outcomes',comparator,'.rds')))
