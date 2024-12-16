#### Health econ, all countries ####

## out [SIM 1]:
## national net monetary benefit

##############################
########## SET UP ############
##############################

select <- dplyr::select

price_used <- c('midpoint','lower','upper')[1]

# sensitivity analyses
outp_include <- F # including outpatient/non-hospitalisation visits T/F
disease_modification <- F; mod_val <- 0.5
WTP_choice <- c('lancet','gdp')[1]; WTP_GDP_ratio <- 1 # proportion of GDP per capita for the willingness_to_pay threshold
discount_SA <- F

econ_folder_name <- paste0(ifelse(disease_modification, '_disease_mod',''),
                           ifelse(outp_include, '_outpatient',''),
                           ifelse((WTP_choice=='gdp'), '_gdp_',''),
                           ifelse((WTP_choice=='gdp'), WTP_GDP_ratio,''),
                           ifelse(discount_SA, '_discount0', ''),
                           ifelse(price_used != 'midpoint', paste0('_doseprice_',price_used), ''))
print(paste0('Folder = ', ifelse(econ_folder_name == '', 'base', paste0('base',econ_folder_name))))
scenario_name <- 'base'

if((disease_modification+discount_SA+outp_include+(WTP_choice=='gdp'))>1){
  print('BTW there is more than one econ sensitivity analysis on')
}

## PARAMETERS
cost_discount_rate_val <- c(0.03, 0.03)[1 + discount_SA]
DALY_discount_rate_val <- c(0.03, 0)[1 + discount_SA]
flu_duration <- 4/365
wastage <- 0.1

### TODO - are we adding in wastage? ###

if(!dir.exists(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name)))){
  dir.create(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name)))
}

national_ifrs <- data.table(read_csv(here::here('data','econ','national_ifrs.csv'),
                                     show_col_types=F))
setnames(national_ifrs, 'country_code','iso3c')

#### produce econ data ####
source(here::here('scripts','econ','making_econ_data','make_econ_data.R'))
      
## loading epi data

infs_out <- readRDS(here::here('output','data','epi','rds_output','vacc_global.rds'))

##############################
########### DOSES ############
##############################

prices <- data.table(read_csv(here::here('data','MMGH','prices.csv'), show_col_types=F))
country_specs <- data.table(read.xlsx(here::here('data','MMGH','country_specs.xlsx')))
country_specs <- country_specs[iso3c %in% infs_out$iso3c]
country_specs[, country_type := case_when(
  iso3c == 'USA' ~ 'us',
  income_g == 'HIC' & !iso3c=='USA' ~ 'hics',
  income_g == 'UMIC' ~ 'umics',
  income_g %in% c('LMIC','LIC') & procure_mech == 'UNICEF' ~ 'lmic_un_proc',
  income_g %in% c('LMIC','LIC') & procure_mech == 'Self-procuring' ~ 'lmic_self_proc',
)]

doses <- demand_input[, c('iso3c','WHO_region','income_g','vacc_scenario','year','doses','vacc_used')]
doses[, doses := ceiling(doses)]
doses <- doses[iso3c %in% infs_out$iso3c]
doses <- doses[, lapply(.SD, sum), by = c('iso3c','WHO_region','income_g','vacc_scenario','vacc_used','year')]
doses[, vacc_type := substr(vacc_used,1,1)]

doses <- doses[country_specs[, c('iso3c','country_type')], on='iso3c']

doses <- doses[prices[, c('vacc_type','country_type',..price_used)], on=c('vacc_type','country_type')]
setnames(doses, paste0(price_used), 'price')

setorder(doses, iso3c, vacc_scenario, year)

doses[, doses_cost := doses*price]
length_dt <- nrow(doses)

doses <- rbindlist(replicate(n = 100, expr = doses, simplify = FALSE))
doses$simulation_index <- rep(1:100, each = length_dt)

# costs of doses
delivery_cost_samples <- data.table(read_csv(here::here('data','econ','delivery_cost_samples.csv'), show_col_types=F))
delivery_cost_samples <- delivery_cost_samples[iso3c %in% unique(doses$iso3c)]
doses <- doses[delivery_cost_samples, on=c('iso3c','simulation_index')]

doses[, total_delivery_cost := doses*delivery_cost]
doses[, total_cost := total_delivery_cost + doses_cost]

## discounting
doses[, discount_year := year - start_year_of_analysis]
doses[, discount_rate := (1 + cost_discount_rate_val)^(-discount_year)]
doses[, discounted_doses_cost := total_cost*discount_rate]
doses[, discount_year := NULL]

##############################
############ EPI #############
##############################

if(!file.exists(here::here('output','data','econ',paste0(scenario_name,econ_folder_name)))){
  dir.create(here::here('output','data','econ',paste0(scenario_name,econ_folder_name)))
}
if(!file.exists(here::here('output','figures','econ',paste0(scenario_name,econ_folder_name)))){
  dir.create(here::here('output','figures','econ',paste0(scenario_name,econ_folder_name)))
}

#### MELTING EPI DATA ####

if(disease_modification == T){
  econ_cases_agg <- melt(infs_out %>% select(vacc_type,simulation_index,iso3c,year,income_g,IU1,IU2,IU3,IU4,IV1,IV2,IV3,IV4), 
                         id.vars = c('vacc_type','simulation_index','iso3c','year','income_g'))
  econ_cases_agg[, vaccinated := case_when(substr(variable,2,2)=='V' ~ T,
                                           substr(variable,2,2)=='U' ~ F)]
  econ_cases_agg[, age_grp := substr(variable,3,3)]
  econ_cases_agg[, variable := NULL]
}else{
  econ_cases_agg <- melt(infs_out %>% select(vacc_type,simulation_index,iso3c,year,income_g,I1,I2,I3,I4), 
                         id.vars = c('vacc_type','simulation_index','iso3c','year','income_g'))
  econ_cases_agg[, age_grp := as.numeric(substr(variable,2,2))]
  econ_cases_agg[, variable := NULL]
}
setnames(econ_cases_agg, 'value','infections')

#### IFRS ####
# TODO - move to making_econ, rerun with new model age groups

print('IFRs')
## load ifrs
national_ifrs <- data.table(read_csv(here::here('data','econ','national_ifrs.csv'),
                                     show_col_types=F))
setnames(national_ifrs, 'country_code','iso3c')
national_ifrs <- national_ifrs[iso3c %in% unique(econ_cases_agg$iso3c)]

econ_cases_agg <- econ_cases_agg[national_ifrs[,c('iso3c','simulation_index','age_grp','ifr')], on=c('iso3c','simulation_index','age_grp')]
econ_cases_agg[,deaths := ifr*infections]
if(disease_modification==T){
  econ_cases_agg[vaccinated==T,deaths := mod_val*deaths]
}

#### IHRS ####
# TODO - change model age groups for IHRs, move to make_econ

print('IHRs')
## load ihrs
global_ihrs <- data.table(read_csv(here::here('data','econ','global_ihrs.csv'),
                                   show_col_types=F))
outpatient_ratios <- data.table(read_csv(here::here('data','econ','outpatient_ratios.csv'),
                                         show_col_types=F))[country_code %in% unique(econ_cases_agg$iso3c)]
setnames(outpatient_ratios, 'country_code','iso3c')

econ_cases_agg <- econ_cases_agg[global_ihrs, on=c('simulation_index','age_grp')]
econ_cases_agg[,hospitalisations := ihr*infections]
if(disease_modification==T){
  econ_cases_agg[vaccinated==T,hospitalisations := mod_val*hospitalisations]
  econ_cases_agg[, vaccinated:=NULL]
  econ_cases_agg <- econ_cases_agg[, lapply(.SD,sum), by = c('iso3c','simulation_index','year','scenario','age_grp','vacc_type','income_g')]
}

if(outp_include == T){
  print('outpatients')
  econ_cases_agg <- econ_cases_agg[outpatient_ratios, on=c('iso3c','simulation_index'),
                                   ratio:=ratio]
  econ_cases_agg[, outpatients := ratio*hospitalisations]
  econ_cases_agg[, ratio:=NULL]
}

#### SYMPTOMATIC + FEVER ####

## from carrat (DOI: 10.1093/aje/kwm375)
print('symptomatic, fever')

symp_samples <- data.table(read_csv(here::here('data','econ','symp_samples.csv'),
                                    show_col_types=F))

econ_cases_agg <- econ_cases_agg[symp_samples, on=c('simulation_index')]
econ_cases_agg[, symptomatics := symp_prob*infections][, fevers := fever_prob*infections][, non_fevers := symptomatics - fevers]

#### YLLS ####

## discounting at same rate as DALYs, using 'lxqx' method
print('YLLs')
yll_df <- yll_df[iso3c %in% unique(econ_cases_agg$iso3c)]

econ_cases_agg <- econ_cases_agg[yll_df, on=c('iso3c','age_grp'), yll := yll]
econ_cases_agg[,YLLs := yll*deaths]

## removing probabilities etc.
econ_cases_agg[, c('ifr','ihr','symp_prob','fever_prob','yll'):=NULL]

#### YLDS ####
print('YLDs')

DALY_weight_samples <- data.table(read_csv(here::here('data','econ','DALY_weight_samples.csv'),
                                           show_col_types=F))

econ_cases_agg <- econ_cases_agg[DALY_weight_samples, on=c('simulation_index')]
econ_cases_agg[, non_fever_DALYs := flu_duration*non_fever_DALY*non_fevers][, fever_DALYs := flu_duration*fever_DALY*fevers][, hosp_DALYs := flu_duration*hosp_DALY*hospitalisations]
econ_cases_agg[, c('non_fever_DALY','fever_DALY','hosp_DALY'):=NULL]

## adding YLLs and YLDs to make DALYs
econ_cases_agg[, total_DALYs := non_fever_DALYs + fever_DALYs + hosp_DALYs + YLLs]

#### HEALTHCARE COSTS ####
print('healthcare costs')

load(here::here('data','econ','predicted_costs'))
cost_predic_c <- dcast(pred_costs[,c('iso3c','outcome','study_pop',
                                     'simulation_index','gdpcap','sample_cost')], iso3c + study_pop + gdpcap + simulation_index ~ outcome, value.var = 'sample_cost')
cost_predic_c[study_pop == 'adults', age_grp := 3]
cost_predic_c[study_pop == 'children', age_grp := 1]
cost_predic_c[study_pop == 'elderly', age_grp := 4]
cost_predic_c <- rbind(cost_predic_c, cost_predic_c[study_pop == 'adults',][,age_grp := 2])
cost_predic_c[, study_pop := NULL]
setnames(cost_predic_c, 'hospital', 'hosp_cost')
setnames(cost_predic_c, 'outpatient', 'outp_cost')
cost_predic_c <- cost_predic_c[iso3c %in% unique(econ_cases_agg$iso3c)]

econ_cases_agg <- econ_cases_agg[cost_predic_c, on=c('age_grp','iso3c','simulation_index')]
econ_cases_agg[, total_hosp_cost := hosp_cost*hospitalisations]
if(outp_include == T){
  econ_cases_agg[, total_outp_cost := outp_cost*outpatients]
}
econ_cases_agg[, c('hosp_cost','outp_cost'):=NULL]

#### WTP THRESHOLDS ####
## adding WTP
wtp_thresh <- data.table(read_csv(here::here('data','econ','WTP_thresholds.csv'), show_col_type=F))
if(WTP_choice == 'lancet'){
  econ_cases_agg <- econ_cases_agg[wtp_thresh, on=c('iso3c'), cet := cet]
}
if(WTP_choice == 'gdp'){
  econ_cases_agg[, cet := WTP_GDP_ratio*gdpcap]
}

econ_cases_agg[, cost_of_DALYs := total_DALYs*cet]
econ_cases_agg[, cet := NULL]

#### DISCOUNTING ####
print('discounting')

econ_cases_agg[, discount_year := year - start_year_of_analysis]
econ_cases_agg[, cost_discount_rate := (1 + cost_discount_rate_val)^(-discount_year)]
econ_cases_agg[, DALY_discount_rate := (1 + DALY_discount_rate_val)^(-discount_year)]
if(outp_include == T){
  econ_cases_agg[, discounted_epi_costs := (total_hosp_cost + total_outp_cost)*cost_discount_rate]
}else{
  econ_cases_agg[, discounted_epi_costs := (total_hosp_cost)*cost_discount_rate]
}
econ_cases_agg[, discounted_DALYs_cost := cost_of_DALYs*DALY_discount_rate]

econ_cases_agg[, c('discount_year','cost_discount_rate','DALY_discount_rate'):=NULL]

write_rds(econ_cases_agg, here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'econ_cases_agg.rds'))
write_rds(doses, here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'doses.rds'))













