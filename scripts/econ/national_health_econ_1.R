#### Health econ, all countries ####

## out [SIM 1]:
## national net monetary benefit

##############################
########## SET UP ############
##############################

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
                           ifelse(discount_SA, '_discount0', ''))
print(paste0('Folder = ', ifelse(econ_folder_name == '', 'base', econ_folder_name)))
scenario_name <- 'base'

if((disease_modification+discount_SA+outp_include+(WTP_choice=='gdp'))>1){
  print('BTW there is more than one econ sensitivity analysis on')
}

## PARAMETERS
cost_discount_rate_val <- c(0.03, 0.03)[1 + discount_SA]
DALY_discount_rate_val <- c(0.03, 0)[1 + discount_SA]
flu_duration <- 4/365
wastage <- 0.1

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

# TODO - check why doses are not integers

doses <- demand_input[, c('iso3c','WHO_region','income_g','vacc_scenario','year','doses','vacc_used')]
doses <- doses[iso3c %in% infs_out$iso3c]
doses <- doses[, lapply(.SD, sum), by = c('iso3c','WHO_region','income_g','vacc_scenario','vacc_used','year')]
doses[, vacc_type := substr(vacc_used,1,1)]

doses <- doses[country_specs[, c('iso3c','country_type')], on='iso3c']

doses <- doses[prices[, c('vacc_type','country_type',..price_used)], on=c('vacc_type','country_type')]
setnames(doses, paste0(price_used), 'price')

doses[, doses_cost := doses*price]

# TODO - add delivery costs!

## discounting
doses[, discount_year := year - start_year_of_analysis]
doses[, discount_rate := (1 + cost_discount_rate_val)^(-discount_year)]
doses[, discounted_doses_cost := doses_cost*discount_rate]
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
# TODO - change model age groups for IFRs

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
# TODO - change model age groups for IHRs

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
# TODO - move calculations to make_econ_data.R

## from carrat (DOI: 10.1093/aje/kwm375)
print('symptomatic, fever')
# symp_probs <- data.table(
#   outcome = c('symptoms','fever'),
#   med = c(66.9, 34.9)/100,
#   l95 = c(58.3, 26.7)/100,
#   u95 = c(74.5, 44.2)/100
#   )
# ## determining distribution
# f.gamma <- function(shape, rate, x) {
#   p <- pgamma(x, shape, rate)
#   # return both
#   return(c(p))
# }
# delta <- function(fit, actual) sum((fit-actual)^2)
# objective <- function(theta, x, prob, ...) {
#   ab <- (theta)
#   fit <- f.gamma(ab[1], ab[2], x=as.numeric(x),...)
#   # fit <- f.beta(ab[1], ab[2], x=as.numeric(x),...)
#   return (delta(fit, prob))
# }
# fcn_fitting <- function(rates,
#                         probs){
#
#   x <- c(unlist(unname(rates)))
#   sol <- suppressWarnings(optim(f=objective,p=c(1,1),
#                                 # method="BFGS",
#                                 x=x,
#                                 prob=c(probs),
#                                 control = list(reltol = 1e-15)
#   ))
#   parms <- (sol$par)
#   return(parms)
# }
#
# for(i in 1:nrow(symp_probs)){
#   parms <- fcn_fitting(symp_probs[i,2:4], c(0.5, 0.025, 0.975))
#   symp_probs[i,"shape"] <- parms[1]
#   symp_probs[i,"rate"] <- parms[2]
#   symp_probs[i,"med_fit"] <- qgamma(p=c(0.5), shape=parms[1], rate=parms[2])
#   symp_probs[i,"l95_fit"] <- qgamma(p=c(0.025), shape=parms[1], rate=parms[2])
#   symp_probs[i,"u95_fit"] <- qgamma(p=c(0.975), shape=parms[1], rate=parms[2])
# }
#
# symp_samples <- data.table(
#   simulation_index = 1:100,
#   symp_prob = rgamma(100, shape = unlist(symp_probs[outcome=='symptoms','shape']), rate = unlist(symp_probs[outcome=='symptoms','rate'])),
#   fever_prob = rgamma(100, shape = unlist(symp_probs[outcome=='fever','shape']), rate = unlist(symp_probs[outcome=='fever','rate'])))
# write_csv(symp_samples, file='econ/outcome_calculations/data/symp_samples.csv')

symp_samples <- data.table(read_csv(here::here('data','econ','symp_samples.csv'),
                                    show_col_types=F))

econ_cases_agg <- econ_cases_agg[symp_samples, on=c('simulation_index')]
econ_cases_agg[, symptomatics := symp_prob*infections][, fevers := fever_prob*infections][, non_fevers := symptomatics - fevers]


#### YLLS ####

## discounting at same rate as DALYs, using 'lxqx' method

print('YLLs')
# TODO - move calc_ylls.R to make_econ_data.R
yll_df <- national_ifrs[simulation_index==1, c('iso3c','age_grp','cluster_name')]
source(here::here('data','econ','calc_ylls.R'))
pb <- txtProgressBar(min = 0, max = 186, style = 3, width = 50, char = "=")

for(i in 1:length(unique(yll_df$iso3c))){
  iso3c_i <- unique(yll_df$iso3c)[i]
  yll_df[iso3c == iso3c_i, 'yll'] <- yll(LT = UNLT[ISO3_code == iso3c_i & MidPeriod == 2022.5],
                                                r = DALY_discount_rate_val,
                                                smr = 1,
                                                weight_method = "lxqx", # weight method to average LE by age group: "lx" "lxqx" "equal" "pop_ifr"
                                                model_ages = model_age_groups)$d_LEx
  setTxtProgressBar(pb, i)
}

econ_cases_agg <- econ_cases_agg[yll_df, on=c('iso3c','age_grp'), yll := yll]
econ_cases_agg[,YLLs := yll*deaths]

## removing probabilities etc.
econ_cases_agg[, c('ifr','ihr','symp_prob','fever_prob','yll'):=NULL]

#### YLDS ####
print('YLDs')

## weights from GBD
# DALY_weights <- data.table(
#   outcome = c('non_fever','fever','hospitalisation'),
#   med = c(0.006, 0.051, 0.133),
#   l95 = c(0.002, 0.032, 0.088),
#   u95 = c(0.012, 0.074, 0.190)
# )
# for(i in 1:nrow(DALY_weights)){
#   parms <- fcn_fitting(DALY_weights[i,2:4], c(0.5, 0.025, 0.975))
#   DALY_weights[i,"shape"] <- parms[1]
#   DALY_weights[i,"rate"] <- parms[2]
#   DALY_weights[i,"med_fit"] <- qgamma(p=c(0.5), shape=parms[1], rate=parms[2])
#   DALY_weights[i,"l95_fit"] <- qgamma(p=c(0.025), shape=parms[1], rate=parms[2])
#   DALY_weights[i,"u95_fit"] <- qgamma(p=c(0.975), shape=parms[1], rate=parms[2])
# }
# DALY_weight_samples <-  data.table(
#   simulation_index = 1:100,
#   non_fever_DALY = rgamma(100, shape = unlist(DALY_weights[outcome=='non_fever','shape']), rate = unlist(DALY_weights[outcome=='non_fever','rate'])),
#   fever_DALY = rgamma(100, shape = unlist(DALY_weights[outcome=='fever','shape']), rate = unlist(DALY_weights[outcome=='fever','rate'])),
#   hosp_DALY = rgamma(100, shape = unlist(DALY_weights[outcome=='hospitalisation','shape']), rate = unlist(DALY_weights[outcome=='hospitalisation','rate']))
# )
# write_csv(DALY_weight_samples, file='econ/outcome_calculations/data/DALY_weight_samples.csv')

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

#### DISCOUNTING ####
print('discounting')

econ_cases_agg[, discount_year := year - start_year_of_analysis]
econ_cases_agg[, cost_discount_rate := (1 + cost_discount_rate_val)^(-discount_year)]
econ_cases_agg[, DALY_discount_rate := (1 + DALY_discount_rate_val)^(-discount_year)]
if(outp_include == T){
  econ_cases_agg[, discounted_costs := (total_hosp_cost + total_outp_cost)*cost_discount_rate]
}else{
  econ_cases_agg[, discounted_costs := (total_hosp_cost)*cost_discount_rate]
}
econ_cases_agg[, discounted_DALYs := total_DALYs*DALY_discount_rate]

econ_cases_agg[, c('discount_year','cost_discount_rate','DALY_discount_rate'):=NULL]

## adding WTP
wtp_thresh <- data.table(read_csv(here::here('data','econ','WTP_thresholds.csv'), show_col_type=F))
if(WTP_choice == 'lancet'){
  econ_cases_agg <- econ_cases_agg[wtp_thresh, on=c('iso3c'), cet := cet]
  econ_cases_agg[, disc_cost_of_DALYs := discounted_DALYs*cet]
  econ_cases_agg[, cet := NULL]
}
if(WTP_choice == 'gdp'){
  econ_cases_agg[, disc_cost_of_DALYs := WTP_GDP_ratio*discounted_DALYs*gdpcap]
}

## NET MONETARY BENEFIT ##

econ_add <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','discounted_costs','disc_cost_of_DALYs')]
econ_add <- econ_add[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c')]

doses_adding <- doses[, c('iso3c','vacc_scenario','discounted_doses_cost')]
setnames(doses_adding, 'vacc_scenario','vacc_type')
doses_adding <- doses_adding[, lapply(.SD, sum), by = c('iso3c','vacc_type')]

econ_nmb <- econ_add[doses_adding, on = c('iso3c','vacc_type')]
econ_nmb[, epi_cost := discounted_costs + disc_cost_of_DALYs]
econ_nmb <- econ_nmb[, c('vacc_type','iso3c','simulation_index','epi_cost','discounted_doses_cost')]

base_econ <- econ_nmb[vacc_type=='0']
setnames(base_econ, 'epi_cost','epi_cost_base')
setnames(base_econ, 'discounted_doses_cost','doses_cost_base')
base_econ[, vacc_type := NULL]
econ_nmb <- econ_nmb[!vacc_type=='0']

econ_nmb <- econ_nmb[base_econ, on=c('iso3c','simulation_index')]

econ_nmb[, incr_epi := epi_cost - epi_cost_base]
econ_nmb[, incr_vacc := discounted_doses_cost - doses_cost_base]
econ_nmb[, nmb := - incr_epi - incr_vacc]

ggplot(econ_nmb[iso3c%in% c('GBR','USA','FRA','GHA','NGA','SVN')]) + 
  geom_boxplot(aes(x=vacc_type, y=nmb/1e9, fill=vacc_type)) + 
  facet_wrap(iso3c~., scales='free') + theme_bw() +
  scale_fill_manual(values = vtn_colors) +
  geom_hline(yintercept=0, lty=2)

ggplot(econ_nmb[iso3c%in% c('GBR','USA','FRA','GHA','NGA','SVN')]) + 
  geom_point(aes(x=incr_vacc, y=-incr_epi, col=vacc_type)) + 
  facet_wrap(iso3c~., scales='free') + theme_bw() +
  xlab('Incremental vaccine cost') + ylab('Incremental epi cost') +
  scale_color_manual(values = vtn_colors) +
  geom_line(aes(x=incr_vacc, y=incr_vacc), lty=2)

# where does all the variation come from??

ggplot(econ_cases_agg[iso3c%in% c('GBR','USA','FRA','GHA') & age_grp==4 & year==2046]) +
  geom_boxplot(aes(x=vacc_type, y=deaths, fill=vacc_type)) +
  facet_wrap(iso3c~., scales='free') + theme_bw() +
  scale_fill_manual(values = vtn_colors)

ggplot(econ_cases_agg[iso3c%in% c('GBR','USA','FRA','GHA') & age_grp==4 & year==2046]) +
  geom_boxplot(aes(x=vacc_type, y=infections, fill=vacc_type)) +
  facet_wrap(iso3c~., scales='free') + theme_bw() +
  scale_fill_manual(values = vtn_colors)
