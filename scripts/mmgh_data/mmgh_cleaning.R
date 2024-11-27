#### MMGH data cleaning ####

library(here)
library(data.table)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)
library(purrr)

## read in raw data ##

############################################################
#### country WHO regions/income groups/Gavi/procurement ####
############################################################

country_specs <- data.table(read_xlsx(here::here('data','MMGH','raw','202410_FVIVA country procurement classification.xlsx')))

# make names more usable
setnames(country_specs, 'Country name','country')
setnames(country_specs, 'ISO','iso3c')
setnames(country_specs, 'WHO Region','WHO_region')
setnames(country_specs, 'Income Group','income_g')
setnames(country_specs, 'Gavi/Non-Gavi','gavi')
setnames(country_specs, 'Procurement Mechanism','procure_mech')

# adding universal policy logical
univ <- data.table(read_xlsx(here::here('data','MMGH','raw','Updated influenza model_A.1 scenario.xlsx'), sheet = '1a. Assumptions table', skip = 2)) %>% 
  select(`ISO code`, starts_with('Policy_all'))
univ <- univ[, 1:2] %>% drop_na()
setnames(univ, 'ISO code','iso3c')
setnames(univ, 'Policy_all_population...10','univ_pol_old')
univ <- univ %>% mutate(univ_pol = case_when(univ_pol_old=='No' ~ F,
                                             T ~ T))
country_specs <- country_specs[univ[,c('iso3c','univ_pol')], on='iso3c']

write.xlsx(country_specs, here::here('data','MMGH','country_specs.xlsx'))

###############################
#### national annual doses ####
###############################

## function ##
clean_demand <- function(dt_raw){
  
  setnames(dt_raw, 'Country','country')
  setnames(dt_raw, 'ISO code','iso3c')
  setnames(dt_raw, 'WHO','WHO_region')
  setnames(dt_raw, 'WB group (2022 classification)','income_g')
  
  if(ncol(dt_raw) > 33){
    exclude_all <- (suppressWarnings(as.numeric(gsub('[.]', '', str_sub(colnames(dt_raw), start = -4)))) >= 33)
    exclude_all[is.na(exclude_all)] <- F
    dt_all_wide <- dt_raw[,!..exclude_all]
  }else{
    dt_all_wide <- dt_raw
  }
  
  dt_all <- melt(dt_all_wide, id.vars=c('country','iso3c','WHO_region','income_g'))
  
  setnames(dt_all, 'variable','year')
  setnames(dt_all, 'value','doses')
  
  dt_all[, year := as.numeric(substr(year, 1, 4))]
  
  if((sum(!dt_all$year == rep(2023:2050, each=length(unique(dt_all$iso3c)))) > 0)){stop('Years are wrong')}
  
  if(length(unique(dt_all$iso3c)) != 193){print(paste0('N countries = ', length(unique(dt_all$iso3c))))}
  
  dt_all
}

## map 

demand_pops_curr <- list(
  list(sheet = "5a. Demand children base frozen", pop_name = 'children'),
  list(sheet = "5b. Demand 65+ base frozen", pop_name = '65+'),
  list(sheet = "5c. Demand HWF base frozen", pop_name = 'HWF'),
  list(sheet = "5e. Demand PW base frozen", pop_name = 'PW'),
  list(sheet = "5f. Demand comorb base frozen", pop_name = 'comorb'),
  list(sheet = "5g. Demand 18-64yo base frozen", pop_name = '18-64yo'),
  list(sheet = "5h. Total demand base frozen", pop_name = 'total')
)

results_curr <- map(demand_pops_curr, ~{
  dt_raw <- data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw',
                                                             paste0('Updated influenza model_A.1 scenario.xlsx')), 
                                                  sheet = .x$sheet, skip = 3)))
  res <- clean_demand(dt_raw)
  res$pop_name <- .x$pop_name
  print(.x$pop_name)
  res
})

demand_total <- cbind(rbindlist(results_curr),
                      vacc_scenario = '0')

demand_pops <- list(
  list(sheet = "6a. Demand children new", pop_name = 'children'),
  list(sheet = "6b. Demand 65+ new", pop_name = '65+'),
  list(sheet = "6c. Demand HWF new", pop_name = 'HWF'),
  list(sheet = "6e. Demand PW new", pop_name = 'PW'),
  list(sheet = "6f. Demand comorb new", pop_name = 'comorb'),
  list(sheet = "6g. Demand 18-64yo new", pop_name = '18-64yo'),
  list(sheet = "6h. Total demand new", pop_name = 'total')
)

for(vacc_scenario_i in c('A.1','A.2','B.1','B.2','C')){
  print(paste0('VACC SCENARIO = ', vacc_scenario_i))
  results <- map(demand_pops, ~{
    dt_raw <- data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw',
                                                               paste0('Updated influenza model_', vacc_scenario_i,' scenario.xlsx')), 
                                                    sheet = .x$sheet, skip = 3)))
    res <- clean_demand(dt_raw)
    res$pop_name <- .x$pop_name
    print(.x$pop_name)
    res
  })
  
  demand_total <- rbind(demand_total,
                        cbind(rbindlist(results),
                              vacc_scenario = vacc_scenario_i)
                        )
}

## TODO - missing DMA in comorb sheets
# setdiff(unique(demand_total[pop_name=='HWF',iso3c]),unique(demand_total[pop_name=='comorb',iso3c]))

write.csv(demand_total, here::here('data','MMGH','demand_total.csv'))

#####################################
#### national annual populations ####
#####################################

## function ##
clean_pop <- function(dt_raw){
  
  n_col_orig <- (ncol(dt_raw %>% select(!starts_with('20'))) + length(2022:2050))
  
  setnames(dt_raw, 'Country','country')
  setnames(dt_raw, 'ISO code','iso3c')
  setnames(dt_raw, 'WHO','WHO_region')
  setnames(dt_raw, 'WB group (2022 classification)','income_g')
  
  dt_raw <- dt_raw %>% 
    select(country, iso3c, WHO_region, income_g, starts_with('20'))
  
  n_col_keep <- (4 + length(2022:2050))
  
  if(ncol(dt_raw) > n_col_keep){
    exclude <- (suppressWarnings(as.numeric(gsub('[.]', '', str_sub(colnames(dt_raw), start = -4)))) > n_col_orig)
    exclude[is.na(exclude)] <- F
    dt_wide <- dt_raw[,!..exclude]
  }else{
    dt_wide <- dt_raw
  }
  
  dt <- melt(dt_wide, id.vars=c('country', 'iso3c', 'WHO_region', 'income_g'))
  
  setnames(dt, 'variable','year')
  setnames(dt, 'value','pop')
  
  dt[, year := as.numeric(substr(year, 1, 4))]
  
  if((sum(!dt$year == rep(2022:2050, each=length(unique(dt$iso3c)))) > 0)){stop('Years are wrong')}
  
  dt
}

## map 

pop_pops <- list(
  list(sheet = "2a. 6-59 mo pop.", pop_name = '6-59mo'),
  list(sheet = "2c. 5-17 yo pop.", pop_name = '5-17yo'),
  list(sheet = "2b. 18-64 yo pop.", pop_name = '18-64yo'),
  list(sheet = "2d. 65+ yo pop.", pop_name = '65+yo'),
  list(sheet = "2e. HWF pop.", pop_name = 'HWF'),
  list(sheet = "2f. pregnant women pop", pop_name = 'pregnant'),
  list(sheet = "2g. pop with comorbidities", pop_name = 'comorb')
)

results_pop <- map(pop_pops, ~{
  dt_raw <- data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw','Updated influenza model_A.1 scenario.xlsx'), 
                                                  sheet = .x$sheet, skip = 2))) # consistent across vaccination scenarios
  res <- clean_pop(dt_raw)
  res$pop_name <- .x$pop_name
  print(.x$pop_name)
  res
})

pops_total <- rbindlist(results_pop)

## TODO - DMA missing from comorb sheet

write.csv(pops_total, here::here('data','MMGH','pops_total.csv'))

###############################
#### years of introduction ####
###############################

intro_yrs <- data.table(income_g = rep(unique(pops_total$income_g)[1:4], each=5),
                        vacc_scenario = c('A.1','A.2','B.1','B.2','C'),
                        intro_year = 0)

for(vacc_scenario_i in unique(intro_yrs$vacc_scenario)){
  print(paste0('VACC SCENARIO = ', vacc_scenario_i))
  sheet1 <- data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw',
                                                               paste0('Updated influenza model_', vacc_scenario_i,' scenario.xlsx')), 
                                                    sheet = 'Summary', skip = 9)))[1:3,1:2]
  intro_yrs[vacc_scenario == vacc_scenario_i & grepl('High',income_g), intro_year := as.numeric(sheet1[grepl('HIC', `Introduction dates`), 2])]
  intro_yrs[vacc_scenario == vacc_scenario_i & grepl('Upper',income_g), intro_year := as.numeric(sheet1[grepl('UMIC', `Introduction dates`), 2])]
  intro_yrs[vacc_scenario == vacc_scenario_i & grepl('Low',income_g), intro_year := as.numeric(sheet1[grepl('L&', `Introduction dates`), 2])]
}

write_csv(intro_yrs, here::here('data','MMGH','intro_years.csv'))

# ###################################
# #### national annual coverage ####
# ##################################
# 
# ## function ##
# clean_cov <- function(dt_raw, names){
# 
#   vals <- gsub("\\d|\\.", "", names)
#   name_indices <- which(!vals=='')
# 
#   setnames(dt_raw, 'Country','country')
#   setnames(dt_raw, 'ISO code','iso3c')
#   setnames(dt_raw, 'WHO','WHO_region')
#   setnames(dt_raw, 'WB group (2022 classification)','income_g')
# 
#   children_1 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[1]:(name_indices[2]-1)], pop_name = 'children', vaccine = 1)
#   over65_1 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[2]:(name_indices[3]-1)], pop_name = 'over65', vaccine = 1)
#   HWF_1 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[3]:(name_indices[4]-1)], pop_name = 'HWF', vaccine = 1)
#   PW_1 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[4]:(name_indices[5]-1)], pop_name = 'PW', vaccine = 1)
#   comorb_1 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[5]:(name_indices[6]-1)], pop_name = 'comorb', vaccine = 1)
#   age_5_64_1 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[6]:(name_indices[7]-2)], pop_name = 'age_5_64', vaccine = 1)
#   children_2 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[7]:(name_indices[8]-1)], pop_name = 'children', vaccine = 2)
#   over65_2 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[8]:(name_indices[9]-1)], pop_name = 'over65', vaccine = 2)
#   HWF_2 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[9]:(name_indices[10]-1)], pop_name = 'HWF', vaccine = 2)
#   PW_2 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[10]:(name_indices[11]-1)], pop_name = 'PW', vaccine = 2)
#   comorb_2 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[11]:(name_indices[12]-1)], pop_name = 'comorb', vaccine = 2)
#   age_5_64_2 <- cbind(dt_raw[, 1:4], dt_raw[, name_indices[12]:ncol(dt_raw)], pop_name = 'age_5_64', vaccine = 2)
# 
#   dt <- data.table()
# 
#   for(variable in c('children_1','over65_1','HWF_1','PW_1','comorb_1','age_5_64_1',
#                     'children_2','over65_2','HWF_2','PW_2','comorb_2','age_5_64_2')){
# 
#     dt_v <- suppressWarnings(melt(get(variable), id.vars=c('country', 'iso3c', 'WHO_region', 'income_g','pop_name','vaccine')))
#     setnames(dt_v, 'variable','year')
#     setnames(dt_v, 'value','cov')
#     dt_v[, year := as.numeric(substr(year, 1, 4))]
#     dt_v[is.na(cov), cov:=0]
# 
#     dt <- rbind(dt, dt_v)
#   }
# 
#   dt
# }
# 
# sheet <- "3b. Coverage new scenarios"
# dt_raw <- data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw','Updated influenza model_A.1 scenario.xlsx'),
#                                                   sheet, skip = 2)))
# names <- colnames(data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw','Updated influenza model_A.1 scenario.xlsx'),
#                                                sheet, skip = 1))))
# results_cov <- clean_cov(dt_raw, names)
# 
# ## TODO - is there still the right number of countries?
# ## previously: Niue (NIU) is missing again
# 
# write.xlsx(results_cov, here::here('data','MMGH','cov_total.xlsx'))

#########################
#### price estimates ####
#########################

prices_raw <- data.table(read_xlsx(here::here('data','MMGH','raw','prices_ppt.xlsx'), skip = 1))

prices_raw <- prices_raw[!2:3,]
prices_raw <- prices_raw[,!c(2,3)]
setnames(prices_raw, '...1', 'vacc_type')
setnames(prices_raw, 'U.S.', 'us')
setnames(prices_raw, 'Other HICs', 'hics')
setnames(prices_raw, 'UMICs', 'umics')
setnames(prices_raw, 'Self-procuring LMICs', 'lmic_self_proc')
setnames(prices_raw, 'UN procuring LMICs', 'lmic_un_proc')

prices <- melt(prices_raw, id.vars='vacc_type')
setnames(prices, 'variable','country_type')
setnames(prices, 'value','price')

prices[, range_vals := ((str_split(gsub('\\$| -| –','', price), ' ')))]
for(i in 1:nrow(prices)){
  prices[i, midpoint := mean(as.numeric(unlist(prices$range_vals[i])))]  
}
prices <- prices[, !'range_vals']

write.csv(prices, here::here('data','MMGH','prices.csv'))

# prices$vacc_type <- factor(prices$vacc_type, levels=unique(prices$vacc_type))
# ggplot(prices) +
#   geom_line(aes(x=vacc_type, y=midpoint, group = country_type, col=country_type))




