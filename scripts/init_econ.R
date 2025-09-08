#### Market surplus analysis ####
#### Running econ ####

#### framework ####

# in:
# health outcome data (from previous work)
# economic data (from previous work)
# country procurement classifications (from MMGH)
# vaccine prices (from MMGH)

# out [SIM 1]:
# national net monetary benefit

# rerun [SIM 2]:
# exclude countries where net monetary benefit < 0

#### load relevant packages ####
source(here::here('scripts','setup','packages.R'))

#### colour schemes etc. ####
# same as https://github.com/lucy-gf/flu_model_LG
source(here::here('scripts','setup','aesthetics.R'))

################################################
############## set key parameters ##############
################################################

model_age_groups <- c(0,5,18,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))

start_year_of_analysis <- 2025
years_of_analysis <- 26

simulations <- 100
ageing <- T
key_dates <- c('01-04', '01-10') # vaccination and ageing dates (hemisphere-dependent)
vacc_calendar_weeks <- 12

vaccine_variable <- c('doses','coverage')[1] 

#### load MMGH data, merge subpopulations into model age groups ####
source(here::here('scripts','mmgh_data','mmgh_transform.R'))

#### produce shrunk datasets ####
# source(here::here('scripts','econ','shrink_epi_data.R'))
## alternatively load 'vacc_global.rds' from OneDrive to skip this step

#### loop over sensitivity analyses ####

for(SA_option in 0:3){
  
  WTP_choice <- ifelse(SA_option %in% 1:2, 'gdp','lancet'); WTP_GDP_ratio <- c(1, 0.3)[SA_option] # proportion of GDP per capita for the willingness_to_pay threshold
  discount_SA <- ifelse(SA_option == 3, T, F)
  print(paste0('Sens analysis:', ifelse(SA_option == 0, ' none', ''),
               ifelse(WTP_choice == 'gdp', paste0(' GDP (WTP = ', WTP_GDP_ratio, ' x GDPpc)'), ''),
               ifelse(discount_SA == T, ' discounting DALYs at 0%', '')))
  
  for(price_used in c('upper','lower')){
    
    #### produce national outputs ####
    source(here::here('scripts','econ','national_health_econ_1_SRP.R'))
    
    #### make national INMB data.tables ####
    source(here::here('scripts','econ','INMB_outputs_SRP.R'))
    
    #### plot outputs ####
    source(here::here('scripts','econ','INMB_plots_SRP.R'))
    
    #### plot new fig 2 ####
    source(here::here('scripts','econ','INMB_plots_SRP_CE.R'))
    
    #### make tables outputs ####
    source(here::here('scripts','econ','INMB_tables_SRP.R'))
    
  }
  
}

#### make sensitivity analysis comparison outputs ####
source(here::here('scripts','econ','sens_analyses_outputs.R'))


