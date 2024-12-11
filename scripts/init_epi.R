#### Market surplus analysis ####
#### Running epi ####

#### framework ####

# in:
# vaccine demand (from MMGH) 
# epi data (from previous work, ITZ-specific)
# vaccine type characteristics (from PPCs, updated 2024 versions)

# out: 
# epidemic data (from simulations)

#### load relevant packages ####
library(here)
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

vaccine_variable <- c('doses','coverage')[1] # using MMGH doses or % coverage?
# currently only set up for doses as we don't have current coverage data

################################################
################################################
################################################

#### load vaccine types ####
source(here::here('next_gen_flu','vacc_types.R'))

#### load MMGH data, merge subpopulations into model age groups ####
source(here::here('scripts','mmgh_data','mmgh_transform.R'))

#### run epidemics ####
for(itz_input in c('ARG','AUS','CAN','CHN','GBR','GHA','TUR')[c(4,7)]){
  source(here::here('scripts','epidemics','epid_simulations.R'))
}






