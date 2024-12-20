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

# in:
# costs to producer
## 1 - research investment
## 2 - cost of vaccine dose production
## 3 - profits of vaccine sales

# out: 
# national costs and benefits
# producer costs and benefits
# therefore distribution

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

#### run first econ analysis ####
source(here::here('scripts','econ','national_health_econ_1.R'))

#### make national INMB data.tables ####
source(here::here('scripts','econ','INMB_outputs.R'))

#### plot outputs ####
source(here::here('scripts','econ','INMB_plots.R'))

#### make tables (has to come after plots) ####
source(here::here('scripts','econ','INMB_tables.R'))





