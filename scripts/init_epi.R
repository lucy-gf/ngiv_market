#### Market surplus analysis ####
#### Running epi ####

#### framework ####

# in:
# vaccine demand (from MMGH) 
# epi data (from previous work, ITZ-specific)
# vaccine type characteristics (from PPCs)

# out: 
# epidemic data (from simulations)

#### load relevant packages ####
library(here)
source(here::here('scripts','setup','packages.R'))

#### colour schemes etc. ####
# same as https://github.com/lucy-gf/flu_model_LG
source(here::here('scripts','setup','aesthetics.R'))

#### set key parameters ####

model_age_groups <- c(0,5,20,65) # MODEL AGE GROUPS - fixed 
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))
start_year_of_analysis <- 2025
years_of_analysis <- 26
simulations <- 1 # 100
ageing <- T

#### run epidemics ####
itz_input <- c('ARG','AUS','CAN','CHN','GBR','GHA','TUR')[2]
source(here::here('scripts','epidemics','epid_simulations.R'))







