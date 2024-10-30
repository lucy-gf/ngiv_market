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

#### set key parameters ####











