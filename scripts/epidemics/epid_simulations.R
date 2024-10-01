#### Epidemic simulations ####

# using:
# coverage assumptions (from MMGH)
# epi data (from previous work, ITZ-specific)

## load data
mmgh_cov <- here::here('data','MMGH','coverage.csv')

## run epidemic simulations
source(here::here('next_gen_flu','flu_parallel.R'))




