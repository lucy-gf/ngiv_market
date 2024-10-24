#### Epidemic simulations ####

# using:
# coverage assumptions (from MMGH)
# epi data (from previous work, ITZ-specific)

## load data
mmgh_cov <- data.table(read_xlsx(here::here('data','MMGH','demand_total.xlsx')))
sampled_epids <- data.table(read.csv(here::here("data","epi","sampled_epidemics.csv")))
sampled_epids <- sampled_epids[simulation_cal_year <= years_of_analysis]
itzs <- data.table(read_csv(here::here('data','epi','itzs.csv'), show_col_types=F))

# example doses
ggplot(mmgh_cov[iso3c%in% c('USA','FRA','ZMB','UZB') & pop=='total']) + facet_grid(iso3c~.,scales='free') +
  geom_line(aes(x=year,y=doses)) + geom_line(aes(year,doses_improved),col=2,lty=2) 

## run epidemic simulations
# source(here::here('next_gen_flu','flu_parallel.R'))




