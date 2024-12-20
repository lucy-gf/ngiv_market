#### IGNORE ####

## troubleshooting ##
source(here::here('scripts','setup','packages.R'))
source(here::here('next_gen_flu','functions','demography.R'))
source(here::here('next_gen_flu','vacc_types.R'))
source(here::here('next_gen_flu','functions','flu_sim.R'))

# how do MMGH countries compare to our list?

mmgh_cov <- data.table(read_xlsx(here::here('data','MMGH','cov_total.xlsx')))
mmgh_iso <- unique(mmgh_cov$iso3c) # length = 193

itzs <- data.table(read_csv(here::here('data','epi','itzs.csv'), show_col_types=F))
lshtm_iso <- unique(itzs$codes) # length = 186

countrycode(setdiff(lshtm_iso, mmgh_iso), origin='iso3c', destination='country.name') # length = 8
# French Guiana, Hong Kong, Macao, New Caledonia, Puerto Rico, Palestine, Taiwan, Kosovo            

## how do population sizes differ to UN projections for 2050?
cov_main <- 0.5 # coverage level in targeted age groups
age_targeting <- 0:17 # ages being targeted
vacc_calendar_start <- '01-10'
vacc_calendar_weeks <- 12
vaccine_programs <- c(
  fcn_vacc_prog(NA, 0,
                vacc_calendar_start, 
                vacc_calendar_weeks,
                T),
  fcn_vacc_prog(age_targeting, cov_main,
                vacc_calendar_start, 
                vacc_calendar_weeks)
)
model_age_groups <- c(0,5,20,65)
age_group_names <- paste0(model_age_groups,"-", c(model_age_groups[2:length(model_age_groups)],99))

dem_dt <- data.table()
for(iso in lshtm_iso){
  dt <- fcn_weekly_demog(country = iso,
                   ageing = T,
                   ageing_date = '01-04', 
                   dates_in = seq.Date(as.Date('01-01-2025',format='%d-%m-%Y'),as.Date('31-12-2050',format='%d-%m-%Y'),by=7),
                   demographic_start_year = 2025,
                   vaccine_program = vaccine_programs[[1]],
                   init_vaccinated = c(0,0,0,0),
                   model_age_groups = model_age_groups) %>% filter(week==max(week),V==T) %>% 
    select(country, age_grp, total_as)
  dem_dt <- rbind(dem_dt, dt)
  print(iso)
}

dem_totals <- dem_dt %>% group_by(country) %>% summarise(tot_pop = sum(total_as))

library(wpp2022)
data(popproj1)
un_pop <- popproj1 %>% mutate(pop_2050 = 1000*`2050`) %>% select(country_code, name, pop_2050) 

pop_ratio <- function(iso){
  iso_n <- ifelse(iso=='XKX','Kosovo', countrycode(iso, origin='iso3c',destination='country.name'))
  iso_nv <- unname(unlist(country_itzs_names[country==iso_n|country_altern==iso_n, c('country','country_altern','country_altern_2')]))
  round(((un_pop %>% filter((name%in%iso_nv)))$pop_2050)/((dem_totals %>% filter(country==iso))$tot_pop), 2)
}

dem_totals$ratio <- NA
for(iso in lshtm_iso){
  print(paste0(pop_ratio(iso), ', ', iso))
  if(length(pop_ratio(iso))>0){
    if(abs(pop_ratio(iso) - 1) > 0.3){print('LOOOOOOOOOK HERE')}
    dem_totals[dem_totals$country==iso,]$ratio <- pop_ratio(iso)
  }
}

ggplot(dem_totals) + 
  geom_point(aes(tot_pop, ratio)) +
  scale_x_log10()

## TOTAL GLOBAL POP IN 2050 ##

unlist(unname(dem_dt %>% summarise(sum(total_as))/1000000000))

un_pop[1,3]/1000000000




#### 
load("~/Desktop/research asst/Global Code/data/vacc_output_BASE50/econ_inp_CAN.Rdata")
test <- econ_inp[country_code=='CAN']
colSums(test[scenario=='no_vacc' & simulation_index==1 & year < 2030][,4:7] + test[scenario=='no_vacc' & simulation_index==1 & year < 2030][,12:15])

tdata <- infs_out[iso3c=='CAN' & year(time) < 2030 & vacc_type=='0']
tdata[, year := year(time)]
tdata[, time:=NULL]
tdata <- tdata[, lapply(.SD, sum), by=c('year','vacc_type','simulation_index','iso3c')]
colSums(tdata[,5:8])


sum(test[simulation_index==1 & year %in% 2025:2050 & scenario == 'no_vacc', 4:19])/26 
sum(infs_out[iso3c=='CAN' & simulation_index==1 & year(time) %in% 2025:2050 & vacc_type=='0', 2:5])/26 











