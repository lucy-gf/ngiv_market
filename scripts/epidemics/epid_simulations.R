#### Epidemic simulations ####

# using:
# coverage assumptions (from MMGH)
# epi data (from previous work, ITZ-specific)

## load data
mmgh_cov <- data.table(read_xlsx(here::here('data','MMGH','cov_total.xlsx')))
mmgh_pop <- data.table(read_xlsx(here::here('data','MMGH','pops_total.xlsx')))

# TODO - align lists of countries between itzs and MMGH data
itzs <- data.table(read_csv(here::here('data','epi','itzs.csv'), show_col_types=F))
itzs[, cluster_code := case_when(cluster_name=='Asia-Europe' ~ 'TUR',
                                 cluster_name=='Africa' ~ 'GHA',
                                 cluster_name=='Eastern and Southern Asia' ~ 'CHN',
                                 cluster_name=='Europe' ~ 'GBR',
                                 cluster_name=='Southern America' ~ 'ARG',
                                 cluster_name=='Oceania-Melanesia-Polynesia' ~ 'AUS',
                                 cluster_name=='Northern America' ~ 'CAN',
                                 T ~ NA)]
itzs[, hemisphere := case_when(cluster_name=='Asia-Europe' ~ 'NH',
                                 cluster_name=='Africa' ~ 'NH',
                                 cluster_name=='Eastern and Southern Asia' ~ 'NH',
                                 cluster_name=='Europe' ~ 'NH',
                                 cluster_name=='Southern America' ~ 'SH',
                                 cluster_name=='Oceania-Melanesia-Polynesia' ~ 'SH',
                                 cluster_name=='Northern America' ~ 'NH',
                                 T ~ NA)]

## taking model age groups s
# TODO - how to incorporate comomorbidities etc. at a later date
cov_dt <- mmgh_cov[pop_name %in% c('children','over65','age_5_64')]
cov_dt_add <- cov_dt[pop_name == 'age_5_64']
cov_dt_add[, age_grp := 3]
cov_dt[pop_name == 'children', age_grp := 1]
cov_dt[pop_name == 'age_5_64', age_grp := 2]
cov_dt[pop_name == 'over65', age_grp := 4]
cov_dt <- rbind(cov_dt, cov_dt_add)
cov_dt <- arrange(cov_dt, age_grp, vaccine, country, year) %>% select(country,iso3c,age_grp,vaccine,year,cov)

# ## DATA EXPLORATION - IGNORE
# # example coverage in a few countries
cov_dt[, age_name := case_when(age_grp == 1 ~ '0-4',age_grp == 2 ~ '5-19',age_grp == 3 ~ '20-64',age_grp == 4 ~ '65+')]
cov_dt$age_name <- factor(cov_dt$age_name, levels=c('0-4','5-19','20-64','65+'))
ggplot() +
  geom_line(data=cov_dt[iso3c%in% c('GBR','BRB','FRA','GHA') & vaccine==1], aes(year,cov,col=as.factor(age_name)),lwd=0.8) +
  geom_line(data=cov_dt[iso3c%in% c('GBR','BRB','FRA','GHA') & vaccine==2], aes(year,cov,col=as.factor(age_name)),lty=2,lwd=0.8) +
  # facet_grid(iso3c~pop_name,scales='free') + theme_bw() + ylim(0,1)
  facet_grid(country~.,scales='free') + theme_bw() + ylim(0,1) + labs(col='Age group') + ylab('Age-specific coverage') +
  scale_color_viridis(discrete=T, direction=-1) + xlab('Year')
ggsave(here::here('output','figures','epi','coverage_examples.png'),
       width=20,height=14,units="cm")
# test <- dcast(cov_dt,  country + iso3c + year + age_grp ~ vaccine, value.var='cov')
# test[, diff := `2`-`1`]
# View(test[diff>0]) # all in over 65s

## set up next_gen_flu code
source(here::here('next_gen_flu','flu_parallel.R'))
  
hemisphere_input <- itzs[cluster_code==itz_input]$hemisphere[1]
isos <- itzs[cluster_code==itz_input]$codes
isos <- intersect(isos, unique(mmgh_cov$iso3c)) # TODO - quick fix for now about countries included

sampled_epids <- data.table(read_csv(here::here('data','epi','sampled_epids',paste0('sampled_epidemics_30_100_',itz_input,'_wr0.csv')), show_col_types=F))
epids <- sampled_epids[simulation_cal_year <= years_of_analysis & simulation_index <= simulations]
ageing_date <- '01-04'
ageing_day <- as.numeric(substr(ageing_date, 1, 2))
ageing_month <- as.numeric(substr(ageing_date, 4, 5))

infs_out <- data.table()

for(iso3c_input in isos){
  
  iso_time <- Sys.time()
  print(iso3c_input)
  
  ## VACCINE DATA
  cov_matrix <- cov_dt[iso3c==iso3c_input]
  vacc_calendar_start <- '01-10'
  vacc_calendar_weeks <- 12
  
  vaccine_programs <- fcn_vacc_prog_mmgh(
    cov_matrix,
    vacc_calendar_start,
    vacc_calendar_weeks
  )
  
  ## EPIDEMIC DATA
  matching_function <- function(epid, hemisphere){
    if(hemisphere=='NH'){
      vec <- epid$N_A_match
      vec[which(epid$strain=='B')] <- epid$N_B_match[which(epid$strain=='B')]
    }else{
      vec <- epid$S_A_match
      vec[which(epid$strain=='B')] <- epid$S_B_match[which(epid$strain=='B')]
    }
    vec
  }
  
  epid_dt <- epids %>% select(simulation_index, sus, trans, contains('match'), strain, day, month, year, simulation_cal_year,
                              pushback, init_ageing_date, init_nye, r0) %>% 
    rename(susceptibility=sus, transmissibility=trans, r0_to_scale=r0) %>% mutate(strain = substr(strain,5,5)) 
  epid_dt$match <- matching_function(epid_dt, hemisphere_input)
  
  epid_dt <- epid_dt %>% select(!c(strain,contains('_match'))) %>% 
    mutate(start_date_late = as.Date(paste0(as.numeric(day), '-', as.numeric(month), '-', 
                                            (start_year_of_analysis + simulation_cal_year - 1)), '%d-%m-%Y'),
           original_date = as.Date(paste0(as.numeric(day), '-', month, '-', year), '%d-%m-%Y'),
           ageing_year_start = case_when(month(start_date_late) < ageing_month ~ start_year_of_analysis + simulation_cal_year - 2,
                                         (month(start_date_late) = ageing_month) & (day(start_date_late) < ageing_day) ~ start_year_of_analysis + simulation_cal_year - 2,
                                         T ~ start_year_of_analysis + simulation_cal_year - 1)) %>% 
    mutate(epid_start_date = case_when(!is.na(pushback) ~ start_date_late - pushback,
                                       is.na(pushback) ~ case_when(is.na(init_ageing_date) ~ as.Date(paste0('01-01-', start_year_of_analysis), format = '%d-%m-%Y'),
                                                                   !is.na(init_ageing_date) ~ as.Date(paste0(ageing_date,  '-', ageing_year_start), format = '%d-%m-%Y')))) %>% 
    mutate(initial_infected = case_when(!is.na(pushback) ~ 10,
                                        is.na(pushback) ~ case_when(is.na(init_ageing_date) ~ init_nye,
                                                                    !is.na(init_ageing_date) ~ init_ageing_date)),
           epid_start_date = last_monday(epid_start_date)) %>% 
    select(!c(pushback,init_ageing_date,init_nye,day,month,year,simulation_cal_year)) %>% 
    mutate(period_start_date = as.Date(paste0('01-01-',start_year_of_analysis),format='%d-%m-%Y'), end_date = as.Date(paste0('01-01-',start_year_of_analysis + years_of_analysis),format='%d-%m-%Y'))
  
  
  #### RUN OUTPUTS #### 
  ## (parallelised across all vaccine types) ##
  
  infs_rds_list <- mclapply(1:length(vaccine_programs), flu_parallel, mc.cores=length(vaccine_programs))
  infs_dt <- rbindlist(infs_rds_list)
  infs_dt[, iso3c := iso3c_input]
  
  if(nrow(infs_out)==0){
    infs_out <- infs_dt
  }else{
    infs_out <- rbind(infs_out, infs_dt)
  }
  
  print(round(Sys.time() - iso_time, 2))
}

#### SAVE OUTPUTS ####

saveRDS(infs_out, file = here::here('output','data','epi',paste0(itz_input),paste0('vacc_',itz_input,'.rds')))

