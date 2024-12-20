#### Epidemic simulations ####

### if using vaccines, will only take one ITZ input at a time,
### if using no_vacc, will be parallelised over all ITZs

fcn_parallel_itz <- function(itz_input){
  
# using:
# coverage/demand assumptions (from MMGH)
# epi data (from previous work, ITZ-specific)

if(vaccine_variable == 'coverage'){
  print('Not set up for coverage yet')
} 

## set up next_gen_flu code
source(here::here('next_gen_flu','flu_parallel.R'))

# set hemisphere and countries in ITZ
hemisphere_input <<- demand_input[cluster_code==itz_input]$hemisphere[1]
isos <<- unique(demand_input[cluster_code==itz_input]$iso3c)

# get epidemics 
sampled_epids <<- data.table(read_csv(here::here('data','epi','sampled_epids',paste0('sampled_epidemics_30_100_',itz_input,'_wr0.csv')), show_col_types=F))
epids <<- sampled_epids[simulation_cal_year <= years_of_analysis & simulation_index <= simulations]
ageing_date <<- ifelse(hemisphere_input=='NH', key_dates[1], key_dates[2])
ageing_day <<- as.numeric(substr(ageing_date, 1, 2))
ageing_month <<- as.numeric(substr(ageing_date, 4, 5))

# if using no_vacc, manually make vacc_type_list
if(vacc_coverage == 'NO'){
  vacc_type_list <<- list(vacc_type_list[[1]])
  names(vacc_type_list) <<- 'no_vacc'
  vacc_type_list[[1]]$VE <<- c(0,0,0,0)
}

infs_out <<- data.table()

# run in a loop over all countries in ITZ
for(iso3c_input in isos){
  
  itz_input <<- itz_input
  iso3c_input <<- iso3c_input
  
  iso_time <<- Sys.time()
  print(iso3c_input)
  
  printed <<- F
  
  ## VACCINE DATA
  doses <<- demand_input[iso3c==iso3c_input]
  vacc_calendar_start <<- ifelse(hemisphere_input=='NH', key_dates[2], key_dates[1])
  
  ## EPIDEMIC DATA
  matching_function <<- function(epid, hemisphere){
    if(hemisphere=='NH'){
      vec <- epid$N_A_match
      vec[which(epid$strain=='B')] <- epid$N_B_match[which(epid$strain=='B')]
    }else{
      vec <- epid$S_A_match
      vec[which(epid$strain=='B')] <- epid$S_B_match[which(epid$strain=='B')]
    }
    vec
  }
  
  # set up epidemic datatable for running simulations (e.g. add matching, epidemic start dates, initial infected)
  epid_dt <<- epids %>% select(simulation_index, sus, trans, contains('match'), strain, day, month, year, simulation_cal_year,
                              pushback, init_ageing_date, init_nye, r0) %>% 
    rename(susceptibility=sus, transmissibility=trans, r0_to_scale=r0) %>% mutate(strain = substr(strain,5,5)) 
  epid_dt$match <<- matching_function(epid_dt, hemisphere_input)
  
  epid_dt <<- epid_dt %>% select(!c(strain,contains('_match'))) %>% 
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
  if(vacc_coverage=='NO'){
    infs_dt <- flu_parallel(1)
  }else{
    infs_rds_list <- mclapply(1:length(vacc_type_list), flu_parallel, mc.cores=length(vacc_type_list))
    infs_dt <- rbindlist(infs_rds_list)
  }
  infs_dt[, iso3c := iso3c_input]
  
  if(nrow(infs_dt[!complete.cases(infs_dt)]) > 0){
    print('NA values found') # shouldn't happen
  }
  
  # merge outputs
  if(nrow(infs_out)==0){
    infs_out <- infs_dt
  }else{
    infs_out <- rbind(infs_out, infs_dt)
  }
  
  print(round(Sys.time() - iso_time, 2))
}

#### SAVE OUTPUTS ####

saveRDS(infs_out, file = here::here('output','data','epi','rds_output',paste0('vacc_',itz_input,ifelse(vacc_coverage=='NO','_novacc',''),'.rds')))

}


