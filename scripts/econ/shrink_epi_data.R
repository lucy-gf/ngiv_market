#### Condensing epi data ####

nosync <- T

# loading MMGH data
intro_years <- data.table(read_csv(here::here('data','MMGH','intro_years.csv'), show_col_types = F))
country_specs <- data.table(read.xlsx(here::here('data','MMGH','country_specs.xlsx')))
intro_years[, income_g := case_when(
  grepl('High', income_g) ~ 'HIC',
  grepl('Upper', income_g) ~ 'UMIC',
  grepl('Lower', income_g) ~ 'LMIC',
  grepl('Low inc', income_g) ~ 'LIC',
)]
setnames(intro_years, 'vacc_scenario', 'vacc_type')
intro_years <- rbind(intro_years, data.table(
  income_g = unique(intro_years$income_g),
  vacc_type = '0', intro_year = 2025
))

total_epi <- data.table()

for(itz in c('ARG','AUS','CAN','CHN','GBR','GHA','TUR')){
  print(paste0(itz))
  if(file.exists(paste0('output/data/epi/rds_output',paste0(ifelse(nosync, '/no-sync.nosync','')),'/vacc_',itz,'.rds'))){
    print(paste0(itz, ' exists'))
    # read in data
    infs_out <- readRDS(paste0('output/data/epi/rds_output',paste0(ifelse(nosync, '/no-sync.nosync','')),'/vacc_',itz,'.rds'))
    # check this is an integer (n countries in data.table)
    print(nrow(infs_out)/(6*100*length(unique(infs_out$time))))
    infs_out[, tot := I1 + I2 + I3 + I4]
    # remove NAs if any (shouldn't be)
    isos_na <- infs_out %>% group_by(iso3c) %>% summarise(sum = sum(tot)) %>% filter(is.na(sum)) %>% select(iso3c)
    if(nrow(isos_na)>0){print(isos_na$iso3c)}
    infs_out <- infs_out[! iso3c %in% isos_na$iso3c]
    
    # sum over weeks in each year
    infs_annual <- infs_out[, !'tot']
    infs_annual[, year := year(time)][, time := NULL]
    infs_annual <- infs_annual[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','year')]
    
    # add procurement details, intro years, etc.
    infs_annual <- infs_annual[country_specs[iso3c %in% infs_annual$iso3c, c('iso3c','income_g')], on = 'iso3c']
    infs_annual <- infs_annual[intro_years[income_g %in% infs_annual$income_g], on = c('vacc_type','income_g')]
    
    # merge data
    total_epi <- rbind(total_epi, infs_annual)
  }
  ## then do the same for no_vacc data
  if(file.exists(paste0('output/data/epi/rds_output',paste0(ifelse(nosync, '/no-sync.nosync','')),'/vacc_',itz,'_novacc.rds'))){
    print(paste0(itz, '_novacc exists'))
    infs_out <- readRDS(paste0('output/data/epi/rds_output',paste0(ifelse(nosync, '/no-sync.nosync','')),'/vacc_',itz,'_novacc.rds'))
    print(nrow(infs_out)/(1*100*length(unique(infs_out$time))))
    infs_out[, vacc_type := 'no_vacc'] 
    infs_out[, tot := I1 + I2 + I3 + I4]
    isos_na <- infs_out %>% group_by(iso3c) %>% summarise(sum = sum(tot)) %>% filter(is.na(sum)) %>% select(iso3c)
    if(nrow(isos_na)>0){print(isos_na$iso3c)}
    infs_out <- infs_out[! iso3c %in% isos_na$iso3c]
    
    infs_annual <- infs_out[, !'tot']
    infs_annual[, year := year(time)][, time := NULL]
    infs_annual <- infs_annual[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','year')]
    
    infs_annual <- infs_annual[country_specs[iso3c %in% infs_annual$iso3c, c('iso3c','income_g')], on = 'iso3c']
    infs_annual[,intro_year := 2025]
    
    total_epi <- rbind(total_epi, infs_annual)
  }
}

print(paste0(length(unique(total_epi$iso3c)), ' countries, ', round(100*length(unique(total_epi$iso3c))/178, 2), '% of global'))

# save merged data
write_rds(total_epi, here::here('output','data','epi','rds_output','vacc_global.rds'))




