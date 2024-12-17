
### ECON PLOTS AND TABLES ###
options(scipen=1000000)

scenario_name <- 'base'
econ_folder_name <- ''

## LOAD DATA ##
econ_cases_agg <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'econ_cases_agg.rds'))
doses <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),'doses.rds'))

## NET MONETARY BENEFIT ##

econ_add <- econ_cases_agg[, c('vacc_type','simulation_index','iso3c','income_g','total_hosp_cost',
                               'discounted_epi_costs','total_DALYs','discounted_DALYs_cost')]
econ_add <- econ_add[, lapply(.SD, sum), by=c('vacc_type','simulation_index','iso3c','income_g')]

doses_adding <- doses[, c('iso3c','simulation_index','vacc_scenario','doses','total_cost','discounted_doses_cost')]
setnames(doses_adding, 'vacc_scenario','vacc_type')
doses_adding <- doses_adding[, lapply(.SD, sum), by = c('iso3c','simulation_index','vacc_type')]

econ_nmb <- econ_add[doses_adding, on = c('iso3c','simulation_index','vacc_type')]

econ_inmb <- econ_nmb[, c('vacc_type','iso3c','income_g','simulation_index','discounted_epi_costs','discounted_DALYs_cost','discounted_doses_cost')]

base_econ <- econ_inmb[vacc_type=='0']
setnames(base_econ, 'discounted_epi_costs','discounted_epi_costs_base')
setnames(base_econ, 'discounted_DALYs_cost','discounted_DALYs_cost_base')
setnames(base_econ, 'discounted_doses_cost','discounted_doses_cost_base')
base_econ[, vacc_type := NULL]
econ_inmb <- econ_inmb[!vacc_type=='0']

econ_inmb <- econ_inmb[base_econ, on=c('iso3c','income_g','simulation_index')]

econ_inmb[, incr_epi_cost := discounted_epi_costs_base - discounted_epi_costs] # saved costs of treatment 
econ_inmb[, incr_DALY_cost := discounted_DALYs_cost_base - discounted_DALYs_cost] # cost of DALYs averted
econ_inmb[, incr_doses_cost := discounted_doses_cost_base - discounted_doses_cost] # saved cost of vaccination (likely negative)
econ_inmb[, inmb := incr_epi_cost + incr_DALY_cost + incr_doses_cost]

econ_inmb[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]
ggplot(econ_inmb[iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
  geom_boxplot(aes(x=vacc_type, y=inmb/1e9, fill=vacc_type)) + 
  facet_wrap(names~., scales='free',nrow=2) + theme_bw() +
  scale_fill_manual(values = vtn_colors) + xlab('') +
  labs(fill='Vaccine type') +
  geom_hline(yintercept=0, lty=2) +
  ylab('Incremental net monetary benefit ($2022, billions)') +
  ggtitle(paste0(scenario_name,econ_folder_name))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),'example_INMBs.png'),
       width=30,height=20,units="cm")

econ_nmb2 <- copy(econ_nmb)
econ_nmb2[, total_cost := total_cost + total_hosp_cost]

econ_nmb2_base <- econ_nmb2[vacc_type=='0']
setnames(econ_nmb2_base, 'total_DALYs','base_total_DALYs')
setnames(econ_nmb2_base, 'total_cost','base_total_cost')
econ_nmb2 <- econ_nmb2[econ_nmb2_base[,c('vacc_type','simulation_index','iso3c','income_g','base_total_DALYs','base_total_cost')], on = c('simulation_index','iso3c','income_g')]
econ_nmb2[, DALYs_averted := base_total_DALYs - total_DALYs]
econ_nmb2[, cost_saved := base_total_cost - total_cost]

econ_nmb2 <- econ_nmb2[, c('vacc_type','iso3c','income_g','total_cost','DALYs_averted','cost_saved')]
econ_nmb2[, names := countrycode(iso3c, destination='country.name',origin='iso3c')]
ggplot(econ_nmb2[!vacc_type=='0' & iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
  geom_point(aes(x=DALYs_averted/1e6, y=(-cost_saved)/1e9, col=vacc_type)) +
  facet_wrap(names~., scales='free',nrow=2) + theme_bw() +
  scale_color_manual(values = vtn_colors) + labs(col='Vaccine type') +
  xlab('DALYs averted (millions)') + ylab('Additional cost incurred ($2022, billions)') +
  ggtitle(paste0(scenario_name,econ_folder_name))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),'example_planes_point.png'),
       width=30,height=20,units="cm")

econ_nmb_meds <- dt_to_meas(econ_nmb2, c('vacc_type','iso3c','names','income_g'))
econ_nmb_meds_w <- dcast(econ_nmb_meds,
                         vacc_type+iso3c+names+income_g~measure, 
                         value.var=c('cost_saved','DALYs_averted'))
ggplot(econ_nmb_meds_w[!vacc_type == '0' & iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
  geom_errorbar(aes(xmin=DALYs_averted_eti95L/1e6, xmax=DALYs_averted_eti95U/1e6,
                    y=(-cost_saved_median)/1e9, col=vacc_type),alpha=0.8) +
  geom_errorbar(aes(x=DALYs_averted_median/1e6, 
                    ymin=(-cost_saved_eti95L)/1e9, 
                    ymax=(-cost_saved_eti95U)/1e9,col=vacc_type),alpha=0.8) +
  geom_point(aes(x=DALYs_averted_median/1e6, y=(-cost_saved_median)/1e9, 
                 col=vacc_type)) +
  facet_wrap(names~., scales='free',nrow=2) + theme_bw() +
  scale_color_manual(values = vtn_colors) + labs(col='Vaccine type') +
  xlab('DALYs averted (millions)') + ylab('Additional cost incurred ($2022, billions)') +
  ggtitle(paste0(scenario_name,econ_folder_name))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),'example_planes.png'),
       width=30,height=20,units="cm")

econ_inmb_meds <- dt_to_meas(econ_inmb, c('vacc_type','iso3c','income_g','names'))
econ_inmb_meds_w <- dcast(econ_inmb_meds[, c('vacc_type','iso3c','income_g','inmb','measure')],
                          vacc_type+iso3c+income_g~measure, value.var='inmb')

econ_inmb_meds_w <- econ_inmb_meds_w[cost_predic_c[simulation_index==1&age_grp==1&iso3c%in%econ_inmb_meds_w$iso3c][,c('iso3c','gdpcap')], on='iso3c']

ggplot(econ_inmb_meds_w) + 
  geom_errorbar(aes(gdpcap,ymin=eti95L,ymax=eti95U,col=vacc_type)) +
  geom_point(aes(gdpcap,y=median,col=vacc_type)) + 
  # scale_y_log10() +
  scale_x_log10() + xlab('GDP per capita') + 
  ylab('Incremental net monetary benefit') + 
  scale_color_manual(values = vtn_colors) + labs(col='Vaccine type') +
  facet_grid(vacc_type~., scales='free') + theme_bw() 

## ALL COUNTRY PLOTS ##

WHO_regions <- data.table(read_csv(here::here('data','econ','WHO_regions.csv'), show_col_types=F))
setnames(WHO_regions, 'country_code','iso3c')
WHO_regions <- WHO_regions[iso3c %in% econ_inmb_meds_w$iso3c]
econ_inmb_meds_w <- econ_inmb_meds_w[WHO_regions, on='iso3c']

zero_val <- min(econ_inmb_meds_w[median > 0]$median, econ_inmb_meds_w[eti95L > 0]$eti95L, 1e5)

ggplot() +
  geom_hline(yintercept = zero_val/1e6, lty=2) + 
  geom_segment(data = econ_inmb_meds_w[eti95L>zero_val],
               aes(x=gdpcap, xend=gdpcap, y=eti95L/1e6, yend=eti95U/1e6,
                   color=WHOREGION),alpha=0.4) +
  geom_segment(data = econ_inmb_meds_w[median>zero_val & eti95L<=zero_val],
               aes(x=gdpcap, xend=gdpcap, y=median/1e6, yend=eti95U/1e6,
                   color=WHOREGION),alpha=0.4) +
  geom_segment(data = econ_inmb_meds_w[median>zero_val & eti95L<=zero_val],
               aes(x=gdpcap, xend=gdpcap, y=zero_val/1e6, yend=eti95U/1e6,
                   color=WHOREGION),alpha=0.25) +
  geom_point(data = econ_inmb_meds_w[median<=zero_val],
             aes(x=gdpcap, y=zero_val/1e6,
                 color=WHOREGION)) +
  geom_point(data = econ_inmb_meds_w[median>zero_val],
             aes(x=gdpcap, y=median/1e6,
                 color=WHOREGION)) +
  ylab('Incremental net monetary benefit ($2022, millions)') + xlab('GDP per capita ($)') +
  labs(col = 'WHO Region') +
  scale_color_manual(values = WHO_colors, labels=who_region_labs2) +
  scale_y_log10(breaks=c(zero_val/1e6,1,10,100,1000,10000,100000),labels=c('< 0',1,10,100,1000,10000,100000)) +
  scale_x_log10(limits=c(200,110000), breaks=c(300,1000,3000,10000,30000,100000)) +
  facet_grid(vacc_type~., scales='fixed') +
  theme_bw() + theme(text=element_text(size=12))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),'global_INMBs.png'),
       width=30,height=20,units="cm")

zero_val <- min(-econ_inmb_meds_w[median < 0]$median, -econ_inmb_meds_w[eti95U < 0]$eti95L, 1e5)

ggplot() +
  geom_hline(yintercept = zero_val/1e6, lty=2) + 
  geom_segment(data = econ_inmb_meds_w[eti95U < -zero_val],
               aes(x=gdpcap, xend=gdpcap, y=-eti95L/1e6, yend=-eti95U/1e6,
                   color=WHOREGION),alpha=0.4) +
  geom_segment(data = econ_inmb_meds_w[median < -zero_val & eti95U >= -zero_val],
               aes(x=gdpcap, xend=gdpcap, y=-median/1e6, yend=-eti95L/1e6,
                   color=WHOREGION),alpha=0.4) +
  geom_segment(data = econ_inmb_meds_w[median < -zero_val & eti95U >= -zero_val],
               aes(x=gdpcap, xend=gdpcap, y=zero_val/1e6, yend=-eti95L/1e6,
                   color=WHOREGION),alpha=0.25) +
  geom_point(data = econ_inmb_meds_w[median >= -zero_val],
             aes(x=gdpcap, y=zero_val/1e6,
                 color=WHOREGION)) +
  geom_point(data = econ_inmb_meds_w[median < -zero_val],
             aes(x=gdpcap, y=-median/1e6,
                 color=WHOREGION)) +
  ylab('Incremental net monetary benefit ($2022, millions)') + xlab('GDP per capita ($)') +
  labs(col = 'WHO Region') +
  scale_color_manual(values = WHO_colors, labels=who_region_labs2) +
  scale_y_log10(breaks=c(zero_val/1e6,0.1,1,10,100,1000,10000,100000),labels=c('> 0',-0.1,-1,-10,-100,-1000,-10000,-100000)) +
  scale_x_log10(limits=c(200,110000), breaks=c(300,1000,3000,10000,30000,100000)) +
  facet_grid(vacc_type~., scales='fixed') +
  theme_bw() + theme(text=element_text(size=12))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),'global_INMBs_negative.png'),
       width=30,height=20,units="cm")


## TABLES ##

# n and % of countries with INMB > 0

n_countries <- econ_inmb_meds_w %>% group_by(vacc_type, WHOREGION) %>% 
  summarise(n_total = n())

tab1 <- econ_inmb_meds_w %>% filter(median > 0) %>% group_by(vacc_type, WHOREGION) %>% 
  summarise(n = n()) %>% right_join(n_countries, by = c('vacc_type', 'WHOREGION')) %>% 
  mutate(n = case_when(is.na(n) ~ 0, T ~ n)) %>% 
  mutate(percentage = round(100*n/n_total, 1))

tab1_global <- tab1 %>% select(vacc_type, n, n_total) %>% 
  group_by(vacc_type) %>% summarise(n = sum(n), n_total = sum(n_total)) %>% 
  mutate(percentage = round(100*n/n_total, 1), WHOREGION = 'Global') %>% 
  select(vacc_type, WHOREGION, n, n_total, percentage)

tab1_save <- tab1 %>% rbind(tab1_global) %>%  
  mutate(positive_INMB = paste0(percentage,'% (', n, '/', n_total, ')')) %>% 
  select(WHOREGION, vacc_type, positive_INMB) %>% arrange(WHOREGION, vacc_type)

write_csv(tab1_save, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'table1.csv'))

# total sum regional INMBs

regional_inmbs <- econ_inmb[WHO_regions, on='iso3c']
regional_inmbs <- regional_inmbs[, c('WHOREGION','vacc_type','simulation_index','inmb')]

global_inmbs <- regional_inmbs[, c('vacc_type','simulation_index','inmb')][, lapply(.SD, sum), by=c('vacc_type','simulation_index')]
global_inmbs[, WHOREGION := 'Global']

regional_inmbs <- regional_inmbs[, lapply(.SD, sum), by=c('WHOREGION','vacc_type','simulation_index')]
regional_inmbs <- rbind(regional_inmbs, global_inmbs)
regional_inmbs <- dt_to_meas(regional_inmbs, c('WHOREGION','vacc_type'))
regional_inmbs_w <- dcast(regional_inmbs, WHOREGION + vacc_type ~ measure, value.var = 'inmb')
regional_inmbs_w[, INMB_millions := paste0(round(median/1e6), ' (',
                                           round(eti95L/1e6), ', ',
                                           round(eti95U/1e6), ')')]

tab2_save <- regional_inmbs_w[, c('WHOREGION','vacc_type','INMB_millions')]

write_csv(tab2_save, here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'table2.csv'))


















