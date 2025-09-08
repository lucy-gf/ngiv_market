
### ECON PLOTS ###
options(scipen=9999)

SA_option <- 0
WTP_choice <- ifelse(SA_option %in% 1:2, 'gdp','lancet'); WTP_GDP_ratio <- c(1, 0.3)[SA_option] # proportion of GDP per capita for the willingness_to_pay threshold
discount_SA <- ifelse(SA_option == 3, T, F)
scenario_name <- 'base'
price_used <- 'lower'

econ_folder_name <- paste0(ifelse(disease_modification, '_disease_mod',''),
                           ifelse(outp_include, '_outpatient',''),
                           ifelse((WTP_choice=='gdp'), '_gdp_',''),
                           ifelse((WTP_choice=='gdp'), WTP_GDP_ratio,''),
                           ifelse(discount_SA, '_discount0', ''),
                           ifelse(price_used != 'midpoint', paste0('_doseprice_',price_used), ''))

comparator <- c('no_vacc','0')[2] # which vaccine scenario is the comparator?
# no vaccination or current seasonal vaccines
comparator_name <- case_when(
  comparator == 'no_vacc' ~ 'no vaccination',
  comparator == '0' ~ 'current seasonal vaccines'
)

if(!dir.exists(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator))){
  dir.create(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator))
}
if(!dir.exists(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),comparator))){
  dir.create(here::here('output','data','econ',paste0(scenario_name, econ_folder_name),comparator))
}

# read in data that was made in INMB_outputs.R
econ_inmb <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_inmb.rds'))
econ_nmb2 <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_nmb2.rds'))
econ_nmb_meds_w <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_nmb_meds_w.rds'))
econ_inmb_meds_w <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_inmb_meds_w.rds'))
threshold_prices <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs',paste0('threshold_prices_',comparator,'.rds')))
threshold_prices_meas_w <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('threshold_prices_meas_w_',comparator,'.rds')))

# countries with median CE > 0 for lower pricepoint
lower_ce <- data.table(read_csv(here::here('output','data','econ','base_doseprice_lower','0','table4_0.csv'), show_col_types = F))
upper_ce <- data.table(read_csv(here::here('output','data','econ','base_doseprice_upper','0','table4_0.csv'), show_col_types = F))

## AVERTED HEALTH OUTCOMES

epi_out <- readRDS(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('incr_epi_outcomes',comparator,'_100sim.rds')))

# dalys averted by region
tmp <- epi_out[,c('simulation_index','iso3c','vacc_type','WHOREGION','incr_total_DALYs')]
tmp <- rbind(cbind(tmp, filter = 'none'),
             cbind(tmp, filter = 'only_ce_lower'),
             cbind(tmp, filter = 'only_ce_upper'))
tmp <- tmp %>% left_join(lower_ce[, c('vacc_type','iso3c','country')], by = c('vacc_type','iso3c')) %>% 
  filter(!(filter=='only_ce_lower' & is.na(country))) %>% select(!country)
tmp <- tmp %>% left_join(upper_ce[, c('vacc_type','iso3c','country')], by = c('vacc_type','iso3c')) %>% 
  filter(!(filter=='only_ce_upper' & is.na(country)))  %>% select(!country)

tmp[, iso3c := NULL]
tmp <- tmp[, lapply(.SD, sum), by=c('simulation_index','vacc_type','WHOREGION','filter')]
#doing this manually with separately calculated total pop
tmp[WHOREGION=="SEAR", pop := 2200430685]
tmp[WHOREGION=="WPR",  pop := 1822232144]
tmp[WHOREGION=="AMR",  pop := 1053543379]
tmp[WHOREGION=="AFR",  pop := 1582549661]
tmp[WHOREGION=="EUR",  pop := 896050485]
tmp[WHOREGION=="EMR",  pop := 927470600]

tmp <- dt_to_meas(tmp, cols = c('vacc_type','WHOREGION','filter'))
tmp_wide <- dcast(tmp, vacc_type + WHOREGION + pop + filter ~ measure, value.var = 'incr_total_DALYs')
tmp_wide <- tmp_wide %>% 
  complete(vacc_type, nesting(WHOREGION, pop), filter, 
           fill = list(eti95L = 0, eti95U = 0, median = 0))

panel1a <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median/1e6,fill=as.factor(vacc_type), col=as.factor(vacc_type), group = filter)) +
  geom_point(aes(shape = filter), position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(ymin = eti95L/1e6, ymax = eti95U/1e6), position = position_dodge(width = 0.9), width = 0.5) +
  ylab('DALYs averted (millions)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  scale_shape_manual(values = c(16,15,17), labels = c('All countries','Only CE at lower\nprice point', 'Only CE at upper\nprice point')) +
  guides(color = 'none', fill = 'none') + labs(shape = '') +
  theme(text=element_text(size=16), 
        # legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel1a

# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_DALYs_averted',comparator,'.png')),
#        width=30,height=10,units="cm")

panel1b <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median*1e5/pop,fill=as.factor(vacc_type), col=as.factor(vacc_type), group = filter)) +
  geom_point(aes(shape = filter), position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(ymin = eti95L*1e5/pop, ymax = eti95U*1e5/pop), width = 0.5, position = position_dodge(width = 0.9)) +
  ylab('DALYs averted per 100,000 population') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  scale_shape_manual(values = c(16,15,17), labels = c('All countries','Only CE at lower\nprice point', 'Only CE at upper\nprice point')) +
  guides(color = 'none', fill = 'none') + labs(shape = '') +
  theme(text=element_text(size=16), 
        # legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel1b

# deaths averted by region
tmp <- epi_out[,c('simulation_index','iso3c','vacc_type','WHOREGION','incr_deaths')]
tmp <- rbind(cbind(tmp, filter = 'none'),
             cbind(tmp, filter = 'only_ce_lower'),
             cbind(tmp, filter = 'only_ce_upper'))
tmp <- tmp %>% left_join(lower_ce[, c('vacc_type','iso3c','country')], by = c('vacc_type','iso3c')) %>% 
  filter(!(filter=='only_ce_lower' & is.na(country))) %>% select(!country)
tmp <- tmp %>% left_join(upper_ce[, c('vacc_type','iso3c','country')], by = c('vacc_type','iso3c')) %>% 
  filter(!(filter=='only_ce_upper' & is.na(country)))  %>% select(!country)

tmp[, iso3c := NULL]
tmp <- tmp[, lapply(.SD, sum), by=c('simulation_index','vacc_type','WHOREGION','filter')]
#doing this manually with separately calculated total pop
tmp[WHOREGION=="SEAR", pop := 2200430685]
tmp[WHOREGION=="WPR",  pop := 1822232144]
tmp[WHOREGION=="AMR",  pop := 1053543379]
tmp[WHOREGION=="AFR",  pop := 1582549661]
tmp[WHOREGION=="EUR",  pop := 896050485]
tmp[WHOREGION=="EMR",  pop := 927470600]

tmp <- dt_to_meas(tmp, cols = c('vacc_type','WHOREGION','filter'))
tmp_wide <- dcast(tmp, vacc_type + WHOREGION + pop + filter ~ measure, value.var = 'incr_deaths')
tmp_wide <- tmp_wide %>% 
  complete(vacc_type, nesting(WHOREGION, pop), filter, 
           fill = list(eti95L = 0, eti95U = 0, median = 0))

panel2a <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median/1e6,fill=as.factor(vacc_type), col=as.factor(vacc_type), group = filter)) +
  geom_point(aes(shape = filter), position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(ymin = eti95L/1e6, ymax = eti95U/1e6), position = position_dodge(width = 0.9), width = 0.5) +
  ylab('Total deaths averted (millions)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  scale_shape_manual(values = c(16,15,17), labels = c('All countries','Only CE at lower\nprice point', 'Only CE at upper\nprice point')) +
  guides(color = 'none', fill = 'none') + labs(shape = '') +
  theme(text=element_text(size=16), 
        # legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel2a

# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_deaths_averted',comparator,'.png')),
#        width=30,height=10,units="cm")

panel2b <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median*1e5/pop,fill=as.factor(vacc_type), col=as.factor(vacc_type), group = filter)) +
  geom_point(aes(shape = filter), position = position_dodge(width = 0.9), size = 3) +
  geom_errorbar(aes(ymin = eti95L*1e5/pop, ymax = eti95U*1e5/pop), width = 0.5, position = position_dodge(width = 0.9)) +
  ylab('Deaths averted per 100,000 population') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  scale_shape_manual(values = c(16,15,17), labels = c('All countries','Only CE at lower\nprice point', 'Only CE at upper\nprice point')) +
  guides(color = 'none', fill = 'none') + labs(shape = '') +
  theme(text=element_text(size=16), 
        # legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel2b

comb_fig <- patchwork::wrap_plots(panel1a,panel1b,panel2a,panel2b, ncol=2, guides = 'collect'); comb_fig
ggsave(here::here('output','figures','econ',paste0('WHO_region_Deaths_and_DALYs_averted',comparator,'.png')),
      plot=comb_fig,width=60,height=30,units="cm")

comb_fig <- patchwork::wrap_plots(panel1a,panel2a, ncol=1, guides = 'collect'); comb_fig
ggsave(here::here('output','figures','econ',paste0('WHO_region_Deaths_and_DALYs_averted',comparator,'.png')),
       plot=comb_fig,width=40,height=30,units="cm")


## total influenza infections averted ##

cat('\nScenario: ', scenario_name, econ_folder_name, ', comparator = ', comparator, '\n', sep='')

millions <- function(x){round(x/1e6, 1)}
billions <- function(x){round(x/1e9, 1)}

tmp <- epi_out[,c('simulation_index','iso3c','vacc_type','incr_infections','incr_total_DALYs','incr_deaths')]
tmp <- rbind(cbind(tmp, filter = 'none'),
             cbind(tmp, filter = 'only_ce_lower'),
             cbind(tmp, filter = 'only_ce_upper'))
tmp <- tmp %>% left_join(lower_ce[, c('vacc_type','iso3c','country')], by = c('vacc_type','iso3c')) %>% 
  filter(!(filter=='only_ce_lower' & is.na(country))) %>% select(!country)
tmp <- tmp %>% left_join(upper_ce[, c('vacc_type','iso3c','country')], by = c('vacc_type','iso3c')) %>% 
  filter(!(filter=='only_ce_upper' & is.na(country))) %>% select(! c(country, iso3c))

epi_print <- tmp[, lapply(.SD, sum), by = c('vacc_type','simulation_index','filter')]
epi_print <- dt_to_meas(epi_print, cols = c('vacc_type', 'filter'))
epi_print_w <- dcast(epi_print, vacc_type + filter ~ measure, value.var = c('incr_infections','incr_total_DALYs','incr_deaths'))
epi_print_w[, inf_av := paste0(billions(incr_infections_median), ' billion (95% CI: ', billions(incr_infections_eti95L), ' - ', billions(incr_infections_eti95U), ' billion, vaccine ', vacc_type,')')]
epi_print_w[, death_av := paste0(millions(incr_deaths_median), ' million (95% CI: ', millions(incr_deaths_eti95L), ' - ', millions(incr_deaths_eti95U), ' million)')]
epi_print_w[, daly_av := paste0(millions(incr_total_DALYs_median), ' million (95% CI: ', millions(incr_total_DALYs_eti95L), ' - ', millions(incr_total_DALYs_eti95U), ' million)')]
epi_print_w[, death_av_l := paste0(millions(incr_deaths_median), ' million (95% CI: ', millions(incr_deaths_eti95L), ' - ', millions(incr_deaths_eti95U), ' million, vaccine ', vacc_type, ')')]
epi_print_w[, daly_av_l := paste0(millions(incr_total_DALYs_median), ' million (95% CI: ', millions(incr_total_DALYs_eti95L), ' - ', millions(incr_total_DALYs_eti95U), ' million, vaccine ', vacc_type, ')')]

print_none_inf_min <- which.min(epi_print_w[filter=='none']$incr_infections_median);print_none_inf_max <- which.max(epi_print_w[filter=='none']$incr_infections_median)
print_none_death_min <- which.min(epi_print_w[filter=='none']$incr_deaths_median);print_none_death_max <- which.max(epi_print_w[filter=='none']$incr_deaths_median)
print_none_daly_min <- which.min(epi_print_w[filter=='none']$incr_total_DALYs_median);print_none_daly_max <- which.max(epi_print_w[filter=='none']$incr_total_DALYs_median)

cat('Compared to ', comparator_name, ', improved influenza vaccines were projected to prevent between ', epi_print_w[filter=='none']$inf_av[print_none_inf_min], 
    ' and ', epi_print_w[filter=='none']$inf_av[print_none_inf_max],
    ' additional influenza infections globally between 2025 and 2050. This was estimated to prevent between ', 
    epi_print_w[filter=='none']$death_av[print_none_death_min], 
    ' and ', epi_print_w[filter=='none']$death_av[print_none_death_max],
    ' extra deaths due to influenza, and avert between ', epi_print_w[filter=='none']$daly_av[print_none_daly_min], 
    ' and ', epi_print_w[filter=='none']$daly_av[print_none_daly_max],
    ' additional DALYs (figures from vaccines ',
    unique(epi_print_w[filter=='none']$vacc_type[print_none_death_min], epi_print_w[filter=='none']$vacc_type[print_none_daly_min]),
    ' and ', unique(epi_print_w[filter=='none']$vacc_type[print_none_death_max], epi_print_w[filter=='none']$vacc_type[print_none_daly_max]),
    ', respectively).',
    sep = '')

# print_low_inf_min <- which.min(epi_print_w[filter=='only_ce_lower']$incr_infections_median);print_low_inf_max <- which.max(epi_print_w[filter=='only_ce_lower']$incr_infections_median)
# print_low_death_min <- which.min(epi_print_w[filter=='only_ce_lower']$incr_deaths_median);print_low_death_max <- which.max(epi_print_w[filter=='only_ce_lower']$incr_deaths_median)
# print_low_daly_min <- which.min(epi_print_w[filter=='only_ce_lower']$incr_total_DALYs_median);print_low_daly_max <- which.max(epi_print_w[filter=='only_ce_lower']$incr_total_DALYs_median)
# 
# cat('Compared to ', comparator_name, ', improved influenza vaccines were projected to prevent between ', epi_print_w[filter=='only_ce_lower']$inf_av[print_low_inf_min], 
#     ' and ', epi_print_w[filter=='only_ce_lower']$inf_av[print_low_inf_max],
#     ' additional influenza infections globally between 2025 and 2050. This was estimated to prevent between ', 
#     epi_print_w[filter=='only_ce_lower']$death_av[print_low_death_min], 
#     ' and ', epi_print_w[filter=='only_ce_lower']$death_av[print_low_death_max],
#     ' extra deaths due to influenza, and avert between ', epi_print_w[filter=='only_ce_lower']$daly_av[print_low_daly_min], 
#     ' and ', epi_print_w[filter=='only_ce_lower']$daly_av[print_low_daly_max],
#     ' additional DALYs (figures from vaccines ',
#     unique(epi_print_w[filter=='only_ce_lower']$vacc_type[print_low_death_min], epi_print_w[filter=='only_ce_lower']$vacc_type[print_low_daly_min]),
#     ' and ', unique(epi_print_w[filter=='only_ce_lower']$vacc_type[print_low_death_max], epi_print_w[filter=='only_ce_lower']$vacc_type[print_low_daly_max]),
#     ', respectively).',
#     sep = '')









