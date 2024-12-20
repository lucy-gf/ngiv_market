
### ECON PLOTS ###
options(scipen=1000000)

scenario_name <- 'base'
econ_folder_name <- '' # change this if looking at a sensitivity analysis

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

# boxplot of INMBs for each vaccine type in select countries
ggplot(econ_inmb[iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
  geom_boxplot(aes(x=vacc_type, y=inmb/1e9, fill=vacc_type)) + 
  facet_wrap(names~., scales='free',nrow=2) + theme_bw() +
  scale_fill_manual(values = vtn_colors) + xlab('') +
  labs(fill='Vaccine type') +
  geom_hline(yintercept=0, lty=2) +
  ylab('Incremental net monetary benefit ($2022, billions)') +
  ggtitle(paste0(scenario_name,econ_folder_name))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('example_INMBs_',comparator,'.png')),
       width=30,height=20,units="cm")

# incremental cost-benefit plane in select countries - scatter
ggplot(econ_nmb2[!vacc_type==comparator & iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
  geom_point(aes(x=DALYs_averted/1e6, y=(-cost_saved)/1e9, col=vacc_type)) +
  facet_wrap(names~., scales='free',nrow=2) + theme_bw() +
  scale_color_manual(values = vtn_colors) + labs(col='Vaccine type') +
  xlab('DALYs averted (millions)') + ylab('Additional cost incurred ($2022, billions)') +
  ggtitle(paste0(scenario_name,econ_folder_name))
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('example_planes_point_',comparator,'.png')),
       width=30,height=20,units="cm")

# incremental cost-benefit plane in select countries - intervals
ggplot(econ_nmb_meds_w[!vacc_type == comparator & iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
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
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('example_planes_',comparator,'.png')),
       width=30,height=20,units="cm")

ggplot(econ_inmb_meds_w) + 
  geom_errorbar(aes(gdpcap,ymin=eti95L,ymax=eti95U,col=vacc_type)) +
  geom_point(aes(gdpcap,y=median,col=vacc_type)) + 
  # scale_y_log10() +
  scale_x_log10() + xlab('GDP per capita') + 
  ylab('Incremental net monetary benefit') + 
  scale_color_manual(values = vtn_colors) + labs(col='Vaccine type') +
  facet_grid(vacc_type~., scales='free') + theme_bw() 

## ALL COUNTRY PLOTS ##

zero_val <- min(econ_inmb_meds_w[median > 0]$median, econ_inmb_meds_w[eti95L > 0]$eti95L, 1e5)

# plot INMBs wrt GDP per capita on a log-log scale, cutting off any countries where INMB < 0
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
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('global_INMBs_',comparator,'.png')),
       width=30,height=20,units="cm")

zero_val <- min(-econ_inmb_meds_w[median < 0]$median, -econ_inmb_meds_w[eti95U < 0]$eti95L, 1e5)

# doing the same but for *negative* INMBs, flipping the axes
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
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('global_INMBs_negative_',comparator,'.png')),
       width=30,height=20,units="cm")













