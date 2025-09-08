
### ECON PLOTS ###
options(scipen=1000000)

scenario_name <- 'base'
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

# ggplot(econ_inmb_meds_w) + 
#   geom_boxplot(aes(x = vacc_type, y = median/gdpcap, fill=vacc_type, col = vacc_type)) +
#   geom_boxplot(aes(x = vacc_type, y = median/gdpcap, fill=vacc_type), outlier.shape = NA) +
#   # geom_point(aes(x = vacc_type, y = median, col=vacc_type)) + 
#   xlab('Vaccine type') + 
#   ylab('Incremental net monetary benefit') + 
#   scale_fill_manual(values = vtn_colors) + 
#   scale_color_manual(values = vtn_colors) +
#   labs(col='Vaccine type') +
#   scale_y_continuous(transform = 'pseudo_log') + 
#   facet_wrap(.~income_g, scales='fixed', nrow=1) + theme_bw() +
#   theme(legend.position = 'none')

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

#NNV for no-vacc scenarios only
if(comparator=='no_vacc'){
  
  nnv_tot <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','nnv_tot.rds'))
  global_nnv <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','global_nnv.rds'))
  
  # plot global nnv
  ggplot() +
    geom_point(data=global_nnv,
               aes(x=vacc_type, y=nnv_inf, col=as.factor(vacc_type)),
               position=position_jitterdodge(dodge.width=0.9,jitter.width=2.5), shape=4, alpha=0.7) +
    theme_bw() + ylab('Number needed to vaccinate') +
    scale_color_manual(values=vtn_colors) +
    labs(col = 'Vaccine type') +
    scale_y_log10(breaks=c(0.1,0.3,1,3,10), labels=c(0.1,0.3,1,3,10), limits=c(0.08,10)) +
    xlab('Vaccine type') +
    theme(text=element_text(size=14),
          legend.position='none')
  
  ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('global_nnv_inf_',comparator,'.png')),
         width=30,height=20,units="cm")
  
  # nnv by WHO region
  ggplot() +
    geom_boxplot(data=nnv_tot[!vacc_type=="0",],
                 aes(x=country, y=nnv_inf, col=as.factor(vacc_type)),
                 shape=4, alpha=0.7, outliers=FALSE) +
    theme_bw() + ylab('Number needed to vaccinate') +
    scale_color_manual(values=vtn_colors) +
    labs(col = 'Vaccine type') +
    scale_y_log10(breaks=c(0.1,0.3,1,3,10,30), labels=c(0.1,0.3,1,3,10,30), limits=c(0.08,300)) +
    xlab('Country') +
    facet_grid(vacc_type~WHOREGION, scales = "free_x") +
    theme(text=element_text(size=14),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('country_nnv_inf_',comparator,'.png')),
         width=30,height=20,units="cm")
}

## THRESHOLD PRICES

# boxplot of threshold prices for each vaccine type in select countries
ggplot(threshold_prices[iso3c%in% c('GBR','USA','CUB','GHA','CHL','SVN','DEU','ARG')]) + 
  geom_boxplot(aes(x=vacc_type, y=threshold_price, fill=vacc_type)) + 
  facet_wrap(country~., scales='free',nrow=2) + theme_bw() +
  scale_fill_manual(values = vtn_colors) + xlab('') +
  labs(fill='Vaccine type') +
  geom_hline(yintercept=0, lty=2) +
  ylab('Threshold Price ($2022)') +
  ggtitle(paste0(scenario_name,econ_folder_name))

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('example_Threshhold_Price_',comparator,'.png')),
       width=30,height=20,units="cm")

# plot vaccine threshold price by country
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

ggplot() +
  geom_pointrange(data=threshold_prices_meas_w[!vacc_type=="0",],
               aes(x=gdpcap,y=mean,ymin=eti95L,ymax=eti95U,col=as.factor(vacc_type)),shape=4) +
  theme_bw() + ylab('Threshold Price (USD)') +
  scale_color_manual(values=vtn_colors) +
  labs(col = 'Vaccine type') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  # scale_y_continuous(trans="S_sqrt",breaks=c(-100,-10,0,10,100,500,1000), labels=c(-100,-10,0,10,100,500,1000), limits=c(-100,1000)) +
  # scale_y_log10(breaks=c(0.1,1,10,100,1000), labels=c(0.1,1,10,100,1000), limits=c(0,1000)) +
  xlab('Countries by GDP per Capita') +
  facet_grid(.~vacc_type) +
  theme(text=element_text(size=14),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('country_threshold_price',comparator,'.png')),
       width=30,height=20,units="cm")

# alternative plot vaccine threshold price by country
# zero_val <- min(threshold_prices_meas_w[median > 0]$median, threshold_prices_meas_w[eti95L > 0]$eti95L)
zero_val = 0.1

thresh_national <- ggplot() +
  geom_hline(yintercept = zero_val/1e0, lty=2) + 
  geom_segment(data = threshold_prices_meas_w[eti95L>zero_val],
               aes(x=gdpcap, xend=gdpcap, y=eti95L/1e0, yend=eti95U/1e0,
                   color=WHOREGION),alpha=0.4) +
  geom_segment(data = threshold_prices_meas_w[mean>zero_val & eti95L<=zero_val],
               aes(x=gdpcap, xend=gdpcap, y=mean/1e0, yend=eti95U/1e0,
                   color=WHOREGION),alpha=0.4) +
  geom_segment(data = threshold_prices_meas_w[mean>zero_val & eti95L<=zero_val],
               aes(x=gdpcap, xend=gdpcap, y=zero_val/1e0, yend=eti95U/1e0,
                   color=WHOREGION),alpha=0.25) +
  geom_point(data = threshold_prices_meas_w[mean<=zero_val],
             aes(x=gdpcap, y=zero_val/1e0,
                 color=WHOREGION)) +
  geom_point(data = threshold_prices_meas_w[mean>zero_val],
             aes(x=gdpcap, y=mean/1e0,
                 color=WHOREGION)) +
  ylab('Threshold vaccine price (USD 2022)') + xlab('GDP per capita (USD 2022)') +
  labs(col = 'WHO Region') +
  scale_color_manual(values = WHO_colors, labels=who_region_labs2) +
  scale_y_log10(limits=c(zero_val,3000), breaks=c(zero_val,1,10,100,1000),labels=c('<0.1',1,10,100,1000)) +
  scale_x_log10(limits=c(200,110000), breaks=c(1000,10000,100000)) +
  facet_grid(.~vacc_type, scales='fixed') +
  theme_bw() + theme(text=element_text(size=12)); thresh_national

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('country_threshold_price',comparator,'.png')),
       width=30,height=14,units="cm")


# plot vaccine threshold price by region
# S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
# IS_sqrt <- function(x){x^2*sign(x)}
# S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)
# 
# ggplot() +
#   geom_boxplot(data=threshold_prices[!vacc_type=="0",],
#                aes(x=country, y=threshold_price, group = WHOREGION, col=as.factor(vacc_type)),
#                shape=4, alpha=0.7, outliers=FALSE) +
#   theme_bw() + ylab('Threshold Price (USD)') +
#   scale_color_manual(values=vtn_colors) +
#   labs(col = 'Vaccine type') +
#   # scale_y_continuous(trans="S_sqrt",breaks=c(-100,-10,0,10,100,500), labels=c(-100,-10,0,10,100,500), limits=c(-100,750)) +
#   # scale_y_log10(breaks=c(0.1,0.3,1,3,10,30), labels=c(0.1,0.3,1,3,10,30), limits=c(0.08,300)) +
#   xlab('Country') +
#   facet_grid(vacc_type~., scales = "free_x") +
#   theme(text=element_text(size=14),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_threshold_price',comparator,'.png')),
#        width=30,height=20,units="cm")

# boxplot of median threshold prices by region
thresh_box <- ggplot(data=threshold_prices_meas_w[!vacc_type=="0",]) +
  geom_boxplot(aes(x=vacc_type, y=mean, fill=as.factor(vacc_type), col=as.factor(vacc_type)), outliers = T) +
  geom_boxplot(aes(x=vacc_type, y=mean), alpha=0, outliers=F) +
  ylab('Threshold price (USD 2022)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='free', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=12), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
        ); thresh_box

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_threshold_price',comparator,'.png')),
       width=30,height=20,units="cm")


thresh_national + thresh_box + 
  plot_layout(nrow = 2) + plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('threshold_patchwork_',comparator,'.png')),
       width=36,height=30,units="cm")

# grouped across region
# ggplot(data=threshold_prices[!vacc_type=="0",]) +
#   geom_boxplot(aes(x=vacc_type, y=threshold_price,
#                    fill=as.factor(vacc_type), col=as.factor(vacc_type))) +
#   geom_boxplot(aes(x=vacc_type, y=threshold_price, fill = NA, alpha=0.6), outliers=F) +
#   ylab('Threshold price (USD)') +
#   scale_fill_manual(values=vtn_colors) +
#   scale_color_manual(values=vtn_colors) +
#   labs(fill = 'Vaccine type', color = 'Vaccine type') +
#   facet_wrap(WHOREGION~., scales='free', nrow=1) +
#   theme_bw() + xlab('Country') +
#   theme(text=element_text(size=14), 
#         legend.position='none',
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())

# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region2_threshold_price',comparator,'.png')),
#        width=30,height=20,units="cm")


## AVERTED HEALTH OUTCOMES

# read data from INMB_outputs.R
epi_out_by_age_meas_w <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('incr_epi_outcomes_by_age',comparator,'.rds')))
epi_out_meas_w  <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('incr_epi_outcomes',comparator,'.rds')))
epi_out <- readRDS(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('incr_epi_outcomes',comparator,'_100sim.rds')))

# dalys averted by region
tmp <- epi_out[,c('simulation_index','vacc_type','WHOREGION','incr_total_DALYs')]
tmp <- tmp[, lapply(.SD, sum), by=c('simulation_index','vacc_type','WHOREGION')]
#doing this manually with separately calculated total pop
tmp[WHOREGION=="SEAR", pop := 2200430685]
tmp[WHOREGION=="WPR",  pop := 1822232144]
tmp[WHOREGION=="AMR",  pop := 1053543379]
tmp[WHOREGION=="AFR",  pop := 1582549661]
tmp[WHOREGION=="EUR",  pop := 896050485]
tmp[WHOREGION=="EMR",  pop := 927470600]

tmp <- dt_to_meas(tmp, cols = c('vacc_type','WHOREGION'))
tmp_wide <- dcast(tmp, vacc_type + WHOREGION + pop ~ measure, value.var = 'incr_total_DALYs')

panel1a <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median/1e6,fill=as.factor(vacc_type), col=as.factor(vacc_type))) +
  geom_bar(position='dodge',stat='identity') +
  geom_errorbar(aes(ymin = eti95L/1e6, ymax = eti95U/1e6), col = 1, width = 0.5) +
  ylab('DALYs averted (millions)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=16), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel1a

# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_DALYs_averted',comparator,'.png')),
#        width=30,height=10,units="cm")

panel1b <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median*1e5/pop,fill=as.factor(vacc_type), col=as.factor(vacc_type))) +
  geom_bar(position='dodge',stat='identity') +
  geom_errorbar(aes(ymin = eti95L*1e5/pop, ymax = eti95U*1e5/pop), col = 1, width = 0.5) +
  ylab('DALYs averted per 100,000 population') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=16), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel1b

# deaths averted by region
tmp <- epi_out[,c('simulation_index','vacc_type','WHOREGION','incr_deaths')]
tmp <- tmp[, lapply(.SD, sum), by=c('simulation_index','vacc_type','WHOREGION')]
#doing this manually with separately calculated total pop
tmp[WHOREGION=="SEAR", pop := 2200430685]
tmp[WHOREGION=="WPR",  pop := 1822232144]
tmp[WHOREGION=="AMR",  pop := 1053543379]
tmp[WHOREGION=="AFR",  pop := 1582549661]
tmp[WHOREGION=="EUR",  pop := 896050485]
tmp[WHOREGION=="EMR",  pop := 927470600]
tmp <- dt_to_meas(tmp, cols = c('vacc_type','WHOREGION'))
tmp_wide <- dcast(tmp, vacc_type + WHOREGION + pop ~ measure, value.var = 'incr_deaths')

panel2a <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median/1e6,fill=as.factor(vacc_type), col=as.factor(vacc_type))) +
  geom_bar(position='dodge',stat='identity') +
  geom_errorbar(aes(ymin = eti95L/1e6, ymax = eti95U/1e6), col = 1, width = 0.5) +
  ylab('Total deaths averted (millions)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=16), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel2a

# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_deaths_averted',comparator,'.png')),
#        width=30,height=10,units="cm")

panel2b <- ggplot(data=tmp_wide, aes(x=vacc_type,y=median*1e5/pop,fill=as.factor(vacc_type), col=as.factor(vacc_type))) +
  geom_bar(position='dodge',stat='identity') +
  geom_errorbar(aes(ymin = eti95L*1e5/pop, ymax = eti95U*1e5/pop), col = 1, width = 0.5) +
  ylab('Deaths averted per 100,000 population') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_wrap(WHOREGION~., scales='fixed', nrow=1, labeller = labeller(WHOREGION=who_region_labs)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=16), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  );panel2b

comb_fig <- gridExtra::grid.arrange(panel1a,panel1b,panel2a,panel2b,ncol=2)
ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('WHO_region_Deaths_and_DALYs_averted',comparator,'.png')),
      plot=comb_fig,width=60,height=30,units="cm")

# # dalys averted by country
# ggplot(data=epi_out_meas_w) +
#   geom_pointrange(aes(x=gdpcap,y=incr_total_DALYs_mean,
#                        ymin=incr_total_DALYs_eti95L, ymax=incr_total_DALYs_eti95U,
#                        col=as.factor(WHOREGION)),alpha=0.4) +
#   ylab('Total DALYs Averted (millions)') +
#   scale_color_manual(values=WHO_colors, labels = who_region_labs2) +
#   labs(fill = 'WHO Region', color = 'WHO Region') +
#   scale_y_log10(limits=c(1000,100000000), breaks=c(1000,10000,100000,1000000,10000000,100000000)) +
#   scale_x_log10(limits=c(200,110000), breaks=c(300,1000,3000,10000,30000,100000)) +
#   facet_wrap(vacc_type~., scales='fixed', ncol=5, labeller = labeller(WHOREGION=who_region_labs)) +
#   theme_bw() + xlab('GDP per Capita') +
#   theme(text=element_text(size=12), 
#         legend.position='bottom',
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()
#   )
# 
# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),comparator,paste0('country_DALYs_averted',comparator,'.png')),
#        width=30,height=20,units="cm")


## total influenza infections averted ##

cat('\nScenario: ', scenario_name, econ_folder_name, ', comparator = ', comparator, '\n', sep='')

millions <- function(x){round(x/1e6, 1)}
billions <- function(x){round(x/1e9, 1)}
epi_print <- epi_out[, c('vacc_type','simulation_index','incr_infections','incr_total_DALYs','incr_deaths')][, lapply(.SD, sum), by = c('vacc_type','simulation_index')]
epi_print <- dt_to_meas(epi_print, cols = 'vacc_type')
epi_print_w <- dcast(epi_print, vacc_type ~ measure, value.var = c('incr_infections','incr_total_DALYs','incr_deaths'))
epi_print_w[, inf_av := paste0(billions(incr_infections_median), ' (95% CI: ', billions(incr_infections_eti95L), ' - ', billions(incr_infections_eti95U), ')')]
epi_print_w[, death_av := paste0(millions(incr_deaths_median), ' (95% CI: ', millions(incr_deaths_eti95L), ' - ', millions(incr_deaths_eti95U), ')')]
epi_print_w[, daly_av := paste0(millions(incr_total_DALYs_median), ' (95% CI: ', millions(incr_total_DALYs_eti95L), ' - ', millions(incr_total_DALYs_eti95U), ')')]

cat('Infections averted:\n')
cat(unlist(epi_print_w$vacc_type), sep = '      ')
cat('\n')
cat(unlist(epi_print_w$inf_av), sep ='  ')
cat('\n')
cat('Deaths averted:\n')
cat(unlist(epi_print_w$vacc_type), sep = '      ')
cat('\n')
cat(unlist(epi_print_w$death_av), sep ='  ')
cat('\n')
cat('DALYs averted:\n')
cat(unlist(epi_print_w$vacc_type), sep = '      ')
cat('\n')
cat(unlist(epi_print_w$daly_av), sep ='  ')
cat('\n')











