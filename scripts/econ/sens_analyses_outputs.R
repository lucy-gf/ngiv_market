
## MAKING PLOTS/TABLES ACROSS ECONOMIC SENSITIVITY ANALYSES ##

threshold_prices_meas_w <- data.table()
econ_inmb_meds_w <- data.table()
econ_inmb_mean_w <- data.table()
econ_inmb_meds_w_lower <- data.table()
econ_inmb_mean_w_lower <- data.table()
upper_inmbs <- data.table()
lower_inmbs <- data.table()

for(SA_option in 0:3){
  
  WTP_choice <- ifelse(SA_option %in% 1:2, 'gdp','lancet'); WTP_GDP_ratio <- c(1, 0.3)[SA_option] # proportion of GDP per capita for the willingness_to_pay threshold
  discount_SA <- ifelse(SA_option == 3, T, F)
  print(paste0('Sens analysis:', ifelse(SA_option == 0, ' none', ''),
               ifelse(WTP_choice == 'gdp', paste0(' GDP (WTP = ', WTP_GDP_ratio, ' x GDPpc)'), ''),
               ifelse(discount_SA == T, ' discounting DALYs at 0%', '')))
  
  scenario_name <- 'base'
  price_used <- 'upper'
  econ_folder_name <- paste0(ifelse((WTP_choice=='gdp'), '_gdp_',''),
                             ifelse((WTP_choice=='gdp'), WTP_GDP_ratio,''),
                             ifelse(discount_SA, '_discount0', ''),
                             ifelse(price_used != 'midpoint', paste0('_doseprice_',price_used), ''))
  
  threshold_prices_meas_w_SA <- read_rds(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), 'outputs',paste0('threshold_prices_meas_w_',comparator,'.rds')))
  threshold_prices_meas_w_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_upper','',econ_folder_name)))]
  
  threshold_prices_meas_w <- rbind(threshold_prices_meas_w, threshold_prices_meas_w_SA)
  
  econ_inmb_meds_w_SA <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_inmb_meds_w.rds'))
  econ_inmb_meds_w_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_upper','',econ_folder_name)))]
  
  econ_inmb_meds_w <- rbind(econ_inmb_meds_w, econ_inmb_meds_w_SA)
  
  econ_inmb_mean_w_SA <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_inmb_mean_w.rds'))
  econ_inmb_mean_w_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_upper','',econ_folder_name)))]
  
  econ_inmb_mean_w <- rbind(econ_inmb_mean_w, econ_inmb_mean_w_SA)
  
  upper_inmbs_SA <- data.table(read_csv(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table2_',comparator,'.csv')), show_col_types = F))
  # upper_inmbs_SA <- upper_inmbs_SA[WHOREGION == 'Global']
  upper_inmbs_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_upper','',econ_folder_name)))]
  upper_inmbs_SA[, include := 'all']
  
  upper_inmbs <- rbind(upper_inmbs, upper_inmbs_SA)
  
  upper_inmbs_SA <- data.table(read_csv(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table3_',comparator,'.csv')), show_col_types = F))
  # upper_inmbs_SA <- upper_inmbs_SA[WHOREGION == 'Global']
  upper_inmbs_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_upper','',econ_folder_name)))]
  upper_inmbs_SA[, include := 'only_ce']
  
  upper_inmbs <- rbind(upper_inmbs, upper_inmbs_SA)
  
  price_used <- 'lower'
  econ_folder_name <- paste0(ifelse((WTP_choice=='gdp'), '_gdp_',''),
                             ifelse((WTP_choice=='gdp'), WTP_GDP_ratio,''),
                             ifelse(discount_SA, '_discount0', ''),
                             ifelse(price_used != 'midpoint', paste0('_doseprice_',price_used), ''))
  
  econ_inmb_meds_w_lower_SA <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_inmb_meds_w.rds'))
  econ_inmb_meds_w_lower_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_lower','',econ_folder_name)))]
  
  econ_inmb_meds_w_lower <- rbind(econ_inmb_meds_w_lower, econ_inmb_meds_w_lower_SA)
  
  econ_inmb_mean_w_lower_SA <- read_rds(here::here('output','data','econ',paste0(scenario_name,econ_folder_name),'outputs','econ_inmb_mean_w.rds'))
  econ_inmb_mean_w_lower_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_lower','',econ_folder_name)))]
  
  econ_inmb_mean_w_lower <- rbind(econ_inmb_mean_w_lower, econ_inmb_mean_w_lower_SA)
  
  lower_inmbs_SA <- data.table(read_csv(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table2_',comparator,'.csv')), show_col_types = F))
  # lower_inmbs_SA <- lower_inmbs_SA[WHOREGION == 'Global']
  lower_inmbs_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_lower','',econ_folder_name)))]
  lower_inmbs_SA[, include := 'all']
  
  lower_inmbs <- rbind(lower_inmbs, lower_inmbs_SA)
  
  lower_inmbs_SA <- data.table(read_csv(here::here('output','data','econ',paste0(scenario_name, econ_folder_name), comparator,paste0('table3_',comparator,'.csv')), show_col_types = F))
  # lower_inmbs_SA <- lower_inmbs_SA[WHOREGION == 'Global']
  lower_inmbs_SA[, SA := ifelse(SA_option == 0, 'base', gsub('_','',gsub('_doseprice_lower','',econ_folder_name)))]
  lower_inmbs_SA[, include := 'only_ce']
  
  lower_inmbs <- rbind(lower_inmbs, lower_inmbs_SA)
  
}

econ_folder_name <- paste0('_SA_comparison_doseprice_upper_', comparator)

if(!dir.exists(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name)))){
  dir.create(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name)))
}
if(!dir.exists(here::here('output','data','econ',paste0(scenario_name, econ_folder_name)))){
  dir.create(here::here('output','data','econ',paste0(scenario_name, econ_folder_name)))
}

supp.labs.sa <- c('Base','DALYs 0% discounted','WTP 0.3 x GDPpc', 'WTP 1 x GDPpc')
names(supp.labs.sa) <- c('base','discount0','gdp0.3', 'gdp1')

supp.labs.ce <- c('All countries','Only cost-effective countries')
names(supp.labs.ce) <- c('all','only_ce')

# # boxplot of median threshold prices by region
# ggplot(data=threshold_prices_meas_w[!vacc_type=="0",]) +
#   geom_boxplot(aes(x=vacc_type, y=mean, fill=as.factor(vacc_type), col=as.factor(vacc_type)), outliers = T) +
#   geom_boxplot(aes(x=vacc_type, y=mean), alpha=0, outliers=F) +
#   ylab('Threshold price (USD)') +
#   scale_fill_manual(values=vtn_colors) +
#   scale_color_manual(values=vtn_colors) +
#   labs(fill = 'Vaccine type', color = 'Vaccine type') +
#   facet_grid(SA~WHOREGION, scales='free', labeller = labeller(WHOREGION = who_region_labs,
#                                                               SA = supp.labs.sa)) +
#   theme_bw() + xlab('Vaccine type') +
#   theme(text=element_text(size=12), 
#         legend.position='none'#,
#         # axis.text.x=element_blank(),
#         # axis.ticks.x=element_blank()
#   )
# 
# ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('WHO_region_threshold_price',comparator,'.png')),
#        width=30,height=35,units="cm")

# boxplot of median threshold prices by region
ggplot(data=threshold_prices_meas_w[!vacc_type=="0",]) +
  geom_boxplot(aes(x=vacc_type, y=mean, fill=as.factor(vacc_type), col=as.factor(vacc_type)), outliers = T) +
  geom_boxplot(aes(x=vacc_type, y=mean), alpha=0, outliers=F) +
  ylab('Threshold price (USD)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_grid(WHOREGION~SA, scales='fixed', labeller = labeller(WHOREGION = who_region_labs,
                                                               SA = supp.labs.sa)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=12), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  ) + scale_y_continuous(transform = 'pseudo_log', breaks = c(-30, -10, -3, 0, 3, 10, 30, 100, 300, 1000, 3000)) 

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('WHO_region_threshold_price',comparator,'.png')),
       width=30,height=35,units="cm")

# boxplot of median threshold prices by region
ggplot(data=threshold_prices_meas_w[!vacc_type=="0",]) +
  geom_boxplot(aes(x=vacc_type, y=mean, fill=as.factor(vacc_type), col=as.factor(vacc_type)), outliers = T) +
  geom_boxplot(aes(x=vacc_type, y=mean), alpha=0, outliers=F) +
  ylab('Threshold price (USD)') +
  scale_fill_manual(values=vtn_colors) +
  scale_color_manual(values=vtn_colors) +
  labs(fill = 'Vaccine type', color = 'Vaccine type') +
  facet_grid(WHOREGION~SA, scales='free', labeller = labeller(WHOREGION = who_region_labs,
                                                               SA = supp.labs.sa)) +
  theme_bw() + xlab('Vaccine type') +
  theme(text=element_text(size=12), 
        legend.position='none'#,
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()
  ) + scale_y_continuous(transform = 'pseudo_log', breaks = c(-30, -10, -3, 0, 3, 10, 30, 100, 300, 1000, 3000)) 

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('WHO_region_threshold_price',comparator,'_free.png')),
       width=30,height=35,units="cm")


## TABLES ##

n_countries <- econ_inmb_meds_w %>% group_by(SA, vacc_type, WHOREGION) %>% 
  summarise(n_total = n())

## upper price point ##

tab1 <- econ_inmb_meds_w %>% filter(median > 0) %>% group_by(SA, vacc_type, WHOREGION) %>% 
  summarise(n = n()) %>% right_join(n_countries, by = c('SA', 'vacc_type', 'WHOREGION')) %>% 
  mutate(n = case_when(is.na(n) ~ 0, T ~ n)) %>% 
  mutate(percentage = round(100*n/n_total, 1))

tab1_global <- tab1 %>% select(SA, vacc_type, n, n_total) %>% 
  group_by(SA, vacc_type) %>% summarise(n = sum(n), n_total = sum(n_total)) %>% 
  mutate(percentage = round(100*n/n_total, 1), WHOREGION = 'Global') %>% 
  select(SA, vacc_type, WHOREGION, n, n_total, percentage)

tile1 <- tab1_global %>% 
  ggplot() + 
  geom_tile(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), fill = percentage/100)) + 
  theme_bw() + scale_fill_viridis(limits = c(0,0.8)) + 
  theme(text = element_text(size = 14)) +
  geom_text(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), label = format(round(percentage/100, 2), nsmall = 2)),
            col = 'white') +
  labs(x = 'Vaccine type', y = '', fill = 'Proportion of countries\ncost-effective') + 
  ggtitle('Upper price point') + 
  scale_y_discrete(breaks = names(supp.labs.sa), labels = supp.labs.sa)+
  plot_layout(guides = 'collect', axes = "collect"); tile1

## lower price point ##

tab2 <- econ_inmb_meds_w_lower %>% filter(median > 0) %>% group_by(SA, vacc_type, WHOREGION) %>% 
  summarise(n = n()) %>% right_join(n_countries, by = c('SA', 'vacc_type', 'WHOREGION')) %>% 
  mutate(n = case_when(is.na(n) ~ 0, T ~ n)) %>% 
  mutate(percentage = round(100*n/n_total, 1))

tab2_global <- tab2 %>% select(SA, vacc_type, n, n_total) %>% 
  group_by(SA, vacc_type) %>% summarise(n = sum(n), n_total = sum(n_total)) %>% 
  mutate(percentage = round(100*n/n_total, 1), WHOREGION = 'Global') %>% 
  select(SA, vacc_type, WHOREGION, n, n_total, percentage)

tile2 <- tab2_global %>% 
  ggplot() + 
  geom_tile(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), fill = percentage/100)) + 
  theme_bw() + scale_fill_viridis(limits = c(0,0.8)) + 
  theme(text = element_text(size = 14),
        legend.position = 'none') +
  geom_text(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), label = format(round(percentage/100, 2), nsmall = 2)),
            col = 'white') +
  labs(x = 'Vaccine type', y = '', fill = 'Proportion of countries\ncost-effective') + 
  ggtitle('Lower price point') + 
  scale_y_discrete(breaks = names(supp.labs.sa), labels = supp.labs.sa) +
  plot_layout(guides = 'collect', axes = "collect"); tile2

p1 <- (tile2 + tile1) + plot_layout(guides = 'collect', nrow = 1); p1

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('proportion_CE.png')),
       width=48,height=18,units="cm")


## GLOBAL INMB ## 

upper_inmbs[, INMB_mill_num := as.numeric(word(INMB_millions, 1))]
lower_inmbs[, INMB_mill_num := as.numeric(word(INMB_millions, 1))]

colorscale <- c('royalblue4', 'lightblue','gray80','salmon','firebrick4')
breaks <- c(-1600, -1, 0, 1, 1600)

upper_all <- upper_inmbs %>% 
  filter(include == 'all',
         WHOREGION == 'Global') %>% 
  ggplot() + 
  geom_tile(aes(x = vacc_type, y = factor(SA, levels = rev(unique(upper_inmbs$SA))), fill = INMB_mill_num/1000)) + 
  theme_bw() + 
  scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), na.value = "#e5e5e5", limits = c(-1600, 1600)) +
  theme(text = element_text(size = 14)) +
  labs(x = 'Vaccine type', y = '', fill = 'Global INMB (billions)') + 
  geom_text(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), label = round(INMB_mill_num/1000, 0)),
            col = 'white') +
  ggtitle('Upper price point') + 
  scale_y_discrete(breaks = names(supp.labs.sa), labels = supp.labs.sa); upper_all

lower_all <- lower_inmbs %>% 
  filter(include == 'all',
         WHOREGION == 'Global') %>% 
  ggplot() + 
  geom_tile(aes(x = vacc_type, y = factor(SA, levels = rev(unique(upper_inmbs$SA))), fill = INMB_mill_num/1000)) + 
  theme_bw() +  scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), na.value = "#e5e5e5", limits = c(-1600, 1600)) +
  theme(text = element_text(size = 14)) +
  labs(x = 'Vaccine type', y = '', fill = 'Global INMB (billions)') + 
  geom_text(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), label = round(INMB_mill_num/1000, 0)),
            col = 'white') +
  ggtitle('Lower price point') + 
  scale_y_discrete(breaks = names(supp.labs.sa), labels = supp.labs.sa); lower_all

upper_only_ce <- upper_inmbs %>% 
  filter(include == 'only_ce',
         WHOREGION == 'Global') %>% 
  ggplot() + 
  geom_tile(aes(x = vacc_type, y = factor(SA, levels = rev(unique(upper_inmbs$SA))), fill = INMB_mill_num/1000)) + 
  theme_bw() + 
  scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), na.value = "#e5e5e5", limits = c(-1600, 1600)) +
  theme(text = element_text(size = 14)) +
  labs(x = 'Vaccine type', y = '', fill = 'Global INMB (billions)') + 
  geom_text(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), label = round(INMB_mill_num/1000, 0)),
            col = 'white') +
  ggtitle('Upper price point\n(only cost-effective countries)') + 
  scale_y_discrete(breaks = names(supp.labs.sa), labels = supp.labs.sa); upper_only_ce

lower_only_ce <- lower_inmbs %>% 
  filter(include == 'only_ce',
         WHOREGION == 'Global') %>% 
  ggplot() + 
  geom_tile(aes(x = vacc_type, y = factor(SA, levels = rev(unique(upper_inmbs$SA))), fill = INMB_mill_num/1000)) + 
  theme_bw() +  
  scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), na.value = "#e5e5e5", limits = c(-1600, 1600)) +
  theme(text = element_text(size = 14)) +
  labs(x = 'Vaccine type', y = '', fill = 'Global INMB (billions)') + 
  geom_text(aes(x = vacc_type, y = factor(SA, levels = rev(unique(tab1_global$SA))), label = round(INMB_mill_num/1000, 0)),
            col = 'white') +
  ggtitle('Lower price point\n(only cost-effective countries)') + 
  scale_y_discrete(breaks = names(supp.labs.sa), labels = supp.labs.sa); lower_only_ce

p2 <- (lower_all + upper_all + lower_only_ce + upper_only_ce) +
  plot_layout(ncol = 2, nrow = 2, guides = 'collect'); p2

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('global_INMB.png')),
       width=38,height=26,units="cm")


layout <- '
AABB
AABB
CCCC
CCCC
CCCC
CCCC
'
p1 + p2 + plot_layout(nrow = 2, design = layout)

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('combined_tiles.png')),
       width=38,height=38,units="cm")


save_inmbs <- rbind(upper_inmbs %>% select(!INMB_mill_num) %>% mutate(doseprice = 'Upper'),
                    lower_inmbs %>% select(!INMB_mill_num) %>% mutate(doseprice = 'Lower')) %>% 
  mutate(include = case_when(
      include == 'all' ~ 'All countries adopt',
      T ~ 'Countries adopt if cost-effective'
    ),
    SA = case_when(
      SA == 'base' ~ 'Base',
      SA == 'discount0' ~ 'DALYs 0% discounted',
      SA == 'gdp0.3' ~ 'WTP 0.3 x GDPpc',
      SA == 'gdp1' ~ 'WTP 1 x GDPpc'
    )
  )

write_csv(save_inmbs, here::here('output','data','econ',paste0(scenario_name, econ_folder_name),paste0('global_INMB.csv')))


lower_bars <- econ_inmb_mean_w_lower %>% 
  filter(SA == 'base') %>% 
  mutate(negative = case_when(mean < 0 ~ '< 0', T ~ '> 0')) %>% 
  ggplot() +
  geom_bar(aes(x = negative, y = mean/1e9, fill = WHOREGION),
           position = 'stack', stat = 'identity') +
  geom_hline(data = econ_inmb_mean_w_lower %>% filter(SA == 'base') %>% 
               group_by(vacc_type) %>% summarise(sum = sum(mean/1e9), negative = '< 0'), 
             aes(yintercept = sum), lty = 2) +
  scale_y_continuous(breaks = seq(-600, 600, by = 200)) + 
  facet_grid(. ~ vacc_type, scales = 'free_y', labeller = labeller(SA = supp.labs.sa,
                                                                  include = supp.labs.ce)) +
  scale_fill_manual(values = WHO_colors, labels=who_region_labs2) +
  theme_bw() + ggtitle('Lower price point') + 
  labs(y = 'INMB (billions)', x = 'INMB negative/positive', fill = 'WHO Region'); lower_bars 

upper_bars <- econ_inmb_mean_w %>% 
  filter(SA == 'base') %>% 
  mutate(negative = case_when(mean < 0 ~ '< 0', T ~ '> 0')) %>% 
  ggplot() +
  geom_bar(aes(x = negative, y = mean/1e9, fill = WHOREGION), #col = 'white', lwd = 0.1,
           position = 'stack', stat = 'identity') +
  geom_hline(data = econ_inmb_mean_w %>% filter(SA == 'base') %>% 
               group_by(vacc_type) %>% summarise(sum = sum(mean/1e9), negative = '< 0'), 
             aes(yintercept = sum), lty = 2) +
  scale_y_continuous(breaks = seq(-600, 600, by = 200)) + 
  facet_grid(. ~ vacc_type, scales = 'free_y', labeller = labeller(SA = supp.labs.sa,
                                                                   include = supp.labs.ce)) +
  scale_fill_manual(values = WHO_colors, labels=who_region_labs2) +
  theme_bw() + ggtitle('Upper price point') + 
  labs(y = 'INMB (billions)', x = 'INMB negative/positive', fill = 'WHO Region'); upper_bars 

lower_bars + upper_bars + plot_layout(guides = 'collect', nrow = 2)

ggsave(here::here('output','figures','econ',paste0(scenario_name, econ_folder_name),paste0('INMB_distribution.png')),
       width=30,height=26,units="cm")



