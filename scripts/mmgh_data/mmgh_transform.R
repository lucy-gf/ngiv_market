#### MMGH data transform ####
#### ie transforming their subpopulations into our model age groups ####

mmgh_demand <- data.table(read_csv(here::here('data','MMGH','demand_total.csv'), col_select=c(-1), show_col_types=F))
mmgh_pop <- data.table(read_csv(here::here('data','MMGH','pops_total.csv'), col_select=c(-1), show_col_types=F))
country_specs <- data.table(read_xlsx(here::here('data','MMGH','country_specs.xlsx')))

# unique(mmgh_demand$pop_name) 
# unique(mmgh_pop$pop_name)

## only using countries in both models (178 countries):
# load countries
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

mmgh_demand <- mmgh_demand[iso3c %in% itzs$codes]
mmgh_pop <- mmgh_pop[iso3c %in% itzs$codes]
country_specs <- country_specs[iso3c %in% itzs$codes]

itzs_final <- itzs[codes %in% country_specs$iso3c]
write_csv(itzs_final, here::here('data','epi','analysis_countries.csv'))

## merging 18-64 demand ##

demand_input <- copy(mmgh_demand)
demand_input[pop_name == 'children', model_age_group := 1.5]
demand_input[pop_name == '18-64yo', model_age_group := 3]
demand_input[pop_name == 'HWF', model_age_group := 3]
demand_input[pop_name == 'PW', model_age_group := 3]
demand_input[pop_name == 'comorb', model_age_group := 3]
demand_input[pop_name == '65+', model_age_group := 4]
demand_input <- demand_input[!pop_name=='total',]
demand_input[, pop_name := NULL]

demand_input <- demand_input[, lapply(.SD, sum), by=c('country','iso3c','WHO_region','income_g','year','model_age_group','vacc_scenario')]

## merging 18-64 populations ##

pop_input <- copy(mmgh_pop)
pop_input[pop_name == '6-59mo', model_age_group := 1]
pop_input[pop_name == '5-17yo', model_age_group := 2]
pop_input[pop_name == '18-64yo', model_age_group := 3]
pop_input[pop_name == 'HWF', model_age_group := 3]
pop_input[pop_name == 'pregnant', model_age_group := 3]
pop_input[pop_name == 'comorb', model_age_group := 3]
pop_input[pop_name == '65+yo', model_age_group := 4]
pop_input[, pop_name := NULL]

pop_input <- pop_input[, lapply(.SD, sum), by=c('country','iso3c','WHO_region','income_g','year','model_age_group')]
pop_input <- pop_input[year %in% unique(demand_input$year)]

## merging children when univ_pol == T ##

univ_isos <- country_specs[univ_pol == T]$iso3c
non_univ_isos <- country_specs[univ_pol == F]$iso3c

# if no universal policy, all doses assigned to model age group 1
demand_input[iso3c %in% non_univ_isos & model_age_group == 1.5, model_age_group := 1]

# if universal policy, assigned proportional to age group sizes

pop_props <- pop_input[model_age_group == 1]
pop_props$prop_659mo <- pop_input[model_age_group == 1]$pop/(pop_input[model_age_group %in% 1]$pop + pop_input[model_age_group %in% 2]$pop)

demand_input <- rbind(demand_input,
                      demand_input[iso3c %in% univ_isos & model_age_group == 1.5] %>% mutate(model_age_group=2))
demand_input[iso3c %in% univ_isos & model_age_group == 1.5, model_age_group := 1]

demand_input <- demand_input[pop_props[,c('iso3c','year','prop_659mo')], on=c('iso3c','year')]

demand_input[iso3c %in% univ_isos & model_age_group == 1, doses := doses*prop_659mo]
demand_input[iso3c %in% univ_isos & model_age_group == 2, doses := doses*(1 - prop_659mo)]

demand_input <- rbind(demand_input,
                      demand_input[iso3c %in% non_univ_isos & model_age_group == 1] %>% mutate(model_age_group = 2,
                                                                                               doses = 0))

demand_input <- demand_input %>% select(!prop_659mo)

demand_input <- demand_input[pop_input[,c('iso3c','year','model_age_group','pop')], on = c('iso3c','year','model_age_group')]

setnames(itzs, 'codes','iso3c')
itzs <- itzs[iso3c %in% unique(demand_input$iso3c)]
demand_input <- demand_input[itzs[,c('iso3c','cluster_code','hemisphere')], on='iso3c']

intro_years <- data.table(read_csv(here::here('data','MMGH','intro_years.csv'), show_col_types = F))
intro_years <- rbind(intro_years, intro_years[vacc_scenario=='A.1'] %>% mutate(vacc_scenario='0', intro_year=2010))
demand_input <- demand_input[intro_years, on=c('income_g', 'vacc_scenario')]
demand_input[year < intro_year, vacc_used := '0']
demand_input[year >= intro_year, vacc_used := vacc_scenario]

demand_input <- demand_input %>% arrange(vacc_scenario,country,year,model_age_group) %>% filter(year >= start_year_of_analysis)

# save data
write_csv(demand_input, here::here('data','MMGH','demand_input.csv'))


## figures for paper ###

demand_input$vacc_used <- factor(demand_input$vacc_used, 
                                 levels = rev(unique(demand_input$vacc_used)))

demand_input %>% group_by(WHO_region, vacc_scenario, year, vacc_used) %>% 
  summarise(pop = sum(pop),
         doses = sum(doses)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, y = doses/1e6, fill = vacc_used),
           stat = 'identity', position = 'stack') +
  theme_bw() + 
  scale_fill_manual(values = vtn_colors, guide = guide_legend(reverse = TRUE)) + 
  facet_grid(WHO_region ~ vacc_scenario, scales = 'free') +
  labs(fill = 'Vaccine in use', x = 'Year', y = 'Doses (millions)')


demand_input %>% group_by(WHO_region, vacc_scenario, year, vacc_used) %>% 
  summarise(pop = sum(pop),
         doses = sum(doses)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, y = 1e5*doses/pop, fill = vacc_used),
           stat = 'identity', position = 'stack') +
  theme_bw() + 
  scale_fill_manual(values = vtn_colors, guide = guide_legend(reverse = TRUE)) + 
  facet_grid(WHO_region ~ vacc_scenario, scales = 'free',
             labeller = labeller(WHO_region = who_region_labs_o)) +
  labs(fill = 'Vaccine in use', x = 'Year', y = 'Vaccine doses per 100,000 population') +
  theme(text = element_text(size = 16))

ggsave(here::here('output','figures','epi','vaccine_doses_by_pop.png'),
       width = 14, height = 10)

demand_input %>% group_by(model_age_group, vacc_scenario, year, WHO_region) %>% 
  summarise(pop = sum(pop),
            doses = sum(doses)) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = 1e5*doses/pop, col = vacc_scenario), lwd = 0.8) +
  theme_bw() + 
  scale_color_manual(values = vtn_colors, guide = guide_legend(reverse = F)) + 
  facet_grid(WHO_region ~ model_age_group, scales = 'free',
             labeller = labeller(model_age_group = supp.labs.agegrps,
                                 WHO_region = who_region_labs_o)) +
  labs(col = 'Vaccine scenario', x = 'Year', y = 'Vaccine doses per 100,000 population') +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(breaks = c(2030,2040,2050))

ggsave(here::here('output','figures','epi','vaccine_doses_by_age_group.png'),
       width = 14, height = 12)



