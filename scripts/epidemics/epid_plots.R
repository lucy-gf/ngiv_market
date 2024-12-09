#### PLOTS ####

library(here)
source(here::here('scripts','setup','packages.R'))
source(here::here('scripts','setup','aesthetics.R'))

pop_proj_WPP_data <- data.table(read_csv(here::here('next_gen_flu','data','pop_proj_WPP_data.csv'), show_col_types = F))
intro_years <- data.table(read_csv(here::here('data','MMGH','intro_years.csv'), show_col_types = F))

all_out <- data.frame()
all_cum <- data.frame()
all_av <- data.frame()

for(itz in c('ARG','AUS','CAN','CHN','GBR','GHA','TUR')){
  print(paste0(itz))
  if(file.exists(paste0('output/data/epi/rds_output/vacc_',itz,'.rds'))){
    print(paste0(itz, ' exists'))
    infs_out <- readRDS(here::here('output','data','epi','rds_output',paste0('vacc_',itz,'.rds')))
    infs_out[, tot := I1 + I2 + I3 + I4]
    isos_na <- infs_out %>% group_by(iso3c) %>% summarise(sum = sum(tot)) %>% filter(is.na(sum)) %>% select(iso3c)
    infs_out <- infs_out[! iso3c %in% isos_na$iso3c]
    
    infs_cum <- infs_out[, c('vacc_type','simulation_index','iso3c','tot')][, lapply(.SD, cumsum), by=c('vacc_type','simulation_index','iso3c')]
    infs_cum[, time:=infs_out$time]
    
    infs_out$vacc_type <- factor(infs_out$vacc_type, levels=unique(infs_out$vacc_type))
    infs_cum$vacc_type <- factor(infs_cum$vacc_type, levels=unique(infs_cum$vacc_type))
    
    infs_av <- infs_cum[!vacc_type == '0']
    infs_av <- infs_av[infs_cum[vacc_type == '0'], on=c('simulation_index','iso3c','time'), base := i.tot]
    infs_av[, averted := base - tot]
    
    all_out <- rbind(all_out, infs_out)
    all_cum <- rbind(all_cum, infs_cum)
    all_av <- rbind(all_av, infs_av)
  }
}

print(paste0(length(unique(all_av$iso3c)), ' countries, ', round(100*length(unique(all_av$iso3c))/178, 2), '% of global'))

## cumulative weekly infections (global)
cum_global <- all_cum[, c('time','vacc_type','simulation_index','tot')]
cum_global <- cum_global[, lapply(.SD, sum), by=c('time','vacc_type','simulation_index')]
cum_global_meas <- dt_to_meas(cum_global, c('time','vacc_type'))
cum_global_meas_w <- dcast(cum_global_meas, time + vacc_type ~ measure, value.var = 'tot')

ggplot(cum_global_meas_w) + 
  geom_ribbon(aes(x=time, ymin=eti95L/1000000, ymax=eti95U/1000000, fill=vacc_type, group=vacc_type), alpha=0.25) +
  geom_line(aes(x=time, y=median/1000000, col=vacc_type, group=vacc_type), lwd=1) +
  ylab('Cumulative infections (millions)') + xlab('Time') + 
  labs(col='Vaccine type', fill='Vaccine type') + 
  scale_color_manual(values = vtn_colors) + scale_fill_manual(values = vtn_colors) +
  theme_bw() + theme(text = element_text(size = 14)) +
  ggtitle(paste0('Cumulative infections, ', length(unique(all_av$iso3c)), ' countries'))
ggsave(here::here('output','figures','epi','global_cumulative_infs.png'),
       width=32,height=25,units="cm")

## averted weekly infections (global)
av_global <- all_av[, c('time','vacc_type','simulation_index','averted')]
av_global <- av_global[, lapply(.SD, sum), by=c('time','vacc_type','simulation_index')]
av_global_meas <- dt_to_meas(av_global, c('time','vacc_type'))
av_global_meas_w <- dcast(av_global_meas, time + vacc_type ~ measure, value.var = 'averted')

ggplot(av_global_meas_w) + 
  geom_ribbon(aes(x=time, ymin=eti95L/1000000, ymax=eti95U/1000000, fill=vacc_type, group=vacc_type), alpha=0.25) +
  geom_line(aes(x=time, y=median/1000000, col=vacc_type, group=vacc_type), lwd=1) +
  ylab('Averted infections (millions)') + xlab('Time') + 
  labs(col='Vaccine type', fill='Vaccine type') + 
  scale_color_manual(values = vtn_colors) + scale_fill_manual(values = vtn_colors) +
  theme_bw() + theme(text = element_text(size = 14)) +
  ggtitle(paste0('Averted infections, ', length(unique(all_av$iso3c)), ' countries'))
ggsave(here::here('output','figures','epi','global_averted_infs.png'),
       width=32,height=25,units="cm")

## mean annual infections

mean_ann <- all_out[, c('time','iso3c','vacc_type','simulation_index','tot')]
mean_ann[, year := year(time)][, time := NULL]
mean_ann <- mean_ann[, lapply(.SD, sum), by=c('iso3c','vacc_type','simulation_index','year')]
mean_ann <- mean_ann[, lapply(.SD, mean), by=c('iso3c','vacc_type','simulation_index')]
mean_ann[, year := NULL]
mean_ann_meas <- dt_to_meas(mean_ann, c('iso3c','vacc_type'))
mean_ann_meas_w <- dcast(mean_ann_meas, iso3c + vacc_type ~ measure, value.var = 'tot')

names <- countrycode(unique(mean_ann$iso3c), origin='iso3c', destination='country.name')
pops <- data.table(names = pop_proj_WPP_data[name %in% names & Year == 2025]$name,
                   tot_pop = 1000*rowSums(pop_proj_WPP_data[name %in% names & Year == 2025, 4:24]))
pops$iso3c <- countrycode(pops$names, origin='country.name', destination='iso3c') 

mean_ann_meas_w <- mean_ann_meas_w[pops, on='iso3c']

ggplot(mean_ann_meas_w) + 
  geom_bar(aes(x=iso3c, y=median/tot_pop, fill=vacc_type, group=vacc_type), 
           position='dodge', stat='identity') +
  geom_errorbar(aes(x=iso3c, ymin=eti95L/tot_pop, ymax=eti95U/tot_pop, group=vacc_type),
                position=position_dodge(.9)) +
  ylab('Annual attack rate (rel. to 2025 population)') + xlab('Country') + 
  labs(fill='Vaccine type') + scale_fill_manual(values = vtn_colors) +
  theme_bw() + theme(text = element_text(size = 14)) +
  geom_hline(yintercept = 1, lty = 2) + scale_y_continuous(breaks = 1:5/5) +
  ggtitle(paste0('Mean annual attack rate'))
ggsave(here::here('output','figures','epi','mean_annual_ar.png'),
       width=50,height=20,units="cm")











