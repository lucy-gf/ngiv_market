#### PLOTS ####

library(patchwork)
library(countrycode)
source(here::here('scripts','setup','aesthetics.R'))

itz <- c('ARG','AUS','CAN','CHN','GBR','GHA','TUR')[3]

infs_out <- readRDS(here::here('output','data','epi',paste0(itz),paste0('vacc_',itz,'.rds')))
infs_out[, tot := I1 + I2 + I3 + I4]

infs_cum <- infs_out[, c('vacc_type','simulation_index','iso3c','tot')][, lapply(.SD, cumsum), by=c('vacc_type','simulation_index','iso3c')]
infs_cum[, time:=infs_out$time]

infs_out$vacc_type <- factor(infs_out$vacc_type, levels=unique(infs_out$vacc_type))
infs_cum$vacc_type <- factor(infs_cum$vacc_type, levels=unique(infs_cum$vacc_type))

infs_av <- infs_cum[!vacc_type == '0']
infs_av <- infs_av[infs_cum[vacc_type == '0'], on=c('simulation_index','iso3c','time'), base := i.tot]
infs_av[, averted := base - tot]

incidence <- ggplot(infs_out) + 
  geom_line(aes(x=time, y=tot/1000000, col=vacc_type, group=interaction(vacc_type, simulation_index)), lwd=0.8) +
  ylab('Infections (millions)') + xlab('Time') + 
  scale_color_manual(values = vtn_colors) + 
  labs(col='Vaccine type') + facet_grid(iso3c~., scales='free') + 
  theme_minimal() + theme(text = element_text(size = 14))

cumulative <- ggplot(infs_cum) + 
  geom_line(aes(x=time, y=tot/1000000, col=vacc_type, group=interaction(vacc_type, simulation_index)), lwd=0.8) +
  ylab('Cumulative infections (millions)') + xlab('Time') + 
  labs(col='Vaccine type') + 
  scale_color_manual(values = vtn_colors) + facet_grid(iso3c~., scales='free') + 
  theme_minimal() + theme(text = element_text(size = 14))

averted <- ggplot(infs_av) + 
  geom_line(aes(x=time, y=averted/1000000, col=vacc_type, group=interaction(vacc_type, simulation_index)), lwd=0.8) +
  ylab('Cumulative infections (millions)') + xlab('Time') + 
  labs(col='Vaccine type') + 
  scale_color_manual(values = vtn_colors) + facet_grid(iso3c~., scales='free') + 
  theme_minimal() + theme(text = element_text(size = 14))

names <- countrycode(unique(infs_out$iso3c), origin='iso3c', destination='country.name')
pops <- data.table(names = pop_proj_WPP_data[name %in% names & Year == 2025]$name,
                   tot_pop = 1000*rowSums(pop_proj_WPP_data[name %in% names & Year == 2025, 4:24]))
pops$iso3c <- countrycode(pops$names, origin='country.name', destination='iso3c')
attacks <- copy(infs_out)
attacks[, year := year(time)]
attacks <- attacks[, c('year','iso3c','simulation_index','vacc_type','tot')]
attacks <- attacks[, lapply(.SD, sum), by=c('year','iso3c','vacc_type')]
attacks <- attacks[pops, on='iso3c']
attacks[, ar := tot/(tot_pop*length(unique(infs_out$simulation_index)))]
attacks <- attacks[year > 2024]

attack_rates <- ggplot(attacks) + 
  geom_bar(aes(x=year, y=ar, fill=vacc_type), position='dodge', stat='identity') +
  ylab('Attack rate (rel. to 2025 pop)') + xlab('Year') + 
  labs(fill='Vaccine type') +
  scale_fill_manual(values = vtn_colors) + facet_grid(iso3c~., scales='free') + 
  theme_bw() + theme(text = element_text(size = 14)) +
  geom_hline(yintercept=1, lty=2) + 
  scale_x_continuous(breaks = unique(attacks$year))

attack_rates_l <- ggplot(attacks) + 
  geom_line(aes(x=year, y=ar, col=vacc_type), lwd=0.8) +
  ylab('Attack rate (rel. to 2025 pop)') + xlab('Year') + 
  labs(col='Vaccine type') + labs(fill='Vaccine type') +
  scale_color_manual(values = vtn_colors) + facet_grid(iso3c~., scales='free') + 
  theme_bw() + theme(text = element_text(size = 14)) +
  geom_hline(yintercept=1, lty=2) + ylim(c(0,NA)) +
  scale_x_continuous(breaks = unique(attacks$year))

design <- '
AAABBB
AAABBB
CCCCCC
CCCCCC'

incidence + averted + 
  attack_rates_l + plot_layout(guides='collect', design = design)

ggsave(here::here('output','figures','epi',paste0(itz, '_infections.png')),
       width=32,height=36,units="cm")

