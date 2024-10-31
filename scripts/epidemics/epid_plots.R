#### PLOTS ####

library(patchwork)
library(countrycode)

itz <- 'AUS'

infs_out <- readRDS(here::here('output','data','epi',paste0(itz),paste0('vacc_',itz,'.rds')))
infs_out[, tot := I1 + I2 + I3 + I4]

infs_cum <- infs_out[, c('vacc_type','simulation_index','iso3c','tot')][, lapply(.SD, cumsum), by=c('vacc_type','simulation_index','iso3c')]
infs_cum[, time:=infs_out$time]

infs_out$vacc_type <- factor(infs_out$vacc_type, levels=unique(infs_out$vacc_type))
infs_cum$vacc_type <- factor(infs_cum$vacc_type, levels=unique(infs_cum$vacc_type))

incidence <- ggplot(infs_out) + 
  geom_line(aes(x=time, y=tot/1000000, col=vacc_type), lwd=0.8) +
  ylab('Infections (millions)') + xlab('') + 
  scale_color_manual(values = vt_colors1) + 
  labs(col='Vaccine type') + facet_grid(iso3c~., scales='free') + 
  theme_minimal() + theme(text = element_text(size = 14))

cumulative <- ggplot(infs_cum) + 
  geom_line(aes(x=time, y=tot/1000000, col=vacc_type), lwd=0.8) +
  ylab('Cumulative infections (millions)') + xlab('Time') + 
  labs(col='Vaccine type') + 
  scale_color_manual(values = vt_colors1) + facet_grid(iso3c~., scales='free') + 
  theme_minimal() + theme(text = element_text(size = 14))

incidence + cumulative + plot_layout(guides='collect', nrow=1)

ggsave(here::here('output','figures','epi',paste0(itz, '_infections.png')),
       width=32,height=30,units="cm")

