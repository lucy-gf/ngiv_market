#### Making health econ data ####

#### YLDS ####
print('YLDs')

# ## weights from GBD
# DALY_weights <- data.table(
#   outcome = c('non_fever','fever','hospitalisation'),
#   med = c(0.006, 0.051, 0.133),
#   l95 = c(0.002, 0.032, 0.088),
#   u95 = c(0.012, 0.074, 0.190)
# )
# for(i in 1:nrow(DALY_weights)){
#   parms <- fcn_fitting(DALY_weights[i,2:4], c(0.5, 0.025, 0.975))
#   DALY_weights[i,"shape"] <- parms[1]
#   DALY_weights[i,"rate"] <- parms[2]
#   DALY_weights[i,"med_fit"] <- qgamma(p=c(0.5), shape=parms[1], rate=parms[2])
#   DALY_weights[i,"l95_fit"] <- qgamma(p=c(0.025), shape=parms[1], rate=parms[2])
#   DALY_weights[i,"u95_fit"] <- qgamma(p=c(0.975), shape=parms[1], rate=parms[2])
# }
# DALY_weight_samples <-  data.table(
#   simulation_index = 1:100,
#   non_fever_DALY = rgamma(100, shape = unlist(DALY_weights[outcome=='non_fever','shape']), rate = unlist(DALY_weights[outcome=='non_fever','rate'])),
#   fever_DALY = rgamma(100, shape = unlist(DALY_weights[outcome=='fever','shape']), rate = unlist(DALY_weights[outcome=='fever','rate'])),
#   hosp_DALY = rgamma(100, shape = unlist(DALY_weights[outcome=='hospitalisation','shape']), rate = unlist(DALY_weights[outcome=='hospitalisation','rate']))
# )
# write_csv(DALY_weight_samples, file='data/econ/DALY_weight_samples.csv')

DALY_weight_samples <- data.table(read_csv('data/econ/DALY_weight_samples.csv', show_col_types=F))





