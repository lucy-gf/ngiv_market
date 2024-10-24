## troubleshooting ##

# how do MMGH countries compare to our list?

mmgh_cov <- data.table(read_xlsx(here::here('data','MMGH','demand_total.xlsx')))
mmgh_iso <- unique(mmgh_cov$iso3c) # length = 193

itzs <- data.table(read_csv(here::here('data','epi','itzs.csv'), show_col_types=F))
lshtm_iso <- unique(itzs$codes) # length = 186

countrycode(setdiff(lshtm_iso, mmgh_iso), origin='iso3c', destination='country.name') # length = 8
# French Guiana, Hong Kong, Macao, New Caledonia, Puerto Rico, Palestine, Taiwan, Kosovo            

