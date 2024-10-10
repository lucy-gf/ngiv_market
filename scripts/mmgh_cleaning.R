#### MMGH data cleaning ####

library(here)
library(data.table)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)
library(purrr)

## read in raw data ##

#### country WHO regions/income groups/Gavi/procurement ####

country_specs <- data.table(read_xlsx(here::here('data','MMGH','raw','202410_FVIVA country procurement classification.xlsx')))

# make names more usable
setnames(country_specs, 'Country name','country')
setnames(country_specs, 'ISO','iso3c')
setnames(country_specs, 'WHO Region','WHO_region')
setnames(country_specs, 'Income Group','income_g')
setnames(country_specs, 'Gavi/Non-Gavi','gavi')
setnames(country_specs, 'Procurement Mechanism','procure_mech')

write.xlsx(country_specs, here::here('data','MMGH','country_specs.xlsx'))

## likely will be an issue about which countries are in each analysis, e.g. for ITZs

#### national annual demand ####

## function ##
clean_demand <- function(dt_raw){
  
  setnames(dt_raw, 'Country','country')
  setnames(dt_raw, 'ISO code','iso3c')
  setnames(dt_raw, 'WHO','WHO_region')
  setnames(dt_raw, 'WB group (2022 classification)','income_g')
  
  ## all vaccine types? need to check ##
  
  exclude_all <- (suppressWarnings(as.numeric(gsub('[.]', '', str_sub(colnames(dt_raw), start = -4)))) >= 33)
  exclude_all[is.na(exclude_all)] <- F
  
  dt_all_wide <- dt_raw[,!..exclude_all]
  
  dt_all <- melt(dt_all_wide, id.vars=c('country','iso3c','WHO_region','income_g'))
  
  setnames(dt_all, 'variable','year')
  setnames(dt_all, 'value','doses')
  
  dt_all[, year := as.numeric(substr(year, 1, 4))]
  
  if((sum(!dt_all$year == rep(2023:2050, each=length(unique(dt_all$iso3c)))) > 0)){stop('Years are wrong')}
  
  ## improved vaccine types ##
  
  exclude_imp <- (suppressWarnings(as.numeric(gsub('[.]', '', str_sub(colnames(dt_raw), start = -4)))) %in% c(5:33, 62:1000))
  exclude_imp[is.na(exclude_imp)] <- F
  
  dt_imp_wide <- dt_raw[,!..exclude_imp]
  
  dt_imp <- melt(dt_imp_wide, id.vars=c('country','iso3c','WHO_region','income_g'))
  
  setnames(dt_imp, 'variable','year')
  setnames(dt_imp, 'value','doses')
  
  dt_imp[, year := as.numeric(substr(year, 1, 4))]
  
  if((sum(!dt_imp$year == rep(2023:2050, each=length(unique(dt_imp$iso3c)))) > 0)){stop('Years are wrong')}
  
  ## combining ##
  
  dt <- dt_all[dt_imp, on = c('country','iso3c'), doses_improved := i.doses]
  
  dt
}

## map 

demand_pops <- list(
  list(sheet = "6a. Demand children new", pop_name = 'children'),
  list(sheet = "6b. Demand 65+ new", pop_name = '65+'),
  list(sheet = "6c. Demand HWF new", pop_name = 'HWF'),
  list(sheet = "6e. Demand PW new", pop_name = 'PW'),
  list(sheet = "6f. Demand comorb new", pop_name = 'comorb'),
  list(sheet = "6g. Demand 18-64yo new", pop_name = '18-64yo'),
  list(sheet = "6h. Total demand new", pop_name = 'total')
)

results <- map(demand_pops, ~{
  dt_raw <- data.table(suppressMessages(read_xlsx(here::here('data','MMGH','raw','Updated influenza model_v2.xlsx'), 
                                 sheet = .x$sheet, skip = 3)))
  res <- clean_demand(dt_raw)
  res$pop <- .x$pop_name
  print(.x$pop_name)
  res
})

demand_total <- rbindlist(results)

write.xlsx(demand_total, here::here('data','MMGH','demand_total.xlsx'))

#### price estimates ####

prices_raw <- data.table(read_xlsx(here::here('data','MMGH','raw','prices_ppt.xlsx'), skip = 1))

prices_raw <- prices_raw[!3,]
prices_raw <- prices_raw[,!c(2,3)]
setnames(prices_raw, '...1', 'vacc_type')
setnames(prices_raw, 'U.S.', 'us')
setnames(prices_raw, 'Other HICs', 'hics')
setnames(prices_raw, 'UMICs', 'umics')
setnames(prices_raw, 'Self-procuring LMICs', 'lmic_self_proc')
setnames(prices_raw, 'UN procuring LMICs', 'lmic_un_proc')

prices <- melt(prices_raw, id.vars='vacc_type')
setnames(prices, 'variable','country_type')
setnames(prices, 'value','price')

prices[, range_vals := ((str_split(gsub('\\$| -| –','', price), ' ')))]
for(i in 1:nrow(prices)){
  prices[i, midpoint := mean(as.numeric(unlist(prices$range_vals[i])))]  
}
prices <- prices[, !'range_vals']

write.xlsx(prices, here::here('data','MMGH','prices.xlsx'))

# prices$vacc_type <- factor(prices$vacc_type, levels=unique(prices$vacc_type))
# ggplot(prices) + 
#   geom_line(aes(x=vacc_type, y=midpoint, group = country_type, col=country_type))




