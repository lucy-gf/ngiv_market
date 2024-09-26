#### Economic analysis ####

# in:
# coverage assumptions (from MMGH)
# epi data (from previous work, ITZ-specific)

# out: 
# epidemic data (from simulations)

# in:
# health outcome data (from previous work)
# economic data (from previous work)
# vaccine procurement costs (from MMGH)

# out [SIM 1]:
# national net monetary benefit

# rerun [SIM 2]:
# exclude countries where net monetary benefit < 0

# in:
# costs to producer
## 1 - research investment
## 2 - cost of vaccine dose production

# out: 
# national costs and benefits
# producer costs and benefits
# therefore distribution

#### load relevant packages ####

library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(odin)
library(parallel)
library(countrycode)
library(ggplot2)
library(readxl)
suppressPackageStartupMessages(library(tidyverse))
library(viridis)
library(patchwork)
library(wpp2022)
library(WDI)






