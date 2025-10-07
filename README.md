# Global impact of next-generation influenza vaccines

For use:

**IMPORTANT:** must have `next_gen_flu` (https://github.com/lucy-gf/next_gen_flu) installed within `ngiv_market`

- `/scripts/` and `/next_gen_flu/` for R code
- `/data/` for input data
- `/output/` for epi and econ model outputs (data and figures)

## Within `/scripts/`

`/setup/` contains setups for packages, ggplot aesthetics, etc.

To clean MMGH data, use `/mmgh_data/` (`mmgh_cleaning.R` turns raw (gitignored) excel sheets into data.tables, `mmgh_transform.R` makes the data.tables into the right model age groups etc.)

To run epi analysis, use `scripts/init_epi.R`, which is set up to run each ITZ separately using 6 cores each (parallelised across vaccine types, if using NGIVs), or runs all ITZs in parallel using 7 cores (if running 'no_vacc' scenario).

To run econ analysis, use `scripts/init_econ.R`.

## Epidemic simulations

1. `init_epi.R` sets the ITZ of choice and key model inputs
2. `epid_simulations.R` loads and sets up the data
3. For each country in the ITZ, `next_gen_flu/flu_parallel.R` projects the population over the time period of interest (using `next_gen_flu/functions/demography.R` and parallelises simulations over all vaccine types 
4. In `next_gen_flu/functions/flu_sim.R`, `many_flu()` splits the epidemiological inputs into individual epidemics, and `one_flu()` simulates the epidemic using the model set up in `next_gen_flu/functions/transmission_model.R`

## Economic analysis

1. `init_econ.R` sets key model inputs, runs the economic analysis
2. `shrink_epi_data.R` makes the epidemic outputs into the level of detail needed (e.g. years not weeks), for smaller files. Can skip this step if you have access to `vacc_global.rds` already
3. `/making_econ_data/` contains code for calculating/sampling economic parameters (mainly fixed to increase comparability)
4. `national_health_econ_1.R` turns age-, year-, and country-specific infections into DALYs and costs
5. `INMB_outputs_SRP.R` turns the DALYs, costs, and annual doses into net monetary benefit, INMB, etc.
6. `INMB_plots_SRP.R`, `INMB_plots_SRP_CE.R`, `INMB_tables_SRP.R` turn these into figures and tables
7. `sens_analyses_outputs.R` makes plots regarding sensitivity analyses
