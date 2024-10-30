# Global market surplus of next-generation influenza vaccines

- `/scripts/` and `/next_gen_flu/` for R code
- `/data/` for input data
- `/output/` for epi and econ model outputs (data and figures)

## Within `/scripts/`

`/setup/` contains setups for packages, ggplot aesthetics, etc.

To clean MMGH data, use `/mmgh_data/`.

To run epi analysis, use `scripts/init_epi.R`, which is set up to run each ITZ separately using 6 cores each.

To run econ analysis, use `scripts/init_econ.R`.

## Epidemic simulations

1. `init_epi.R` sets the ITZ of choice and key model inputs
2. `epid_simulations.R` loads and sets up the data
3. For each country in the ITZ, `next_gen_flu/flu_parallel.R` projects the population over the time period of interest (using `next_gen_flu/functions/demography.R` and parallelises simulations over all vaccine types (including 'no vaccinations') 
4. In `next_gen_flu/functions/flu_sim.R`, `many_flu()` splits the epidemiological inputs into individual epidemics, and `one_flu()` simulates the epidemic using the model set up in `next_gen_flu/functions/transmission_model.R`

