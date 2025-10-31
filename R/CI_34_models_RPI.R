source('../R/functions.R')
source('../R/CI_34_models_RPI_functions.R')
source('../R/CI_30_models_functions.R')

if (CI_isParent()) CI_startMatter()

CURRENT_STAGE <<- 6
CI__change_setting(item = 'CURRENT_STAGE', value = CURRENT_STAGE)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE),
              title = paste0('Stage ', CURRENT_STAGE, ' - modelling RPI data'))

if (CI$setting[['FRESH_START']]) CI_clear_models_RPI_data()

## Baseline model - only run once
if (CI$setting[['RERUN_BASELINES']]) {
    # Ensure all necessary functions are sourced inside the future if using parallel processing
    # and avoid passing non-exportable objects (like externalptr) to futures.
    CI_34_RPI_baseline_models()
}

## Reference index - run each year
CI_34_RPI_reference_models()

## Critical index - run each year
CI_34_RPI_critical_models()

## Put the posteriors together and standardise the format
## This does not actually calculate the distance - this has already been done!
CI_models_RPI_distance()
CI_models_aggregation(Indicator = 'RPI', level = 'BIOREGION.agg') 
CI_models_aggregation(Indicator = 'RPI', level = 'NRM') 
CI_models_aggregation(Indicator = 'RPI', level = 'TUMRA') 
CI_models_aggregation(Indicator = 'RPI', level = 'GBRMPA.MA') 
CI_models_aggregation(Indicator = 'RPI', level = 'GBRMP') 
CI_models_aggregation(Indicator = 'RPI', level = 'ZONE') 
