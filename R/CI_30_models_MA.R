source('../R/functions.R')
source('../R/CI_30_models_MA_functions.R')
source('../R/CI_30_models_functions.R')

if (CI_isParent()) CI_startMatter()

CURRENT_STAGE <<- 3
CI__change_setting(item = 'CURRENT_STAGE', value = CURRENT_STAGE)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE),
              title = paste0('Stage ', CURRENT_STAGE, ' - modelling MA data'))

if (CI$setting[['FRESH_START']]) CI_clear_models_MA_data()

## Generate baseline models
## source("CI_25_MA_baseline_models.R")

## Combine baseline models
CI_models_MA_get_baselines()

## Fit models
CI_models_MA_prepare_data()
CI_models_MA_prepare_nest() 
CI_models_MA_fit_models()
CI_models_MA_cellmeans()
CI_models_MA_preds()

## Calculate distances to baselines
CI_models_MA_distance()
## CI_models_MA_varify_scores()
CI_models_MA_aggregation(level = 'BIOREGION.agg') 
CI_models_MA_aggregation(level = 'NRM') 
CI_models_MA_aggregation(level = 'TUMRA') 
CI_models_MA_aggregation(level = 'GBRMPA.MA') 
CI_models_MA_aggregation(level = 'GBRMP') 
