source('../R/functions.R')
source('../R/CI_33_models_JU_functions.R')
source('../R/CI_30_models_functions.R')

if (CI_isParent()) CI_startMatter()

CURRENT_STAGE <<- 5
CI__change_setting(item = 'CURRENT_STAGE', value = CURRENT_STAGE)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE),
              title = paste0('Stage ', CURRENT_STAGE, ' - modelling JU data'))

if (CI$setting[['FRESH_START']]) CI_clear_models_JU_data()


## Generate baseline models
source("CI_27_JU_baseline_models.R")

## Combine baseline models
CI_models_JU_get_baselines()

## Fit models
CI_models_JU_prepare_data()
CI_models_JU_prepare_nest() 
CI_models_JU_fit_models()
CI_models_JU_cellmeans()
CI_models_JU_preds()

## Calculate distances to baselines
CI_models_JU_distance()
CI_models_aggregation(Indicator = 'JU', level = 'BIOREGION.agg') 
CI_models_aggregation(Indicator = 'JU', level = 'NRM') 
CI_models_aggregation(Indicator = 'JU', level = 'TUMRA') 
CI_models_aggregation(Indicator = 'JU', level = 'GBRMPA.MA') 
CI_models_aggregation(Indicator = 'JU', level = 'GBRMP') 
CI_models_aggregation(Indicator = 'JU', level = 'ZONE') 
