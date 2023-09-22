source('../R/functions.R')
source('../R/CI_32_models_CC_functions.R')
source('../R/CI_30_models_functions.R')

if (CI_isParent()) CI_startMatter()

CURRENT_STAGE <<- 4
CI__change_setting(item = 'CURRENT_STAGE', value = CURRENT_STAGE)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE),
              title = paste0('Stage ', CURRENT_STAGE, ' - modelling CC data'))

if (CI$setting[['FRESH_START']]) CI_clear_models_CC_data()

## Generate baseline models
source("CI_26_CC_baseline_models.R")

## Combine baseline models
CI_models_CC_get_baselines()

## Fit models
CI_models_CC_prepare_data()
CI_models_CC_prepare_nest() 
CI_models_CC_fit_models()
CI_models_CC_cellmeans()
CI_models_CC_preds()

## Calculate distances to baselines
CI_models_CC_distance()
CI_models_aggregation(Indicator = 'CC', level = 'BIOREGION.agg') 
CI_models_aggregation(Indicator = 'CC', level = 'NRM') 
CI_models_aggregation(Indicator = 'CC', level = 'TUMRA') 
CI_models_aggregation(Indicator = 'CC', level = 'GBRMPA.MA') 
CI_models_aggregation(Indicator = 'CC', level = 'GBRMP') 
CI_models_aggregation(Indicator = 'CC', level = 'ZONE') 
