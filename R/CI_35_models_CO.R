source('../R/functions.R')
source('../R/CI_35_models_CO_functions.R')
source('../R/CI_30_models_functions.R')

if (CI_isParent()) CI_startMatter()

CURRENT_STAGE <<- 6
CI__change_setting(item = 'CURRENT_STAGE', value = CURRENT_STAGE)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE),
              title = paste0('Stage ', CURRENT_STAGE, ' - modelling Composition data'))

if (CI$setting[['FRESH_START']]) CI_clear_models_CO_data()

## Fit models
CI_models_CO_prepare_data()
CI_models_CO_calc_mds()
