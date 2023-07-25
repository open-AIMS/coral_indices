source('../R/functions.R')
source('../R/CI_40_collation_functions.R')

if (CI_isParent()) CI_startMatter()

CURRENT_STAGE <<- 8
CI__change_setting(item = 'CURRENT_STAGE', value = CURRENT_STAGE)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE),
              title = paste0('Stage ', CURRENT_STAGE, ' - collating indices'))

if (CI$setting[['FRESH_START']]) CI_clear_models_collation_data()

## Combine indices
CI_models_collate_indices()
