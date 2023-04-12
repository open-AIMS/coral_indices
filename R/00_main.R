source('../R/functions.R')

if (CI_isParent()) CI_startMatter()

if (CI$setting[['FRESH_START']]) source('CI_05_clear_data.R')

source('CI_10_get_data.R')
CI__change_setting(item = 'runStage', value = 1)
