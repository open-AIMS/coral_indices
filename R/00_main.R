source('../R/functions.R')

if (CI_isParent()) CI_startMatter()

if (CI$setting[['FRESH_START']]) source('CI_05_clear_data.R')
