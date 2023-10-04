source('../R/functions.R')

## singularity exec -B .:/home/Project ../coral_indicators.sif Rscript 00_main.R --final_year=2023 --fresh_start=true --runStage=1:5 --rerun_baselines=true

if (CI_isParent()) CI_startMatter()

## if (CI$setting[['FRESH_START']]) source('CI_05_clear_data.R')

if (1 %in% CI$setting[['runStage']]) source('CI_10_get_data.R')
## CI__change_setting(item = 'runStage', value = 1)

if (2 %in% CI$setting[['runStage']]) source('CI_20_process_data.R')

if (3 %in% CI$setting[['runStage']]) source('CI_31_models_MA.R')

if (4 %in% CI$setting[['runStage']]) source('CI_32_models_CC.R')

if (5 %in% CI$setting[['runStage']]) source('CI_33_models_JU.R')

if (6 %in% CI$setting[['runStage']]) source('CI_34_models_RPI.R')

if (7 %in% CI$setting[['runStage']]) source('CI_35_models_CO.R')

## singularity exec -B .:/home/Project ../coral_indicators.sif Rscript 00_main.R --final_year=2023 --fresh_start=true --runStage=8 --rerun_baselines=true
if (8 %in% CI$setting[['runStage']]) source('CI_40_collation.R')
