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

## Baseline - only run once - include random seed
## Baseline 1. Set configs, settings (may not be needed) and load functions
## - restrincted to up to and including 2020
## - RPI_b_baseline_config.R
##2. determine whether a reef is in recovery
## - identify which obs are in decline
## - RPI_baseline_1_extract CoralData.R
##3. filter tradjectories so only including those with >= 4 obs (Reef/Depth)
## - RPI_baseline_2_filterBaselineTrajectories.R
##4. Calculate summaries add benthic data
## - RPI_baseline_3_processData.R
## 5. Fit two-phase model to each trajectory
## - RPI_baseline_4_coralFitGrowthModel.R

##6. Gather all data together and filter the chains to even up sample
##   size
## - RPI_baseline_6_gather_and_thin.R

##7. Filter out those that dont meet the convergence criteria
##   (and standards)
## - generate a baseline data frame
## - RPI_baseline_8_rmNonCOnvergence_and_calc_peak_density.R
## The last script above has been replaced by
## RPI_baseline_8_predict10YearIncrease.R
## which calculates something about the last 10 years



## Reference index - run each (year)
##1. primary data set up
## - RPI_i_RefIndex_primarydata_setup.R
##2. loop for each report year
##2.1. Config - setup directories, filters, load functions
##   - RPI_i_RefIndex_config.R
##2.2. Extract coral recovery trajectories
##   - RPI_RefIndex_1_extractCoralData.R
##2.3. Predict cover for curent year from baseline parameters
##   - RPI_RefIndex_2_PredictLastObs_bioregion.R
##3. Gather and calculate baseline index score
## - RPI_RefIndex_3_gather_preds.R

## Critical index - run each (year)
##1. primary data set up
##2. loop for each report year
##2.1. Config - setup directories, filters, load functions
##   - RPI_x_CriticalIndex_config.R
##2.2. Extract coral recovery trajectories
##   - RPI_x_CriticalIndex_1_extractCoralData.R
##2.3. filter out obs that are at least the 4th along the trajectory or more
##   - parameterise the model with all but the last obs and then predict the last obs
##   - RPI_x_CriticalIndex_2_PredsFromCurrentTraj.R
##2.4. filter out obs 2 and 3 in trajectory, search for previous trajectory (with >= 3 obs),
##     parameterise it and use it to predict CC for point of interest
##   - RPI_x_CriticalIndex_3_PredsFromPreviousTraj.R
##2.5. gather two sources of predictions together
##   - RPI_x_CriticalIndex_5_gatherpreds.R <- put this outside the loop
##3. Calculate the score
## - RPI_x_CriticalIndex_6_calculateScore.R
