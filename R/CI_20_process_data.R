source('../R/functions.R')
source('../R/CI_20_process_data_functions.R')

if (CI_isParent()) CI_startMatter()

#######################
## Reef names lookup 
## AIMS_REEF_MANE and REEF_ZONE required to link to new disturbance table DISTURBANCES2
## OUTSTANDING is full mapping to potential reporting zones...
## This should most probably be done via intersection of sample waypoints with spatial layers.
#######################

CURRENT_STAGE <<- 2
CI__change_setting(item = 'CURRENT_STAGE', value = 2)
CI__add_stage(stage = paste0('STAGE',CURRENT_STAGE), title = 'Stage 2 - process data') 

if (CI$setting[['FRESH_START']]) CI_clear_processed_data()

## AIMS data
CI_process_add_report_year()
CI_process_transect_area()
CI_process_locations_data()
CI_process_total_points()
CI_process_hc_points()
CI_process_ma_points()
CI_process_a_points()
CI_process_combine_points()

## Obtain (pre-wrangled) external data
CI_process_external_data()

## Combine AIMS and external data
CI_process_combine_data()

## Assign regions
CI_process_spatial()
CI_process_assign_regions()

## Juvenile density
CI_process_juvenile_density()
CI_process_juvenile_offset()
CI_process_juvenile_data_for_baselines()

## Unique site locations
CI_process_unique_site_location()
