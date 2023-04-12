######################################################################
## Notes made by Angus                                              ##
##                                                                  ##
## For all mmp data use mmp_site_name as REEF                       ##
## For al LTMP data use AIMS_REEF_NAME as REEF                      ##
######################################################################


source('../R/functions.R')
if (CI_isParent()) CI_startMatter()

#######################
## Reef names lookup 
## AIMS_REEF_MANE and REEF_ZONE required to link to new disturbance table DISTURBANCES2
## OUTSTANDING is full mapping to potential reporting zones...
## This should most probably be done via intersection of sample waypoints with spatial layers.
#######################


CI_get_names_lookups()


