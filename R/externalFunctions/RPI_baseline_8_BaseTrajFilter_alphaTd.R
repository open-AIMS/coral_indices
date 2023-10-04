################################################################################
#Filter thinned chains by the proj.site.rpids that remain after excluding high Tds and low alphas
#There are 6 bioregions that don't have any baseline trajectories.
#They need to be aggregated as sensibly as possible with other bioregions

if (RPI_PURPOSE == "baseline") {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                "_", RPI_PURPOSE, ".RData",sep=""))
} else {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                ".RData",sep=""))
}
## load(file=paste(PROC_DATA_DIR, 
##                 sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
##                 ,".RData",sep=""))

load(file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

load(file=paste0(PROC_DATA_DIR, "filter.Td.RData"))

## load(file=paste(PROC_DATA_DIR,
##                 sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
##                 ,".RData",sep=""))

RPI.baseline<- rm.poor.chains %>%
  left_join(filt.rec.traj %>% dplyr::select(proj.site.rpid, max.report.year) %>% distinct) %>%
  filter(proj.site.rpid %in% filter.Td$proj.site.rpid) %>% droplevels() %>%
  mutate(BIOREGION.rpi.agg=factor(case_when(BIOREGION %in% c("18", "29")~"bio.18and29",
                                            BIOREGION %in% c("9", "26")~"bio.9and26",
                                            BIOREGION %in% c("39")~"bio.39",
                                            BIOREGION %in% c("12","35","36")~"bio.12and35and36",
                                            BIOREGION %in% c("3", "4")~"bio.3and4",
                                            BIOREGION %in% c("27")~"bio.27",
                                            BIOREGION %in% c("11")~"bio.11",
                                            BIOREGION %in% c("37")~"bio.37",
                                            BIOREGION %in% c("23")~"bio.23",
                                            BIOREGION %in% c("6")~"bio.6",
                                            BIOREGION %in% c("26")~"bio.26",
                                            BIOREGION %in% c("16")~"bio.16",
                                            BIOREGION %in% c("17")~"bio.17",
                                            BIOREGION %in% c("34", "38")~"bio.34and38",
                                            BIOREGION %in% c("19", "22")~"bio.19and22")))

## save(RPI.baseline, file=paste0(PROC_DATA_DIR, "RPI.baseline.", K.version, ".", base.traj.filter, ".RData"))

save(RPI.baseline, file=paste0(PROC_DATA_DIR, "RPI.baseline.RData"))
