################################################################################
## Filter out trajectories with mpsrf>1.2

load(file=paste0(PROC_DATA_DIR, "mcmc.baseline.traj.ess500.RData"))

#There were 2 trajectories that were above 1.1 but barely. After looking at their chains,
#..... they didn't look bad enough to throw out.
rm.poor.chains<- mcmc.baseline.traj.ess500 %>%
  filter(!mpsrf>1.2) %>% droplevels()

save(rm.poor.chains, file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))


################################################################################
#Not all of these trajectories meet an acceptable standard for setting recovery benchmarks.

#Which trajectories are on track to achieve an average annual increase of 2% per year over 10 years of recovery?
#Starting at 20% HC cover (the minimum required for a positive carbonate budget)

#.. will predict 'HC cover increase after 10 years of recovery' for each trajectory,
#.. starting at 20% HC,
#.. using it's own parameters.

if (RPI_PURPOSE == "baseline") {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                "_", RPI_PURPOSE, ".RData",sep=""))
} else {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                ".RData",sep=""))
}

dates<- filt.rec.traj %>% dplyr::select(proj.site.rpid, REPORT_YEAR, Date) %>% unique() %>%
  group_by(proj.site.rpid) %>%
  arrange(proj.site.rpid, REPORT_YEAR) %>%
  mutate(Time.since.disturbance =  as.double(Date - Date[1])/365)

#Load baseline dataframe
load(file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

cover.transect<- filt.rec.traj %>% left_join(dates) %>%
  spread(GROUP_CODE, value=COVER) %>%
  filter(proj.site.rpid %in% rm.poor.chains$proj.site.rpid) %>% droplevels()

# define the model for prediction
source(paste0(MODEL_DIR, 'DefineTwoPhaseGeneralModelSingleSpeciesTypeII.R'))
model <- list(ode_func = general_logistic_twophase,       # RHS for ODE model
              ode_sol = general_logistic_twophase_analytic,
              like_sampler = like_sampler,                # simulation of data generation process (for pred. checks)
              varnames = vlab)

count=0

traj.list<- vector(mode="list", length=length(unique(cover.transect$proj.site.rpid)))
names(traj.list)<- unique(cover.transect$proj.site.rpid)
  
  
  for(traj in unique(cover.transect$proj.site.rpid)){
    
    count=count+1
    print(paste("trajectory count", count, sep=" "))
    
    print(paste("Trajectory", traj, sep=" "))
    
    #Select the abiotic cover at the survey time closest to 10 years.
    AB.at.4years<- cover.transect %>% filter(proj.site.rpid==traj, TRANSECT_NO==1) %>% droplevels() %>%
      ungroup() %>%
      filter(Time.since.disturbance<10) %>% droplevels() %>%
      filter(Time.since.disturbance==max(Time.since.disturbance)) %>% droplevels()
    
    AB<- as.numeric(AB.at.4years$AB[1])
    
    #Select the model parameters from the corresponding bioregion group
    baseline.traj<- rm.poor.chains %>% filter(proj.site.rpid == traj) %>% droplevels() %>%
      ungroup()
    
    #Predict HC for the last observation of the trajectory
    current.traj.preds <- predict.from.fixed.HCstart(baseline.traj, 
                                                model,1, 10, 20, AB)  %>% 
      mutate(proj.site.rpid=traj,
             HC.start=20,
             pred.time=10) %>%
      mutate(COVER.diff=HC_PRED-HC.start)
    
    traj.list[[traj]]=current.traj.preds
    
  }
  
  pred.start.df<-do.call("rbind", traj.list)
  
save(pred.start.df, file=paste0(PROC_DATA_DIR, "pred.start.df.RData"))

HC.increase.thresholds<- pred.start.df %>%
  left_join(filt.rec.traj %>% dplyr::select(proj.site.rpid, BIOREGION, DEPTH.f) %>% distinct) %>%
  ungroup %>%
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
                                            BIOREGION %in% c("19", "22")~"bio.19and22"))) %>%
  group_by(proj.site.rpid) %>%
  mutate(median.COVER.diff=round(median(COVER.diff, digits=0)),
         ref.trajectory=factor(ifelse(median.COVER.diff>=20, "Yes", "No")))

save(HC.increase.thresholds, file=paste0(PROC_DATA_DIR, "HC.increase.thresholds.RData"))

#******************************************************************************
#*filter posteriors by reference trajectories that meet minimum standards
load(file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

load(file=paste0(PROC_DATA_DIR, "HC.increase.thresholds.RData"))

if (RPI_PURPOSE == "baseline") {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                "_", RPI_PURPOSE, ".RData",sep=""))
} else {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                ".RData",sep=""))
}

RPI.baseline <- rm.poor.chains %>%
  left_join(filt.rec.traj %>% dplyr::select(proj.site.rpid, max.report.year) %>% distinct) %>% ## add max report year
  left_join(HC.increase.thresholds %>%
              dplyr::select(proj.site.rpid, BIOREGION.rpi.agg, ref.trajectory) %>% distinct) %>%
  filter(ref.trajectory=="Yes" | proj.site.rpid == "North Direction Island deep slope 717") %>% droplevels()

save(RPI.baseline, file=paste0(PROC_DATA_DIR, "RPI.baseline.RData"))
