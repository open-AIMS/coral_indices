################################################################################
##Check peak density distributions for the four parameters
#Peak density is effective for identifying particularly slow trajectories
#........that are undesirable for setting baseline standards of recovery

load(file=paste0(PROC_DATA_DIR, "mcmc.baseline.traj.ess500.RData"))

#There were 2 trajectories that were above 1.1 but barely. After looking at their chains,
#..... they didn't look bad enough to throw out.
rm.poor.chains<- mcmc.baseline.traj.ess500 %>%
  filter(!mpsrf>1.2) %>% droplevels()

save(rm.poor.chains, file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

load(file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

par.list=vector(mode="list", length=length(unique(rm.poor.chains$Parameter)))
names(par.list)<- unique(rm.poor.chains$Parameter)

for (p in unique(rm.poor.chains$Parameter)) {
  
  density.df<- rm.poor.chains %>%
    filter(Parameter==p) %>% droplevels() %>%
    ungroup() %>%
    group_by(proj.site.rpid) %>%
    mutate(density.max=which.max(density(value)$y)) %>%
    ungroup()
  
  rpid.max.density<- density.df %>%
    dplyr::select(ZONE, Shelf, zone.shelf, NRM, BIOREGION, REEF, DEPTH.f, REEF.d, RP_ID, proj.site.rpid, density.max) %>% unique
  
  rpid.list=vector(mode="list", length=length(unique(density.df$proj.site.rpid)))
  names(rpid.list)<- unique(density.df$proj.site.rpid)
  
  for(rpid in unique(density.df$proj.site.rpid)) {
    
    max.density.y.df= rpid.max.density %>% filter(proj.site.rpid==rpid) %>% droplevels()
    max.density.y= max.density.y.df$density.max
    
    density.df.rpid=density.df %>% filter(proj.site.rpid==rpid) %>% droplevels()
    
    peak.density.df= data.frame(peak.density=density(density.df.rpid$value)$x[max.density.y])
    
    rpid.list[[rpid]]=peak.density.df %>% mutate(proj.site.rpid=rpid) %>%
      left_join(max.density.y.df)
    
  }
  
  rpid.peak.density.df<- do.call("rbind", rpid.list)
  
  par.list[[p]]=rpid.peak.density.df %>% mutate(Parameter=p)
  
}

parameter.peak.density.df<- do.call("rbind", par.list) %>%
  mutate(bioregion.depth=paste(BIOREGION, DEPTH.f, sep=" "))

save(parameter.peak.density.df, file=paste0(PROC_DATA_DIR, "parameter.peak.density.df.RData"))

################################################################################
# Parameter peak densities

load(file=paste0(PROC_DATA_DIR, "parameter.peak.density.df.RData"))

## load(file=paste(PROC_DATA_DIR, 
##            sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
##            ,".RData",sep=""))
if (RPI_PURPOSE == "baseline") {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                "_", RPI_PURPOSE, ".RData",sep=""))
} else {
    load(file=paste(PROC_DATA_DIR, 
                sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs),
                ".RData",sep=""))
}

calculate.min.report.year<- filt.rec.traj %>%
  group_by(proj.site.rpid) %>% 
  mutate(min.report.year=min(REPORT_YEAR))
  
spread.parameters<- parameter.peak.density.df %>%
  left_join(calculate.min.report.year %>% 
              dplyr::select(proj.site.rpid, min.report.year, max.report.year) %>% distinct) %>%
  dplyr::select(-density.max) %>%
  spread(key="Parameter", value="peak.density") %>%
  mutate(phase.1.gr=alpha*alphaD) #Calculate phase 1 growth rate

save(spread.parameters, file=paste0(PROC_DATA_DIR, "spread.parameters.RData"))
################################################################################
# When I re-ran this to test code, the reefs that got filtered out would change slightly,
#.... which obviously isn't ideal.
#Instead, I've named the trajectories that got filtered out, so that they don't change when re-run
#.... But not sure if there is a better solution?


        ################################################################################
        load(file=paste0(PROC_DATA_DIR, "spread.parameters.RData"))

        ##Particularly slow recovery trajectories have a low alpha (2nd phase growth rate)
        ##Slow recoveries are also evident as trajectories with high Td and relatively low alpha D

        #These criteria filter out particularly slow recovery trajectories.
        filter.alpha<- spread.parameters %>%
          filter(!alpha<0.04) %>% droplevels()

        filter.Td<- filter.alpha %>%
          filter(!(Td>4 & phase.1.gr<0.04)) %>% droplevels() %>%
          filter(!(Td>2 & phase.1.gr<0.01)) %>% droplevels()

        save(filter.Td, file=paste0(PROC_DATA_DIR, "filter.Td.RData"))
