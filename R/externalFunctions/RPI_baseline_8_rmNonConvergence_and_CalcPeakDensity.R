################################################################################
## Filter out trajectories with mpsrf>1.2

load(file=paste0(PROC_DATA_DIR, "mcmc.baseline.traj.ess500.RData"))

#There were 2 trajectories that were above 1.1 but barely. After looking at their chains,
#..... they didn't look bad enough to throw out.
rm.poor.chains<- mcmc.baseline.traj.ess500 %>%
  filter(!mpsrf>1.2) %>% droplevels()

save(rm.poor.chains, file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

################################################################################
##Check peak density distributions for the four parameters
#Peak density is effective for identifying particularly slow trajectories
#........that are undesirable for setting baseline standards of recovery

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

load(file=paste(PROC_DATA_DIR, 
           sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
           ,".RData",sep=""))

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


        # ################################################################################
        # load(file=paste0(PROC_DATA_DIR, "spread.parameters.RData"))
        # 
        # ##Particularly slow recovery trajectories have a low alpha (2nd phase growth rate)
        # ##Slow recoveries are also evident as trajectories with high Td and relatively low alpha D
        # 
        # #These criteria filter out particularly slow recovery trajectories.
        # filter.alpha<- spread.parameters %>% 
        #   filter(!alpha<0.04) %>% droplevels()
        # 
        # filter.Td<- filter.alpha %>%
        #   filter(!(Td>4 & phase.1.gr<0.04)) %>% droplevels() %>%
        #   filter(!(Td>2 & phase.1.gr<0.01)) %>% droplevels() 
        # 
        # save(filter.Td, file=paste0(PROC_DATA_DIR, "filter.Td.RData"))
        # 
        # ################################################################################
        # #Filter thinned chains by the proj.site.rpids that remain after excluding high Tds and low alphas
        # 
        # load(file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))
        # 
        # load(file=paste0(PROC_DATA_DIR, "filter.Td.RData"))
        # 
        # load(file=paste(PROC_DATA_DIR, 
        #                 sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
        #                 ,".RData",sep=""))
        # 
        # baseline.trajectories<- rm.poor.chains %>%
        #   left_join(filt.rec.traj %>% dplyr::select(proj.site.rpid, max.report.year) %>% distinct) %>%
        #   filter(proj.site.rpid %in% filter.Td$proj.site.rpid) %>% droplevels()
        # 
        # save(baseline.trajectories, file=paste0(PROC_DATA_DIR, "baseline.trajectories.RData"))

################################################################################
# lastly, there are baseline trajectories that are outliers, 
  #....reducing sensitivity of the resulting index. They are filtered out from baseline.
  #They can generally be described as reefs with very low alphas or reefs with long time delays combined with low phase 1 growth rates
#Also, There are 6 bioregions that don't have any baseline trajectories.
  #They need to be aggregated as sensibly as possible with other bioregions

#load(file=paste0(PROC_DATA_DIR, "baseline.trajectories.RData"))

load(file=paste0(PROC_DATA_DIR, "rm.poor.chains.RData"))

load(file=paste(PROC_DATA_DIR, 
                   sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
                   ,".RData",sep=""))

RPI.baseline<- #baseline.trajectories %>%
  rm.poor.chains %>%
  left_join(filt.rec.traj %>% dplyr::select(proj.site.rpid, max.report.year) %>% distinct) %>% ## add max report year
  filter(!proj.site.rpid %in% c("Pandora deep slope 282",              #*
                                "Shute Harbour deep slope 337",        #*
                                "Daydream shallow slope 53",           #*
                                "Havannah Island deep slope 615",      #*
                                "Pelican shallow slope 292",           #*
                                "Keppels South shallow slope 207",     #*
                                "Peak deep slope 287",                 #*
                                "Snapper North shallow slope 346",     #*
                                "Palms West shallow slope 303",        #*
                                "Palms West shallow slope 304",        #*
                                "Camp East shallow slope 1",
                                "Magnetic deep slope 161",
                                "Pandora shallow slope 276",
                                "Palms West shallow slope 305",
                                "Havannah Island deep slope 614",
                                "Chicken Reef deep slope 377",
                                "Myrmidon Reef deep slope 439",
                                "Dip Reef deep slope 572",
                                "Hook shallow slope 196",
                                "Hook deep slope 200",
                                "Seaforth shallow slope 327",
                                "Seaforth deep slope 331",
                                "Border Island deep slope 534",
                                "Langford and Bird Isles deep slope 654",
                                "Reef 19-131 deep slope 735",
                                "Reef 19-138 deep slope 740",
                                "Hyde Reef deep slope 635",
                                "Hyde Reef deep slope 636",
                                "Rebe Reef deep slope 730",
                                "North Keppel deep slope 265",
                                "Peak shallow slope 286",
                                "Peak deep slope 288",
                                "Rat shallow slope 22",
                                "Reef 21-296 deep slope 492",
                                "Reef 21-139 deep slope 483",
                                "East Cay deep slope 576",
                                "Turner Cay deep slope 790",
                                "Franklands West deep slope 147",
                                "Hastings Reef deep slope 605",
                                "Michaelmas Cay deep slope 701",
                                "Opal Reef deep slope 724",
                                "Opal Reef deep slope 725",
                                "St. Crispin Reef deep slope 780",
                                "St. Crispin Reef deep slope 781")) %>%
  droplevels() %>%
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

save(RPI.baseline, file=paste0(PROC_DATA_DIR, "RPI.baseline.RData"))