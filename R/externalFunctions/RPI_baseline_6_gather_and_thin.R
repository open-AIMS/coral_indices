################################################################################
#Unlist the posteriors and convert to dataframes
#Assess approriate thinning level (want trajectories to be sampled equally)
#Thin the chains

#*******************************************************************************
#Load MCMC object
load(file=paste(PROC_DATA_DIR,"mcmc.bioregion.baseline.RData",sep=""))

#Load the matching list with spatial variables
load(file=paste(PROC_DATA_DIR,
                sprintf(FILTER_REFMT_OUT_DATA_FMT,max_init,min_final,min_obs)
                ,".RData",sep=""))

mcmc.list=vector(mode='list', length=length(filt.rec.traj.proc.mcmc.res))
names(mcmc.list)<- unique(names(filt.rec.traj.proc.mcmc.res))

mpsrf.list.group=vector(mode='list', length=length(unique(names(filt.rec.traj.proc.mcmc.res))))
names(mpsrf.list.group)<- unique(names(filt.rec.traj.proc.mcmc.res))

for (i in unique(names(filt.rec.traj.proc.mcmc.res))) {

  print(paste("bioregion", i, sep=" "))

  reef.group.list<- filt.rec.traj.proc.mcmc.res[[i]]

  rpid.list<- vector(mode='list', length=length(unique(names(reef.group.list))))
  names(rpid.list)<-unique(names(reef.group.list))

  mpsrf.list<- vector(mode='list', length=length(unique(names(reef.group.list))))
  names(mpsrf.list)<-unique(names(reef.group.list))


  for(mm in unique(names(reef.group.list))){

    print(paste("RP_ID", mm, sep=" "))
    ## Fitted model

    spatial.var<- filt.rec.traj.proc[[i]][[mm]]
    
    #get posteriors for 4 parameters per trajectory
    mcmc <- reef.group.list[[mm]]
    mcmc.df<- ggs(mcmc) %>% mutate(RP_ID=factor(mm), BIOREGION=factor(i)) %>% as.data.frame

    rpid.list[[mm]]=mcmc.df

    #Get mpsrf statistic
    #Attach spatial info
    mpsrf= data.frame(mpsrf=gelman.diag(reef.group.list[[mm]])[[2]],
                      ess=effectiveSize(reef.group.list[[mm]])[[2]]) %>%
      mutate(RP_ID=factor(mm), NRM=spatial.var$NRM[1], BIOREGION=factor(i), ZONE=spatial.var$ZONE[1], 
             Shelf=spatial.var$Shelf[1], REEF=spatial.var$REEF[1],
             DEPTH.f=spatial.var$DEPTH.f[1], REEF.d=spatial.var$REEF.d[1], proj.site.rpid=spatial.var$proj.site.rpid[1])
    


    mpsrf.list[[mm]]=mpsrf

    }


  rpid.list.df=do.call('rbind', rpid.list) #%>% t %>%
  #as.data.frame

  mpsrf.list.df=do.call('rbind', mpsrf.list)

  mcmc.list[[i]]=rpid.list.df

  mpsrf.list.group[[i]]=mpsrf.list.df

}


mpsrf.group.df<- do.call('rbind', mpsrf.list.group)
save(mpsrf.group.df, file=paste0(PROC_DATA_DIR, "mpsrf.group.df.RData"))

mcmc.group.df<- do.call('rbind', mcmc.list) 
#add mpsrf to mcmc
mcmc.bioregions.mpsrf<- mcmc.group.df %>% ungroup() %>%
  left_join(mpsrf.group.df)

#save mcmc
save(mcmc.bioregions.mpsrf, file=paste0(PROC_DATA_DIR, "mcmc.bioregions.mpsrf.RData"))

#R studio memory full

#Clear environment
## rm(list = ls())
## Rather than clear all memory, lets just clear the objects created in this script
rm(list = c("filt.rec.traj.proc.mcmc.res",
            "mcmc.list",
            "mpsrf.list.group",
            "reef.group.list",
            "rpid.list",         
            "mpsrf.list",
            "spatial.var",       
            "mcmc",
            "mcmc.df",
            "mpsrf",           
            "mpsrf.group.df",
            "mcmc.group.df",          
            "mcmc.bioregions.mpsrf"
            ))
gc()

#Set up datapaths and load in functions again
## source('RPI_b_baseline_config.R')

#load mcmc object
load(file=paste0(PROC_DATA_DIR, "mcmc.bioregions.mpsrf.RData"))

#*******************************************************************************
#How many iterations do the trajectories have?
tic()

print("filtering mcmc table configs")

#This step takes a long time......
mcmc.converge.info<- mcmc.bioregions.mpsrf %>%
    ungroup() %>%
    group_by(proj.site.rpid) %>%
    mutate(iterations=max(Iteration)) %>%
    dplyr::select(ZONE, Shelf, NRM, BIOREGION, REEF, DEPTH.f, REEF.d, RP_ID, proj.site.rpid, iterations, mpsrf, ess) %>% unique()

toc()

save(mcmc.converge.info, file=paste0(PROC_DATA_DIR, "mcmc.converge.info.RData"))

#*******************************************************************************
# Optimise thinning

load(file=paste0(PROC_DATA_DIR, "mcmc.converge.info.RData"))

#Want all parameter distributions from all trajectories to have the same sample size
#Need to select a sample size that is close to the minimum ESS recorded,
#.........to ensure independence of samples, 
#.........but not so low it starts changing the parameter distributions for the others....

mcmc.table<- mcmc.converge.info %>%
  mutate(ess.thinning.interval=iterations/ess,
         thinning.interval.ess500=iterations/500,
         ess.lessthan.500=factor(ifelse(ess<500, "Y", "N")),
         non.independence.gap=thinning.interval.ess500-ess.thinning.interval)

# Sample size of 500 seems to work pretty well

#save as a table that can be annotated
save(mcmc.table, file=paste0(PROC_DATA_DIR, "mcmc.table.RData"))
write.csv(mcmc.table, file=paste0(PROC_DATA_DIR, "mcmc.table.csv"))

##Filter all trajectories to a sample size of 500.
load(file=paste0(PROC_DATA_DIR, "mcmc.bioregions.mpsrf.RData"))

mcmc.baseline.traj.ess500<- mcmc.bioregions.mpsrf %>%
  ungroup() %>%
  left_join(mcmc.table) %>%
  mutate(thin.to.ess500=round(thinning.interval.ess500)) %>%
  group_by(NRM, BIOREGION, ZONE, Shelf, REEF, DEPTH.f, REEF.d, RP_ID, proj.site.rpid,
           Chain, Parameter) %>%
  slice(which(row_number() %% thin.to.ess500 == 1)) %>%
  mutate(zone.shelf=paste(ZONE, Shelf, sep=" "))

check<- mcmc.baseline.traj.ess500 %>% group_by(proj.site.rpid, Chain, Parameter) %>%
  summarise(ss=n())

save(mcmc.baseline.traj.ess500, file=paste0(PROC_DATA_DIR, "mcmc.baseline.traj.ess500.RData"))


