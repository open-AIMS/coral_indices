#' Apply MCMC general model
#' 
#' @name RRC_coralFitGrowthModel.R
#' @description  script applies Markov chain Monte Carlo to sample the Bayesian posterior of the parameters
#' for the general logistic model with delays. The generalised model is a two phase ODE model.
#'
#' @author David J. Warne (david.warne@qut.edu.au)
#'           ARC Centre of Excellence for Mathematical and Statistical Frontiers
#'           School of Mathematical Sciences 
#'           Science and Engineering Faculty
#'           Queensland University of Technology
#'
#' @author Grace E. M. Heron (g.heron@qut.edu.au)
#'           ARC Centre of Excellence for Mathematical and Statistical Frontiers
#'           School of Mathematical Sciences 
#'           Science and Engineering Faculty
#'           Queensland University of Technology
#'
NULL

print("Fitting Coral growth model with 8000 iterations")

#*******************************************************************************
## import model definition
source(paste0(MODEL_DIR,'DefineTwoPhaseGeneralModelSingleSpeciesTypeII.R'))

#*******************************************************************************
## build MCMC sampler configuration
  conf <- list(chains = 4,        # number of independent chains to use  
               iter = 8000,       # number of sampling iterations (or number of iterations between diagnostic checks) 
               burnin = 8000,     # number of iterations per burning/warmup step 
               CPUs = 4,          # number of CPUs available for parallel chains (optimal CPUs = chains)
               nadapt = 1,        # number of adaptation steps
               initscale = 0.1,
               Rthresh = 1.1,     # stopping criteria threshold for Gelman-Rubin statistic diagnostic check
               ESSthresh = 400,  # stopping criteria threshold for Effective Sample Size diagnostic check
               maxChecks = 5,
               convcheck = TRUE,  # repeat iterations until stopping criteria are satisfied #has been added #default TRUE
               maxInits = 10000
               )  

#*******************************************************************************
## Fit Growth Model
  
  ## load filter results 
  load(paste(PROC_DATA_DIR, sprintf(FILTER_REFMT_OUT_DATA_FMT,
                                   max_init,min_final,min_obs)
             ,".RData",sep=""))
  
  ## Perform model calibration to each recovery trajectory using MCMC 
  tic()
  filt.rec.traj.proc.mcmc.res <- vector("list", length = length(filt.rec.traj.proc))
  
  names(filt.rec.traj.proc.mcmc.res)<- unique(names(filt.rec.traj.proc))
  
  count=0
  
  for (i in unique(names(filt.rec.traj.proc))) {
  ## print("Running in parallel") 
  ## cl <- makeCluster(10)
  ## registerDoParallel(cl)
  ## filt.rec.traj.proc.mcmc.res <- foreach(i = unique(names(filt.rec.traj.proc)),
  ##                                        .packages = c('tidyverse', 'foreach', 'doParallel',
  ##                                                      'future')
  ##                                                 ) %dopar% {
    
    reef.group.list<- filt.rec.traj.proc[[i]]
    
    rpid.list<- vector(mode='list', length=length(reef.group.list))
    names(rpid.list)<-unique(names(reef.group.list))
    
    for(jj in unique(names(reef.group.list))){
      
      count=count+1
      print(paste("trajectory number", count, sep=" "))
      ## ensure data is sorted in ascending time order
      traj <- reef.group.list[[jj]] %>% arrange(REPORT_YEAR)
      
      ## build data object
      data <- list(nVisits = length(traj$HC[-1]),         # number of visits excluding initial visit  
                   c0 = traj$HC[1],                       # cover of initial visit
                   t0 = traj$T[1]/365.0,                  # time of initial visit in years
                   ts = traj$T[-1]/365.0,                 # time of final visit in years
                   K = K.limit - traj$AB[length(traj$HC)],    # carrying capacity cover
                   C = traj$HC[-1],                       # cover value time series 
                   Serr = traj$HC_sd[-1])                 # cover standard deviation time series
      
      ## update upper bound for T_d
      model$upper[4] <- traj$T[length(traj$T)]/365.0
      print(paste0("MCMC sampling for rpid ", jj, " in bioregion ", i))
      ## Store model in list
      set.seed(123)
      samples <- adaptMCMC_fit_ode_model(data,model,conf)
      
      if (length(samples)!=0) {
        ## Store predictions in list
        rpid.list[[jj]]=samples
      }
      
      save(samples, file=paste0(MCMC_OUTPUT_DIR, "mcmc.samples.rpid.", jj, ".bioregion.", i, ".RData"))
      
    }
    
    
    
    filt.rec.traj.proc.mcmc.res[[i]] <- rpid.list
      ## rpid.list
  }
  ## stopCluster(cl)
  ## names(filt.rec.traj.proc.mcmc.res)<- unique(names(filt.rec.traj.proc))
  toc()
  
  ## save results
  save(filt.rec.traj.proc.mcmc.res,file=paste(PROC_DATA_DIR,"mcmc.bioregion.baseline.RData",sep=""))
