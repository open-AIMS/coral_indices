#' Append derived data function
#' 
#' @param traj Trajectory data
#'
#' @description This function just adds the number of observations
append_num_obs <- function(traj) {
  num_obs <- length(unique(traj$REPORT_YEAR))
  visits <- traj %>% 
    select(REPORT_YEAR,Date) %>% 
    unique() %>% 
    arrange(REPORT_YEAR)
  diff_in_days = as.numeric(visits$Date[num_obs]-visits$Date[1],units = "days")
  return(mutate(traj,NUM_OBS = num_obs,DURATION = diff_in_days))
}

#' Trajectory filter function
#' @description User defined filter function implementing the condition
#'   \code{HC_0 < 5} percent and \code{HC_tn > 15} percent and \code{n >= 5}
#'
#' @param traj coral cover data subset that matches a \code{RP_ID} or site pair
trajectory_filter_condition <- function(traj) {
  # Short-cut if not enough observations
  if (traj$NUM_OBS[1] < min_obs) {
    return(FALSE)
  } else {
    # condition total coral cover % = total HC % + total SC % < thresh_cover %
    first_visit <- min(traj$REPORT_YEAR)
    last_visit <- max(traj$REPORT_YEAR)
    init_state <- traj %>% 
      filter(REPORT_YEAR == first_visit,GROUP_CODE == 'HC')
    final_state <- traj %>% 
      filter(REPORT_YEAR == last_visit,GROUP_CODE == 'HC')
    # site level cover is derived from the mean of transect level cover
    return(mean(init_state$COVER) < max_init 
           && mean(final_state$COVER) > min_final)
  }
}

#' Trajectory filter function
#' @description  User defined filter function implementing the condition
#'   \code{HC_0 < 5} percent and \code{n >= 5}
#'
#' @param traj coral cover data subset that matches a \code{RP_ID} or site pair
trajectory_filter_condition_no_end <- function(traj) {
  # Short-cut if not enough observations
  if (traj$NUM_OBS[1] < min_obs) {
    return(FALSE)
  } else {
    # condition total coral cover % = total HC % + total SC % < thresh_cover %
    first_visit <- min(traj$REPORT_YEAR)
    last_visit <- max(traj$REPORT_YEAR)
    init_state <- traj %>% 
      filter(REPORT_YEAR == first_visit,GROUP_CODE == 'HC')
    final_state <- traj %>% 
      filter(REPORT_YEAR == last_visit,GROUP_CODE == 'HC')
    # site level cover is derived from the mean of transect level cover
    tf <- (mean(init_state$COVER) < max_init 
           && mean(final_state$COVER) > mean(init_state$COVER))
    # check no complete zeros
    for (visit in traj$REPORT_YEAR) {
      state <- traj %>% 
        filter(REPORT_YEAR == visit,GROUP_CODE == 'HC')
      tf <- tf && (mean(state$COVER) > 0.0)
    }
    return(tf)
  }
}


################################################################################
predict.ongoing.random <- function(baseline.df, ongoing.df,model,N){
  traj.ids <- ongoing.df %>% select(c("RP_ID")) %>% unique() 
  # for each ongoing trajectory
  preds <- data.frame(HC_PRED = c(), RP_ID = c())
  for (traj.id in traj.ids$RP_ID){
    # use all samples across past individual estimates
    rp.mcmc <- baseline.df %>% 
      ungroup() %>% 
      select(c("Iteration", "Chain", "RP_ID", "Parameter","value")) %>% 
      spread(key=Parameter,value=value) %>% 
      select(-c("Chain","Iteration","RP_ID"))
    
    #For 1 parameter per draw, comment these three lines out
    #Nall <- nrow(rp.mcmc)
    #pps <- rep(0,N)
    #samples <- sample(Nall,N,replace=FALSE)
    
    traj.rp.id <- ongoing.df  %>% filter(RP_ID == traj.id)
    for (draw.id in 1:1000){
      # for 1 parameter per draw
      Nall <- nrow(rp.mcmc)
      pps <- rep(0,N)
      samples <- sample(Nall,N,replace=FALSE)
      
      # get obs n and HC n-1 and T of n-1
      traj <- traj.rp.id %>% filter(TRANSECT_NO == draw.id) %>% arrange(REPORT_YEAR) %>%
        mutate('T' =  as.double(Date - Date[1]))
      n <- nrow(traj)
      # build data object 
      data <- list(nVisits = 1, c0 = traj$HC[n-1],t0 = traj$T[n-1]/365.0, 
                   ts = traj$T[n]/365.0, K = K.limit - traj$AB[n], 
                   #C = traj$HC[n], Serr = traj$lower.error[n])
                   #C = traj$HC[n], 
                   #Serr = traj$HC_se[n])
                   Serr = 0.0)
      
      for (i in 1:N){
        theta <- as.numeric(rp.mcmc[samples[i],])
        names(theta) <- names(rp.mcmc[samples[i],])
        sim <- model$like_sampler(data,model,theta)
        pps[i] <- sim[2]
      }
      rp.mcmc.pred <- data.frame(HC_PRED = pps, TRANSECT_NO = rep(draw.id,N), RP_ID = rep(traj.id,N),
                                 #HC_OBS = rep(traj$HC[n],N),HC_se = rep(traj$lower.error[n],N))
                                 HC_OBS = rep(traj$HC[n],N))#,HC_se = rep(traj$HC_se[n],N))
      preds <- rbind(preds,rp.mcmc.pred)
    }
  }
  return(preds)
}

################################################################################
predict.ongoing.fixed <- function(baseline.df, ongoing.df,model,N){
  traj.ids <- ongoing.df %>% select(c("RP_ID")) %>% unique() 
  # for each ongoing trajectory
  preds <- data.frame(HC_PRED = c(), RP_ID = c())
  for (traj.id in traj.ids$RP_ID){
    # use all samples across past individual estimates
    rp.mcmc <- baseline.df %>% 
      ungroup() %>% 
      select(c("Iteration", "Chain", "RP_ID", "Parameter","value")) %>% 
      spread(key=Parameter,value=value) %>% 
      select(-c("Chain","Iteration","RP_ID"))
    
    #For 1 parameter per draw, comment these three lines out
    Nall <- nrow(rp.mcmc)
    pps <- rep(0,N)
    samples <- sample(Nall,N,replace=FALSE)
    
    traj.rp.id <- ongoing.df  %>% filter(RP_ID == traj.id)
    for (draw.id in 1:1000){
      # for 1 parameter per draw
      #Nall <- nrow(rp.mcmc)
      #pps <- rep(0,N)
      #samples <- sample(Nall,N,replace=FALSE)
      
      # get obs n and HC n-1 and T of n-1
      traj <- traj.rp.id %>% filter(TRANSECT_NO == draw.id) %>% arrange(REPORT_YEAR) %>%
        mutate('T' =  as.double(Date - Date[1]))
      n <- nrow(traj)
      # build data object 
      data <- list(nVisits = 1, c0 = traj$HC[n-1],t0 = traj$T[n-1]/365.0, 
                   ts = traj$T[n]/365.0, K = K.limit - traj$AB[n], 
                   #C = traj$HC[n], Serr = traj$lower.error[n])
                   #C = traj$HC[n], 
                   #Serr = traj$HC_se[n])
                   Serr = 0.0)
      
      for (i in 1:N){
        theta <- as.numeric(rp.mcmc[samples[i],])
        names(theta) <- names(rp.mcmc[samples[i],])
        sim <- model$like_sampler(data,model,theta)
        pps[i] <- sim[2]
      }
      rp.mcmc.pred <- data.frame(HC_PRED = pps, TRANSECT_NO = rep(draw.id,N), RP_ID = rep(traj.id,N),
                                 #HC_OBS = rep(traj$HC[n],N),HC_se = rep(traj$lower.error[n],N))
                                 HC_OBS = rep(traj$HC[n],N))#,HC_se = rep(traj$HC_se[n],N))
      preds <- rbind(preds,rp.mcmc.pred)
    }
  }
  return(preds)
}

################################################################################
predict.previous.random <- function(baseline.df, ongoing.df,model,N){
  traj.ids <- ongoing.df %>% select(c("RP_ID")) %>% unique() 
  # for each ongoing trajectory
  preds <- data.frame(HC_PRED = c(), RP_ID = c())
  for (traj.id in traj.ids$RP_ID){
    # use all samples across past individual estimates
    rp.mcmc <- baseline.df %>% 
      ungroup() %>% 
      select(c("Iteration", "Chain", "previous.RP_ID", "Parameter","value")) %>% 
      spread(key=Parameter,value=value) %>% 
      select(-c("Chain","Iteration","previous.RP_ID"))
    
    #For 1 parameter per draw, comment these three lines out
    #Nall <- nrow(rp.mcmc)
    #pps <- rep(0,N)
    #samples <- sample(Nall,N,replace=FALSE)
    
    traj.rp.id <- ongoing.df  %>% filter(RP_ID == traj.id)
    for (draw.id in 1:1000){
      # for 1 parameter per draw
      Nall <- nrow(rp.mcmc)
      pps <- rep(0,N)
      samples <- sample(Nall,N,replace=FALSE)
      
      # get obs n and HC n-1 and T of n-1
      traj <- traj.rp.id %>% filter(TRANSECT_NO == draw.id) %>% arrange(REPORT_YEAR) %>%
        mutate('T' =  as.double(Date - Date[1]))
      n <- nrow(traj)
      # build data object 
      data <- list(nVisits = 1, c0 = traj$HC[n-1],t0 = traj$T[n-1]/365.0, 
                   ts = traj$T[n]/365.0, K = K.limit - traj$AB[n], 
                   #C = traj$HC[n], Serr = traj$lower.error[n])
                   #C = traj$HC[n], 
                   #Serr = traj$HC_se[n])
                   Serr = 0.0)
      
      for (i in 1:N){
        theta <- as.numeric(rp.mcmc[samples[i],])
        names(theta) <- names(rp.mcmc[samples[i],])
        sim <- model$like_sampler(data,model,theta)
        pps[i] <- sim[2]
      }
      rp.mcmc.pred <- data.frame(HC_PRED = pps, TRANSECT_NO = rep(draw.id,N), RP_ID = rep(traj.id,N),
                                 #HC_OBS = rep(traj$HC[n],N),HC_se = rep(traj$lower.error[n],N))
                                 HC_OBS = rep(traj$HC[n],N))#,HC_se = rep(traj$HC_se[n],N))
      preds <- rbind(preds,rp.mcmc.pred)
    }
  }
  return(preds)
}

predict.from.fixed.HCstart <- function(baseline.df, model, N, spec.time, HC.start, AB.cover){
  traj.ids <- baseline.df %>% select(c("RP_ID")) %>% unique() 
  # for each ongoing trajectory
  preds <- data.frame(HC_PRED = c(), RP_ID = c())
  for (traj.id in traj.ids$RP_ID){
    # use all samples across past individual estimates
    rp.mcmc <- baseline.df %>% 
      ungroup() %>% 
      select(c("Iteration", "Chain", "RP_ID", "Parameter","value")) %>% 
      spread(key=Parameter,value=value) %>% 
      select(-c("Chain","Iteration","RP_ID"))


    for (draw.id in 1:1000){
      # for 1 parameter per draw
      Nall <- nrow(rp.mcmc)
      pps <- rep(0,N)
      samples <- sample(Nall,N,replace=FALSE)

      # build data object 
      data <- list(nVisits = 1, c0 = HC.start,t0 = 0, 
                   ts = spec.time, K = K.limit - AB,
                   Serr = 1)

      for (i in 1:N){
        theta <- as.numeric(rp.mcmc[samples[i],])
        names(theta) <- names(rp.mcmc[samples[i],])
        sim <- model$like_sampler(data,model,theta)
        pps[i] <- sim[2]
      }
      rp.mcmc.pred <- data.frame(HC_PRED = pps, TRANSECT_NO = rep(draw.id,N), RP_ID = rep(traj.id,N)
                                 #HC_OBS = rep(traj$HC[n],N),HC_se = rep(traj$lower.error[n],N))
      )#,HC_se = rep(traj$HC_se[n],N))
      preds <- rbind(preds,rp.mcmc.pred)
    }
  }
  return(preds)
}
