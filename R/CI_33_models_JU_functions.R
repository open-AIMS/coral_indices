CI_clear_models_JU_data <- function() {
    files <- list.files(path = paste0(DATA_PATH, "modelled"),
                        pattern = "JU.*|data_ju.*",
                        full.names = TRUE)
    unlink(files)
}

CI_models_JU_get_baselines <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'get_baselines',
                   label = "Get baseline model posteriors", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "parameters/JUV_baseline.RData"))

        baselines <- juv.baseline %>%
            ## mutate(DEPTH.f = ifelse(DEPTH.f == 'deep', 'deep slope', 'shallow slope'))
            mutate(DEPTH.f = ifelse(str_detect(DEPTH.f, 'deep'), 'deep slope', 'shallow slope'))
        
        save(baselines,
             file = paste0(DATA_PATH, 'modelled/JU__baseline_posteriors.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'get_baselines',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Get baseline model posteriors'), return=NULL)
}

CI__clean_ju_data <- function(data) {
    df.a <- data %>%
        ungroup() %>%
        filter(!REPORT_YEAR == "NA") %>%
        droplevels %>%
        dplyr::select(-VISIT_NO)
    ## Model terminated when there were
    ## reefs with only 1 level of the
    ## "REPORT_YEAR" factor. Remove
    ## Could use Murray's function in
    ## place of this
    report.year.ss <- df.a %>%
        dplyr::select(REEF.d, REPORT_YEAR) %>%
        unique() %>% 
        group_by(REEF.d) %>%
        summarise(report.year.ss = n()) %>%
        ungroup()
    df.a <- df.a %>%
        left_join(report.year.ss) %>%
        filter(!report.year.ss == "1") %>%
        droplevels
    
    ## For each site, if there is a year
    ## with no juvs, then the year is
    ## removed, and put back in later
    ## with an estimate of 0
    detect.zeros <- df.a %>% 
        group_by(REEF.d, fYEAR) %>%
        summarise(juv.transect.sum = sum(value)) %>%
        ungroup
    
    df.b<- df.a %>% 
        left_join(detect.zeros) %>% 
        filter(!juv.transect.sum==0) %>% 
        droplevels
    ## Finally, model also terminates if
    ## there is only 1 level of 'year'
    ## greater than 0 (i.e. no variance)
    no.variance <- df.b %>%
        dplyr::select(REEF.d, fYEAR, juv.transect.sum) %>% 
        filter(juv.transect.sum>0) %>%
        droplevels() %>%
        dplyr::select(REEF.d, fYEAR) %>%
        unique() %>%
        group_by(REEF.d) %>%
        summarise(ss.years.morethanzero = n()) %>%
        ungroup

    df.b %>% 
        left_join(no.variance) %>% 
        filter(ss.years.morethanzero>1) %>% 
        droplevels %>%
        dplyr::select(-any_of(c("juv.transect.sum",
                                "report.year.ss")))
}

CI_models_JU_prepare_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'prepare_data',
                   label = "Prepare modelling data", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "processed/juv.df.RData"))
        load(file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))

        juv <- juv.df %>%
            group_by(REEF.d) %>%
            mutate(LONGITUDE = mean(LONGITUDE),
                   LATITUDE = mean(LATITUDE),
                   ) %>%
            ungroup() %>%
            dplyr::select(-any_of(c("total.juv.excl.turb",
                                    "Turbinaria", "Non_Acropora",
                                    "Non_Acr_exTurb"))) %>%
            dplyr::rename("Total" = total.juv) %>%
            pivot_longer(cols = c(Total, Acropora),
                         names_to = 'Taxa',
                         values_to = 'value') %>% 
            group_by(DEPTH.f, Taxa) %>%
            nest()

        data <- juv %>%
            mutate(data = map(.x = data,
                              .f = ~ CI__clean_ju_data(.x) %>%
                                  suppressMessages() %>%
                                  suppressWarnings()
                              )
                   )
        
        save(data,
              file = paste0(DATA_PATH, 'modelled/data_ju.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'prepare_data',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare modelling data'), return=NULL)
}

CI_models_JU_prepare_nest <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'data_nest',
                   label = "Prepare data nest", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ju.RData"))
        
        ## nest the data
        mods <- data %>% 
            unnest(data) %>%
            ungroup() %>% 
            group_by(REEF.d, Taxa) %>%
            ## note in more recent versions of dplyr (1.1.0 -
            ## cur_data_all has been replaced by pick)
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            mutate(n = 1:n())
        ## Prepare the data
        mods <- mods %>%
            mutate(newdata = map(.x = data,
                                 .f = ~ .x %>%
                                     droplevels() %>% 
                                     ## tidyr::expand(Site = Site, fYEAR = fYEAR, # replace
                                     tidyr::expand(REEF.d = REEF.d, fYEAR = fYEAR,
                                                   Site = NA, Transect = NA,
                                                   value = NA) %>%
                                     distinct()
                                 ),
                   Full_data = pmap(.l = list(data, newdata),
                                    .f = ~ ..1 %>% bind_rows(..2))
                   )
        save(mods,
              file = paste0(DATA_PATH, 'modelled/JU__mods.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'data_nest',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare data nest'), return=NULL)
}


CI__fit_JU_model <- function(form, data, family='nbinomial', n, N) {
    CI_tryCatch({
        ## site <- unique(data$Site)
        reef <- unique(data$REEF.d)
        taxa <- na.omit(unique(data$Taxa))
        CI__append_label(stage = CI__get_stage(), item = 'fit_models',
                         n, N)
        if (file.exists(paste0(DATA_PATH, "modelled/JU__", reef, '_', taxa, '__model.RData'))) {
            CI_log(status = 'INFO', logFile = LOG_FILE, Category = "--JU models--",
                   msg = paste0("Reuse ", reef, " (", taxa, ") JU model"))
            return(NULL)
        }
        mod <- inla(formula = form,
                    ## offset = log(data$avail.area),
                    E = data$avail.area,
                    data = data,
                    family = family, 
                    ## control.family=list(link='logit'),
                    control.predictor = list(link = 1, compute = TRUE),
                    control.compute = list(
                        dic = TRUE, cpo = TRUE, waic = TRUE,
                        config = TRUE) 
                    )
        save(mod, file = paste0(DATA_PATH, "modelled/JU__", reef, '_', taxa, '__model.RData'))
        draws <- inla.posterior.sample(n=1000, mod, seed=123) %>%
            suppressWarnings() %>%
            suppressMessages()
        save(draws, file = paste0(DATA_PATH, "modelled/JU__", reef, '_', taxa, '__draws.RData'))
    }, logFile=LOG_FILE, Category='--JU models--',
    msg=paste0('Fit ', reef, ' (', taxa,') JU model'), return=NULL)
}


CI_models_JU_fit_models <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'fit_models',
                   label = "Fit JU models", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ju.RData"))
        load(file = paste0(DATA_PATH, 'modelled/JU__mods.RData'))

        form <- value ~ fYEAR +
            f(Site , model='iid')
        
        ## Fit the models - output models and draws to
        purrr::pwalk(.l = list(mods$Full_data,mods$n),
                     .f = ~ CI__fit_JU_model(form = form, data = ..1,
                                             ## family = 'zeroinflatednbinomial1',
                                             ## family = 'nbinomial2',
                                             family = 'nbinomial',
                                             n = ..2,
                                             N = nrow(mods))
                   )

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'fit_models',status = 'success')

    }, logFile=LOG_FILE, Category='--JU models--',
    msg=paste0('Fit MA models'), return=NULL)
}

CI__cellmeans_JU_model <- function(obs_data, Full_data, newdata, n, N) {
    CI_tryCatch({
        reef <- unique(newdata$REEF.d)
        taxa <- na.omit(unique(obs_data$Taxa))
        CI__append_label(stage = CI__get_stage(), item = 'cellmeans',
                         n, N)
        if (file.exists(paste0(DATA_PATH, "modelled/JU__", reef, '_', taxa, '__posteriors.RData'))) {
            CI_log(status = 'INFO', logFile = LOG_FILE, Category = "--JU models--",
                   msg = paste0("Reuse ", reef, " (", taxa, ") JU cellmeans"))
            return(NULL)
        }
        draws <- get(load(file = paste0(DATA_PATH, "modelled/JU__", reef, '_', taxa, '__draws.RData')))
        cellmeans <- sapply(draws, function(x)
            x[[2]][(nrow(Full_data)-nrow(newdata)+1):nrow(Full_data)]) 
        posteriors <- newdata %>%
            dplyr::select(fYEAR, REEF.d) %>%
            cbind(exp(cellmeans)) %>%
            pivot_longer(cols = matches('[0-9]'), names_to = 'Rep') %>%
            mutate(REEF.d = reef,
                   .draw = as.integer(Rep)) %>%
            dplyr::select(-Rep) %>%
            left_join(obs_data %>%
                      dplyr::select(fYEAR, REEF.d) %>%
                      distinct()) %>% 
            suppressWarnings() %>%
            suppressMessages()
        save(posteriors, file = paste0(DATA_PATH, "modelled/JU__", reef, '_', taxa, '__posteriors.RData'))
    }, logFile=LOG_FILE, Category='--JU models--',
    msg=paste0('Posteriors for ', reef, ' (', taxa, ') JU model'), return=NULL)
}

CI_models_JU_cellmeans <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'cellmeans',
                   label = "Cell means of JU models", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ju.RData"))
        load(file = paste0(DATA_PATH, 'modelled/JU__mods.RData'))
        ## Calculate cellmeans
        cellmeans <- purrr::pwalk(.l = list(mods$data, mods$Full_data, mods$newdata, mods$n, nrow(mods)),
                                 .f = ~ CI__cellmeans_JU_model(..1, ..2, ..3, ..4, ..5)) 
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'cellmeans',status = 'success')

    }, logFile=LOG_FILE, Category='--JU models--',
    msg=paste0('Cell means of JU models'), return=NULL)
}

CI_models_JU_preds <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'predictions',
                   label = "Calculate predictions", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ju.RData"))
        load(file = paste0(DATA_PATH, 'modelled/JU__mods.RData'))
        CI_log(status = "SUCCESS",
               logFile = LOG_FILE,
               Category = list.files(paste0(DATA_PATH,"modelled"), pattern = "JU__mods"),
               msg=NULL)
        mods <- mods %>%
            mutate(Pred = map2(.x = REEF.d, .y = Taxa,
                               .f = ~ get(load(file = paste0(DATA_PATH,
                                                             "modelled/JU__", .x, '_', .y, '__posteriors.RData'))) %>%
                                   dplyr::select(fYEAR, REEF.d, .draw, value)),
                   Summary = pmap(.l = list(data, Pred),
                                  .f = ~  ..2 %>% posterior::as_draws() %>%
                                      group_by(fYEAR, REEF.d) %>%  
                                      tidybayes::summarise_draws(mean,
                                                                 sd,
                                                                 median,
                                                                 HDInterval::hdi) %>%
                                      left_join(..1 %>%
                                                dplyr::select(fYEAR, Site) %>%
                                                distinct()) %>%
                                      suppressMessages() %>%
                                      suppressWarnings()
                              )
               )
        pwalk(.l = list(mods$REEF.d, mods$Taxa, mods$Summary),
              .f = ~ ..3 %>% save(file = paste0(DATA_PATH, "modelled/JU__", ..1, '_', ..2, '__summary.RData')))
        
        save(mods,
             file = paste0(DATA_PATH, 'modelled/JU__preds.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'predictions',status = 'success')
        
    }, logFile=LOG_FILE, Category='--JU models--',
    msg=paste0('Calculate predictions'), return=NULL)
}


CI__index_JU <- function(dat, taxa, baselines) {
    dat %>%
        left_join(baselines %>%
                  filter(Taxa == taxa) %>%
                  dplyr::rename(baseline = value)) %>%
        mutate(
            distance.met = log2(baseline/value),
            cap.dist.met = as.numeric(case_when(distance.met < -3 ~ -3,
                                              distance.met > 3 ~3,
                                              distance.met > -3 & distance.met < 3 ~
                                                  distance.met)),
            rescale.dist.metric = scales::rescale(cap.dist.met, from = c(-3, 3), to = c(1, 0))) %>%
        dplyr::select(-any_of(ends_with("met"))) %>%
        pivot_longer(cols = ends_with('metric'), names_to = 'Metric', values_to = '.value') %>%
        filter(!is.na(REEF.d)) %>% 
        suppressMessages() %>%
        suppressWarnings()
}


CI_models_JU_distance <- function() {
    ## This is going to need modifying once Manu has a model for
    ## critical juvenile densities.  The posteriors of this model
    ## will be compared to the Acropora posteriors (rather than the
    ## Acropora posteriors of the baseline model)
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'indices',
                   label = "Calculate indices", status = 'pending')
    CI_tryCatch({

        baselines <- get(load(file = paste0(DATA_PATH,
                                            'modelled/JU__baseline_posteriors.RData')))
        mods <- get(load(file = paste0(DATA_PATH, "modelled/JU__preds.RData")))
        load(file=paste0(DATA_PATH, 'processed/site.location.RData'))
        ## load(file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))

        mods <- mods %>%
            mutate(Pred = map(.x = Pred,
                              .f = ~ .x %>%
                                  left_join(site.location %>%
                                            dplyr::select(REEF, REEF.d, BIOREGION.agg,
                                                          DEPTH.f) %>%
                                            distinct()) %>%
                                  suppressMessages() %>%
                                  suppressWarnings())) %>% 
            mutate(Scores = map2(.x = Pred, .y = Taxa,
                                .f = ~ CI__index_JU(.x, .y, baselines) %>%
                                    filter(Metric %in% c('rescale.dist.metric',
                                                         'pcb.rescale.dist.metric')) %>%
                                    mutate(fYEAR = factor(fYEAR, levels = unique(fYEAR))) %>%
                                    arrange(fYEAR, .draw)
                               )) %>%
            dplyr::select(-any_of(c("data", "newdata","Full_data", "Pred", "Summary"))) %>%
            ## Since each of Total and Acropora are modelled separately, we need to unnest
            ## the Scores dataframes and then rbind them together for each Reef, before
            ## pivotting and finally re-grouping.
            dplyr::select(-any_of(c("REEF.d","Taxa", "n"))) %>%
            unnest(Scores) %>%
            dplyr::select(-Metric) %>%
            dplyr::rename(Metric = Taxa) %>%
            group_by(REEF.d) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            dplyr::rename(Scores = data) %>%
            mutate(Summary = map(.x = Scores,
                                 .f = ~ .x %>%
                                     dplyr::select(-any_of(c(
                                                "P_CODE",
                                                "Model",
                                                "value",
                                                "baseline",
                                                "DEPTH.f"))) %>%
                                     group_by(fYEAR, REEF, REEF.d, BIOREGION.agg, Metric) %>%
                                     tidybayes::summarise_draws(median, mean, sd,
                                                     HDInterval::hdi,
                                                     `p<0.5` = ~ mean(.x < 0.5)
                                                     )
                                 ),
                   Below = map(.x = Summary,
                               .f = ~ .x %>%
                                   ungroup() %>%
                                   mutate(Below = ifelse(upper < 0.5, 1, 0),
                                          PBelow = ifelse(`p<0.5` > 0.9, 1, 0)) %>%
                                   dplyr::select(fYEAR, Metric, Below, PBelow) %>%
                                   distinct()
                               )
                   ) %>% 
            suppressMessages() %>%
            suppressWarnings()

        save(mods,
              file = paste0(DATA_PATH, 'modelled/JU__scores_reef_year.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'indices',status = 'success')

    }, logFile=LOG_FILE, Category='--JU models--',
    msg=paste0('Calculate indices'), return=NULL)
}
