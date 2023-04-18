CI_clear_models_MA_data <- function() {
    ## unlink("../data/modelled/*.*", recursive = TRUE)
}

CI_models_MA_get_baselines <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'get_baselines',
                   label = "Get baseline model posteriors", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/meshdraws_MApShallow.RData"))
        load(file = paste0(DATA_PATH, "modelled/meshdraws_MApDeep_inshore.RData"))
        load(file = paste0(DATA_PATH, "modelled/baselineMAoffshore_mesh_draws.RData"))
        load(file=paste0(DATA_PATH, 'processed/site.location.RData'))
        
        ## note: offshore baseline estimates for bioregions for which
        ## there are no observations

        ## ma.baselines.reef.draws.temp <- ma.offshore.baseline.draws %>%
        ##     ungroup() %>%
        ##     mutate(Rep = as.integer(sub('V', '', Rep)) )%>%
        ##     left_join(site.location) %>%
        ##     filter(!is.na(DEPTH.f)) %>% # remove NA's intreoduced for bioregions with no data
        ##     rbind(ma.inshore.deep.baseline.draws %>%
        ##           left_join(site.location)) %>%
        ##     rbind(ma.inshore.shallow.baseline.draws %>%
        ##           left_join(site.location)) %>%
        ##     rename(baseline.value = value)

        ## save(ma.baselines.reef.draws,
        ##      file = paste0(DATA_PATH, 'modelled/ma.baselines.reef.draws.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'get_baselines',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Get baseline model posteriors'), return=NULL)
}

CI_models_MA_prepare_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'prepare_data',
                   label = "Prepare modelling data", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "processed/points.analysis.data.transect.RData"))

        data <- points.analysis.data.transect %>%
            mutate(DEPTH.f = factor(case_when(DEPTH > 3 ~"deep slope",
                                              DEPTH <= 3 ~"shallow slope")),
                   Site = factor(paste(REEF, DEPTH.f, SITE_NO)),
                   REEF.d = factor(paste(REEF, DEPTH.f)),
                   Transect = factor(paste(REEF, DEPTH.f, SITE_NO, TRANSECT_NO)),
                   fYEAR = factor(as.numeric(as.character(REPORT_YEAR)))) %>%
            group_by(Site) %>%
            mutate(Obs=factor(1:n())) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        ## Model terminated when there were reefs with only 1 level of
        ## the "REPORT_YEAR" factor. Remove
        report.year.ss <- data %>% 
            dplyr::select(Site, fYEAR) %>% 
            distinct() %>% 
            group_by(Site) %>% 
            summarise(report.year.ss = n()) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        df.a <- data %>%
            left_join(report.year.ss) %>% 
            filter(!report.year.ss == "1") %>% 
            droplevels() %>%
            suppressMessages() %>%
            suppressWarnings()

        ## Also failed if there were all zeros at a site, when this
        ## occurs transform T1 value to 1
        ## This seems a very adhoc and prone to unexpected consequences solution
        ## I will replace it!
        ## zero.site.year <- df.a %>%
        ##     group_by(Site, fYEAR) %>%
        ##     summarise(MAs = sum(MA)) %>%
        ##     ungroup() %>%
        ##     suppressMessages() %>%
        ##     suppressWarnings()

        ## data <- df.a %>%
        ##     left_join(zero.site.year) %>%
        ##     mutate(MA = ifelse(MAs == '0' & TRANSECT_NO == '1', 1, MA),
        ##            total.MA = ifelse(MAs == '0' & TRANSECT_NO == '1', 1, MA)) %>%
        ##     suppressMessages() %>%
        ##     suppressWarnings()
        data <- df.a %>%
            mutate(MA = ifelse(MA == 0 & A == 0, NA, MA),
                   total.MA = MA,
                   A = ifelse(is.na(MA) & A == 0, NA, A))

        save(data,
              file = paste0(DATA_PATH, 'modelled/data_ma.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'prepare_data',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare modelling data'), return=NULL)
}


CI_models_MA_prepare_nest <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'data_nest',
                   label = "Prepare data nest", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ma.RData"))
        
        ## nest the data
        mods <- data %>% 
            ## group_by(Site) %>%  ## Angus said that Site-level models were an accident
            group_by(REEF.d) %>%
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
                                                     Transect = NA, Obs = NA,
                                                     MA = NA, A = NA) %>%
                                       distinct()
                                   ),
                   Full_data = pmap(.l = list(data, newdata),
                                    .f = ~ ..1 %>% bind_rows(..2))
                   )
        save(mods,
              file = paste0(DATA_PATH, 'modelled/MA__mods.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'data_nest',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare data nest'), return=NULL)
}



CI__fit_MA_model <- function(form, data, family='binomial', n, N) {
    CI_tryCatch({
        ## site <- unique(data$Site)
        reef <- unique(data$REEF.d)
        CI__append_label(stage = CI__get_stage(), item = 'fit_models',
                         n, N)
        if (file.exists(paste0(DATA_PATH, "modelled/MA__", reef, '__model.RData'))) {
            CI_log(status = 'INFO', logFile = LOG_FILE, Category = "--MA models--",
                   msg = paste0("Reuse ", reef, " MA model"))
            return(NULL)
        }
        mod <- inla(formula = form,
                    data = data,
                    Ntrials = data$A,
                    family = family, 
                    ## control.family=list(link='logit'),
                    control.predictor = list(link = 1, compute = TRUE),
                    control.compute = list(
                        dic = TRUE, cpo = TRUE, waic = TRUE,
                        config = TRUE) 
                    )
        save(mod, file = paste0(DATA_PATH, "modelled/MA__", reef, '__model.RData'))
        draws <- inla.posterior.sample(n=1000, mod, seed=123) %>%
            suppressWarnings() %>%
            suppressMessages()
        save(draws, file = paste0(DATA_PATH, "modelled/MA__", reef, '__draws.RData'))
    }, logFile=LOG_FILE, Category='--MA models--',
    msg=paste0('Fit ', reef, ' MA model'), return=NULL)
}

CI__cellmeans_MA_model <- function(obs_data, Full_data, newdata) {
    CI_tryCatch({
        reef <- unique(newdata$REEF.d)
        draws <- get(load(file = paste0(DATA_PATH, "modelled/MA__", reef, '__draws.RData')))
        cellmeans <- sapply(draws, function(x)
            x[[2]][(nrow(Full_data)-nrow(newdata)+1):nrow(Full_data)]) 
        posteriors <- newdata %>%
            dplyr::select(fYEAR, REEF.d) %>%
            cbind(plogis(cellmeans)) %>%
            pivot_longer(cols = matches('[0-9]'), names_to = 'Rep') %>%
            mutate(REEF.d = reef) %>%
            left_join(obs_data %>%
                      dplyr::select(fYEAR, REEF.d) %>%
                      distinct()) %>% 
            suppressWarnings() %>%
            suppressMessages()
        save(posteriors, file = paste0(DATA_PATH, "modelled/MA__", reef, '__posteriors.RData'))
    }, logFile=LOG_FILE, Category='--MA models--',
    msg=paste0('Posteriors for ', reef, ' MA model'), return=NULL)
}



CI_models_MA_fit_models <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'fit_models',
                   label = "Fit MA models", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ma.RData"))
        load(file = paste0(DATA_PATH, 'modelled/MA__mods.RData'))

        form <- MA ~ fYEAR +
            f(Site , model='iid') +
            f(Transect , model='iid') +
            f(Obs, model='iid')       
        ## Fit the models - output models and draws to
        ## DATA_PATH/modelled/MA__.*__.RData
        purrr::pwalk(.l = list(mods$Full_data,mods$n),
                     .f = ~ CI__fit_MA_model(form = form, data = ..1,
                                             family = 'binomial', n = ..2, N = nrow(mods))
                   )

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'fit_models',status = 'success')

    }, logFile=LOG_FILE, Category='--MA models--',
    msg=paste0('Fit MA models'), return=NULL)
}


CI_models_MA_cellmeans <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'cellmeans',
                   label = "Cell means of MA models", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ma.RData"))
        ## Calculate cellmeans
        cellmeans <- purrr::pwalk(.l = list(mods$data[1], mods$Full_data[1], mods$newdata[1]),
                                 .f = ~ CI__cellmeans_MA_model(..1, ..2, ..3)) 
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'cellmeans',status = 'success')

    }, logFile=LOG_FILE, Category='--MA models--',
    msg=paste0('Cell means of MA models'), return=NULL)
}

CI_models_MA_preds <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'predictions',
                   label = "Calculate predictions", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ma.RData"))
        mods <- mods %>%
            mutate(Pred = map(.x = REEF.d,
                              .f = ~ get(load(file = paste0(DATA_PATH,
                                                            "modelled/MA__", .x, '__posteriors.RData'))) %>%
                                  dplyr::select(fYEAR, REEF.d, .draw = Rep, value)),
                   Summary = pmap(.l = list(data, Pred),
                                  .f = ~ ..2 %>% posterior::as_draws() %>%
                                      group_by(fYEAR, REEF.d) %>% 
                                      posterior::summarise_draws(mean,
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
        pwalk(.l = list(mods$REEF.d, mods$Summary),
              .f = ~ ..2 %>% save(file = paste0(DATA_PATH, "modelled/MA__", ..1, '__summary.RData')))

        save(mods,
              file = paste0(DATA_PATH, 'modelled/MA__models.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'fit_models',status = 'success')

    }, logFile=LOG_FILE, Category='--MA models--',
    msg=paste0('Fit MA models'), return=NULL)
}

CI_models_MA_distance <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'template',
                   label = "Template", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/MA__models.RData"))
        ## load(file = paste0(DATA_PATH, "modelled/MA__models.RData"))
        
        ## save(template,
        ##       file = paste0(DATA_PATH, 'modelled/template.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'template',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Template'), return=NULL)
}

CI_template <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'template',
                   label = "Template", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/template.RData"))
        
        ## save(template,
        ##       file = paste0(DATA_PATH, 'modelled/template.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'template',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Template'), return=NULL)
}

