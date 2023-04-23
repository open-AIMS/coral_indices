CI_clear_models_CC_data <- function() {
    ## unlink("../data/modelled/*.*", recursive = TRUE)
}

CI_models_CC_prepare_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'prepare_data',
                   label = "Prepare modelling data", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "processed/points.analysis.data.transect.RData"))
        load(file = paste0(DATA_PATH, "processed/spatial_lookup.RData"))

        site.data <- points.analysis.data.transect %>%
            mutate(DEPTH.f=factor(case_when(DEPTH>3~"deep slope",
                                            DEPTH<=3~"shallow slope")),
                   Site=factor(paste(REEF,DEPTH.f,SITE_NO)),
                   REEF.d=factor(paste(REEF, DEPTH.f)),
                   Transect=factor(paste(REEF,DEPTH.f,SITE_NO,TRANSECT_NO)),
                   fYEAR=factor(as.numeric(as.character(REPORT_YEAR)))) %>%
            group_by(Site) %>%
            mutate(Obs=factor(1:n())) %>%
            ungroup

        df.a <- site.data %>% 
            left_join(spatial_lookup) %>%
            mutate(BIOREGION = as.factor(BIOREGION)) %>%
            dplyr::select(-VISIT_NO)

        ## Model terminated when there were reefs with only 1 level of
        ## the "REPORT_YEAR" factor. Remove Could use Murray's
        ## function in place of this
        report.year.ss <- df.a %>%
            dplyr::select(Site, REPORT_YEAR) %>%
            unique() %>% 
            group_by(Site) %>%
            summarise(report.year.ss = n())

        df.a <- df.a %>%
            left_join(report.year.ss) %>%
            filter(!report.year.ss == "1") %>%
            droplevels

        detect.zeros <- df.a %>%
            group_by(Site, fYEAR) %>%
            summarise(HC.transect.sum = sum(HC))

        data <- df.a %>%
            left_join(detect.zeros) %>%
            filter(!HC.transect.sum == 0) %>%
            droplevels
        
        save(data,
              file = paste0(DATA_PATH, 'modelled/data_cc.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'prepare_data',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare modelling data'), return=NULL)
}

CI_models_CC_prepare_nest <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'data_nest',
                   label = "Prepare data nest", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_cc.RData"))
        
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
                                                     HC = NA, total.points = NA) %>%
                                       distinct()
                                   ),
                   Full_data = pmap(.l = list(data, newdata),
                                    .f = ~ ..1 %>% bind_rows(..2))
                   )
        save(mods,
              file = paste0(DATA_PATH, 'modelled/CC__mods.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'data_nest',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare data nest'), return=NULL)
}

CI__fit_CC_model <- function(form, data, family='binomial', n, N) {
    CI_tryCatch({
        ## site <- unique(data$Site)
        reef <- unique(data$REEF.d)
        CI__append_label(stage = CI__get_stage(), item = 'fit_models',
                         n, N)
        if (file.exists(paste0(DATA_PATH, "modelled/CC__", reef, '__model.RData'))) {
            CI_log(status = 'INFO', logFile = LOG_FILE, Category = "--CC models--",
                   msg = paste0("Reuse ", reef, " CC model"))
            return(NULL)
        }
        mod <- inla(formula = form,
                    data = data,
                    Ntrials = data$total.points,
                    family = family, 
                    ## control.family=list(link='logit'),
                    control.predictor = list(link = 1, compute = TRUE),
                    control.compute = list(
                        dic = TRUE, cpo = TRUE, waic = TRUE,
                        config = TRUE) 
                    )
        save(mod, file = paste0(DATA_PATH, "modelled/CC__", reef, '__model.RData'))
        draws <- inla.posterior.sample(n=1000, mod, seed=123) %>%
            suppressWarnings() %>%
            suppressMessages()
        save(draws, file = paste0(DATA_PATH, "modelled/CC__", reef, '__draws.RData'))
    }, logFile=LOG_FILE, Category='--CC models--',
    msg=paste0('Fit ', reef, ' CC model'), return=NULL)
}

CI_models_CC_fit_models <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'fit_models',
                   label = "Fit CC models", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_cc.RData"))
        load(file = paste0(DATA_PATH, 'modelled/CC__mods.RData'))

        form <- HC ~ fYEAR +
            f(Site , model='iid') +
            f(Transect , model='iid')
        
        ## Fit the models - output models and draws to
        ## DATA_PATH/modelled/CC__.*__.RData
        purrr::pwalk(.l = list(mods$Full_data,mods$n),
                     .f = ~ CI__fit_CC_model(form = form, data = ..1,
                                             family = 'binomial', n = ..2, N = nrow(mods))
                   )

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'fit_models',status = 'success')

    }, logFile=LOG_FILE, Category='--CC models--',
    msg=paste0('Fit MA models'), return=NULL)
}
