CI__make_predictions <- function(newdata, mod, mesh) {
    draws <- inla.posterior.sample(1000, result=mod, seed=123) %>%
        suppressWarnings()

    points.grid <- newdata %>% 
        dplyr::select(LONGITUDE, LATITUDE)
    ## sets up an interpolator to actual location points from our new data
    proj.grid <- inla.mesh.projector(mesh, loc = as.matrix(points.grid))

    ## Rearrange draws so that it is a matrix of cellmeans, rather than a list
    cellmeans <- sapply(draws, function(x) x[['latent']])

    ## Index the cell means for fixed effects
    i.mod <- sapply(c('APredictor','^Predictor','spatial.field','Site','Intercept',
                      'fYEAR'),
                    function(x) grep(x, draws[[1]]$latent %>% rownames, perl=TRUE))

    ## get the partial predictions for the spatial field (all nodes on the mesh)
    cellmeans.spatial <- cellmeans[i.mod[["spatial.field"]],] 

    ##Get the predictions for fixed effects (covariate).  These
    ## are the intercept and the partial effects of the covariates
    ## Generate model matrix
    Xmat <- model.matrix(~1, data = newdata)

    wch <- grep('Intercept', names(i.mod))
    ii = unlist(i.mod[wch]) 

    ## multiply the predictions by the fixed effects for the covariates
    cellmeans.full.1 <- (cellmeans[ii,]) %*% t(Xmat)

    ## inla.mesh.project uses proj.grid to convert from mesh nodes
    ## to points beyond nodes the conversion is applied to the
    ## parameters from cellmeans.spatial transpose from rows to
    ## columns
    cellmeans.spatial <- t(inla.mesh.project(proj.grid, field = cellmeans.spatial))

    ## add the fixed and spatial effects together
    cellmeans.full.2 <- cellmeans.full.1 + cellmeans.spatial
    
    ## Backtransform
    if (any(grepl("poisson|nbinomial", mod$.args$family))) {
        cellmeans.spatial.2 <- cellmeans.full.2 %>%
            as.matrix() %>%
            exp()
    } else {
        cellmeans.spatial.2 <- cellmeans.full.2 %>%
            as.matrix() %>%
            plogis()
    }

    cellmeans.spatial.2
}


CI__cellmeans_model <- function(Indicator, obs_data, Full_data, newdata, n, N) {
    CI_tryCatch({
        reef <- unique(newdata$REEF.d)
        if (any(str_detect(colnames(obs_data), "Taxa"))) { # for juveniles
        }
        CI__append_label(stage = CI__get_stage(), item = 'cellmeans',
                         n, N)
        if (file.exists(paste0(DATA_PATH, "modelled/", Indicator, "__", reef, '__posteriors.RData'))) {
            CI_log(status = 'INFO', logFile = LOG_FILE, Category = paste0("--", Indicator, " models--"),
                   msg = paste0("Reuse ", reef, " ", Indicator, " cellmeans"))
            return(NULL)
        }
        draws <- get(load(file = paste0(DATA_PATH, "modelled/", Indicator, "__", reef, '__draws.RData')))
        cellmeans <- sapply(draws, function(x)
            x[[2]][(nrow(Full_data)-nrow(newdata)+1):nrow(Full_data)]) 
        posteriors <- newdata %>%
            dplyr::select(fYEAR, REEF.d) %>%
            cbind(plogis(cellmeans)) %>%
            pivot_longer(cols = matches('[0-9]'), names_to = 'Rep') %>%
            mutate(REEF.d = reef,
                   .draw = as.integer(Rep)) %>%
            dplyr::select(-Rep) %>%
            left_join(obs_data %>%
                      dplyr::select(fYEAR, REEF.d) %>%
                      distinct()) %>% 
            suppressWarnings() %>%
            suppressMessages()
        save(posteriors, file = paste0(DATA_PATH, "modelled/", Indicator, "__", reef, '__posteriors.RData'))
    }, logFile=LOG_FILE, Category=paste0('--', Indicator, ' models--'),
    msg=paste0('Posteriors for ', reef, ' ', Indicator, ' model'), return=NULL)
}

CI_models_cellmeans <- function(Indicator = 'CC') {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'cellmeans',
                   label = paste0("Cell means of ", Indicator, " models"), status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "modelled/data_ma.RData"))
        load(file = paste0(DATA_PATH, 'modelled/MA__mods.RData'))
        ## Calculate cellmeans
        cellmeans <- purrr::pwalk(.l = list(mods$data, mods$Full_data, mods$newdata, mods$n, nrow(mods)),
                                 .f = ~ CI__cellmeans_model(Indicator, ..1, ..2, ..3, ..4, ..5)) 
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'cellmeans',status = 'success')

    }, logFile=LOG_FILE, Category=paste0('--', Indicator, ' models--'),
    msg=paste0('Cell means of ', Indicator, ' models'), return=NULL)
}

CI_models_aggregation <- function(Indicator = 'CC', level = 'NRM') {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = paste0('agg_', level),
                   label = paste0("Aggregate to ", level), status = 'pending')
    CI_tryCatch({

        mods <- get(load(file = paste0(DATA_PATH, 'modelled/', Indicator, '__scores_reef_year.RData')))
        spatial_lookup <- get(load(file = paste0(DATA_PATH, 'processed/spatial_lookup.RData')))

        ## Number of reefs below 0.5
        mods.n <- mods %>%
            dplyr::select(REEF.d, Below) %>%
            unnest(Below) %>%
            left_join(spatial_lookup %>%
                      dplyr::select(REEF.d, !!level, Shelf) %>%
                      distinct()
                      ) %>%
            filter(!is.na(!!sym(level))) %>%           ## exclude all reefs outside boundary 
            nest(data = everything()) %>%
            mutate(WithShelf = map(.x = data,
                                   .f = ~ .x %>% 
                                       group_by(fYEAR, Metric, !!sym(level),
                                                Shelf, across(any_of('Taxa'))) %>%
                                       summarise(n.below = sum(Below, na.rm = TRUE),
                                                 n.Pbelow = sum(PBelow, na.rm = TRUE),
                                                 tn.reefs = n()) 
                                   ),
                   NoShelf = map(.x = data,
                                 .f = ~ .x %>% 
                                     mutate(Shelf = "All") %>% 
                                     group_by(fYEAR, Metric, !!sym(level),
                                              Shelf, across(any_of('Taxa'))) %>%
                                     summarise(n.below = sum(Below, na.rm = TRUE),
                                               n.Pbelow = sum(PBelow, na.rm = TRUE),
                                               tn.reefs = n()) 
                                 ),
                   Combined = map2(.x = NoShelf, .y = WithShelf,
                                   .f = ~ .x %>% rbind(.y))
                   ) %>%
            dplyr::select(Combined) %>% 
            unnest(Combined) %>%
            group_by(!!sym(level)) %>%
            nest() %>%
            dplyr::rename(Below = data) %>% 
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        ## Scores 
        mods <- b <- mods %>%
            dplyr::select(Scores) %>% 
            unnest(Scores) %>% 
            dplyr::select(fYEAR, REEF.d, .draw, any_of('Taxa'), Metric, .value) %>%
            left_join(spatial_lookup %>%
                      dplyr::select(REEF.d, !!level, Shelf) %>%
                      distinct()
                      ) %>%
            filter(!is.na(!!sym(level))) %>%           ## exclude all reefs outside boundary 
            group_by(!!sym(level)) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>% 
            ## if the supplied posterior only contains a single draw,
            ## duplicate this 1000 times
            mutate(data = map(.x = data,
                             .f = ~ { if (max(.x$.draw == 1)) {
                                         .x %>%
                                             group_by(fYEAR, REEF.d, !!sym(level),
                                                      across(any_of('Taxa')), Shelf, Metric) %>%
                                             sample_n(size = 1000, replace = TRUE) %>%
                                             mutate(.draw = 1:n())

                                      } else .x
                                      })) %>%
            mutate(
                ## Aggregate scores marginalising over shelf
                ## Scores = map(.x = data,
                ##              .f = ~ .x %>%
                ##                  ungroup() %>% 
                ##                  group_by(fYEAR, !!sym(level),.draw,
                ##                           across(any_of('Taxa')), Metric) %>%
                ##                  ## be sure to keep each function vectorized in below
                ##                  ## otherwise it will be very slow
                ##                  summarise(.mean1 = mean(.value, na.rm = TRUE),
                ##                            .sd1 = sd(.value, na.rm = TRUE),
                ##                            ## .value = .mean1 + (rnorm(1, 0, 1) * .sd1)) %>%
                ##                            shape1 = .mean1 * (((.mean1 * (1 - .mean1)) / .sd1^2) - 1),
                ##                            shape2 = (1 - .mean1) * (((.mean1 * (1 - .mean1)) / .sd1^2) - 1),
                ##                            .value = rbeta(1,shape1, shape2)) %>%
                ##                  mutate(Shelf = 'All')
                ##              ),
                Scores = map(.x = data,
                                .f = ~ .x %>%
                                    ungroup() %>% 
                                    group_by(fYEAR, !!sym(level), .draw,
                                             across(any_of('Taxa')), Metric) %>%
                                    summarise(.value = mean(sample(.value, replace = TRUE),
                                                            na.rm = TRUE)) %>%
                                    mutate(Shelf = 'All')
                                ),
                ## Scores = map(.x = data,
                ##                 .f = ~ .x %>%
                ##                     ungroup() %>% 
                ##                     group_by(fYEAR, !!sym(level), .draw,
                ##                              across(any_of('Taxa')), Metric) %>%
                ##                     summarise(.value = mean(.value, na.rm = TRUE)) %>%
                ##                     mutate(Shelf = 'All')
                ##                 ),
                ## Aggregate scores to shelf level
                ## ScoresShelf = map(.x = data,
                ##              .f = ~ .x %>%
                ##                  ungroup() %>% 
                ##                  group_by(fYEAR, !!sym(level), .draw,
                ##                           across(any_of('Taxa')), Shelf, Metric) %>%
                ##                  summarise(.mean1 = mean(.value, na.rm = TRUE),
                ##                            .sd1 = sd(.value, na.rm = TRUE),
                ##                            .value = .mean1 + (rnorm(1, 0, 1) * .sd1)) 
                ##              ),
                ScoresShelf = map(.x = data,
                             .f = ~ .x %>%
                                 ungroup() %>% 
                                 group_by(fYEAR, !!sym(level), .draw,
                                          across(any_of('Taxa')), Shelf, Metric) %>%
                                 summarise(.value = mean(sample(.value, replace = TRUE),
                                                         na.rm = TRUE)) 
                             ),
                ## ScoresShelf = map(.x = data,
                ##              .f = ~ .x %>%
                ##                  ungroup() %>% 
                ##                  group_by(fYEAR, !!sym(level), .draw,
                ##                           across(any_of('Taxa')), Shelf, Metric) %>%
                ##                  summarise(.value = mean(.value, na.rm = TRUE)) 
                ##              ),
                ## Combine together
                Scores = map2(.x = Scores, .y = ScoresShelf,
                              .f = ~ .x %>% rbind(.y)),
                ## Summarise
                Summary = map(.x = Scores,
                              .f = ~ .x %>%
                                  group_by(fYEAR, !!sym(level),
                                           across(any_of('Taxa')), Shelf, Metric) %>%
                                  summarise_draws(median = ~ median(.x, na.rm = TRUE),
                                                  mean = ~ mean(.x, na.rm = TRUE),
                                                  sd = ~ sd(.x, na.rm = TRUE),
                                                  HDInterval::hdi,
                                                  `p<0.5` = ~ mean(.x < 0.5, na.rm = TRUE))
                              )
            ) %>% 
            suppressMessages() %>%
            suppressWarnings()

        ## Add the Below values to the Summary
        mods <- mods %>%
            left_join(mods.n) %>% 
            mutate(Summary = map2(.x = Summary, .y = Below,
                                  .f = ~ .x %>% left_join(.y))) %>% 
            suppressMessages() %>%
            suppressWarnings()
        
        save(mods,
              file = paste0(DATA_PATH, 'modelled/', Indicator, '__scores_', level,'_year.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = paste0('agg_', level), status = 'success')

    }, logFile=LOG_FILE, Category=paste0('--', Indicator, ' models--'),
    msg=paste0('Aggregate to ', level), return=NULL)
}


## set.seed(123)
## A <- data.frame(rbind(a1 = rnorm(100, 30, 1) ,a2 = rnorm(100,35,1), a3 = rnorm(10, 40, 1))) %>%
##     cbind(Group = c("a1", "a2", "a3")) %>% pivot_longer(cols = -Group)
## A %>% group_by(name) %>% summarise(.value = mean(value)) %>% ungroup() %>% median_hdci(.value)
## A %>% median_hdci(value)
## A %>% group_by(name) %>% summarise(.value = rnorm(1, mean(value), sd(value))) %>% median_hdci(.value)
## A %>% group_by(name) %>% summarise(.value = rnorm(1, mean(value), sd(value))) %>% pull(.value) %>% sd()

## set.seed(123)
## A <- data.frame(rbind(a1 = rnorm(100, 30, 0) ,a2 = rnorm(100,35,0), a3 = rnorm(10, 40, 0))) %>%
##     cbind(Group = c("a1", "a2", "a3")) %>% pivot_longer(cols = -Group)
## A %>% group_by(name) %>% summarise(.value = mean(value)) %>% ungroup() %>% median_hdci(.value)
## A %>% median_hdci(value)
## A$value %>% sd()
## A %>% group_by(name) %>% summarise(.value = rnorm(1, mean(value), sd(value))) %>% median_hdci(.value)
## A %>% group_by(name) %>% summarise(.value = rnorm(1, mean(value), sd(value))) %>% pull(.value) %>% sd()

## set.seed(123)
## A <- data.frame(rbind(a1 = rnorm(100, 30, 1) ,a2 = rnorm(100,30,1), a3 = rnorm(10, 30, 1))) %>%
##     cbind(Group = c("a1", "a2", "a3")) %>% pivot_longer(cols = -Group)
## A %>% group_by(name) %>% summarise(.value = mean(value)) %>% ungroup() %>% median_hdci(.value)
## A %>% median_hdci(value)
## A$value %>% sd()
## A %>% group_by(name) %>% summarise(.value = rnorm(1, mean(value), sd(value))) %>% median_hdci(.value)
## A %>% group_by(name) %>% summarise(.value = rnorm(1, mean(value), sd(value))) %>% pull(.value) %>% sd()



## set.seed(123)
## A <- data.frame(rbind(a1 = rnorm(100000, 30, 1) ,a2 = rnorm(100000,35,1), a3 = rnorm(10, 40, 1))) %>%
##     cbind(Group = c("a1", "a2", "a3")) %>% pivot_longer(cols = -Group)
## A %>% group_by(name) %>% summarise(.value = mean(value)) %>% ungroup() %>% median_hdci(.value)
## A %>% median_hdci(value)
## A$value %>% sd()

## set.seed(123)
## A <- data.frame(rbind(a1 = rnorm(100000, 30, 0) ,a2 = rnorm(100000,35,0), a3 = rnorm(10, 40, 0))) %>%
##     cbind(Group = c("a1", "a2", "a3")) %>% pivot_longer(cols = -Group)
## A %>% group_by(name) %>% summarise(.value = mean(value)) %>% ungroup() %>% median_hdci(.value)
## A %>% median_hdci(value)
## A$value %>% sd()

## set.seed(123)
## A <- data.frame(rbind(a1 = rnorm(100000, 30, 1) ,a2 = rnorm(100000,30,1), a3 = rnorm(10, 30, 1))) %>%
##     cbind(Group = c("a1", "a2", "a3")) %>% pivot_longer(cols = -Group)
## A %>% group_by(name) %>% summarise(.value = mean(value)) %>% ungroup() %>% median_hdci(.value)
## A %>% median_hdci(value)
## A$value %>% sd()

## median_hdci(rnorm(10000, 35, 1))

my_rescale <- function(x, to = c(0,1), from = range(x, na.rm = TRUE, finite = TRUE)) {
    (x - from[[1]])/(from[[2]] - from[[1]]) * diff(to) + to[1]
}
