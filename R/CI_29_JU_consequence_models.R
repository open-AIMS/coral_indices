CI_29_JU_consequence_models <- function() {
    
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'ju_consequence',
                   label = "JU consequence model", status = 'pending')

    CI_tryCatch({
        source('../R/functions.R')
        source('../R/CI_29_JU_consequence_models_functions.R')
        load(file = paste0(DATA_PATH, "parameters/IPM_juv.RData"))

        ## Critical value from IPM
        Crit.value <- IPM_juv$mean[1]  # just inshore at the moment

        juv.ma.site <- CI__get_JUV_MA_site_data()
        ## Model to predict the density of Juvenile Acropora ~ MApLag
        ## The desire is to produce a a single value (distribution
        ## perhaps) of MApLag, that can be used to set a value of MAp
        ## that corresponds to the critical density of Acropora
        ## juveniles estimated by Manu for the Juvenile critical
        ## indicator. I don't have this exact value but suspect it
        ## will be ~0.7 I would like to produce a plot along the lines
        ## of

        ## Model Formula.
        form <- formula(Acropora ~ log(MApLag + 0.01) +
                            f(reef.depth.site, model = 'iid'))
        
        a <- juv.ma.site %>%
            group_by(Habitat) %>%
            summarise(data = list(data.frame(reef.depth.site,
                                             MApLag, Acropora, avail.area)),
                      .groups = "drop") %>% 
            ## Create prediction data
            mutate(newdata = map(.x = data,
                                 .f = ~ .x %>%
                                     tidyr::expand(reef.depth.site = NA,
                                                   MApLag = seq(min(.$MApLag, na.rm = TRUE),
                                                                max(.$MApLag, na.rm = TRUE),
                                                                len = 100),
                                                   Acropora = NA, avail.area = 1) %>%
                                     distinct()
                                 )) %>%
            ## Modelling data
            mutate(fulldata = map2(.x = data, .y = newdata,
                                   .f = ~ .x %>% bind_rows(.y))) %>%
            ## indices
            mutate(Indices = map2(.x = data, .y = newdata,
                                  .f = ~ (1:nrow(.y)) + nrow(.x))) %>% 
            ## Fit model
            mutate(Mod = map(.x = fulldata,
                             .f = ~ {
                                 set.seed(123)
                                 
                                 inla(form,
                                      offset = log(.x$avail.area),
                                      data = .x, 
                                      family = 'nbinomial',
                                      verbose = F,
                                      control.family = list(link = 'log'),
                                      control.predictor = list(link = 1, compute = TRUE),
                                      control.compute = list(
                                          dic = TRUE, cpo = TRUE, waic = TRUE,
                                          config = TRUE)
                                      )
                             })) %>%
            ## Posterior Draws
            mutate(Draws = map(.x = Mod,
                               .f = ~ inla.posterior.sample(n = 1000, .x, seed = 123))) %>%
            ## Cellmeans
            mutate(Cellmeans = map2(.x = Draws, .y = Indices,
                                   .f = ~ sapply(.x, function(x) x[[2]][.y]))) %>%
            ## MA dist
            mutate(MA.dist = map2(.x = newdata, .y = Cellmeans,
                                 .f = ~ .x %>%
                                     dplyr::select(MApLag) %>%
                                     cbind(exp(.y)) %>%
                                     pivot_longer(cols = matches('[0-9]'), names_to = 'draw'))) %>%
            mutate(model.pred = map(.x = MA.dist,
                                    .f = ~ .x %>%
                                        group_by(MApLag) %>%
                                        median_hdci(value) %>%
                                        rename(lower=`.lower`, upper=`.upper`, median=value) )) %>%
            ## Plots 
            mutate(gg = map(.x = model.pred,
                            .f = ~ .x %>%
                                ggplot(aes(y = median, x = MApLag)) +
                                geom_line()+
                                geom_ribbon(aes(ymin = lower, ymax = upper),
                                            show.legend = NA, color = 'blue',alpha = 0.4)+
                                scale_x_continuous('MA proportion')+
                                scale_y_continuous('Acropora density')+
                                geom_hline(yintercept = Crit.value,
                                           linetype = 2, color = 'red',
                                           linewidth = 0.5) +
                                        #scale_y_log10('Acropora density')+
                                theme_classic(base_size = 9)))
        map2(.x = a$Habitat,
             .y = a$gg,
             .f = ~ ggsave(filename = paste0(OUTPUT_PATH, "figures/JU_consequence_", .x, ".png"),
                           .y,
                           width = 6, height = 6)
             )

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'ju_consequence',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('JU consequence model.'), return=NULL)
}
