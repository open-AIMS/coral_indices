CI_28_JU_IPM_models <- function() {
    
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'ju_ipm',
                   label = "JU IPM model", status = 'pending')

    CI_tryCatch({
        source('../R/functions.R')
        source('../R/CI_28_JU_IPM_models_functions.R')

        scn <- expand.grid(S = c("Inshore", "Offshore"), 
                 R = NA, Exp = NA,
                 Pocilloporid.dens = 20,
                 Merulinidae.dens = 20,
                 Poritidae.dens = 20,
                 expCov = 30,#Percentage cover expected in 10 years
                 reset.juv = c(FALSE)) 
        scn <- scn %>%
            group_by(S) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            ## read in the indivudual IPM model predictions
            mutate(covPoc = map(.x = data,
                                .f = ~ getIPM(.x, "Pocilloporid")),
                   covMer = map(.x = data,
                                .f = ~ getIPM(.x, "Merulinidae")),
                   covPor = map(.x = data,
                                .f = ~ getIPM(.x, "Poritidae"))) %>%
            ## Combine non-Acropora coral predictions tother
            mutate(OthCov = pmap(.l = list(covPoc, covPor, covMer),
                                 .f = ~ ..1 %>%
                                     bind_rows(..2, ..3) %>%
                                     group_by(sim, draw) %>%
                                     summarise(cover=sum(cover),
                                               .groups = "drop") %>% 
                                     ungroup %>%
                                     summarise(
                                         mean = mean(cover, na.rm = TRUE),
                                         ## sd = sd(cover, na.rm = TRUE),
                                         ## sdl = mean_sdl(cover)$ymin,
                                         ## sdu = mean_sdl(cover)$ymax,
                                         ## lower=mean_cl_boot(cover)$ymin,
                                         ## upper=mean_cl_boot(cover)$ymax,
                                         nsims = length(unique(sim)),
                                         ndraws = length(unique(draw)),
                                         .groups="drop") %>%
                                     summarise(sim = rep(1:nsims, each = ndraws),
                                               draw = rep(1:ndraws, times = nsims),
                                               cover = mean
                                               )
                                     )) %>%
            ## read the Acropora IPM and join in the Other coral cover
            mutate(AcrJuv = map(.x = data,
                                .f = ~ getIPM(.x, "Acropora")),
                   AcrJuv = map2(.x = AcrJuv, .y = OthCov,
                                .f = ~ .x %>% left_join(.y %>% dplyr::rename(OthCov = cover)))
                   ) %>%
            ## Model cover against juv abundance
            mutate(Mod = map(.x = AcrJuv,
                             .f = ~ .x %>%
                                 lm(formula = cover ~ juv_ab, data = .))) %>%
            ## Estimate juv abundance from cover
            mutate(JuvAbund = map2(.x = Mod, .y = AcrJuv,
                                   .f = ~ estimate_juv(.x, expCov, unique(.y$OthCov)))) %>%
            dplyr::select(Shelf = S, mean = JuvAbund) %>%
            unnest(mean)
        
            ## mutate(Mods = map(.x = AcrJuv,
            ##                  .f = ~ .x %>%
            ##                      group_by(sim,draw) %>%
            ##                      nest() %>%
            ##                      mutate(Mod = map(.x = data,
            ##                                       .f = ~ .x %>% 
            ##                                           lm(formula = cover ~ juv_ab, data = .)
            ##                                       )),
            ##                  ),
            ##        Mods = map(.x = Mods,
            ##                  .f = ~ .x %>%
            ##                      mutate(Pred = map2(.x = Mod, .y = data,
            ##                                        .f = ~ estimate_juv(.x, expCov, unique(.y$OthCov))
            ##                             ))
            ##                  ),
            ##        MinJuv = map(.x = Mods,
            ##                     .f = ~ .x %>%
            ##                         dplyr::select(sim, draw, Pred) %>%
            ##                         unnest(Pred)
            ##                     )
            ##        )

        IPM_juv <- scn 
        save(IPM_juv, file = paste0(DATA_PATH, "parameters/IPM_juv.RData"))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'ju_ipm',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('JU IPM model.'), return=NULL)
}

