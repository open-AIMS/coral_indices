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
                                               .groups = "drop")
                                 )) %>%
            mutate(AcrJuv = map(.x = data,
                                .f = ~ getIPM(.x, "Acropora")),
                   AcrJuv = map2(.x = AcrJuv, .y = OthCov,
                                .f = ~ .x %>% left_join(.y %>% dplyr::rename(OthCov = cover)))
                   ) %>%
            mutate(Mods = map(.x = AcrJuv,
                             .f = ~ .x %>%
                                 group_by(sim,draw) %>%
                                 nest() %>%
                                 mutate(Mod = map(.x = data,
                                                  .f = ~ .x %>% 
                                                      lm(formula = cover ~ juv_ab, data = .)
                                                  )),
                             ),
                   Mods = map(.x = Mods,
                             .f = ~ .x %>%
                                 mutate(Pred = map2(.x = Mod, .y = data,
                                                   .f = ~ estimate_juv(.x, expCov, unique(.y$OthCov))
                                        ))
                             ),
                   MinJuv = map(.x = Mods,
                                .f = ~ .x %>%
                                    dplyr::select(sim, draw, Pred) %>%
                                    unnest(Pred)
                                )
                   )

        ## This leads to wildly varying MinJuv values

        scn[1,'MinJuv'][[1]][[1]]
        ## # A tibble: 3,000 × 3
        ## # Groups:   sim, draw [3,000]
        ##      sim  draw      Pred
        ##    <dbl> <dbl>     <dbl>
        ##  1     1     1 -102.    
        ##  2     2     1    0.01  
        ##  3     3     1    0.01  
        ##  4     1     2    0.0314
        ##  5     2     2    0.0514
        ##  6     3     2    0.0599
        ##  7     1     3  216.    
        ##  8     2     3  162.    
        ##  9     3     3  199.    
        ## 10     1     4    0.314 

        scn[1,'Mods'][[1]][[1]]
        ## # A tibble: 3,000 × 5
        ## # Groups:   sim, draw [3,000]
        ##      sim  draw data             Mod    Pred     
        ##    <dbl> <dbl> <list>           <list> <list>   
        ##  1     1     1 <tibble [7 × 9]> <lm>   <dbl [1]>
        ##  2     2     1 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  3     3     1 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  4     1     2 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  5     2     2 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  6     3     2 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  7     1     3 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  8     2     3 <tibble [6 × 9]> <lm>   <dbl [1]>
        ##  9     3     3 <tibble [6 × 9]> <lm>   <dbl [1]>
        ## 10     1     4 <tibble [6 × 9]> <lm>   <dbl [1]>
        ## # … with 2,990 more rows
        
        scn[1,'Mods'][[1]][[1]][1,'data'][[1]][[1]]
        ## # A tibble: 7 × 9
        ##   juv_ab  time pop_size cover Species  Region Shelf   Exposure OthCov
        ##    <dbl> <int>    <dbl> <dbl> <chr>    <lgl>  <fct>   <lgl>     <dbl>
        ## 1      1    10     3.17 0.164 Acropora NA     Inshore NA         21.0
        ## 2     20    10    20    0     Acropora NA     Inshore NA         21.0
        ## 3     40    10    40    0     Acropora NA     Inshore NA         21.0
        ## 4     60    10    60    0     Acropora NA     Inshore NA         21.0
        ## 5     80    10    80    0     Acropora NA     Inshore NA         21.0
        ## 6    100    10   100    0     Acropora NA     Inshore NA         21.0
        ## 7    120    10   120    0     Acropora NA     Inshore NA         21.0
        scn[1,'Mods'][[1]][[1]][2,'data'][[1]][[1]]
        ## # A tibble: 6 × 9
        ##   juv_ab  time pop_size cover Species  Region Shelf   Exposure OthCov
        ##    <dbl> <int>    <dbl> <dbl> <chr>    <lgl>  <fct>   <lgl>     <dbl>
        ## 1     20    10       20     0 Acropora NA     Inshore NA         20.9
        ## 2     40    10       40     0 Acropora NA     Inshore NA         20.9
        ## 3     60    10       60     0 Acropora NA     Inshore NA         20.9
        ## 4     80    10       80     0 Acropora NA     Inshore NA         20.9
        ## 5    100    10      100     0 Acropora NA     Inshore NA         20.9
        ## 6    120    10      120     0 Acropora NA     Inshore NA         20.9
        scn[1,'Mods'][[1]][[1]][6,'data'][[1]][[1]]
        ## # A tibble: 6 × 9
        ##   juv_ab  time pop_size cover Species  Region Shelf   Exposure OthCov
        ##    <dbl> <int>    <dbl> <dbl> <chr>    <lgl>  <fct>   <lgl>     <dbl>
        ## 1     20    10     176.  20.6 Acropora NA     Inshore NA         25.1
        ## 2     40    10     354.  40.9 Acropora NA     Inshore NA         25.1
        ## 3     60    10     521.  57.9 Acropora NA     Inshore NA         25.1
        ## 4     80    10     702.  80.3 Acropora NA     Inshore NA         25.1
        ## 5    100    10     876.  98.9 Acropora NA     Inshore NA         25.1
        ## 6    120    10    1066. 125.  Acropora NA     Inshore NA         25.1
        scn[1,'Mods'][[1]][[1]][7,'data'][[1]][[1]]
        ## # A tibble: 6 × 9
        ##   juv_ab  time pop_size  cover Species  Region Shelf   Exposure OthCov
        ##    <dbl> <int>    <dbl>  <dbl> <chr>    <lgl>  <fct>   <lgl>     <dbl>
        ## 1     20    10     25.7 0.0571 Acropora NA     Inshore NA         5.63
        ## 2     40    10     43.4 0.0343 Acropora NA     Inshore NA         5.63
        ## 3     60    10     70.3 0.103  Acropora NA     Inshore NA         5.63
        ## 4     80    10     89.1 0.0914 Acropora NA     Inshore NA         5.63
        ## 5    100    10    115.  0.148  Acropora NA     Inshore NA         5.63
        ## 6    120    10    135.  0.148  Acropora NA     Inshore NA         5.63
        

                                     
        pred_y <- function(x)predict.lm(m, data.frame(juv_ab=x),type = "response")
        pred_x <- function(y) optim(1, \(x) (y-pred_y(x))^2, method='BFGS')[[1]]
        res<-data.frame(Region=R,Shelf=S,Exposure=Exp, 
                        mean=pred_x(expCov-OthCov$mean)/100, 
                        lower=pred_x(expCov-OthCov$lower)/100,
                        upper=pred_x(expCov-OthCov$upper)/100)


        
        scn <- expand.grid(S = c("Inshore", "Offshore"), 
                 R = NA, Exp = NA,
                 Poc.dens = 20,
                 Por.dens = 20,
                 Mer.dens = 20, #ind/100m2
                 expCov = 30,#Percentage cover expected in 10 years
                 reset.juv = c(FALSE)) 
        
        scn <- scn %>% mutate(scn = paste(R, S, Exp, reset.juv, sep = "_"))
        scn = split(scn, f = scn$scn)

        cond <- scn[[1]]
        S=cond$S
        R=cond$R
        Exp=cond$Exp
        Poc.dens=cond$Poc.dens
        Por.dens=cond$Por.dens
        Mer.dens=cond$Mer.dens
        expCov=cond$expCov
        reset.juv=cond$reset.juv

        ##Mound Growth Corals - Poritidae
        load(file.path("../data/parameters",
                       sprintf("JuvCover_Mound_growth_%s_%s_%s_ResJuv_%s_Morph_Poritidae.Rdata",
                               R,S,Exp, reset.juv)))

        covPor<-results%>%
            dplyr::filter(time==10, juv_ab==Por.dens)%>%
            mutate(Species="Poritidae", Exposure=Exp)
        
        covPor.t<-results%>%
            dplyr::filter(juv_ab==Por.dens)%>%
            mutate(Species="Poritidae", Exposure=Exp) 
        
        ##Mound Growth Corals - Merulinidae
        load(file.path("../data/parameters",
                       sprintf("JuvCover_Mound_growth_%s_%s_%s_ResJuv_%s_Morph_Merulinidae.Rdata",
                              R,S,Exp, reset.juv)))
        
        covMer<-results%>%
            dplyr::filter(time==10, juv_ab==Mer.dens)%>%
            mutate(Species="Merulinidae", Exposure=Exp)
        
        covMer.t<-results%>%
            dplyr::filter(juv_ab==Por.dens)%>%
            mutate(Species="Merulinidae", Exposure=Exp)
        
        ##Pocilloporid Corals
        load(file.path("../data/parameters",
                       sprintf("JuvCover_Pocilloporidae_%s_%s_%s_ResJuv_%s_Morph_NA.Rdata",
                               R,S,Exp, reset.juv)))
        
        covPoc<-results%>%
            dplyr::filter(time==10, juv_ab==Poc.dens)%>%
            mutate(Species="Merulinidae", Exposure=Exp)
        covPoc.t<-results%>%
            dplyr::filter(juv_ab==Poc.dens)%>%
            mutate(Species="Pocilloporidae", Exposure=Exp) 
        
        OthCov<-covPoc%>%
            bind_rows(covPor,covMer)%>%
            group_by(sim, draw)%>%
            summarise(cover=sum(cover))%>%
            ungroup%>%
            summarise(
                mean=mean_cl_boot(cover)$y,
                lower=mean_cl_boot(cover)$ymin,
                upper=mean_cl_boot(cover)$ymax,
                .groups="drop")

        ##Acropora Corals
        load(file.path("../data/parameters",
                       sprintf("JuvCover_Acropora_%s_%s_%s_ResJuv_%s_Morph_NA.Rdata",
                               R,S,Exp, reset.juv)))
        AcrJuv<-results%>%
            dplyr::filter(time==10)%>%
            mutate(Species="Acropora", Exposure=Exp)
        
        
        ##Estimate minimum juvenile abundace of Acropora
        m<-lm(cover~juv_ab, data=AcrJuv) #Use LM to interpolate values

        pred_y <- function(x)predict.lm(m, data.frame(juv_ab=x),type = "response")
        pred_x <- function(y) optim(1, \(x) (y-pred_y(x))^2, method='BFGS')[[1]]
        res<-data.frame(Region=R,Shelf=S,Exposure=Exp, 
                        mean=pred_x(expCov-OthCov$mean)/100, 
                        lower=pred_x(expCov-OthCov$lower)/100,
                        upper=pred_x(expCov-OthCov$upper)/100)
        
        
        res<-res%>%mutate(reset.juv=reset.juv)
        
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'ju_ipm',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('JU IPM model.'), return=NULL)
}

