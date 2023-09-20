CI_clear_models_collation_data <- function() {
    ## unlink("../data/modelled/*.*", recursive = TRUE)
}

CI_models_collate_indices <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'collate_indices',
                   label = "Collation of indices", status = 'pending')
    CI_tryCatch({

        ## site.location <- get(load(file = paste0(DATA_PATH, 'processed/site.location.RData')))
        spatial_lookup <- get(load(file = paste0(DATA_PATH, "processed/spatial_lookup.RData")))
        files <- list.files(path = paste0(DATA_PATH, "modelled"),
                              pattern = "..__scores_.*_year.RData",
                              full.names = TRUE)
        indices <- purrr::map_df(.x = files,
                              .f = ~ {
                                  level <- str_replace(.x,
                                                       ".*scores_(.*)_year.RData",
                                                       "\\1")
                                  name <- case_when(
                                                    level == "reef" ~ "REEF",
                                                    level == "BIOREGION.agg" ~ "BIOREGION.agg",
                                                    level == "NRM" ~ "NRM",
                                                    level == "TUMRA" ~ "TUMRA",
                                                    level == "GBRMPA.MA" ~ "GBRMPA.MA",
                                                    level == "ZONE" ~ "ZONE",
                                                    level == "GBRMP" ~ "GBRMP")
                                  indicator <- str_replace(.x,
                                                           ".*modelled/([A-Z]{2})__.*",
                                                           "\\1") 
                                  indicator <-  case_when(
                                      indicator == "CC" ~ "Coral.cover",
                                      indicator == "MA" ~ "Macroalgae",
                                      indicator == "JU" ~ "Juvenile.density",
                                      indicator == "CO" ~ "Community.composition",
                                      indicator == "RPI" ~ "Recovery.performance"
                                  )
                                  x <- get(load(.x)) %>%
                                      dplyr::select(Summary) %>%
                                      unnest(Summary) %>% 
                                      suppressMessages() %>%
                                      suppressWarnings()
                                  if (level == "reef") {
                                      x <- x %>%
                                          ## need to put DEPTH.f back into the output
                                          ## so that we can add the lat/longs in after that
                                          left_join(spatial_lookup %>%
                                                    dplyr::select(REEF.d, DEPTH.f, Shelf) %>%
                                                    distinct()) %>%
                                          left_join(
                                              ## site.location %>%
                                              spatial_lookup %>% 
                                              ## group_by(!!sym(name), DEPTH.f) %>%
                                              ## summarise(across(c(LATITUDE, LONGITUDE), mean))
                                              ## ensure each reef has same lat/long despite diff
                                              ## site depths - Manu wanted this
                                              group_by(!!sym(name)) %>%
                                              mutate(across(c(Latitude, Longitude), mean)) %>%
                                              group_by(!!sym(name), Shelf, DEPTH.f) %>%
                                              summarise(across(c(Latitude, Longitude), mean))
                                          ) %>%
                                          suppressMessages() %>%
                                          suppressWarnings()
                                  } else {
                                      x <- x %>% 
                                          mutate(DEPTH.f = NA,
                                                 Latitude = NA, Longitude = NA) %>% 
                                          suppressMessages() %>%
                                          suppressWarnings()
                                  }
                                  x <- x %>%  
                                      mutate(Level = {{level}},
                                             Name = !!sym(name),
                                             Indicator = {{indicator}},
                                             tn.reefs = ifelse(Level == "reef", NA, tn.reefs),
                                             n.below = ifelse(Level == "reef", NA, n.below),
                                             Reference = case_when(
                                                 Metric == 'pcb.rescale.dist.metric' ~ 'Critical',
                                                 Metric == 'rescale.dist.metric' ~ 'Baseline',
                                                 Metric == 'rescale.consequence.metric' ~ 'Critical',
                                                 Metric == 'distance.metric' ~ 'Baseline',
                                                 Metric == 'Total' ~ 'Baseline',
                                                 Metric == 'Acropora' ~ 'Critical',
                                                 Metric == 'Reference' ~ 'Baseline',
                                                 Metric == 'Critical' ~ 'Critical',
                                                 Metric == 'Reference' ~ 'reference',
                                                 Metric == 'Critical' ~ 'critical'
                                                 )) %>%
                                      dplyr::select(Level,
                                                    Year = fYEAR,
                                                    Name,
                                                    Shelf,
                                                    Latitude,
                                                    Longitude,
                                                    Depth = DEPTH.f,
                                                    Indicator,
                                                    Metric,
                                                    Reference,
                                                    Median = median,
                                                    Lower = lower,
                                                    Upper = upper,
                                                    tn.reefs = tn.reefs,
                                                    n.below = n.below) %>% 
                                      suppressMessages() %>%
                                      suppressWarnings()
                                  }
                              )
            
        
        save(indices, file = paste0(DATA_PATH, 'modelled/indices.RData'))
        write_csv(indices, file = paste0(TABS_PATH, "/Indices.csv"))
        
        ## save(template,
        ##       file = paste0(DATA_PATH, 'modelled/template.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'collate_indices',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Collation of indices'), return=NULL)
}

## tests

tests <- function(level = 'NRM', value = 'Burdekin') {
    cat(paste0('\n##-----', level, ' (', value, ') tn.reefs from indices in 2000 ---------\n'))
    indices %>%
        filter(Level == level,
               Year == 2000,
               Indicator == 'Coral.cover',
               Reference == 'Baseline') %>%
        as.data.frame %>%
        print()

    
    cat(paste0('\n##-----all unique ', level, ' (', value, ') reefs in site.locations ---------\n'))
    if (is.na(value)) {
        SS <- site.location %>%
            filter(is.na(!!sym(level))) %>%
            dplyr::select(REEF) %>%
            distinct() %>%
            pull(REEF) %>%
            unique
    } else {
        SS <- site.location %>%
            filter(!!sym(level) == value) %>%
            dplyr::select(REEF) %>%
            distinct() %>%
            pull(REEF) %>%
            unique
    }
    print(SS)
    
    cat(paste0('\n##-----all unique ', level, ' (', value, ') reefs in points.analysis.data.transect in 2000---------\n'))
    points.analysis.data.transect %>%
        filter(REPORT_YEAR == 2000, REEF %in% SS) %>%
        dplyr::select(REEF) %>%
        unique %>%
        print()
}

## tests(level = 'NRM', value = 'Burdekin')
## tests(level = 'NRM', value = 'Burnett Mary')
## tests(level = 'NRM', value = 'Cape York')
## tests(level = 'NRM', value = 'Fitzroy')
## tests(level = 'NRM', value = 'Mackay Whitsunday')
## tests(level = 'NRM', value = 'Wet Tropics')
## tests(level = 'GBRMP', value = 'GBRMP')
