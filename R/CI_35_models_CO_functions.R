CI_clear_models_CO_data <- function() {
    files <- list.files(path = paste0(DATA_PATH, "modelled"),
                        pattern = "CO.*|data_co.*",
                        full.names = TRUE)
    unlink(files)
}


CI_models_CO_prepare_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'prepare_data',
                   label = "Prepare data", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, 'processed/comp.transect.RData'))

        comp.transect <- comp.transect %>%
            mutate(P_CODE = ifelse(P_CODE == "RMRAP", "RM", P_CODE)) %>%
            mutate(DEPTH.f=factor(case_when(DEPTH>3~"deep slope",
                                            DEPTH<=3~"shallow slope")),
                   Site=factor(paste(REEF,DEPTH.f,SITE_NO)),
                   REEF.d=factor(paste(REEF, DEPTH.f)),
                   Transect=factor(paste(REEF,DEPTH.f,SITE_NO,TRANSECT_NO)),
                   fYEAR=factor(as.numeric(as.character(REPORT_YEAR)))) %>%
            suppressMessages() %>%
            suppressWarnings()
            
        ## Reef-level aggregation of cover data            
        coral.comms <- comp.transect %>%
            group_by(P_CODE, REEF.d, COMP_2021, VISIT_NO, REPORT_YEAR) %>%
            summarise(cover = mean(cover)) %>%
            ungroup() %>%
            group_by(REEF.d) %>%
            mutate(StartYR = min(REPORT_YEAR),
                   EndYR = max(REPORT_YEAR),
                   SurveyDuration = EndYR - StartYR) %>% 
            filter(EndYR >= 2016,
                   SurveyDuration >= 5) %>%
            droplevels() %>%
            pivot_wider(id_cols = everything(),
                        names_from = COMP_2021,
                        values_from = cover) %>%
            ## pivot longer according to all the taxa
            pivot_longer(-any_of(c("P_CODE", "REEF.d", "VISIT_NO", "REPORT_YEAR",
                                   "StartYR", "EndYR", "SurveyDuration")),
                         names_to = 'Taxa',
                         values_to = 'values') %>%
            ## replace missing values with 0's
            mutate(values = replace_na(values, 0)) %>%
            ## sqrt transform cover data
            mutate(values = sqrt(values)) %>% 
            mutate(REEF.d = factor(REEF.d)) %>%
            group_by(P_CODE, REEF.d, VISIT_NO, REPORT_YEAR) %>%
            ## relative abundance
            mutate(values = values/sum(values)) %>%
            ## pivot wider
            pivot_wider(id_cols = everything(),
                        names_from = Taxa,
                        values_from = values) %>%
            suppressMessages() %>%
            suppressWarnings()

        ## coral cover summary
        coral.cover <- comp.transect %>%
            group_by(REEF.d, GROUP_CODE, COMP_2021,
                     VISIT_NO, REPORT_YEAR) %>%
            summarise(cover = mean(cover)) %>%
            filter(GROUP_CODE == "HC") %>%     ## limit to just HC
            ungroup() %>%
            group_by(REEF.d, REPORT_YEAR) %>%
            summarise(cover = sum(cover)) %>%
            ungroup() %>%
            mutate(adj.cover = cover/100) %>%  ## adjusted cover for plotting later
            suppressMessages() %>%
            suppressWarnings()

        coral.comms <- coral.comms %>%
            left_join(coral.cover) %>%
            dplyr::relocate(cover, adj.cover, .after = SurveyDuration) %>% 
            suppressMessages() %>%
            suppressWarnings()
        
        save(coral.comms,
              file = paste0(DATA_PATH, 'modelled/coral.comms.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'prepare_data',status = 'success')
                                                                                            
    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare data'), return=NULL)
}

CI_models_CO_prepare_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'prepare_data',
                   label = "Prepare data", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, 'modelled/coral.comms.RData'))

        coral.comms <- coral.comms %>%
            group_by(REEF.d) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            mutate(data = map(.x = data,
                              .f = ~ .x %>% arrange(REPORT_YEAR) %>%
                                  tibble::column_to_rownames(var = "REPORT_YEAR"))) %>%
            mutate(NMDS = map(.x = data,
                              .f = ~ {
                                  dat <- .x %>%
                                      dplyr::select(-any_of(c("P_CODE", "REEF.d", "VISIT_NO",
                                                              "StartYR", "EndYR", "SurveyDuration",
                                                              "cover", "adj.cover")))
                                  .x %>% vegan::metaMDS(dat)


reef.nmds <- vector('list', length=length(coral.comms))

for (i in 1:length(reef.nmds)) {
  reef.nmds[[i]] <- vegan::metaMDS(coral.comms[[i]][,c(10:ncol(coral.comms[[i]]))])
  reef.nmds[[i]] <- as.data.frame(vegan::scores(reef.nmds[[i]], 'sites'))
  reef.nmds[[i]]$Year <- coral.comms[[i]]$REPORT_YEAR
  reef.nmds[[i]]$Reef <- unique(coral.comms[[i]]$REEF)
  reef.nmds[[i]]$nMDS_stress <- vegan::metaMDS(coral.comms[[i]][,c(10:ncol(coral.comms[[i]]))], k=2, weakties=F)[[22]] # add stress
}

reef.nmds <- data.table::rbindlist(reef.nmds)
        
        save(coral.comms,
              file = paste0(DATA_PATH, 'modelled/coral.comms.RData'))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'prepare_data',status = 'success')
                                                                                            
    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Prepare data'), return=NULL)
}
