CI_clear_processed_data <- function() {
    ## Clear the /data/processed folder
    unlink("../data/processed/*.*", recursive = TRUE)
}

CI_process_add_report_year <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'add_year',
                   label = "Add report year", status = 'pending')
    CI_tryCatch({
        load(paste0(PRIMARY_DATA_PATH, 'sample.reef.RData'))

        sample.reef.report.year<-sample.reef %>%
            mutate(REPORT_YEAR = ifelse(P_CODE == "RM", VISIT_NO+1992,
                            ifelse(P_CODE == 'IN', VISIT_NO+2004, year(Date)))) %>%
            mutate(REPORT_YEAR = ifelse(P_CODE == 'AP' & VISIT_NO %in% c('2','4'), NA, REPORT_YEAR)) %>%
            filter(REPORT_YEAR < (CI$setting$FINAL_YEAR + 1))

        save(sample.reef.report.year,
             file = paste0(DATA_PATH, 'processed/sample.reef.report.year.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'add_year',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Add report year.'), return=NULL)
}

# define juvenile transect area for each project
CI_process_transect_area <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'transect_area',
                   label = "Add transect area", status = 'pending')
    CI_tryCatch({

        juv.tran.area <- data.frame(P_CODE = c("RM","IN","GH","RR","AP"),
                                    tran.area = c(8.5,34,34,34,34)) 

        save(juv.tran.area,
             file = paste0(DATA_PATH, 'processed/juv.tran.area.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'transect_area',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Add transect area.'), return=NULL)
}

CI_process_locations_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'locations_data',
                   label = "Process locations data", status = 'pending')
    CI_tryCatch({

        load(paste0(DATA_PATH, "processed/sample.reef.report.year.RData"))

        locations <- sample.reef.report.year %>%
            dplyr::select(P_CODE, REEF, SITE_NO, DEPTH, LATITUDE, LONGITUDE) %>%
            mutate(name = paste(REEF, SITE_NO, sep=' ')) %>%
            group_by(REEF, SITE_NO, name) %>%
            summarise(LATITUDE = mean(LATITUDE),
                      LONGITUDE = mean(LONGITUDE))%>%
            mutate(SITE_NO = as.character(SITE_NO)) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        save(locations,
             file = paste0(DATA_PATH, 'processed/locations.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'locations_data',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process locations data.'), return=NULL)
}

CI_process_total_points <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'total_points',
                   label = "Process total points data", status = 'pending')
    CI_tryCatch({

        load(paste0(DATA_PATH, "primary/points.raw.RData"))
        load(paste0(DATA_PATH, "processed/sample.reef.report.year.RData"))

        ## Total points transect level
        total.points.aims <- points.raw %>%
            left_join(sample.reef.report.year) %>%
            filter(REPORT_YEAR < (CI$setting$FINAL_YEAR + 1)) %>%
            group_by(P_CODE, REEF, DEPTH, VISIT_NO,
                     REPORT_YEAR, SITE_NO, TRANSECT_NO) %>%
            mutate(SITE_NO = as.character(SITE_NO)) %>% # needed as jcu sites are named
            summarise(total.points = sum(POINTS)) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        save(total.points.aims,
             file = paste0(DATA_PATH, 'processed/total.points.aims.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'total_points',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process total points data.'), return=NULL)
}


CI_process_hc_points <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'hcc_points',
                   label = "Process HCC points data", status = 'pending')
    CI_tryCatch({

        load(paste0(DATA_PATH, "primary/video_codes.RData"))
        load(paste0(DATA_PATH, "primary/points.raw.RData"))
        load(file = paste0(DATA_PATH, 'processed/total.points.aims.RData'))

        ## hard coral points                            
        hc.aims <- points.raw %>%
            left_join(video_codes %>%
                      dplyr::select(GROUP_CODE, VIDEO_CODE)) %>%
            filter(GROUP_CODE == 'HC')%>%
            mutate(SITE_NO = as.character(SITE_NO)) %>%
            group_by(P_CODE, REEF, DEPTH, VISIT_NO,
                     SITE_NO, TRANSECT_NO) %>%
            summarise(HC = sum(POINTS)) %>%
            ungroup() %>% 
            right_join(total.points.aims %>%
                       dplyr::select(-total.points)) %>%
            mutate(HC = ifelse(is.na(HC), 0, HC)) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(hc.aims,
             file = paste0(DATA_PATH, 'processed/hc.aims.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'hcc_points',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process HCC points data.'), return=NULL)
}


CI_process_ma_points <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'ma_points',
                   label = "Process MA points data", status = 'pending')
    CI_tryCatch({

        load(paste0(DATA_PATH, "primary/video_codes.RData"))
        load(paste0(DATA_PATH, "primary/points.raw.RData"))
        load(file = paste0(DATA_PATH, 'processed/total.points.aims.RData'))

        ma.aims <- points.raw %>%
            left_join(video_codes %>%
                      dplyr::select(GROUP_CODE, BENTHOS_CODE, COMP_2021, VIDEO_CODE)) %>%
            filter(BENTHOS_CODE == 'MA')%>%
            mutate(SITE_NO = as.character(SITE_NO)) %>%
            group_by(P_CODE, REEF, DEPTH, VISIT_NO,
                     SITE_NO, TRANSECT_NO) %>%
            summarise(MA = sum(POINTS)) %>%
            ungroup() %>% 
            right_join(total.points.aims %>%
                       dplyr::select(-total.points)) %>%
            mutate(MA = ifelse(is.na(MA), 0, MA)) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(ma.aims,
             file = paste0(DATA_PATH, 'processed/ma.aims.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'ma_points',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process MA points data.'), return=NULL)
}

CI_process_a_points <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'a_points',
                   label = "Process A points data", status = 'pending')
    CI_tryCatch({

        load(paste0(DATA_PATH, "primary/video_codes.RData"))
        load(paste0(DATA_PATH, "primary/points.raw.RData"))
        load(file = paste0(DATA_PATH, 'processed/total.points.aims.RData'))

        a.aims <- points.raw %>%
            left_join(video_codes %>%
                      dplyr::select(GROUP_CODE, BENTHOS_CODE, VIDEO_CODE)) %>%
            filter(GROUP_CODE == 'A' | BENTHOS_CODE == 'ST')%>%
            mutate(SITE_NO = as.character(SITE_NO)) %>%
            group_by(P_CODE, REEF, DEPTH, VISIT_NO,
                     SITE_NO, TRANSECT_NO) %>%
            summarise(A = sum(POINTS)) %>%
            ungroup() %>% 
            right_join(total.points.aims %>%
                       dplyr::select(-total.points)) %>%
            mutate(A = ifelse(is.na(A), 0, A)) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(a.aims,
             file = paste0(DATA_PATH, 'processed/a.aims.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'a_points',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process A points data.'), return=NULL)
}

##Points response variables
CI_process_combine_points <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'combine_points',
                   label = "Combine processed points", status = 'pending')
    CI_tryCatch({

        load(paste0(DATA_PATH, "processed/hc.aims.RData"))
        load(paste0(DATA_PATH, "processed/ma.aims.RData"))
        load(paste0(DATA_PATH, "processed/a.aims.RData"))
        load(paste0(DATA_PATH, "processed/total.points.aims.RData"))
        load(paste0(DATA_PATH, "processed/locations.RData"))

        points.analysis.data.transect.aims <- hc.aims %>% 
            left_join(locations %>%
                      dplyr::select(REEF, SITE_NO, LATITUDE, LONGITUDE))%>%
            left_join(a.aims) %>%
            left_join(ma.aims) %>%
            left_join(total.points.aims) %>%
            mutate(REEF = as.factor(REEF),
                   reef.site = as.factor(paste0(REEF, '.', SITE_NO)),
                   reef.site.tran = as.factor(paste0(reef.site, '.', TRANSECT_NO)),
                   Project = as.factor(ifelse(P_CODE == 'RM', 'LTMP', 'MMP'))) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(points.analysis.data.transect.aims,
             file = paste0(DATA_PATH, 'processed/points.analysis.data.transect.aims.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'combine_points',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Combine processed points.'), return=NULL)
}
