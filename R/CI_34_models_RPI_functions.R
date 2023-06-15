CI_clear_models_RPI_data <- function() {
    files <- list.files(path = paste0(DATA_PATH, "modelled"),
                        pattern = "RPI.*|data_rpi.*",
                        full.names = TRUE)
    unlink(files)
}


CI_34_RPI_baseline_models <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'rpi_baselines',
                   label = "Model RPI baselines", status = 'pending')
    CI_tryCatch({

        ## only need to do the following once - perhaps move into RERUN_BASELINES section
        source("../R/externalFunctions/packages.R")
        source("../R/externalFunctions/LTMPDataTools.R")
        source("../R/externalFunctions/LTMPModellingTools.R")
        source("../R/externalFunctions/RPI_functions_other.R")
        CI_process_rpi_configs()
        CI_process_rpi_data_spatial()
        CI_process_rpi_get_cc_data()
        CI_process_rpi_get_samples_data()
        CI_process_groups_data()
        CI_process_groups_data_part2()
        CI_process_rpi_data()
        CI_process_rpi_filter_trajectories()
        ## Calculate mean, sd and se for HC and Abiotic for each recovery trajectory
        CI_calc_rpi_trajectories()
        ## Fit growth model
        CI_model_rpi_fitGrowthModel()
        ## Gather and thin model posteriors
        CI_model_rpi_gatherThin()       
        ## Remove non-convergence and calculate peak density
        CI_model_rpi_calcPeakDensity()
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'rpi_baselines',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Model RPI baselines'), return=NULL)
}

CI_process_rpi_data_spatial <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_rpi_s',
                   label = "Process RPI spatial data", status = 'pending')
    CI_tryCatch({
        load(paste0(DATA_PATH, 'processed/spatial_lookup.RData'))

        ## There are two levels of DEPTH for REEF.d, I think they are
        ##JCU sites. Will take the first level, just need a unique
        ##REEF.d There are two levels of TUMRA for two of the REEF.ds,
        ##I've just selected 1

        spatial_lookup<- spatial_lookup %>%
            group_by(REEF.d) %>%
            filter(DEPTH==first(DEPTH),
                   !(REEF.d=="Pandora Reef deep slope" & TUMRA==NA) & 
                   !(REEF.d=="Farquharson Reef deep slope" & TUMRA=="Mandubarra")) %>% droplevels() %>%
            dplyr::select(-DEPTH )%>%
            suppressMessages() %>%
            suppressWarnings()

        
        save(spatial_lookup, file=paste0(DATA_PATH, "/processed/spatial_lookup_rpi.RData"))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi_s',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI spatial data'), return=NULL)
}

CI_process_rpi_get_cc_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_rpi_cc',
                   label = "Process RPI CC data", status = 'pending')
    CI_tryCatch({

        load(file=paste0(DATA_PATH, "/processed/spatial_lookup_rpi.RData"))

        load_cc_posteriors <- function(r) { 
            if(!file.exists(file = paste0(DATA_PATH, "modelled/CC__", r, "__posteriors.RData"))) {
                return(NULL)
            } else{
                load(file=paste0(DATA_PATH, "modelled/CC__", r, "__posteriors.RData"))
                return(posteriors)
            }
        }

        reef.depth.names <- spatial_lookup %>%
            dplyr::pull(REEF.d) %>%
            unique() 
        reef.posteriors.all <- reef.depth.names %>%
            map_df(.f = ~ load_cc_posteriors(.x)) %>%
            mutate(value = value*100)
        
        save(reef.posteriors.all, file=paste0(DATA_PATH, "/processed/reef.posteriors.all.RData"))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi_cc',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI CC data'), return=NULL)
}


CI_process_rpi_get_samples_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_rpi_samples',
                   label = "Process RPI samples data", status = 'pending')
    CI_tryCatch({

        load(file=paste0(DATA_PATH, "/processed/sample.reef.report.year.RData"))

        samples <- sample.reef.report.year %>%
            mutate(DEPTH.f = factor(ifelse(DEPTH >= 3, "deep slope", "shallow slope")),
                   REEF.d = factor(paste(REEF, DEPTH.f, sep = " "))) %>%
            dplyr::select(-DEPTH, -P_CODE) %>%
            group_by(REEF, DEPTH.f, REEF.d, VISIT_NO, REPORT_YEAR) %>%
            summarise(Latitude = min(LATITUDE),
                      Longitude = min(LONGITUDE),
                      Date = min(Date)) %>%
            mutate(fYEAR = factor(REPORT_YEAR)) %>%
            filter(REPORT_YEAR <= 2020) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        save(samples, file=paste0(DATA_PATH, "/processed/samples_rpi.RData"))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi_samples',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI samples data'), return=NULL)
}

CI_process_groups_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_groups',
                   label = "Process RPI group data", status = 'pending')
    CI_tryCatch({
        load(paste0(DATA_PATH, 'primary/points.raw.RData'))
        load(paste0(DATA_PATH, "primary/video_codes.RData"))

        ## AIMS data
        ##Total points transect level
        total.points <- points.raw %>%
            group_by(P_CODE, REEF,DEPTH, VISIT_NO, SITE_NO) %>%
            mutate(SITE_NO = as.character(SITE_NO)) %>% 
            summarise(total.points = sum(POINTS)) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        
        ## ##group points     
        ## groups.transect <- points.raw %>%
        ##     left_join(video_codes %>% dplyr::select(GROUP_CODE, VIDEO_CODE)) %>%
        ##     mutate(SITE_NO = as.character(SITE_NO),
        ##            TRANSECT_NO = as.character(TRANSECT_NO)) %>%
        ##     group_by(P_CODE, REEF, DEPTH, VISIT_NO, GROUP_CODE, SITE_NO, TRANSECT_NO) %>%
        ##     summarise(Points = sum(POINTS)) %>%
        ##     ungroup %>% 
        ##     left_join(total.points) %>%
        ##     mutate(COVER = (Points/total.points)*100) %>%
        ##     dplyr::select(-Points, -total.points)%>%
        ##     pivot_wider(names_from = "GROUP_CODE", values_from = "COVER", values_fill = 0) %>%
        ##     pivot_longer(cols = c('A', 'AB', 'HC', 'SC', 'SG', 'SP', 'OT'),
        ##                  names_to = 'GROUP_CODE', values_to = 'COVER') %>%
        ##     mutate(P_CODE = as.factor(P_CODE),
        ##            REEF = as.factor(REEF),
        ##            SITE_NO = as.integer(SITE_NO),
        ##            TRANSECT_NO = as.integer(TRANSECT_NO),
        ##            GROUP_CODE = as.factor(GROUP_CODE)) %>%
        ##     suppressMessages() %>%
        ##     suppressWarnings()


        ## save(groups.transect,
        ##      file = paste0(DATA_PATH, 'processed/groups.transect.RData'))

        groups.site <- points.raw %>%
            left_join(video_codes %>% dplyr::select(GROUP_CODE, VIDEO_CODE)) %>%
            mutate(SITE_NO = as.character(SITE_NO)) %>%
            group_by(P_CODE, REEF, DEPTH, VISIT_NO, GROUP_CODE, SITE_NO) %>%
            summarise(Points = sum(POINTS)) %>%
            ungroup %>% 
            left_join(total.points) %>%
            mutate(COVER = (Points/total.points)*100) %>%
            dplyr::select(-Points, -total.points)%>%
            pivot_wider(names_from = "GROUP_CODE", values_from = "COVER", values_fill = 0) %>%
            pivot_longer(cols = c('A', 'AB', 'HC', 'SC', 'SG', 'SP', 'OT'),
                         names_to = 'GROUP_CODE', values_to = 'COVER') %>%
            mutate(P_CODE = as.factor(P_CODE),
                   REEF = as.factor(REEF),
                   SITE_NO = as.integer(SITE_NO),
                   GROUP_CODE = as.factor(GROUP_CODE)) %>%
            suppressMessages() %>%
            suppressWarnings()


        save(groups.site,
             file = paste0(DATA_PATH, 'processed/groups.site.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_groups2',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI group data'), return=NULL)
}

CI_process_groups_data_part2 <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_groups2',
                   label = "Process RPI group data part 2", status = 'pending')
    CI_tryCatch({
        load(file = paste0(DATA_PATH, 'processed/groups.site.RData'))
        load(file = paste0(DATA_PATH, "processed/samples_rpi.RData"))
        
        get.abiotic <- groups.site %>%
            mutate(DEPTH.f = factor(ifelse(DEPTH >= 3, "deep slope", "shallow slope")),
                   REEF.d = factor(paste(REEF, DEPTH.f, sep=" "))) %>%
            dplyr::select(-DEPTH) %>%
            filter(GROUP_CODE == "AB") %>%
            dplyr::select(-GROUP_CODE) %>%
            group_by(P_CODE,REEF.d, VISIT_NO) %>%
            summarise(AB = mean(COVER)) %>%
            left_join(samples %>%
                      dplyr::select(REEF, DEPTH.f, REEF.d, VISIT_NO,
                                    REPORT_YEAR, fYEAR)) %>%
            filter(!is.na(REPORT_YEAR) & REPORT_YEAR <= 2020) %>%
            droplevels() %>%
            ungroup() %>%
            dplyr::select(-VISIT_NO, -P_CODE) %>%
            suppressMessages() %>%
            suppressWarnings()

        ##For HC, each posterior draw is treated as a 'transect' in the pipeline
        load(file = paste0(DATA_PATH, "processed//reef.posteriors.all.RData"))

        reef.posteriors<- reef.posteriors.all %>%
            rename(HC=value,
                   TRANSECT_NO=`.draw`) %>%
            mutate(REPORT_YEAR=as.numeric(as.character(fYEAR))) %>%
            filter(REPORT_YEAR<=2020) %>%
            droplevels() %>%
            suppressMessages() %>%
            suppressWarnings()

        ##Put Abiotic and HC cover together in one dataframe
        groups.transect <- reef.posteriors %>%
            left_join(get.abiotic) %>%
            filter(!is.na(AB)) %>%
            droplevels()  %>% ## locations with NA abiotic are mostly JCU sites
            pivot_longer(cols = c(AB, HC),
                         names_to = "GROUP_CODE", values_to = "COVER") %>% 
            ## gather(AB, HC, key = "GROUP_CODE", value = COVER)  %>%      
            left_join(spatial_lookup %>%
                      dplyr::select(REEF.d, REEF, DEPTH.f) %>%
                      distinct) %>%
            mutate(GROUP_CODE = factor(GROUP_CODE)) %>%
            suppressMessages() %>%
            suppressWarnings()

        ##This forms the primary data 'groups.transect'
        save(groups.transect,
             file = paste0(DATA_PATH, 'processed/groups.transect.RData'))

        
        ##Load the disturbance table and save to the pipeline-specific primary directory

        ##Why are there two identical records for Farquharson Reef for each year??

        load(paste0(DATA_PATH, "primary/disturbances.RData"))

        disturbance <- disturbance.reef %>% 
            mutate(DISTURBANCE = factor(DISTURBANCE),
                   DEPTH.f = factor(ifelse(DEPTH >= 3, "deep slope", "shallow slope")),
                   REEF.d = factor(paste(REEF, DEPTH.f, sep = " "))) %>%
            dplyr::select(-DEPTH) %>%
            left_join(samples %>%
                      dplyr::select(REEF, DEPTH.f, REEF.d, VISIT_NO,
                                    fYEAR, REPORT_YEAR)) %>%
            dplyr::select(-P_CODE, -VISIT_NO) %>%
            filter(RANK %in% c(NA, 1)) %>%
            droplevels() %>%
            group_by(REEF, RANK, DISTURBANCE, STORM_NAME,
                     DISTURBANCE_DATE, DEPTH.f, REEF.d, REPORT_YEAR, fYEAR) %>%
            unique() %>%
            filter(REPORT_YEAR <= 2020) %>%
            droplevels() %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        save(disturbance,
             file = paste0(DATA_PATH, "processed/disturbance_rpi.RData"))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_groups2',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI group data part 2'), return=NULL)
}


CI_process_rpi_data <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_rpi',
                   label = "Process RPI data", status = 'pending')
    CI_tryCatch({
        load(paste0(DATA_PATH, 'processed/groups.transect.RData'))
        load(paste0(DATA_PATH, "processed/sample.reef.report.year.RData"))
        load(file = paste0(DATA_PATH, "processed/disturbance_rpi.RData"))

        groups.transect.2<- groups.transect %>%
            mutate(REEF.d.year = paste(REEF.d, fYEAR, sep = " "))
  
        disturbance.2 <- disturbance %>%
            mutate(REEF.d.year = paste(REEF.d, fYEAR, sep = " ")) %>%
            filter(REEF.d.year %in% unique(groups.transect.2$REEF.d.year)) %>%
            droplevels()

        samples.2 <- samples %>%
            mutate(REEF.d.year = paste(REEF.d, fYEAR, sep = " ")) %>%
            filter(REEF.d.year %in% unique(groups.transect.2$REEF.d.year)) %>%
            droplevels()
        
        ## Use function to assign each reef.d/report_year to a recovery trajectory
        recovery.trajectories <- extract_recovery_trajectories_transect(disturbance.2,
                                                                        samples.2,
                                                                        groups.transect.2)
 
        save(recovery.trajectories,
             file = paste0(DATA_PATH, "processed/time.series.site.transect.RData"))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI data'), return=NULL)
}


CI_process_rpi_filter_trajectories <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_rpi_ft',
                   label = "Process RPI data filter trajectories", status = 'pending')
    CI_tryCatch({

        load(file = paste0(DATA_PATH, "processed/time.series.site.transect.RData"))
        load(paste0(DATA_PATH, 'processed/groups.transect.RData'))
        load(paste0(DATA_PATH, 'processed/spatial_lookup.RData'))

        ## Baseline trajectories need at least 4 observations
        ## Remove missing dates - the model needs dates
        rm.ongoing.trajectories <- recovery.trajectories %>% 
            mutate(proj.site.rpid = paste(REEF.d, RP_ID, sep = " ")) %>%
            ungroup() %>%
            group_by(proj.site.rpid) %>% 
            mutate(max.report.year = max(REPORT_YEAR),
                   NUM_OBS = as.numeric(length(unique(REPORT_YEAR)))) %>%
            filter(!is.na(Date), NUM_OBS > 3) %>%
            droplevels() %>%  ##Filter to trajectories that have a least 4 observations
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        ## This was to prevent missing data, which causes issues further down the pipeline
        groups.transect.traj.reefds <- groups.transect %>%
            filter(REEF.d %in% unique(rm.ongoing.trajectories$REEF.d)) %>%
            droplevels()

        filt.rec.traj <- rm.ongoing.trajectories %>%
            left_join(groups.transect.traj.reefds) %>%
            left_join(spatial_lookup %>% 
                      dplyr::select(ZONE, Shelf, NRM, TUMRA, BIOREGION,
                                    REEF, DEPTH.f, REEF.d) %>%
                      distinct()) %>% 
            mutate(BIOREGION = factor(BIOREGION),
                   ZONE = factor(ZONE),
                   Shelf = factor(Shelf),
                   NRM = factor(NRM),
                   RP_ID = factor(RP_ID),
                   zone.shelf = paste(ZONE, Shelf, sep = " ")) %>%
            ungroup() %>%
            suppressMessages() %>%
            suppressWarnings()

        ## save the trajectory data
        save(filt.rec.traj,
             file = paste(DATA_PATH, "processed/",
                          sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
                         ,".RData",sep=""))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi_ft',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI data filter trajectories'), return=NULL)
}

CI_process_rpi_configs <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'process_rpi_config',
                   label = "Process RPI configs", status = 'pending')
    CI_tryCatch({
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi_config',status = 'success')

        K.limit     <<- 90
        max_init    <<- 100.0 # thresh hold for initial condition (in hard coral cover)
        min_final   <<- 0.0 # minimum threshold for final observation before next disturbance
        min_obs     <<- 4.0 # minimum number of observations
        start_date  <<- lubridate::ymd("1990-01-01") 
        end_date    <<- lubridate::ymd("2022-01-01")

        DATA_DIR                  <<- paste0(DATA_PATH, "primary/")
        MODEL_DIR                 <<- paste0("../R/externalFunctions/")
        DIST_DAT_FILE             <<- "disturbance.RData"
        SAMPLE_DAT_FILE           <<- "samples.RData"
        GRP_LEV_TRANS_DAT_FILE    <<- "groups.transect.RData"
        GROUP_NAMES               <<- "spatial_lookup.RData"

        if (!dir.exists(paste0(DATA_PATH, "processed/RPI_baseline/")))
            dir.create(paste0(DATA_PATH, "processed/RPI_baseline/"))
        PROC_DATA_DIR             <<- paste0(DATA_PATH, "processed/")
        MCMC_OUTPUT_DIR           <<- paste0(DATA_PATH, "processed/RPI_baseline/")

        FILTER_OUT_DATA_FMT         <<- "rec.traj.trans.init%f.final%f.obs%d"
        FILTER_REFMT_OUT_DATA_FMT   <<- "rec.traj.trans.proc.init%f.final%f.obs%d"
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'process_rpi_config',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Process RPI configs'), return=NULL)
}

CI_calc_rpi_trajectories <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'rpi_traj',
                   label = "Calculate RPI trajectories", status = 'pending')
    CI_tryCatch({

        load(file = paste(DATA_PATH, "processed/",
                          sprintf(FILTER_OUT_DATA_FMT,max_init,min_final,min_obs)
                         ,".RData",sep=""))
        
        ## In lists throughout, reefs are often grouped by BIOREGION
        filt.rec.traj.proc <- vector(mode="list",
                                     length = length(unique(filt.rec.traj$BIOREGION)))
        names(filt.rec.traj.proc) <- unique(filt.rec.traj$BIOREGION)

        for(reef_group in unique(filt.rec.traj$BIOREGION)){
            
            df <- filt.rec.traj %>%
                filter(BIOREGION == reef_group) %>%
                droplevels()
            rpid.list <- vector(mode = "list",
                                length = length(unique(df$RP_ID)))
            names(rpid.list) <- unique(df$RP_ID)
            
            for(rp_id in unique(df$RP_ID)){
                rpid.df <- df %>%
                    filter(RP_ID == rp_id) %>%
                    droplevels()
                rpid.reformat <- rpid.df %>% 
                    reformat_recovery_trajectories() %>% 
                    mutate(proj.site.rpid = paste(REEF.d, RP_ID, sep=" "),
                           NRM = unique(rpid.df$NRM),
                           BIOREGION = factor(reef_group),
                           ZONE = unique(rpid.df$ZONE),
                           Shelf = unique(rpid.df$Shelf),
                           zone.shelf = unique(rpid.df$zone.shelf),
                           REEF = unique(rpid.df$REEF),
                           DEPTH.f = unique(rpid.df$DEPTH.f),
                           REEF.d = unique(rpid.df$REEF.d))
                
                rpid.list[[rp_id]] = rpid.reformat
                
            }
            
            ## Nest each list under given reef group 
            filt.rec.traj.proc[[reef_group]] <- rpid.list
        }
        
        ## save the trajectory data with HC and AB data variables
        save(filt.rec.traj.proc,
             file=paste(DATA_PATH, "processed/",
                        sprintf(FILTER_REFMT_OUT_DATA_FMT,max_init,min_final,min_obs)
                       ,".RData",sep=""))
        
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                              item = 'rpi_traj',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Calculate RPI trajectories'), return=NULL)
}

CI_model_rpi_fitGrowthModel <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'rpi_fit',
                   label = "Fit RPI baseline growth model", status = 'pending')
    CI_tryCatch({
        source("../R/externalFunctions/RPI_baseline_4_coralFitGrowthModel.R") 
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'rpi_fit',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Fit RPI baseline growth model'), return=NULL)
}

CI_model_rpi_gatherThin <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'rpi_gather',
                   label = "Gather and thin RPI baseline growth models", status = 'pending')
    CI_tryCatch({
        source("../R/externalFunctions/RPI_baseline_6_gather_and_thin.R") 
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'rpi_gather',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Gather and thin RPI baseline growth models'), return=NULL)
}

CI_model_rpi_calcPeakDensity <- function() {
    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'rpi_peak_density',
                   label = "Calculate peak density RPI baseline growth models", status = 'pending')
    CI_tryCatch({
        source("../R/externalFunctions/RPI_baseline_8_rmNonConvergence_and_CalcPeakDensity.R") 
        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'rpi_peak_density',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('Calculated peak density RPI baseline growth models'), return=NULL)
}
