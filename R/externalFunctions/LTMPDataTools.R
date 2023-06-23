#' LTMPDataTools.R is a utility library for filtering, processing and grouping LTMP data
#'
#' @description 
#' Data processing functions to deal with the LTMP (Long Term Monitoring 
#' Program) and MMP (marine Monitoring Program) Data from AIMS (Australian Institute
#' of Marine Sciences).
#'
#'
#' @section Current features:
#' \itemize{
#'   \item data processing for splitting data into recovery trajectories from 
#'     site-level using LTMP disturbance classifications.
#'   \item Filtering of recovery trajectories based on upper-bound for initial 
#'     percent hard coral cover, lower-bound for final percent hard coral cover, and lower 
#'     on number of observations.
#'   \item Enables the assignment of trjectory groups base on external file or k-means clustering 
#' }
#' @section Warning:
#' All functions process trajectories to the site level, however, functions require transect level
#' data to enumerate the standard error of the site observations and disturbance significance testing. 
#' @section Important disclaimer:
#' This code assumes *.Rdata files of the form provied by Juan Ortiz and Angus Thompson, modifications \emph{may} be
#' required if data tables differ from this. We have done our best to make codes as generic as possible, but occasionally,
#' assumptions may have been made along the way (particularily with respect to table column names).
#' @author
#' \itemize{
#'      \item David J. Warne[1,2,3] (\email{david.warne@qut.edu.au})
#'      \item Grace E. M. Heron[1,3] (\email{g.heron@qut.edu.au})
#' }
#' \enumerate{
#'  \item School of Mathematical Sciences, Faculty of Science, Queensland University of Technology
#'  \item Centre for Data Science, Queensland University of Technology
#'  \item ARC Centre of Excellence for Mathematical and Statistical Frontiers (ACEMS)
#' }
#'
#' @note Functions are specifically focused on the filtering of AIMS coral cover 
#' data at a recovery trajectory level (sequences between two disturbance periods
#' for a given site). 
#' @docType data
#' @name AA_Library_Summary
NULL

# ROXYGEN_STOP


# ROXYGEN_START

#-------------------------------------------------------------------------------
#
#
#-------------------------------------------------------------------------------

#' Extract recovery trajectories by transect
#' 
#' @description  Extraction of recovery trajectories using transect level data.
#' Uses differences in successive transect observations at each site over
#' time to extract to detect start and end of site level recovery trajectories. 
#' Assigns a recovery id to each trajectory at a site level.
#'
#' @param disturbance        data frame of disturbance records
#' @param samples             data frame of visit time/place records
#' @param transect.cover     data frame of percent cover at transect level, can be at
#'                           major group level or benthic code (ker code) level
#'                          by major group ("GROUP_CODE") or ker code ("BENTHOS_CODE")
#' @param code               code base trajectory tests on
#' @param alpha              confidence level for paired t-test
#'
#' @note currently only supports GROUP_CODE grouping, future versions should 
#' implement BENTHOS_CODE grouping (but will probably need to specify groups to 
#' aggregate)
#' @note consider alternative approach to disturbance detection, e.g., a mixture
#' of t-test and disturbance records 
#' @family Data extraction
extract_recovery_trajectories_transect <- function(disturbance,samples,
                                                   transect.cover,
                                                   code = "HC",alpha = 0.95) {

    # to avoid factor warnings
    combined <- sort(union(union(levels(disturbance$REEF),levels(samples$REEF)),
                           levels(transect.cover$REEF)))
    disturbance <- disturbance %>% 
        mutate(REEF=factor(REEF,levels=combined))
    samples <- samples %>% 
        mutate(REEF=factor(REEF,levels=combined))
    transect.cover <- transect.cover %>% 
        mutate(REEF=factor(REEF,levels=combined))
   
    # extend disturbance factor levels to distinguish between recorded distubances
    # and our own entries
    combined <- sort(union(levels(disturbance$DISTURBANCE), c('U','T','S')))
    disturbance <- disturbance %>% 
        mutate(DISTURBANCE=factor(DISTURBANCE,levels=combined))


    # join disturbances and samples to transects
    samples.disturbance.transect.cover <- right_join(
                      right_join(disturbance, samples,by = c("REEF","DEPTH.f", "REEF.d",
                                                             "fYEAR", "REPORT_YEAR", "REEF.d.year")),
                      select(transect.cover, REEF,DEPTH.f,REEF.d,REPORT_YEAR),
                      by = c("REEF","DEPTH.f","REEF.d", "REPORT_YEAR")) %>%
                      mutate(RP_ID = 0,DIFF = 0, Pval = 0)
    #return(samples.disturbance.transect.cover)
    recovery.trajectories.site <- data.frame()
    # for every REEF/SITE/DEPTH.f triple by visit
    unique_sites <- unique(select(samples.disturbance.transect.cover,
                                   REEF,DEPTH.f,REEF.d))
    rp_id <- 0
    for (i in 1:nrow(unique_sites)){
        visit.sequence.site <- samples.disturbance.transect.cover %>% 
                                  filter(REEF == unique_sites$REEF[i],
                                         DEPTH.f == unique_sites$DEPTH.f[i],
                                         REEF.d == unique_sites$REEF.d[i]) %>%
                                  unique() %>%
                                  arrange(REPORT_YEAR)
        # step through sequence applying one-sided paired t-test to subsequent pairs
        num_dist <- 1
        visit.seq <- visit.sequence.site$REPORT_YEAR
        for (j in 1:length(visit.seq)) {
          #print(paste0("i = ", i, ': j = ', j, " ", visit.seq$REEF.d[1], " ", "report year ", visit.seq$REPORT_YEAR[1]))
          # print(paste0("i = ", i, ': j = ', j))
            # first visit is always a new recovery trajectory
            if (j == 1) { 
                rp_id <- rp_id + 1
                num_dist <- 1
                visit.sequence.site$DISTURBANCE[j] <- 'S' 
            } else { 
                # Get transect cover for level code provided for time t and t-1
                transect.cover.pair <- transect.cover %>% 
                          filter(REEF == visit.sequence.site$REEF[1],
                                 DEPTH.f == visit.sequence.site$DEPTH.f[1],
                                 REEF.d == visit.sequence.site$REEF.d[1],
                                 REPORT_YEAR == visit.seq[j] |
                                 REPORT_YEAR == visit.seq[j-1],
                                 GROUP_CODE == code) %>%
                          select(TRANSECT_NO,REPORT_YEAR,COVER)
                transect.cover.t1 <- transect.cover.pair %>%
                          filter(REPORT_YEAR == visit.seq[j-1])
                transect.cover.t2 <- transect.cover.pair %>%
                          filter(REPORT_YEAR == visit.seq[j])

                #compute drop/growth in cover of more than (drop > 0 and growth < 0)
                diff <- mean(transect.cover.t1$COVER) - mean(transect.cover.t2$COVER) 
                visit.sequence.site$DIFF[j] <- diff # store difference

                if (visit.sequence.site$DISTURBANCE[j] %in% 
                       c('d','s','b','c','m','u','f')) { # recorded disturbance
                    rp_id <- rp_id + 1
                    num_dist <- 1
                } else if (diff >= 3.0) { # unrecorded absolute drop >= 5%
                    rp_id <- rp_id + 1
                    num_dist <- num_dist + 1
                    # use U for unknown disturbance that was not recorded 
                    visit.sequence.site$DISTURBANCE[j] <- 'U' 
                } #else { # otherwise perform paired t-test to check for small but 
                #          # statistically significant drops
                # 
                #     # catch cases of missing transect observation, ensure we keep
                #     # only the pairs that match to ensure the paired t-test is
                #     # validly applied
                #     if (length(transect.cover.t1$REPORT_YEAR) < 
                #         length(transect.cover.t2$REPORT_YEAR)) {
                #         transect.cover.t2 <- transect.cover.t2 %>%
                #                filter(TRANSECT_NO %in% transect.cover.t1$TRANSECT_NO)
                #     } else if (length(transect.cover.t1$REPORT_YEAR) > 
                #                length(transect.cover.t2$REPORT_YEAR)) {
                #         transect.cover.t1 <- transect.cover.t1 %>%
                #                filter(TRANSECT_NO %in% transect.cover.t2$TRANSECT_NO)
                #     }
                #     transect.cover.t1 <- transect.cover.t1 %>% arrange(TRANSECT_NO)
                #     transect.cover.t2 <- transect.cover.t2 %>% arrange(TRANSECT_NO)
                # 
                #     # perform paired t-test if there are enough transects
                #     if (length(transect.cover.t1$TRANSECT_NO) > 1 &&
                #         length(transect.cover.t2$TRANSECT_NO) > 1) {
                #         # run paired t-test
                #         res <- t.test(x = transect.cover.t1$COVER, y = transect.cover.t2$COVER,
                #                       alternative = "greater", paired = TRUE, conf.level = alpha)
                #         ## if the t.test yielded a p.va.ue of NA (likkely the result of no variance)
                #         ## make the p.value 1
                #         if (is.na(res$p.value)) {
                #           if (diff>=2.0) {
                #           rp_id <- rp_id + 1
                #           num_dist <- num_dist + 1
                #           # use T for small unknown disturbance as per t-test 
                #           visit.sequence.site$DISTURBANCE[j] <- 'T'
                #           res$estimate<-diff
                #           }
                #           #res$p.value <- 1
                #           print(paste0('Warning, site number ', i, ', visit number ', j,' had no variance in COVER'))
                #         } else if (res$p.value < 1.0 - alpha) {
                #           # any statistically significant pair update the RP_ID
                #             rp_id <- rp_id + 1
                #             num_dist <- num_dist + 1
                #             # use T for small unknown disturbance as per t-test 
                #             visit.sequence.site$DISTURBANCE[j] <- 'T'
                #         }
                #         # store p-value and diff estimate regardless of test result 
                #         visit.sequence.site$Pval[j] <- res$p.value
                #         visit.sequence.site$DIFF[j] <- res$estimate
                #     }
                # } 
            }
            visit.sequence.site$RP_ID[j] <- rp_id
            visit.sequence.site$NUM_DIST[j] <- num_dist
        }    
        recovery.trajectories.site <- bind_rows(recovery.trajectories.site,
                                          visit.sequence.site)
    }
    return(recovery.trajectories.site)
}


#' Reformat recovery trajectories 
#'
#' @description  Reformat a single recovery trajectory as produced by the function
#' \code{filter_recovery_trajectoies_transect} for easier analysis.
#' @details  Rhe transect data is combined but group code to compute the site level
#' data and the standard error included as columns, so that each visit is a single row. 
#' Also the variable T is included as the number of days since the start of recovery.
#'
#' @param traj a single recovery trajectory (\code{RP_ID/REEF.d} is unique)
#' @param codes string for code field to use
#' @param annotate custom names
#'
#' @return a data table such that result\code{[i,]} is the data for the ith visit in this
#' recovery sequence.
#' @note include option to create new groups from sets of BENTHIC CODES to
#' separate out Achroporites.
#' @family Data reformatting
reformat_recovery_trajectories <- function(traj, codes = 'GROUP_CODE', annotate = "") {

    # extract the constant data and unique visits (date is unique for each visit
    # so the number of rows is the same as the number of visits
    output.seq <- traj %>%
                  select(REEF,REEF.d,DEPTH.f,RP_ID,Date,REPORT_YEAR,
                         #DURATION,DISTURBANCE) %>%
      DISTURBANCE) %>%
      
                  unique() %>%
                  arrange(REPORT_YEAR) %>%
                  mutate('T' =  as.double(Date - Date[1]))
    groups <- levels(traj[[codes]])
    # append a column for each group and populate
    for (g in groups){
        res <- traj %>% 
                filter(!!rlang::sym(codes) == g) %>%
                group_by(REPORT_YEAR) %>%
                summarise(mu = mean(COVER),
                          sd = sd(COVER), 
                          se = sd(COVER)/sqrt(length(COVER))) %>%
                arrange(REPORT_YEAR)
        if(nrow(res) == 0) {
            print(traj)
        } else {
        output.seq <- mutate(output.seq,!!paste(g,annotate,sep="") := res$mu, 
                             !!paste(g,"_sd",annotate,sep="") := res$sd,
                             !!paste(g,"_se",annotate,sep="") := res$se)
        }
    }
    return(output.seq)
}
