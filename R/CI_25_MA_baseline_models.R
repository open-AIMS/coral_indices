CI_25_MA_baseline_models <- function() {
    source('../R/functions.R')

    CI__add_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                   item = 'ma_baseline',
                   label = "MA baseline model", status = 'pending')

    CI_tryCatch({
        load(paste0(DATA_PATH, 'processed/points.analysis.data.transect.RData'))
        load(paste0(DATA_PATH, 'processed/spatial_lookup.RData'))
        load(paste0(DATA_PATH, 'primary/disturbances.RData'))

        ## Shallow====================================================================================
        ## - create a categorical version of depth and subset to <3 m Shallow
        ##   only

        ## - remove some post disturbance obs that can create odd MA
        ##   observations

        points <- points.analysis.data.transect %>%
            ungroup() %>%
            left_join(spatial_lookup) %>%
            mutate(P_CODE=as.factor(P_CODE),
                   DEPTH.f=factor(case_when(DEPTH>3~"deep slope",
                                            DEPTH<=3~"shallow slope")),
                   REEF.d=factor(paste(REEF, DEPTH.f)),
                   REPORT_YEAR=as.numeric(as.character(REPORT_YEAR))) %>%
            filter(REPORT_YEAR >2005,
                   REPORT_YEAR <= 2021,
                   DEPTH.f=="shallow slope") %>% 
            droplevels %>%
            mutate(fYEAR = factor(REPORT_YEAR),
                   Site = factor(paste0(REEF.d, SITE_NO)),
                   Transect = factor(paste0(Site, TRANSECT_NO))) %>%
            group_by(REEF.d) %>%
            mutate(LONGITUDE = mean(LONGITUDE),
                   LATITUDE = mean(LATITUDE))%>%
            ungroup

        ## exclude post flood and storm obs as these are often outliers

        disturbance<-disturbance.reef %>% filter(RANK=='1') %>%
            dplyr::select(P_CODE,REEF,DEPTH,VISIT_NO,DISTURBANCE)

        points<-points %>%
            left_join(disturbance) %>%
            filter(!DISTURBANCE %in% c('s','f')) 

        ## STEP 1 - establish the full spatio-temporal grid
        ## eventually replace with an expression that will provide unique
        ## long/lat/year combinations for all possible locations
        ## for now, I will use the locations defined in the data
        points.full.grid <- points %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            tidyr::crossing(fYEAR = points$fYEAR)

        ## STEP 2 - establish a spatio-temporal grid of the actual data.class
        ## This is all observed locations for all monitored years
        points.data.grid <- points %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            tidyr::crossing(fYEAR = points$fYEAR)

        ## STEP 3 - create a matrix of locations
        points.coords <- points.data.grid %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            as.matrix()

        ## STEP 4 - create mesh
        mesh <- inla.mesh.2d(loc = points.coords,
                             max.edge = c(0.5,3)*0.95,
                             offset = c(0.95,3),
                             cutoff = 0.95/5) %>%
            suppressMessages() %>%
            suppressWarnings()
        ##plot(mesh)
        ##points(LATITUDE ~ LONGITUDE, points.coords, add = TRUE)
        ##dev.off()

        ## STEP 5 - generate the SPDE
        spde <- inla.spde2.matern(mesh, alpha = 2)
        ## define the spatial indicies for convenient access to model outputs

        i.spatial <- inla.spde.make.index('spatial.field',
                                          n.spde = spde$n.spde
                                          )
        A.est <- inla.spde.make.A(mesh = mesh,
                                  loc = as.matrix(points[,c('LONGITUDE', 'LATITUDE')])
                                  )
        
        ## STEP 6 - generate the data stack

        stack.est <- inla.stack(data = list(y = points$MA,
                                            Total=points$A),
                                A = list(A.est, 1,1,1,1), 
                                effects = list(
                                    c(list(Intercept=1),i.spatial),
                                    fYEAR=points$fYEAR,
                                    Project = points$Project,
                                    Site = points$Site,
                                    Transect = points$Transect
                                ),
                                tag = 'est'
                                )

        ## STEP 7 - define formula
        form <- y ~ 0 + 
            f(spatial.field,
              model = spde
              ) +
            f(Site, model = 'iid') +
            f(Transect, model = 'iid') +
            f(fYEAR, model = 'iid') 

        form<-update(form, .~.+Intercept)


        ## STEP 8 - fit the model
        mod <- inla(form,
                    data = inla.stack.data(stack.est),
                    family= 'binomial',
                    Ntrials=Total,
                    control.predictor = list(compute = TRUE,
                                             link = 1,
                                             A = inla.stack.A(stack.est)
                                             ),
                    control.compute = list(config = TRUE, waic = TRUE),
                    verbose = FALSE)


        save(mod, mesh,
             file=paste0(DATA_PATH, 'parameters/MA__baseline_shallow.RData'))
        ## meshdraws_MApShallow <- inla.posterior.sample(1000,
        ##                                               result=baselineMApShallow_mod_mesh,
        ##                                               seed=123) %>%
        ##     suppressWarnings()
        ## save(meshdraws_MApShallow, file=paste0(DATA_PATH, 'modelled/meshdraws_MApShallow.RData'))

        ## Deep=======================================================================================
        points<- points.analysis.data.transect %>% ungroup %>%
            left_join(spatial_lookup) %>%
            mutate(P_CODE=as.factor(P_CODE),
                   DEPTH.f=factor(case_when(DEPTH>3~"deep slope",
                                            DEPTH<=3~"shallow slope")),
                   REEF.d=factor(paste(REEF, DEPTH.f)),
                   REPORT_YEAR=as.numeric(as.character(REPORT_YEAR))) %>%
            filter(REPORT_YEAR >2005,
                   REPORT_YEAR <= 2021,
                   DEPTH<9.1,
                   DEPTH.f=="deep slope",
                   BIOREGION %in% c('16','17','18','22','23','29')) %>%
            droplevels %>%
            mutate(fYEAR = factor(REPORT_YEAR),
                   Site = factor(paste0(REEF.d, SITE_NO)),
                   Transect = factor(paste0(Site, TRANSECT_NO)))

        
                                        # exclude post flood and storm obs as these are often outliers

        disturbance<-disturbance.reef %>% filter(RANK=='1') %>%
            dplyr::select(P_CODE,REEF,DEPTH,VISIT_NO,DISTURBANCE)

        points<-points %>%
            left_join(disturbance) %>%
            filter(!DISTURBANCE %in% c('s','f')) 

        ## STEP 1 - establish the full spatio-temporal grid
        ## eventually replace with an expression that will provide unique
        ## long/lat/year combinations for all possible locations
        ## for now, I will use the locations defined in the data
        points.full.grid <- points %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            tidyr::crossing(fYEAR = points$fYEAR)

        ## STEP 2 - establish a spatio-temporal grid of the actual data.class
        ## This is all observed locations for all monitored years
        points.data.grid <- points %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            tidyr::crossing(fYEAR = points$fYEAR)

        ## STEP 3 - create a matrix of locations
        points.coords <- points.data.grid %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            as.matrix()

        ## STEP 4 - create mesh

        mesh <- inla.mesh.2d(loc = points.coords,
                             max.edge = c(0.5,3)*0.95,
                             offset = c(0.95,3),
                             cutoff = 0.95/5)
        ## plot(mesh)
        ## points(LATITUDE ~ LONGITUDE, points.coords, add = TRUE)
                                        #dev.off()

        ## STEP 5 - generate the SPDE
        spde <- inla.spde2.matern(mesh, alpha = 2)
        ## define the spatial indicies for convenient access to model outputs

        i.spatial <- inla.spde.make.index('spatial.field',
                                          n.spde = spde$n.spde
                                          )
        A.est <- inla.spde.make.A(mesh = mesh,
                                  loc = as.matrix(points[,c('LONGITUDE', 'LATITUDE')])
                                  )
        
        ## STEP 6 - generate the data stack
                                        # stack.est <- inla.stack(data = list(y = points$MA,
                                        #                                     Total=points$total.algae),
                                        #                         #A = list(A.est, list(1), list(1), list(1), list(1), list(1), list(1), list(1), list(1)),
                                        #                         A = list(A.est, list(1), list(1), list(1), list(1)), 
                                        #                         effects = list(
                                        #                           i.spatial,
                                        #                           Intercept=rep(1, nrow(points)),
                                        #                           #list(DEPTH.f = points$DEPTH.f),
                                        #                           list(fYEAR = points$fYEAR),
                                        #                           list(Project = points$Project),
                                        #                           list(Site = points$Site)
                                        #                         ),
                                        #                         tag = 'est'
                                        # )
        stack.est <- inla.stack(data = list(y = points$MA,
                                            Total=points$A),
                                A = list(A.est, 1,1,1,1), 
                                effects = list(
                                    c(list(Intercept=1),i.spatial),
                                    fYEAR=points$fYEAR,
                                    Project = points$Project,
                                    Site = points$Site,
                                    Transect = points$Transect
                                ),
                                tag = 'est'
                                )
        ## STEP 7 - define formula
                                        # note that the form statement excludes the intercept initially for convenience when parameterising the rest of the model, but it is added back in later
        form <- y ~ 0 + 
            f(spatial.field,
              model = spde
              ) +
            f(Site, model = 'iid') +
            f(Transect, model = 'iid') +
            f(fYEAR, model = 'iid')


        form<-update(form, .~.+Intercept)


        ## STEP 8 - fit the model
        mod <- inla(form,
                    data = inla.stack.data(stack.est),
                    ## data = inla.stack.data(stack.all),
                    family= 'binomial',
                    Ntrials=Total,
                    control.predictor = list(compute = TRUE,
                                             link = 1,
                                             A = inla.stack.A(stack.est)
                                             ),
                    control.compute = list(config = TRUE, waic = TRUE),
                    ## verbose = TRUE)
                    verbose = FALSE)

                                        # mod.inla$waic$waic

        save(mod, mesh,
             file=paste0(DATA_PATH, 'parameters/MA__baseline_deep.RData'))

        ## meshdraws_MApDeep_inshore  <- inla.posterior.sample(1000,
        ##                                                     result=baselineMApDeep_inshore_mod_mesh,
        ##                                                     seed=123) %>%
        ##     suppressWarnings()
        ## save(meshdraws_MApDeep_inshore , file=paste0(DATA_PATH, 'modelled/meshdraws_MApDeep_inshore.RData'))


        ## Offshore===================================================================================

        points<- points.analysis.data.transect %>% ungroup %>%
            left_join(spatial_lookup) %>%
            mutate(P_CODE=as.factor(P_CODE),
                   DEPTH.f=factor(case_when(DEPTH>3~"deep slope",
                                            DEPTH<=3~"shallow slope")),
                   NRM=as.factor(NRM),
                   REEF=factor(REEF),
                   REPORT_YEAR=as.numeric(as.character(REPORT_YEAR)),
                   BIOREGION=as.character(BIOREGION),
                   BIOREGION.agg=as.factor(case_when(BIOREGION %in% c("4", "3")~"4:3",
                                                     BIOREGION %in% c("35", "36")~"35:36",
                                                     !BIOREGION %in% c("4", "3", "35", "36")~BIOREGION)))%>% 
            ungroup() %>%
            filter(REPORT_YEAR >2005,
                   REPORT_YEAR <= 2021,
                   !BIOREGION %in% c("16","17","18","19","22","23","29")) %>%  # inshore reefs
            droplevels %>%
            mutate(fYEAR = factor(REPORT_YEAR),
                   Site = factor(paste0(REEF, SITE_NO)),
                   Transect = factor(paste0(Site, TRANSECT_NO)))

        disturbance<-disturbance.reef %>% filter(RANK=='1') %>%
            dplyr::select(REEF,VISIT_NO,DISTURBANCE)

        points<-points %>%
            left_join(disturbance) %>%
            filter(!DISTURBANCE=='s') 

        ## STEP 1 - establish the full spatio-temporal grid
        ## eventually replace with an expression that will provide unique
        ## long/lat/year combinations for all possible locations
        ## for now, I will use the locations defined in the data
        points.full.grid <- points %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            tidyr::crossing(fYEAR = points$fYEAR)

        ## STEP 2 - establish a spatio-temporal grid of the actual data.class
        ## This is all observed locations for all monitored years
        points.data.grid <- points %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            tidyr::crossing(fYEAR = points$fYEAR)

        ## STEP 3 - create a matrix of locations
        points.coords <- points.data.grid %>%
            dplyr::select(LONGITUDE, LATITUDE) %>%
            distinct() %>%
            as.matrix()

        ## STEP 4 - create mesh
        gbr.sf <- sf::read_sf('../data/spatial/GBRMP boundary/Great_Barrier_Reef_Marine_Park_Boundary.shp')
        bndry <- gbr.sf %>%
            st_transform(4326) %>%
            st_cast('POINT') %>%
            mutate(Longitude = st_coordinates(.)[,1],
                   Latitude = st_coordinates(.)[,2]) %>%
            select(Longitude, Latitude) %>%
            st_drop_geometry() %>%
            as.matrix() %>%
            inla.nonconvex.hull()
        mesh <- inla.mesh.2d(
            loc = points.coords,
            boundary = bndry,
            max.edge = c(0.5,3)*0.95,
            offset = c(0.95,3),
            cutoff = 0.95/20) ##40 secs with cutoff = 0.95/5


        ## mesh <- inla.mesh.2d(loc = points.coords,
        ##                      max.edge = 2,
        ##                      cutoff = 1)

        ## plot(mesh)
        ## points(LATITUDE ~ LONGITUDE, points.coords, add = TRUE)

        ## ggplot() + gg(data=mesh) +
        ##   geom_point(data = as.data.frame(points.coords), aes(y = LATITUDE, x = LONGITUDE), color = "red") +
        ##   geom_sf(data = bioregion) +
        ##   coord_sf(xlim=c(144,146), ylim=c(-18,-14))


        ## STEP 5 - generate the SPDE
        spde <- inla.spde2.matern(mesh, alpha = 2)
        ## define the spatial indicies for convenient access to model outputs

        i.spatial <- inla.spde.make.index('spatial.field',
                                          n.spde = spde$n.spde
                                          )
        A.est <- inla.spde.make.A(mesh = mesh,
                                  loc = as.matrix(points[,c('LONGITUDE', 'LATITUDE')])
                                  )

        ## STEP 6 - generate the data stack
        stack.est <- inla.stack(data = list(y = points$MA,
                                            Total=points$A),
                                A = list(A.est, 1,1,1,1), 
                                effects = list(
                                    c(list(Intercept=1),i.spatial),
                                    fYEAR=points$fYEAR,
                                    Project = points$Project,
                                    Site = points$Site,
                                    Transect = points$Transect
                                ),
                                tag = 'est'
                                )

        ## ----baselineMeshModel

        ## STEP 7 - define formula
        form <- y ~ 0 + #Intercept +
                                        #sHs95 +
                                        #sk490 +
                                        #schl +
            f(spatial.field,
              model = spde
              ## group = spatial.field.group,
              ## control.group = list(model = "ar1")) +
              ) +
            f(Project, model = 'iid') +
            f(Site, model = 'iid') +
            f(Transect, model = 'iid')+
            f(fYEAR, model = 'iid') #+
                                        #f(DEPTH.f, model = 'iid')
        
        ## STEP 8 - remove formula terms for terms that have fewer than 2 levels
        term.labels <- attr(terms(form), 'term.labels')
        vars <- all.vars(form)[-1]
        vars <- vars[-grep('spatial|spde', vars)]
        for (v in vars) {
            if ((points %>% pull(!!sym(v)) %>% levels() %>% length() < 2) & (points %>% pull(!!sym(v)) %>% class() != "numeric")) {
                ## fixed effects
                if (v %in% term.labels) {
                    wch <- which(v == term.labels)
                    if (length(wch) ==0) next
                    term.labels <- term.labels[-wch]
                    form <- reformulate(term.labels, response = all.vars(form)[1], intercept = FALSE)
                }
                ## random effects
                wch <- grep(paste0('^f.',v), term.labels, perl = TRUE)
                if (length(wch)==0) next
                term.labels <- term.labels[-wch]
                form <- reformulate(term.labels, response = all.vars(form)[1], intercept = FALSE)
            }
        }

        form<-update(form, .~.+Intercept)


        ## STEP 8 - fit the model
        mod <- inla(form,
                    data = inla.stack.data(stack.est),
                    ## data = inla.stack.data(stack.all),
                    family= 'binomial',
                    Ntrials=Total,
                    control.predictor = list(compute = TRUE,
                                             link = 1,
                                             A = inla.stack.A(stack.est)
                                             ),
                    control.compute = list(config = TRUE),
                    ## verbose = TRUE)
                    verbose = FALSE)

        ## ----end

        save(mod, mesh,
             file=paste0(DATA_PATH, 'parameters/MA__baseline_offshore.RData'))

        ## baselineMAoffshore_mesh_draws<- inla.posterior.sample(1000,
        ##                                                       result=mod.inla.MA.offshore, seed=123) %>%
        ##     suppressWarnings()

        ## save(mod.inla.MA.offshore, file=paste0(DATA_PATH, "modelled/mod.inla.MA.offshore.RData"))
        ## save(baselineMAoffshore_mesh_draws, file=paste0(DATA_PATH, 'modelled/baselineMAoffshore_mesh_draws.RData'))

        CI__change_status(stage = paste0('STAGE',CI$setting$CURRENT_STAGE),
                          item = 'ma_baseline',status = 'success')

    }, logFile=LOG_FILE, Category='--Data processing--',
    msg=paste0('MA baseline model.'), return=NULL)
}
