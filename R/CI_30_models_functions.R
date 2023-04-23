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
    cellmeans.spatial.2 <- cellmeans.full.2 %>%
        as.matrix() %>%
        plogis()

    cellmeans.spatial.2
}

