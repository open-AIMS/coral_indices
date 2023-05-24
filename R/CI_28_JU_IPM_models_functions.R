getIPM <- function(meta, sp) {
    if (sp %in% c("Pocilloporid", "Acropora")) {
        spp <- ifelse(sp == "Pocilloporid", "Pocilloporidae", "Acropora")
        load(file.path(DATA_PATH, "parameters",
                       sprintf("JuvCover_%s_%s_%s_%s_ResJuv_%s_Morph_NA.Rdata",
                               spp,
                               meta$R,
                               meta$S,
                               meta$Exp,
                               meta$reset.juv)))
    } else {
        load(file.path(DATA_PATH, "parameters",
                       sprintf("JuvCover_Mound_growth_%s_%s_%s_ResJuv_%s_Morph_%s.Rdata",
                               meta$R,
                               meta$S,
                               meta$Exp,
                               meta$reset.juv,
                               sp)))
        }
    return(
        if (sp == "Acropora") {
            results %>%
                dplyr::filter(time==10) %>%
                mutate(Species = sp,
                       Exposure = meta$Exp)
        } else {
            results %>%
                dplyr::filter(time==10,
                              juv_ab == meta[[paste0(sp,".dens")]]
                              ) %>%
                mutate(Species = sp,
                       Exposure = meta$Exp)
        }
    )
}

estimate_juv <- function(mod, expCov, OthCov) {
    pred_y <- function(x)predict.lm(mod, data.frame(juv_ab=x),type = "response")
    pred_x <- function(y) optim(1, \(x) (y-pred_y(x))^2, method='BFGS')[[1]]
    pred_x(expCov-OthCov)/100
}
