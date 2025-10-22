Baselines <- get(load(file = paste0(DATA_PATH, #KC - AT changed
                                            'modelled/JU__baseline_posteriors.RData')))

spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))

Baseline.shelf.depth<- Baselines |> 
    left_join(spatial_lookup |> dplyr::select(BIOREGION.agg, Shelf) |> distinct())

Baselines.shelf.depth.summary<- Baseline.shelf.depth |> 
    dplyr::select(Shelf, DEPTH.f, Taxa, value) |> 
        group_by(Shelf, DEPTH.f, Taxa) |> 
            ggdist::median_hdci(value)

ggplot(Baselines.shelf.depth.summary, aes(x=Shelf, y=value, colour=DEPTH.f))+
geom_point(position=position_dodge(width=0.3))+
geom_linerange(aes(ymin=`.lower`, ymax=`.upper`), position=position_dodge(width=0.3))+
facet_wrap(~Taxa, ncol=1)+
theme_bw()


Baselines.bioregion.depth.summary<- Baselines |> 
    dplyr::select(BIOREGION.agg, DEPTH.f, Taxa, value) |> 
        group_by(BIOREGION.agg, DEPTH.f, Taxa) |> 
            ggdist::median_hdci(value)

ggplot(Baselines.bioregion.depth.summary |> filter(Taxa=="Total"), aes(x=BIOREGION.agg, y=value, colour=DEPTH.f))+
geom_point(position=position_dodge(width=0.3))+
geom_linerange(aes(ymin=`.lower`, ymax=`.upper`), position=position_dodge(width=0.3))+
#facet_wrap(~Taxa, ncol=1, scales = 'free')+
geom_hline(yintercept=10, linetype='dashed', colour='red')+
geom_hline(yintercept=20, linetype='dashed', colour='orange')+
geom_hline(yintercept=28, linetype='dashed', colour='purple')+
theme_bw()



juv.preds <- get(load(file = paste0(DATA_PATH, "modelled/JU__preds.RData"))) 

JUV_preds <- juv.preds$Pred
# Add REEF.d and Taxa columns to each element before binding
JUV_preds_df <- bind_rows(
    lapply(seq_along(JUV_preds), function(i) {
        df <- JUV_preds[[i]]
        df$REEF.d <- mods$REEF.d[i]
        df$Taxa <- mods$Taxa[i]
        df
    })
) |>
right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f, BIOREGION.agg, Shelf) |> distinct(), by = "REEF.d")

Preds.bioregion.depth.summary<- JUV_preds_df  |> 
    dplyr::select(BIOREGION.agg, DEPTH.f, Taxa, value) |> 
        group_by(BIOREGION.agg, DEPTH.f, Taxa) |> 
            ggdist::median_hdci(value)

ggplot(Preds.bioregion.depth.summary |> filter(Taxa=="Total"), aes(x=BIOREGION.agg, y=value, colour=DEPTH.f))+
geom_point(position=position_dodge(width=0.3))+
geom_linerange(aes(ymin=`.lower`, ymax=`.upper`), position=position_dodge(width=0.3))+
#facet_wrap(~Taxa, ncol=1, scales = 'free')+
geom_hline(yintercept=8, linetype='dashed', colour='red')+
geom_hline(yintercept=20, linetype='dashed', colour='orange')+
geom_hline(yintercept=28, linetype='dashed', colour='purple')+
theme_bw()

ggplot(Preds.bioregion.depth.summary |> filter(Taxa=="Acropora"), aes(x=BIOREGION.agg, y=value, colour=DEPTH.f))+
geom_point(position=position_dodge(width=0.3))+
geom_linerange(aes(ymin=`.lower`, ymax=`.upper`), position=position_dodge(width=0.3))+
#facet_wrap(~Taxa, ncol=1, scales = 'free')+
geom_hline(yintercept=2.08, linetype='dashed', colour='red')+
geom_hline(yintercept=2.72, linetype='dashed', colour='orange')+
theme_bw()

Baselines.bioregion.depth.summary.total<- Baselines |> 
    filter(Taxa=="Total") |>
    dplyr::select(BIOREGION.agg, DEPTH.f, value) |> 
        group_by(BIOREGION.agg, DEPTH.f) |> 
            ggdist::median_hdci(value)

            
Baselines.bioregion.depth.summary.Acropora<- Baselines |> 
    filter(Taxa=="Acropora") |>
    dplyr::select(BIOREGION.agg, DEPTH.f, value) |> 
        group_by(BIOREGION.agg, DEPTH.f) |> 
            ggdist::median_hdci(value)

