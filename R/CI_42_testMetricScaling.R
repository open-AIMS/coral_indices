Baselines <- get(load(file = paste0(DATA_PATH, #KC - AT changed
                                            'modelled/JU__baseline_posteriors.RData')))
mods <- get(load(file = paste0(DATA_PATH, "modelled/JU__preds.RData")))
load(file=paste0(DATA_PATH, 'processed/site.location.RData'))
spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))

        # add in Acropora juvenile limits based on Manu's modelling
        load(file=paste0(DATA_PATH, 'parameters/IPM_juv.RData'))
        
        .draw=tibble(.draw=seq(from=1, to=1000, by=1))
        # updated code to include the IPM_juv estimate - noting this is a little different to the value reported in the indicators tech report.
        Acr.baseline<- site.location |> 
          dplyr::select(DEPTH.f, BIOREGION.agg, Shelf) |>  
          unique() |> 
          left_join(IPM_juv |> rename(value=mean)) |> 
          mutate(Taxa='Acropora') |> 
          cross_join(.draw) |> 
          dplyr::select(-Shelf)
        # remove density of Acropora estimated from INLA models of observed values and replace with that derived from IPM model
        baselines<- Baselines |> 
          filter(Taxa=='Total') |> 
          rbind(Acr.baseline)

JUV_preds <- mods$Pred
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

    # Distance calculation
    JUV_distance <- JUV_preds_df |>
        left_join(baselines %>%
                      droplevels() |>
                      dplyr::rename(baseline = value),
                  by = c("BIOREGION.agg", "DEPTH.f", "Taxa", ".draw")) %>%
        mutate(distance.met = log2(baseline / value))

library(gridExtra)
cap_ranges <- list(
    list(min = -3, max = 3),
    list(min = -2, max = 2),
    list(min = -2.5, max = 2.5)
)
taxa_levels <- unique(baselines$Taxa)

for (taxon in taxa_levels) {
    for (cap in cap_ranges) {
        plot_list <- list()
        # Baseline summary
        Bioregion_baseline_summary <- baselines |>
            filter(Taxa == taxon) |> droplevels() |>
            group_by(BIOREGION.agg, DEPTH.f) |>
            median_hdci(value, .width = c(0.95))

        plot_JUV_Bioregion_depth_baselines <- ggplot(Bioregion_baseline_summary, aes(x = BIOREGION.agg, y = value, colour = DEPTH.f)) +
            geom_point(size = 4, position = position_dodge(width = 0.5)) +
            geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
            labs(title = paste("Baselines -", taxon),
                 x = "Bioregion",
                 y = "Abundance") +
            theme_bw()+
            theme(legend.position = "none")

        # Prediction summary
        Bioregion_preds_summary <- JUV_preds_df |>
            filter(Taxa == taxon) |> droplevels() |>
            group_by(BIOREGION.agg, DEPTH.f, Taxa) |>
            median_hdci(value, .width = c(0.95))

        plot_JUV_Bioregion_depth_preds <- ggplot(Bioregion_preds_summary, aes(x = BIOREGION.agg, y = value, colour = DEPTH.f)) +
            geom_point(size = 4, position = position_dodge(width = 0.5)) +
            geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
            labs(title = paste("Preds - ", taxon),
                 x = "Bioregion",
                 y = "Abundance") +
            theme_bw()+
            theme(legend.position = "none")

    # if (taxon=="Total") {
    #     Bioregion_distance_summary <- JUV_distance |>
    #         filter(Taxa == taxon) |> droplevels() |>
    #         group_by(BIOREGION.agg, DEPTH.f) |>
    #         median_hdci(distance.met, .width = c(0.95))

    #     plot_JUV_Bioregion_depth_distance <- ggplot(Bioregion_distance_summary, aes(x = BIOREGION.agg, y = distance.met, colour = DEPTH.f)) +
    #         geom_point(size = 4, position = position_dodge(width = 0.5)) +
    #         geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
    #         labs(title = paste("Distance -", taxon),
    #              x = "Bioregion",
    #              y = "Distance") +
    #         theme_bw()+
    #         theme(legend.position = "none")

    #     # Capped distance for current cap range
    #     JUV_scaling_cap <- JUV_distance |>
    #         filter(Taxa == taxon) |> droplevels() |>
    #         mutate(cap.dist.met = as.numeric(case_when(
    #             distance.met < cap$min ~ cap$min,
    #             distance.met > cap$max ~ cap$max,
    #             distance.met >= cap$min & distance.met <= cap$max ~ distance.met
    #         )))

    #     Bioregion_CapDistance_summary <- JUV_scaling_cap |>
    #         group_by(BIOREGION.agg, DEPTH.f) |>
    #         dplyr::select(-distance.met) |>
    #         median_hdci(cap.dist.met, .width = c(0.95))

    #     plot_JUV_Bioregion_depth_CapDistance <- ggplot(Bioregion_CapDistance_summary, aes(x = BIOREGION.agg, y = cap.dist.met, colour = DEPTH.f)) +
    #         geom_point(size = 4, position = position_dodge(width = 0.5)) +
    #         geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
    #         labs(title = paste0("Capped Distance [", cap$min, ", ", cap$max, "] - ", taxon),
    #              x = "Bioregion",
    #              y = "Capped Distance") +
    #         theme_bw()+
    #         theme(legend.position = "none")

    #     # Scaled scores for current cap range
    #     JUV_scores <- JUV_scaling_cap |>
    #         mutate(rescale.dist.metric = scales::rescale(cap.dist.met, from = c(cap$min, cap$max), to = c(1, 0)))

    #     Bioregion_score_summary <- JUV_scores |>
    #         group_by(BIOREGION.agg, DEPTH.f) |>
    #         dplyr::select(-cap.dist.met) |>
    #         median_hdci(rescale.dist.metric, .width = c(0.95))

    #     plot_JUV_Bioregion_depth_Score <- ggplot(Bioregion_score_summary, aes(x = BIOREGION.agg, y = rescale.dist.metric, colour = DEPTH.f)) +
    #         geom_point(size = 4, position = position_dodge(width = 0.5)) +
    #         geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
    #         ylim(0, 1) +
    #         labs(title = paste0("Scores - ", taxon),
    #              x = "Bioregion",
    #              y = "Scores") +
    #         theme_bw()+
    #         theme(legend.position = "none")

    #     REEF.d_year_score_summary <- JUV_scores |>
    #         ungroup() |>
    #         group_by(REEF.d, fYEAR) |>
    #         dplyr::select(-cap.dist.met) |>
    #         median_hdci(rescale.dist.metric, .width = c(0.95))

    #     plot_JUV_score_histogram<- ggplot(REEF.d_year_score_summary, aes(x = rescale.dist.metric)) +
    #         geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
    #         labs(title = paste0("Scores Reef x Depth x Year level ", taxon),
    #              x = "Score",
    #              y = "Count") +
    #         theme_bw()+
    #         theme(legend.position = "none")

    # } else {
         Bioregion_distance_summary <- JUV_distance |>
            filter(Taxa == taxon) |> droplevels() |>
            group_by(Shelf, BIOREGION.agg, DEPTH.f) |>
            median_hdci(distance.met, .width = c(0.95))

        plot_JUV_Bioregion_depth_distance <- ggplot(Bioregion_distance_summary, aes(x = BIOREGION.agg, y = distance.met, colour = DEPTH.f)) +
            geom_point(size = 4, position = position_dodge(width = 0.5)) +
            geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
            labs(title = paste("Distance -", taxon),
                 x = "Bioregion",
                 y = "Distance") +
                 facet_wrap(~Shelf, ncol=2)+
            theme_bw()+
            theme(legend.position = "none")

        # Capped distance for current cap range
        JUV_scaling_cap <- JUV_distance |>
            filter(Taxa == taxon) |> droplevels() |>
            mutate(cap.dist.met = as.numeric(case_when(
                distance.met < cap$min ~ cap$min,
                distance.met > cap$max ~ cap$max,
                distance.met >= cap$min & distance.met <= cap$max ~ distance.met
            )))

        Bioregion_CapDistance_summary <- JUV_scaling_cap |>
            group_by(Shelf, BIOREGION.agg, DEPTH.f) |>
            dplyr::select(-distance.met) |>
            median_hdci(cap.dist.met, .width = c(0.95))

        plot_JUV_Bioregion_depth_CapDistance <- ggplot(Bioregion_CapDistance_summary, aes(x = BIOREGION.agg, y = cap.dist.met, colour = DEPTH.f)) +
            geom_point(size = 4, position = position_dodge(width = 0.5)) +
            geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
            labs(title = paste0("Capped Distance [", cap$min, ", ", cap$max, "] - ", taxon),
                 x = "Bioregion",
                 y = "Capped Distance") +
                 facet_wrap(~Shelf, ncol=2)+
            theme_bw()+
            theme(legend.position = "none")

        # Scaled scores for current cap range
        JUV_scores <- JUV_scaling_cap |>
            mutate(rescale.dist.metric = scales::rescale(cap.dist.met, from = c(cap$min, cap$max), to = c(1, 0)))

        Bioregion_score_summary <- JUV_scores |>
            group_by(Shelf, BIOREGION.agg, DEPTH.f) |>
            dplyr::select(-cap.dist.met) |>
            median_hdci(rescale.dist.metric, .width = c(0.95))

        plot_JUV_Bioregion_depth_Score <- ggplot(Bioregion_score_summary, aes(x = BIOREGION.agg, y = rescale.dist.metric, colour = DEPTH.f)) +
            geom_point(size = 4, position = position_dodge(width = 0.5)) +
            geom_linerange(aes(ymin = .lower, ymax = .upper), width = 1, position = position_dodge(width = 0.5)) +
            ylim(0, 1) +
            labs(title = paste0("Scores - ", taxon),
                 x = "Bioregion",
                 y = "Scores") +
                 facet_wrap(~Shelf, ncol=2)+
            theme_bw()+
            theme(legend.position = "none")

        REEF.d_year_score_summary <- JUV_scores |>
            ungroup() |>
            group_by(Shelf, REEF.d, fYEAR) |>
            dplyr::select(-cap.dist.met) |>
            median_hdci(rescale.dist.metric, .width = c(0.95))

        plot_JUV_score_histogram<- ggplot(REEF.d_year_score_summary, aes(x = rescale.dist.metric)) +
            geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
            labs(title = paste0("Scores Reef x Depth x Year level ", taxon),
                 x = "Score",
                 y = "Count") +
                 facet_wrap(~Shelf, ncol=2)+
            theme_bw()+
            theme(legend.position = "none")

    #}

        # Arrange and save plots
        plot_JUV_scale_test <- grid.arrange(
            plot_JUV_Bioregion_depth_baselines,
            plot_JUV_Bioregion_depth_preds,
            plot_JUV_Bioregion_depth_distance,
            plot_JUV_Bioregion_depth_CapDistance,
            plot_JUV_Bioregion_depth_Score,
            plot_JUV_score_histogram,
            nrow = 1
        )

        plot_list[[taxon]] <- plot_JUV_scale_test

# if (taxon=="Total") {
#         ggsave(
#             plot = plot_JUV_scale_test,
#             filename = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_", taxon, "_cap_", cap$min, "_", cap$max, ".png"),
#             width = 20, height = 4
#         )

#     } else {

         ggsave(
            plot = plot_JUV_scale_test,
            filename = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_", taxon, "_cap_", cap$min, "_", cap$max, ".png"),
            width = 30, height = 4
        )

    #} 

        save(
            plot_JUV_scale_test,
            file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_", taxon, "_cap_", cap$min, "_", cap$max, ".RData")
        )
    }
}

#JUV total rescaling
load(file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_Total_cap_-2_2.RData"))
option1=plot_JUV_scale_test
load(file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_Total_cap_-2.5_2.5.RData"))
option2=plot_JUV_scale_test
load(file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_Total_cap_-3_3.RData"))
option3=plot_JUV_scale_test
plot_JUV_total_rescale_options<-grid.arrange(option3, option2, option1, nrow=3)

ggsave(plot=plot_JUV_total_rescale_options, filename=paste0(FIGS_PATH, "/indicator_checks/plot_JUV_total_rescale_options.png"), width=25, height=8)

#JUV Acropora rescaling
load(file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_Acropora_cap_-2_2.RData"))
option1=plot_JUV_scale_test
load(file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_Acropora_cap_-2.5_2.5.RData"))
option2=plot_JUV_scale_test
load(file = paste0(FIGS_PATH, "/indicator_checks/plot_JUV_scale_test_Acropora_cap_-3_3.RData"))
option3=plot_JUV_scale_test
plot_JUV_Acropora_rescale_options<-grid.arrange(option3, option2, option1, nrow=3)

ggsave(plot=plot_JUV_Acropora_rescale_options, filename=paste0(FIGS_PATH, "/indicator_checks/plot_JUV_Acropora_rescale_options.png"), width=25, height=8)
