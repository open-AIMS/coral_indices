
#if it doesn't already exist, create a directory inside output/figures called 'indicator_checks'
if(!dir.exists(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_k80'))){
    dir.create(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_k80'))
}


#***********************
# RPI Indicator trends Reef
#***********************
spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))
load(file = paste0(DATA_PATH, 'modelled/RPI__scores_reef_year.RData'))
RPI_scores<-mods$Summary
RPI_scores_df <- bind_rows(RPI_scores) |>
    right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f) |> distinct(), by = "REEF.d") #gains 45 rows when I add DEPTH.f..... check!

unique_depths <- unique(RPI_scores_df$DEPTH.f)
unique_bioregions <- unique(RPI_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
    for (depth in unique_depths) {
        plot_data <- RPI_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
        if (nrow(plot_data) == 0) next

        metric_source_colors <- c(
            "critical" = "#c883d7ff",
            "reference" = "#2a95caff",
            "Combined" = "#55c667ff"
        )

        plot_RPI_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
            geom_point(position = position_dodge(width = 0.5), size = 4) +
            geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
            geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
            geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
            facet_wrap(~ REEF, scales = "free_y") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Year", y = "RPI score", title = paste(bioregion), color = "Metric") +
            scale_color_manual(values = metric_source_colors)

        ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_k80/plot_RPI_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_RPI_k80.png'),
                     plot = plot_RPI_scores, width = 25, height = 12)
    }
}

#***********************
# RPI Indicator trends NRM
#***********************
load(file = paste0(DATA_PATH, 'modelled/RPI__scores_NRM_year.RData'))
RPI_scores_NRM <- mods$Summary
NRM_names_vector <- sapply(RPI_scores_NRM, function(df) as.character(unique(df$NRM)))
names(RPI_scores_NRM) <- NRM_names_vector
RPI_scores_NRM_df <- bind_rows(RPI_scores_NRM, .id = "NRM")

unique_nrm <- unique(RPI_scores_NRM_df$NRM)
for (nrm in unique_nrm) {
    plot_data <- RPI_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
    if (nrow(plot_data) == 0) next

    metric_source_colors <- c(
            "critical" = "#c883d7ff",
            "reference" = "#2a95caff",
            "Combined" = "#55c667ff"
    )

    plot_RPI_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
        geom_point(position = position_dodge(width = 0.5), size = 4) +
        geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
        geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
        geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
        facet_wrap(~ Shelf, scales = "free_y") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Year", y = "RPI score", title = nrm, color = "Metric") +
        scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_k80/plot_RPI_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_RPI_k80.png'),
                 plot = plot_RPI_scores_NRM, width = 20, height = 5)
}

