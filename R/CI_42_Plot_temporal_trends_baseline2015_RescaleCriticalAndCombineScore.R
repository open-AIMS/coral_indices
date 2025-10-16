#These are plots for when CC, MA, JU and CO baseline timeframes have been changed to 2015.
#Have changed capping for JU reference to -2,2

#!! New addition is that MA critical has been rescaled so that the thresholds are the midpoint of the index, instead of 0.
#!! Also, All critical indices were adjusted so that scores below 0.5 become 0
#Then a combined metric is calculated as the average of the critical and reference scores


#***********************
# CC Indicator trends Reef
#***********************

spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))
load(file = paste0(DATA_PATH, 'modelled/CC__scores_reef_year.RData'))
CC_scores<-mods$Summary
reef_names_vector <- as.character(levels(mods[[1]]))
names(CC_scores) <- reef_names_vector
CC_scores_df <- bind_rows(CC_scores, .id = "REEF.d") |>
right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f) |> distinct(), by = "REEF.d") |>
mutate(Metric= case_when(Metric=="rescale.dist.metric" ~ "distance.metric",
 Metric=="pcb.rescale.dist.metric"~ "consequence.metric",
 Metric=="combined.metric" ~ "combined.metric"))

#if it doesn't already exist, create a directory inside output/figures called 'indicator_checks'
if(!dir.exists(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled'))){
    dir.create(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled'))
}

#Use a loop to plot For each DEPTH.f within each BIOREGION.agg, plot the median by fYEAR with a linerange of lower and upper
#separate 'Metric' by colour
#facet by REEF
#save the plot to output/figures/indicator_checks
unique_depths <- unique(CC_scores_df$DEPTH.f)
unique_bioregions <- unique(CC_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
    for (depth in unique_depths) {
        plot_data <- CC_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
        if (nrow(plot_data) == 0) next
     
        # Define custom colors for Metric/Source combinations
        metric_source_colors <- c(
            "consequence.metric" = "#c883d7ff",
            "distance.metric" = "#2a95caff",
            "combined.metric" = "#55c667ff"
        )

        plot_CC_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
            geom_point(position = position_dodge(width = 0.5), size =4) +
            geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
            geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
            geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
            facet_wrap(~ REEF, scales = "free_y") +
            theme_classic() +
            theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            labs(x = "Year", y = "CC score", title = paste(bioregion, depth),
         color = "Metric") +
      scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_CC_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_criticalRescaleAndCombinedScore.png'), 
           plot = plot_CC_scores, width = 25, height = 12)
  }
} 


#***********************
# CC Indicator trends NRM
#***********************
load(file = paste0(DATA_PATH, 'modelled/CC__scores_NRM_year.RData'))
CC_scores_NRM <- mods$Summary
NRM_names_vector <- sapply(CC_scores_NRM, function(df) as.character(unique(df$NRM)))
names(CC_scores_NRM) <- NRM_names_vector
CC_scores_NRM_df <- bind_rows(CC_scores_NRM, .id = "NRM") |>
mutate(Metric= case_when(Metric=="rescale.dist.metric" ~ "distance.metric",
 Metric=="pcb.rescale.dist.metric"~ "consequence.metric",
 Metric=="combined.metric" ~ "combined.metric"))

unique_nrm <- unique(CC_scores_NRM_df$NRM)
for (nrm in unique_nrm) {

  plot_data <- CC_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
  if (nrow(plot_data) == 0) next

      metric_source_colors <- c(
      "consequence.metric" = "#c883d7ff",
      "distance.metric" = "#2a95caff",
      "combined.metric" = "#55c667ff"
    )

  plot_CC_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "CC score", title = nrm, color = "Metric") +
    scale_color_manual(values = metric_source_colors)

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_CC_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_criticalRescaleAndCombinedScore.png'),
         plot = plot_CC_scores_NRM, width = 20, height = 5)
}

#***********************
# JU Indicator trends Reef
#***********************

spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))
load(file = paste0(DATA_PATH, 'modelled/JU__scores_reef_year.RData'))
JU_scores<-mods$Summary
JU_scores_df <- bind_rows(JU_scores) |>
  right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f) |> distinct(), by = "REEF.d") #gains 45 rows when I add DEPTH.f..... check!


unique_depths <- unique(JU_scores_df$DEPTH.f)
unique_bioregions <- unique(JU_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
  for (depth in unique_depths) {
    plot_data <- JU_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
    if (nrow(plot_data) == 0) next

    metric_source_colors <- c(
      "Acropora" = "#c883d7ff",
      "Total" = "#2a95caff",
      "Combined" = "#55c667ff"
    )

    plot_JU_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
      geom_point(position = position_dodge(width = 0.5), size = 4) +
      geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
      facet_wrap(~ REEF, scales = "free_y") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Year", y = "JU score", title = paste(bioregion), color = "Metric") +
      scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_JU_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_criticalRescaleAndCombinedScore.png'),
           plot = plot_JU_scores, width = 25, height = 12)
  }
}

#***********************
# JU Indicator trends NRM
#***********************
load(file = paste0(DATA_PATH, 'modelled/JU__scores_NRM_year.RData'))
JU_scores_NRM <- mods$Summary
NRM_names_vector <- sapply(JU_scores_NRM, function(df) as.character(unique(df$NRM)))
names(JU_scores_NRM) <- NRM_names_vector
JU_scores_NRM_df <- bind_rows(JU_scores_NRM, .id = "NRM")

unique_nrm <- unique(JU_scores_NRM_df$NRM)
for (nrm in unique_nrm) {
  plot_data <- JU_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
  if (nrow(plot_data) == 0) next

  metric_source_colors <- c(
      "Acropora" = "#c883d7ff",
      "Total" = "#2a95caff",
      "Combined" = "#55c667ff"
  )

  plot_JU_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "JU score", title = nrm, color = "Metric") +
    scale_color_manual(values = metric_source_colors)

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_JU_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_criticalRescaleAndCombinedScore.png'),
         plot = plot_JU_scores_NRM, width = 20, height = 5)
}

#***********************
# MA Indicator trends Reef
#***********************

load(file = paste0(DATA_PATH, 'modelled/MA__scores_reef_year.RData'))
MA_scores<-mods$Summary
reef_names_vector <- sapply(MA_scores, function(df) as.character(unique(df$REEF.d)))
names(MA_scores) <- reef_names_vector
MA_scores_df<- bind_rows(MA_scores, .id = "REEF.d")

unique_depths <- unique(MA_scores_df$DEPTH.f)
unique_bioregions <- unique(MA_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
  for (depth in unique_depths) {
    plot_data <- MA_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
    if (nrow(plot_data) == 0) next

    metric_source_colors <- c(
      "consequence.metric" = "#c883d7ff",
      "distance.metric" = "#2a95caff",
        "combined.metric" = "#55c667ff"
    )

    plot_MA_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
      geom_point(position = position_dodge(width = 0.5), size = 4) +
      geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
      facet_wrap(~ REEF, scales = "free_y") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Year", y = "MA score", title = paste(bioregion), color = "Metric") +
      scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_MA_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_criticalRescaleAndCombinedScore.png'),
           plot = plot_MA_scores, width = 25, height = 12)
  }
}

#***********************
# MA Indicator trends NRM
#***********************
load(file = paste0(DATA_PATH, 'modelled/MA__scores_NRM_year.RData'))
MA_scores_NRM <- mods$Summary
NRM_names_vector <- sapply(MA_scores_NRM, function(df) as.character(unique(df$NRM)))
names(MA_scores_NRM) <- NRM_names_vector
MA_scores_NRM_df <- bind_rows(MA_scores_NRM, .id = "NRM")

unique_nrm <- unique(MA_scores_NRM_df$NRM)
for (nrm in unique_nrm) {
  plot_data <- MA_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
  if (nrow(plot_data) == 0) next

  metric_source_colors <- c(
    "consequence.metric" = "#c883d7ff",
    "distance.metric" = "#2a95caff",
        "combined.metric" = "#55c667ff"
  )

  plot_MA_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "MA score", title = nrm, color = "Metric") +
    scale_color_manual(values = metric_source_colors)

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_MA_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_criticalRescaleAndCombinedScore.png'),
         plot = plot_MA_scores_NRM, width = 20, height = 5)
}

#***********************
# CO Indicator trends Reef
#***********************
load(file = paste0(DATA_PATH, 'modelled/CO__scores_reef_year.RData'))
CO_scores<-mods$Summary
reef_names_vector <- sapply(CO_scores, function(df) as.character(unique(df$REEF.d)))
names(CO_scores) <- reef_names_vector
CO_scores_df<- bind_rows(CO_scores, .id = "REEF.d") |>
right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f, BIOREGION.agg) |> distinct(), by = "REEF.d") |>
mutate(Metric= case_when(Metric=="Reference" ~ "distance.metric",
 Metric=="Critical"~ "consequence.metric",
 Metric=="Combined" ~ "combined.metric"))

unique_depths <- unique(CO_scores_df$DEPTH.f)
unique_bioregions <- unique(CO_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
  for (depth in unique_depths) {
    plot_data <- CO_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
    if (nrow(plot_data) == 0) next

    metric_source_colors <- c(
      "consequence.metric" = "#c883d7ff",
      "distance.metric" = "#2a95caff",
        "combined.metric" = "#55c667ff"
    )

    plot_CO_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
      geom_point(position = position_dodge(width = 0.5), size = 4) +
      geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
      facet_wrap(~ REEF, scales = "free_y") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Year", y = "CO score", title = paste(bioregion), color = "Metric") +
      scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_CO_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_criticalRescaleAndCombinedScore.png'),
           plot = plot_CO_scores, width = 25, height = 12)
  }
}

#***********************
# CO Indicator trends NRM
#***********************
load(file = paste0(DATA_PATH, 'modelled/CO__scores_NRM_year.RData'))
CO_scores_NRM <- mods$Summary
NRM_names_vector <- sapply(CO_scores_NRM, function(df) as.character(unique(df$NRM)))
names(CO_scores_NRM) <- NRM_names_vector
CO_scores_NRM_df <- bind_rows(CO_scores_NRM, .id = "NRM") |>
mutate(Metric= case_when(Metric=="Reference" ~ "distance.metric",
 Metric=="Critical"~ "consequence.metric",
 Metric=="Combined" ~ "combined.metric"))

unique_nrm <- unique(CO_scores_NRM_df$NRM)
for (nrm in unique_nrm) {
  plot_data <- CO_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
  if (nrow(plot_data) == 0) next

  metric_source_colors <- c(
    "consequence.metric" = "#c883d7ff",
    "distance.metric" = "#2a95caff",
     "combined.metric" = "#55c667ff"
  )

  plot_CO_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "CO score", title = nrm, color = "Metric") +
    scale_color_manual(values = metric_source_colors)

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_CO_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_criticalRescaleAndCombinedScore.png'),
         plot = plot_CO_scores_NRM, width = 20, height = 5)
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

        ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_RPI_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_criticalRescaleAndCombinedScore.png'),
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

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/critical_rescaled/plot_RPI_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_criticalRescaleAndCombinedScore.png'),
                 plot = plot_RPI_scores_NRM, width = 20, height = 5)
}
