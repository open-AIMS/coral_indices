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
if(!dir.exists(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale'))){
    dir.create(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale'))
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

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_CC_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_StandardiseRescale.png'), 
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

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_CC_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_StandardiseRescale.png'),
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

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_JU_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_StandardiseRescale.png'),
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

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_JU_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_StandardiseRescale.png'),
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

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_MA_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_StandardiseRescale.png'),
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

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_MA_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_StandardiseRescale.png'),
         plot = plot_MA_scores_NRM, width = 20, height = 5)
}

#######################################################################################
#Not run yet because this change wasn't applicable to CO and RPI indicators
#########################################################################################

# #***********************
# # CO Indicator trends Reef
# #***********************
# load(file = paste0(DATA_PATH, 'modelled/CO__scores_reef_year.RData'))
# CO_scores<-mods$Summary
# reef_names_vector <- sapply(CO_scores, function(df) as.character(unique(df$REEF.d)))
# names(CO_scores) <- reef_names_vector
# CO_scores_df<- bind_rows(CO_scores, .id = "REEF.d") |>
# right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f, BIOREGION.agg) |> distinct(), by = "REEF.d") |>
# mutate(Metric= case_when(Metric=="Reference" ~ "distance.metric",
#  Metric=="Critical"~ "consequence.metric",
#  Metric=="Combined" ~ "combined.metric"))

# unique_depths <- unique(CO_scores_df$DEPTH.f)
# unique_bioregions <- unique(CO_scores_df$BIOREGION.agg)
# for (bioregion in unique_bioregions) {
#   for (depth in unique_depths) {
#     plot_data <- CO_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
#     if (nrow(plot_data) == 0) next

#     metric_source_colors <- c(
#       "consequence.metric" = "#c883d7ff",
#       "distance.metric" = "#2a95caff",
#         "combined.metric" = "#55c667ff"
#     )

#     plot_CO_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
#       geom_point(position = position_dodge(width = 0.5), size = 4) +
#       geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
#       geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
#       geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
#       facet_wrap(~ REEF, scales = "free_y") +
#       theme_classic() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       labs(x = "Year", y = "CO score", title = paste(bioregion), color = "Metric") +
#       scale_color_manual(values = metric_source_colors)

#     ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_CO_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_StandardiseRescale.png'),
#            plot = plot_CO_scores, width = 25, height = 12)
#   }
# }

# #***********************
# # CO Indicator trends NRM
# #***********************
# load(file = paste0(DATA_PATH, 'modelled/CO__scores_NRM_year.RData'))
# CO_scores_NRM <- mods$Summary
# NRM_names_vector <- sapply(CO_scores_NRM, function(df) as.character(unique(df$NRM)))
# names(CO_scores_NRM) <- NRM_names_vector
# CO_scores_NRM_df <- bind_rows(CO_scores_NRM, .id = "NRM") |>
# mutate(Metric= case_when(Metric=="Reference" ~ "distance.metric",
#  Metric=="Critical"~ "consequence.metric",
#  Metric=="Combined" ~ "combined.metric"))

# unique_nrm <- unique(CO_scores_NRM_df$NRM)
# for (nrm in unique_nrm) {
#   plot_data <- CO_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
#   if (nrow(plot_data) == 0) next

#   metric_source_colors <- c(
#     "consequence.metric" = "#c883d7ff",
#     "distance.metric" = "#2a95caff",
#      "combined.metric" = "#55c667ff"
#   )

#   plot_CO_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
#     geom_point(position = position_dodge(width = 0.5), size = 4) +
#     geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
#     geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
#     geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
#     facet_wrap(~ Shelf, scales = "free_y") +
#     theme_classic() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(x = "Year", y = "CO score", title = nrm, color = "Metric") +
#     scale_color_manual(values = metric_source_colors)

#   ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_CO_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_StandardiseRescale.png'),
#          plot = plot_CO_scores_NRM, width = 20, height = 5)
# }

# #***********************
# # RPI Indicator trends Reef
# #***********************
# spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))
# load(file = paste0(DATA_PATH, 'modelled/RPI__scores_reef_year.RData'))
# RPI_scores<-mods$Summary
# RPI_scores_df <- bind_rows(RPI_scores) |>
#     right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f) |> distinct(), by = "REEF.d") #gains 45 rows when I add DEPTH.f..... check!

# unique_depths <- unique(RPI_scores_df$DEPTH.f)
# unique_bioregions <- unique(RPI_scores_df$BIOREGION.agg)
# for (bioregion in unique_bioregions) {
#     for (depth in unique_depths) {
#         plot_data <- RPI_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
#         if (nrow(plot_data) == 0) next

#         metric_source_colors <- c(
#             "critical" = "#c883d7ff",
#             "reference" = "#2a95caff",
#             "Combined" = "#55c667ff"
#         )

#         plot_RPI_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
#             geom_point(position = position_dodge(width = 0.5), size = 4) +
#             geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
#             geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
#             geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
#             facet_wrap(~ REEF, scales = "free_y") +
#             theme_classic() +
#             theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#             labs(x = "Year", y = "RPI score", title = paste(bioregion), color = "Metric") +
#             scale_color_manual(values = metric_source_colors)

#         ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_RPI_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_StandardiseRescale.png'),
#                      plot = plot_RPI_scores, width = 25, height = 12)
#     }
# }

# #***********************
# # RPI Indicator trends NRM
# #***********************
# load(file = paste0(DATA_PATH, 'modelled/RPI__scores_NRM_year.RData'))
# RPI_scores_NRM <- mods$Summary
# NRM_names_vector <- sapply(RPI_scores_NRM, function(df) as.character(unique(df$NRM)))
# names(RPI_scores_NRM) <- NRM_names_vector
# RPI_scores_NRM_df <- bind_rows(RPI_scores_NRM, .id = "NRM")

# unique_nrm <- unique(RPI_scores_NRM_df$NRM)
# for (nrm in unique_nrm) {
#     plot_data <- RPI_scores_NRM_df |> filter(NRM == nrm) |> droplevels()
#     if (nrow(plot_data) == 0) next

#     metric_source_colors <- c(
#             "critical" = "#c883d7ff",
#             "reference" = "#2a95caff",
#             "Combined" = "#55c667ff"
#     )

#     plot_RPI_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
#         geom_point(position = position_dodge(width = 0.5), size = 4) +
#         geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
#         geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
#         geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
#         facet_wrap(~ Shelf, scales = "free_y") +
#         theme_classic() +
#         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#         labs(x = "Year", y = "RPI score", title = nrm, color = "Metric") +
#         scale_color_manual(values = metric_source_colors)

#     ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/plot_RPI_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_StandardiseRescale.png'),
#                  plot = plot_RPI_scores_NRM, width = 20, height = 5)
# }

#######################################################################################
# Check Case Study
##########################################################################################
load(file = paste0(DATA_PATH, 'modelled/indices.RData'))
spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))

Case_study_data <- indices |> 
  filter(Level == "reef", Year == "2024") |>
  dplyr::rename(REEF = Name) |>
  filter(!REEF %in% c("Rat", "Farmers")) |> droplevels() |>
  mutate(DEPTH.f = factor(Depth)) |>
  left_join(spatial_lookup %>% dplyr::select(REEF, DEPTH.f, NRM) %>% distinct()) |>
  filter(NRM == "Fitzroy", Shelf == "Inshore") |>
  filter(
    (Indicator %in% c("Community.composition", "Recovery.performance") & Reference == "Baseline") |
    (Indicator %in% c("Macroalgae", "Coral.cover", "Juvenile.density") & Reference == "Combined")
  ) |>
  mutate(
    Indicator = factor(
      Indicator,
      levels = c("Coral.cover", "Recovery.performance", "Macroalgae", "Juvenile.density", "Community.composition")
    )
  ) |>
  droplevels()

# Define indicator base colors
indicator_colors <- c(
  "Coral.cover" = "#2a95ca",
  "Recovery.performance" = "#55c667",
  "Macroalgae" = "#c883d7",
  "Juvenile.density" = "#FFC000",
  "Community.composition" = "#FF0000"
)

# Create a palette for each indicator (light/dark for each DEPTH.f)
library(scales)
depth_levels <- levels(Case_study_data$DEPTH.f)
indicator_depth_palette <- lapply(indicator_colors, function(col) {
  scales::seq_gradient_pal(col, "black", "lab")(seq(0.2, 0.5, length.out = length(depth_levels)))
})
names(indicator_depth_palette) <- names(indicator_colors)

# Assign color by Indicator and DEPTH.f
Case_study_data$facet_color <- mapply(function(ind, dep) {
  ind_col <- indicator_depth_palette[[as.character(ind)]]
  dep_idx <- which(depth_levels == dep)
  ind_col[dep_idx]
}, Case_study_data$Indicator, Case_study_data$DEPTH.f)

casestudy.plot <- ggplot(Case_study_data, aes(x = REEF, y = Median, color = facet_color)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  geom_linerange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5)) +
  ylim(0, 1) +
  facet_grid(rows = vars(DEPTH.f), cols = vars(Indicator)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Score", title = "Case Study: Fitzroy NRM Inshore 2024 - Combined Metric") +
  scale_color_identity(guide = "none")

ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/casestudy.plot_baseline2015_StandardiseRescale.png'),
         plot = casestudy.plot, width = 20, height = 5)

Case_study_data_NRM <- indices |> 
  filter(Level == "NRM", Year == "2024") |>
  dplyr::rename(NRM = Name) |>
  mutate(DEPTH.f = factor(Depth)) |>
  filter(NRM == "Fitzroy", Shelf == "Inshore") |>
  filter(
    (Indicator %in% c("Community.composition", "Recovery.performance") & Reference == "Baseline") |
    (Indicator %in% c("Macroalgae", "Coral.cover", "Juvenile.density") & Reference == "Combined")
  ) |>
  mutate(
    Indicator = factor(
      Indicator,
      levels = c("Coral.cover", "Recovery.performance", "Macroalgae", "Juvenile.density", "Community.composition")
    )
  ) |>
  droplevels()

casestudy.plot.NRM <- ggplot(Case_study_data_NRM, aes(x = Indicator, y = Median, color = indicator_colors)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  geom_linerange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5)) +
  ylim(0, 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Score", title = "Case Study: Fitzroy NRM Inshore 2024 - Combined Metric") +
  scale_color_identity(guide = "none")

  ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/casestudy.plot.NRM.png'),
         plot = casestudy.plot.NRM, width = 8, height = 6)

library(patchwork)
casestudy.plot.NRM.panel <- casestudy.plot / (plot_spacer() + (casestudy.plot.NRM) + plot_spacer())

ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/casestudy.plot.NRM.panel.png'),
         plot = plot.NRM.panel, width = 15, height = 8)

#Facet by Reef instead of indicator
casestudy.plot.reef <- ggplot(Case_study_data, aes(x = Indicator, y = Median, color = facet_color)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  geom_linerange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5)) +
  ylim(0, 1) +
  facet_grid(rows = vars(DEPTH.f), cols = vars(REEF)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Score", title = "Case Study: Fitzroy NRM Inshore 2024 - Combined Metric") +
  scale_color_identity(guide = "none")

ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/standardise_rescale/casestudy.plot.reef_baseline2015_StandardiseRescale.png'),
         plot = casestudy.plot.reef, width = 20, height = 5)


#######################################################################################
## Condition scoring
########################################################################################
load(file = paste0(DATA_PATH, 'modelled/indices.RData'))

# framework categories reef
condition <-indices |> 
  filter(Level == "reef") |>
  dplyr::rename(REEF = Name) |>
  mutate(DEPTH.f = factor(Depth)) |>
  left_join(spatial_lookup %>% dplyr::select(Shelf, REEF, DEPTH.f, NRM, BIOREGION.agg) |> distinct()) |>
  filter(
    (Indicator %in% c("Community.composition", "Recovery.performance") & Reference == "Baseline") |
    (Indicator %in% c("Macroalgae", "Coral.cover", "Juvenile.density") & Reference == "Combined")
  ) |>
  group_by(NRM, Shelf, DEPTH.f, REEF, Year) |>
  mutate(mean.score=mean(Median)) |>
  mutate(
    Indicator = factor(
      Indicator,
      levels = c("Coral.cover", "Recovery.performance", "Macroalgae", "Juvenile.density", "Community.composition")
    )
  ) |>
  droplevels() |> 
  mutate(BelowThreshold=ifelse(Upper<0.5, 0, 1)) |>
  pivot_wider(id_cols=c(NRM, Shelf, DEPTH.f, REEF, Year, mean.score),names_from = Indicator, values_from = BelowThreshold) |>   # not here that BelowThreshold=0 
  mutate(
         level2=Juvenile.density+Macroalgae+Community.composition,
         Juv_MA=Juvenile.density+Macroalgae,
         cat_grade=ifelse(Coral.cover==1 & Recovery.performance==1 & level2==3, 'A',
                      ifelse(Coral.cover==1 & Recovery.performance==1 & level2>0, 'B',
                             ifelse(Coral.cover==1 & Recovery.performance==1 & level2==0, 'C',
                                    ifelse(Coral.cover==1 & Recovery.performance==0 & level2==3, 'B',
                                           ifelse(Coral.cover==1 & Recovery.performance==0 & level2>0, 'C',
                                                  ifelse(Coral.cover==1 & Recovery.performance==1 & level2==0, 'D',
                                                         ifelse(Coral.cover==0 & Recovery.performance==1 & level2==3, 'B',
                                                                ifelse(Coral.cover==0 & Recovery.performance==1 & level2>0, 'C',
                                                                       ifelse(Coral.cover==0 & Recovery.performance==1 & level2==0, 'D',
                                                                              ifelse(Coral.cover==0 & Recovery.performance==0 & level2==3, 'D', 'E'))))))))))) |>
    mutate(f.score=case_when(cat_grade == "A" ~ 0.9,
                             cat_grade == "B" ~ 0.7,
                             cat_grade == "C" ~ 0.5,
                             cat_grade == "D" ~ 0.3,
                             cat_grade == "E" ~ 0.1,
                             cat_grade == NA ~ NA)) |>
  mutate(Year=as.numeric(as.character(Year)))


trafficLightPalette <- (c('#FF0000','#FFC000','#FFFF00','#92D050','#00B050'))
lims <- LETTERS[1:5]


# For each REEF.d, Year, check if each variable == 0 and create a label string
# Loop over each Region and generate plots


for (nrm in unique(condition$NRM)) {
  nrm_data <- condition |> filter(NRM == nrm, Year>2015) |> droplevels()
  if (nrow(nrm_data) == 0) next
    for (shelf in unique(nrm_data$Shelf)) {
          shelf_data <- nrm_data |> filter(Shelf == shelf) |> droplevels()
          if (nrow(shelf_data) == 0) next
      for (depth in unique(shelf_data$DEPTH.f)) {
            depth_data <- shelf_data |> filter(DEPTH.f == depth, !is.na(cat_grade)) |> droplevels()
            if (nrow(depth_data) == 0) next

              annotation_data <- depth_data |>
                mutate(
                  label_J = ifelse(!is.na(Juvenile.density) & Juvenile.density == 0, "J", ""),
                  label_M = ifelse(!is.na(Macroalgae) & Macroalgae == 0, "M", ""),
                  label_CC = ifelse(!is.na(Coral.cover) & Coral.cover == 0, "CC", ""),
                  label_CO = ifelse(!is.na(Community.composition) & Community.composition == 0, "CO", ""),
                  label_P = ifelse(!is.na(Recovery.performance) & Recovery.performance == 0, "P", "")
                ) |>
                select(
                  REEF, Year, mean.score, f.score, label_J, label_M, label_CC, label_CO, label_P
                ) |>
                tidyr::pivot_longer(
                  cols = starts_with("label_"),
                  names_to = "label_type",
                  values_to = "label"
                ) |>
                filter(label != "") |>
                mutate(
                  y = case_when(
                    label_type == "label_J" ~ 1.1,
                    label_type == "label_M" ~ 1.2,
                    label_type == "label_CC" ~ 1.3,
                    label_type == "label_CO" ~ 1.4,
                    label_type == "label_P" ~ 1.5,
                    TRUE ~ mean.score
                  )
                )

              plot.condition <- ggplot(depth_data, 
                                          aes(x = Year, y = mean.score)) +
                ylim(0, 1.5) +
                geom_line() +
                geom_point(aes(y = mean.score, fill = cat_grade), shape = 21, size = 3) +
                scale_fill_manual(values = rev(trafficLightPalette), limits = lims) +
                labs(x = "", y = "Mean indicator score", colour = 'Condition') +
                geom_hline(yintercept = 1, colour = 'red', linewidth = 0.5) +
                geom_hline(yintercept = c(0.20, 0.40, 0.60), linetype = "dashed", colour = 'black', linewidth = 0.5) +
                theme_bw(base_size = 10) +
                theme(legend.position = "right") +
                facet_wrap(vars(REEF)) +
                geom_text(
                  data = annotation_data,
                  aes(x = Year, y = y, label = label),
                  colour="red",
                  inherit.aes = FALSE,
                  size = 3
                ) +
                ggtitle(paste(nrm, shelf, "-", depth))

              ggsave(
                plot.condition,
                file = paste0(FIGS_PATH, "/indicator_checks/baseline_2015/standardise_rescale/plot.condition.", gsub(" ", "_", nrm), ".", gsub(" ", "_", shelf), ".", gsub(" ", "_", depth), ".png"),
                width = 12, height = 10
              )

      }
  }
}