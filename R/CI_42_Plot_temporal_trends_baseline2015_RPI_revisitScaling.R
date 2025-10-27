if(!dir.exists(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling'))){
    dir.create(paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling'))
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

        ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling/plot_RPI_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '_baseline2015_revisitScaling.png'),
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

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling/plot_RPI_scores_NRM_', gsub(" ", "_", nrm), '_baseline2015_revisitScaling.png'),
                 plot = plot_RPI_scores_NRM, width = 20, height = 5)
}

#######################################################################################
# Check Case Study
##########################################################################################
load(file = paste0(DATA_PATH, 'modelled/indices.RData'))
spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))

Case_study_data_1 <- indices |> 
  filter(Level == "reef", Year == "2024") |>
  dplyr::rename(REEF = Name) |>
  filter(!REEF %in% c("Rat", "Farmers")) |> droplevels() |>
  mutate(DEPTH.f = factor(Depth)) |>
  left_join(spatial_lookup %>% dplyr::select(REEF, DEPTH.f, NRM) %>% distinct()) 
  
for (nrm in unique(Case_study_data_1$NRM)) {
  for (shelf in unique(Case_study_data_1$Shelf)) {
    Case_study_data <- Case_study_data_1 |>
      filter(NRM == nrm, Shelf == shelf) |>
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
      labs(x = "", y = "Score", title = paste("Case Study:", nrm, shelf, "2024 - Combined Metric")) +
      scale_color_identity(guide = "none")

    ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling/casestudy.plot_', gsub(" ", "_", nrm), '_', gsub(" ", "_", shelf), '_baseline2015_revisitScaling.png'),
           plot = casestudy.plot, width = 20, height = 5)

    Case_study_data_NRM <- indices |> 
      filter(Level == "NRM", Year == "2024") |>
      dplyr::rename(NRM = Name) |>
      mutate(DEPTH.f = factor(Depth)) |>
      filter(NRM == nrm, Shelf == shelf) |>
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
      labs(x = "", y = "Score", title = paste("Case Study:", nrm, shelf, "2024 - Combined Metric")) +
      scale_color_identity(guide = "none")

    ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling/casestudy.plot.NRM_', gsub(" ", "_", nrm), '_', gsub(" ", "_", shelf), '_revisitScaling.png'),
           plot = casestudy.plot.NRM, width = 8, height = 6)

    # Facet by Reef instead of indicator
    casestudy.plot.reef <- ggplot(Case_study_data, aes(x = Indicator, y = Median, color = facet_color)) +
      geom_point(size = 4, position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5)) +
      ylim(0, 1) +
      facet_grid(rows = vars(DEPTH.f), cols = vars(REEF)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "Score", title = paste("Case Study:", nrm, shelf, "2024 - Combined Metric")) +
      scale_color_identity(guide = "none")

    ggsave(file = paste0(FIGS_PATH, '/indicator_checks/baseline_2015/RPI_revisitScaling/casestudy.plot.reef_', gsub(" ", "_", nrm), '_', gsub(" ", "_", shelf), '_baseline2015_revisitScaling.png'),
           plot = casestudy.plot.reef, width = 20, height = 5)
  }
}


#######################################################################################
## Condition scoring
########################################################################################
load(file = paste0(DATA_PATH, 'modelled/indices.RData'))
spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))

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
                file = paste0(FIGS_PATH, "/indicator_checks/baseline_2015/RPI_revisitScaling/plot.condition.", gsub(" ", "_", nrm), ".", gsub(" ", "_", shelf), ".", gsub(" ", "_", depth), "_revisitScaling.png"),
                width = 12, height = 10
              )

      }
  }
}