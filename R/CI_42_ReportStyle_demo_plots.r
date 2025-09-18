########################### KC indicator checks #########################################
# #Indicator data from DMS for comparisons
# library(arrow)
# #Get indicator scores from DMS
# # # Providing S3 URL address for dataset of interest
# indicators<-"s3://gbr-dms-data-public/aims-reef-indicators-framework/data.parquet"
# # # Connecting to S3 bucket
# s3_conn <- s3_bucket(indicators)
# # # Accessing dataset
# indicators_all <- open_dataset(indicators) |>
#  collect()

# save(indicators_all, file = paste0(DATA_PATH, "external/indicators_all.RData"))

load(file = paste0(DATA_PATH, "external/indicators_all.RData"))
#glimpse(indicators_all)

#What is the earliest Year in the dataset?
#min(indicators_all$Year)
##################################
# MA 
##################################

#***********************
# MA baselines
#***********************
load(file = paste0(DATA_PATH, 'modelled/MA__baseline_posteriors.RData'))
#glimpse(baselines)

MA_baseline_summaries<- baselines |> 
  group_by(BIOREGION.agg, REEF.d, Model) |> 
  summarise(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = median(value),
    .groups = "drop"
  ) |> 
  arrange(BIOREGION.agg, REEF.d, Model)

#if it doesn't already exist, create a directory inside output/figures called 'indicator_checks'
if(!dir.exists(paste0(FIGS_PATH, '/indicator_checks'))){
  dir.create(paste0(FIGS_PATH, '/indicator_checks'))
}

# Loop over each level of 'Model' and generate a plot for each
models <- unique(MA_baseline_summaries$Model)
for (mod in models) {
  plot_data <- MA_baseline_summaries |> filter(Model == mod) |> droplevels()
  plot_MA_baselines <- ggplot(plot_data, aes(x = REEF.d, y = median)) +
    geom_point(size = 4) +
    geom_linerange(
      aes(ymin = lower, ymax = upper),
      alpha = 0.2
    ) +
    facet_wrap(~ BIOREGION.agg, scales = "free_x") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    ggtitle(paste("MA Baselines -", mod))
  
  ggsave(
    filename = paste0(FIGS_PATH, '/indicator_checks/plot_MA_baselines_', mod, '.png'),
    plot = plot_MA_baselines, width = 20, height = 15
  )
}

#***********************
# MA Indicator trends Reef
#***********************
DMS_MA<-indicators_all |> filter(Indicator=="Macroalgae", Aggregation=="reef") |>
mutate(REEF.d = gsub("\\(|\\)", "", Name)) |>
  dplyr::select(REEF.d, Shelf, Year, Indicator, Reference, Median, Lower, Upper) |>
  rename('fYEAR'='Year', 'DMS_median'='Median', 'DMS_lower'='Lower', 'DMS_upper'='Upper') |>
  mutate(Metric= ifelse(Reference=="Baseline", "distance.metric", "consequence.metric"),
  fYEAR=as.factor(fYEAR))

load(file = paste0(DATA_PATH, 'modelled/MA__scores_reef_year.RData'))
MA_scores<-mods$Summary
reef_names_vector <- sapply(MA_scores, function(df) as.character(unique(df$REEF.d)))
names(MA_scores) <- reef_names_vector
MA_scores_df<- bind_rows(MA_scores, .id = "REEF.d")

#Join MA_scores_df with DMS_MA.
#Join on REEF.d and fYEAR and Metric
#From DMS_MA keep DMS_median, DMS_lower, DMS_upper and Shelf
MA_DMS_scores_reef <- MA_scores_df |>
  left_join(DMS_MA |> dplyr::select(REEF.d, fYEAR, Metric, DMS_median, DMS_lower, DMS_upper, Shelf) |> distinct(), 
            by = c("REEF.d", "fYEAR", "Metric")) |>
  tidyr::pivot_longer(
    cols = c(median, lower, upper, DMS_median, DMS_lower, DMS_upper),
    names_to = "name"
  ) |>
  mutate(
    source = ifelse(grepl("^DMS_", name), "DMS", "new run"),
    stat = gsub("DMS_", "", name)
  ) |>
  select(REEF.d, fYEAR, Metric, Shelf, source, stat, value) |>
  tidyr::pivot_wider(names_from = stat, values_from = value)

#Use a loop to plot For each DEPTH.f within each BIOREGION.agg, plot the median by fYEAR with a linerange of lower and upper
#separate 'Metric' by colour
#facet by REEF
#save the plot to output/figures/indicator_checks
unique_depths <- unique(MA_scores_df$DEPTH.f)
unique_bioregions <- unique(MA_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
  for (depth in unique_depths) {
    plot_data <- MA_DMS_scores_reef |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
    if (nrow(plot_data) == 0) next

    # Define custom colors for Metric/Source combinations
    metric_source_colors <- c(
      "consequence.metric.DMS" = "#821b9eff",
      "consequence.metric.new run" = "#c883d7ff",
      "distance.metric.DMS" = "#0202d9ff",
      "distance.metric.new run" = "#2a95caff"
    )

    plot_MA_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = interaction(Metric, source))) +
      geom_point(position = position_dodge(width = 0.5), size =4) +
      geom_line(aes(group = interaction(Metric, source)), position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
      facet_wrap(~ REEF, scales = "free_y") +
      theme_classic() +
      theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(x = "Year", y = "MA score", title = paste(bioregion, "-", depth),
         color = "Metric/Source") +
      scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_MA_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '.png'), 
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

# Filter DMS data for NRM aggregation and join
DMS_MA_NRM <- indicators_all |> filter(Indicator == "Macroalgae", Aggregation == "NRM") |>
  dplyr::select(Name, Shelf, Year, Indicator, Reference, Median, Lower, Upper) |>
  rename('fYEAR' = 'Year', 'NRM'='Name', 'DMS_median' = 'Median', 'DMS_lower' = 'Lower', 'DMS_upper' = 'Upper') |>
  mutate(Metric = ifelse(Reference == "Baseline", "distance.metric", "consequence.metric"),
         fYEAR = as.factor(fYEAR))

MA_DMS_scores_NRM <- MA_scores_NRM_df |>
  left_join(DMS_MA_NRM |> dplyr::select(NRM, Shelf, fYEAR, Metric, DMS_median, DMS_lower, DMS_upper) |> distinct(),
            by = c("NRM", "Shelf", "fYEAR", "Metric")) |>
  tidyr::pivot_longer(
    cols = c(median, lower, upper, DMS_median, DMS_lower, DMS_upper),
    names_to = "name"
  ) |>
  mutate(
    source = ifelse(grepl("^DMS_", name), "DMS", "new run"),
    stat = gsub("DMS_", "", name)
  ) |>
  select(NRM, Shelf, fYEAR, Metric, source, stat, value) |>
  tidyr::pivot_wider(names_from = stat, values_from = value)

metric_source_colors <- c(
  "consequence.metric.DMS" = "#821b9eff",
  "consequence.metric.new run" = "#c883d7ff",
  "distance.metric.DMS" = "#0202d9ff",
  "distance.metric.new run" = "#2a95caff"
)

unique_nrm <- unique(MA_DMS_scores_NRM$NRM)
for (nrm in unique_nrm) {
  plot_data <- MA_DMS_scores_NRM |> filter(NRM == nrm) |> droplevels()
  if (nrow(plot_data) == 0) next
  plot_MA_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = interaction(Metric, source))) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = interaction(Metric, source)), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "MA score", title = nrm, color = "Metric/Source") +
    scale_color_manual(values = metric_source_colors)
  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_MA_scores_NRM_', gsub(" ", "_", nrm), '.png'),
         plot = plot_MA_scores_NRM, width = 20, height = 5)
}

##################################
# CC
##################################

#***********************
# CC baselines
#***********************
load(file = paste0(DATA_PATH, 'modelled/CC__baseline_posteriors.RData'))
#glimpse(baselines)

CC_baseline_summaries<- baselines |> 
  group_by(BIOREGION.agg, DEPTH.f) |> 
  summarise(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = median(value),
    .groups = "drop"
  ) |> 
  arrange(BIOREGION.agg, DEPTH.f)

#plot the median and 95% credible intervals of the posterior distributions (named 'value') for each reef/model combination and facet by BIOREGION
plot_CC_baselines<-ggplot(CC_baseline_summaries,
aes(x = BIOREGION.agg, y = median*100)) +
  geom_point(size = 4) +
  geom_linerange(
    aes(ymin = lower*100, ymax = upper*100),
    alpha = 0.2
  ) +
  ylab("Hard coral cover (%)") +
  geom_hline(yintercept = 20, color = "red", linetype = "dashed") +
  facet_wrap(~ DEPTH.f, scales = "free_x", nrow=2) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_CC_baselines.png'), 
        plot = plot_CC_baselines, width = 10, height = 10)

#***********************
# CC Indicator trends Reef
#***********************
DMS_CC<-indicators_all |> filter(Indicator=="Coral.cover", Aggregation=="reef") |>
mutate(REEF.d = gsub("\\(|\\)", "", Name)) |>
  dplyr::select(REEF.d, Shelf, Year, Indicator, Reference, Median, Lower, Upper) |>
  rename('fYEAR'='Year', 'DMS_median'='Median', 'DMS_lower'='Lower', 'DMS_upper'='Upper') |>
  mutate(Metric= ifelse(Reference=="Baseline", "distance.metric", "consequence.metric"),
  fYEAR=as.factor(fYEAR))

spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))
load(file = paste0(DATA_PATH, 'modelled/CC__scores_reef_year.RData'))
CC_scores<-mods$Summary
reef_names_vector <- as.character(levels(mods[[1]]))
names(CC_scores) <- reef_names_vector
CC_scores_df <- bind_rows(CC_scores, .id = "REEF.d") |>
right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f) |> distinct(), by = "REEF.d") |>
mutate(Metric= ifelse(Metric=="rescale.dist.metric", "distance.metric", "consequence.metric"))

#Join MA_scores_df with DMS_MA.
#Join on REEF.d and fYEAR and Metric
#From DMS_MA keep DMS_median, DMS_lower, DMS_upper and Shelf
CC_DMS_scores_reef <- CC_scores_df |>
  left_join(DMS_CC |> dplyr::select(REEF.d, fYEAR, Metric, DMS_median, DMS_lower, DMS_upper) |> distinct(), 
            by = c("REEF.d", "fYEAR", "Metric")) |>
  tidyr::pivot_longer(
    cols = c(median, lower, upper, DMS_median, DMS_lower, DMS_upper),
    names_to = "name"
  ) |>
  mutate(
    source = ifelse(grepl("^DMS_", name), "DMS", "new run"),
    stat = gsub("DMS_", "", name)
  ) |>
  select(REEF.d, DEPTH.f, fYEAR, Metric, source, stat, value) |>
  tidyr::pivot_wider(names_from = stat, values_from = value)


#Use a loop to plot For each DEPTH.f within each BIOREGION.agg, plot the median by fYEAR with a linerange of lower and upper
#separate 'Metric' by colour
#facet by REEF
#save the plot to output/figures/indicator_checks
unique_depths <- unique(CC_scores_df$DEPTH.f)
unique_bioregions <- unique(CC_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
  for (depth in unique_depths) {
    plot_data <- CC_DMS_scores_reef |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
    if (nrow(plot_data) == 0) next
   
    # Define custom colors for Metric/Source combinations
    metric_source_colors <- c(
      "consequence.metric.DMS" = "#821b9eff",
      "consequence.metric.new run" = "#c883d7ff",
      "distance.metric.DMS" = "#0202d9ff",
      "distance.metric.new run" = "#2a95caff"
    )

    plot_CC_scores <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = interaction(Metric, source))) +
      geom_point(position = position_dodge(width = 0.5), size =4) +
      geom_line(aes(group = interaction(Metric, source)), position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
      facet_wrap(~ REEF, scales = "free_y") +
      theme_classic() +
      theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(x = "Year", y = "CC score", title = paste(bioregion, "-", depth),
         color = "Metric/Source") +
      scale_color_manual(values = metric_source_colors)

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_CC_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '.png'), 
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
mutate(Metric= ifelse(Metric=="rescale.dist.metric", "distance.metric", "consequence.metric"))

# Filter DMS data for NRM aggregation and join
DMS_CC_NRM <- indicators_all |> filter(Indicator == "Coral.cover", Aggregation == "NRM") |>
  dplyr::select(Name, Shelf, Year, Indicator, Reference, Median, Lower, Upper) |>
  rename('fYEAR' = 'Year', 'NRM'='Name', 'DMS_median' = 'Median', 'DMS_lower' = 'Lower', 'DMS_upper' = 'Upper') |>
  mutate(Metric = ifelse(Reference == "Baseline", "distance.metric", "consequence.metric"),
         fYEAR = as.factor(fYEAR))

CC_DMS_scores_NRM <- CC_scores_NRM_df |>
  left_join(DMS_CC_NRM |> dplyr::select(NRM, Shelf, fYEAR, Metric, DMS_median, DMS_lower, DMS_upper) |> distinct(),
            by = c("NRM", "Shelf", "fYEAR", "Metric")) |>
  tidyr::pivot_longer(
    cols = c(median, lower, upper, DMS_median, DMS_lower, DMS_upper),
    names_to = "name"
  ) |>
  mutate(
    source = ifelse(grepl("^DMS_", name), "DMS", "new run"),
    stat = gsub("DMS_", "", name)
  ) |>
  select(NRM, Shelf, fYEAR, Metric, source, stat, value) |>
  tidyr::pivot_wider(names_from = stat, values_from = value)

unique_nrm <- unique(CC_DMS_scores_NRM$NRM)
for (nrm in unique_nrm) {

  plot_data <- CC_DMS_scores_NRM |> filter(NRM == nrm) |> droplevels()
  if (nrow(plot_data) == 0) next

  plot_CC_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = interaction(Metric, source))) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = interaction(Metric, source)), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "CC score", title = nrm, color = "Metric/Source") +
    scale_color_manual(values = metric_source_colors)

  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_CC_scores_NRM_', gsub(" ", "_", nrm), '.png'),
         plot = plot_CC_scores_NRM, width = 20, height = 5)
}

##################################
# JU
##################################

#***********************
# JU baselines
#***********************
load(file = paste0(DATA_PATH, 'modelled/JU__baseline_posteriors.RData'))
#glimpse(baselines)
JU_baseline_summaries<- baselines |> 
  group_by(BIOREGION.agg, DEPTH.f, Taxa) |> 
  summarise(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    median = median(value),
    .groups = "drop"
  ) |> 
  arrange(BIOREGION.agg, DEPTH.f, Taxa)

#plot the median and 95% credible intervals of the posterior distributions (named 'value') for each reef/model combination and facet by BIOREGION
plot_JU_baselines<-ggplot(JU_baseline_summaries, 
aes(x = BIOREGION.agg, y = median, colour=Taxa)) +
  geom_point(size = 4) +
  geom_linerange(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  facet_wrap(~ DEPTH.f, scales = "free_x", nrow=2) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(y = "Juvenile abundance (?)")

ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_JU_baselines.png'), 
        plot = plot_JU_baselines, width = 10, height = 10)

#***********************
# JU Indicator trends Reef
#***********************
spatial_lookup <- get(load(paste0(DATA_PATH, "processed/spatial_lookup.RData")))
load(file = paste0(DATA_PATH, 'modelled/JU__scores_reef_year.RData'))
JU_scores<-mods$Summary
JU_scores_df <- bind_rows(JU_scores) |>
  right_join(spatial_lookup |> dplyr::select(REEF.d, DEPTH.f) |> distinct(), by = "REEF.d") #gains 45 rows when I add DEPTH.f..... check!

#Plot using same method used for CC
#Use a loop to plot For each DEPTH.f within each BIOREGION.agg, plot the median by fYEAR with a linerange of lower and upper
#separate 'Metric' by colour
#facet by REEF
#save the plot to output/figures/indicator_checks
unique_depths <- unique(JU_scores_df$DEPTH.f)
unique_bioregions <- unique(JU_scores_df$BIOREGION.agg)
for (bioregion in unique_bioregions) {
  for (depth in unique_depths) {

# Define custom colors for Metric/Source combinations
    metric_source_colors <- c(
      "Acropora" = "#c883d7ff",
      "Total" = "#2a95caff"
    )

    plot_data <- JU_scores_df |> filter(BIOREGION.agg == bioregion, DEPTH.f == depth)
    if (nrow(plot_data) == 0) next
    plot_JU_scores <- plot_data |>
      ggplot(aes(x = as.numeric(as.character(fYEAR)), y = median, colour=Metric)) +
      scale_color_manual(values = metric_source_colors)+
      geom_point(size=4) +
      geom_line() +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      facet_wrap(~ REEF) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(x = "Year", y = "JU score", title = paste(bioregion, "-", depth))

    ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_JU_scores_', gsub(" ", "_", bioregion), '_', gsub(" ", "_", depth), '.png'), 
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

  # Define custom colors for Metric/Source combinations
    metric_source_colors <- c(
      "Acropora" = "#c883d7ff",
      "Total" = "#2a95caff"
    )

  plot_JU_scores_NRM <- ggplot(plot_data, aes(x = as.numeric(as.character(fYEAR)), y = median, color = Metric)) +
  scale_color_manual(values = metric_source_colors)+
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_line(aes(group = Metric), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5)) +
    facet_wrap(~ Shelf, scales = "free_y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "JU score", title = nrm, color = "Metric")
  ggsave(filename = paste0(FIGS_PATH, '/indicator_checks/plot_JU_scores_NRM_', gsub(" ", "_", nrm), '.png'),
         plot = plot_JU_scores_NRM, width = 20, height = 5)
}

##################################
# CO
##################################

#***********************
# CO baselines
#***********************


#***********************
# CO Indicator trends Reef
#***********************



#***********************
# CO Indicator trends NRM
#***********************

##################################
# RPI
##################################

#***********************
# RPI baselines
#***********************


#***********************
# RPI Indicator trends Reef
#***********************



#***********************
# RPI Indicator trends NRM
#***********************


####################################################
# Framework grades and indicators on target by reef
####################################################
# Original approach

# Mean of reference and contextual metrics for MA, CC, JU

####################################################
# Framework grades and indicators on target by NRM
####################################################
# Original approach

# Mean of reference and contextual metrics for MA, CC, JU


####################################################
# Standardise baseline timeframes
#####################################################



library(tidybayes)

## NRM region framework scores
load(file="../../CaseStudy/data/final/Framework_index_scores_NRM.RData")
Framework_grades_NRM <- Framework_index_scores_NRM |>
    mutate(f.score=case_when(cat_grade == "A" ~ 0.9,
                             cat_grade == "B" ~ 0.7,
                             cat_grade == "C" ~ 0.5,
                             cat_grade == "D" ~ 0.3,
                             cat_grade == "E" ~ 0.1,
                             cat_grade == NA ~ NA))

# NRM region coral index scores
mmp.index<-get(load(file="../../CaseStudy/data/processed/boot.index.region.RData"))

mmp<-mmp.index$sum

framework<- Framework_grades_NRM |> 
  rename('Region'='NRM') |> 
  mutate(Year=as.numeric(as.character(fYEAR))) |> 
  dplyr::select(Year, Region, f.score,cat_grade)

mmp.frame<-mmp |> 
  left_join(framework)

check<- mmp.frame |> filter(Year>2016)

trafficLightPalette <- (c('#FF0000','#FFC000','#FFFF00','#92D050','#00B050'))
lims <- LETTERS[1:5]

mmp.framework.regional.scores.trend.KC<-ggplot(mmp.frame |> filter(Year>2016), aes(x = Year))+
  #scale_y_continuous(breaks=c(0.2,0.4,0.6), limits=c(0.19,0.70))+
  geom_line(aes(y=Score, color="Coral Index")) +
  geom_line(aes(y=f.score, color="Framework")) +
  scale_color_manual(
        name = "Scoring method",
        values = c("Coral Index" = "black", "Framework" = "grey")
      ) +
  geom_point(aes(y=Score, fill=Grade), shape=21,size=3) +
  geom_point(aes(y=f.score, fill=cat_grade), shape=21,size=3) +
  scale_fill_manual(values=rev(trafficLightPalette), limits=lims)+
  labs(x="", y="Mean indicator score", colour='Scoring method') +
  geom_hline(yintercept=c(0.205,0.405,0.605), linetype="dashed", colour='black', linewidth=0.5)+
  geom_vline(xintercept=c(), linetype="dashed", colour='black', linewidth=0.5)+
  theme_bw(base_size=10)+
  theme(legend.position="right") +
  facet_wrap(vars(Region))
ggsave(mmp.framework.regional.scores.trend.KC, file="../../CaseStudy/output/figures/regional.grade.comparison.KC.png", width=16, height=16, units="cm", scale=1.2)


#For Reef Depth comparison
# reef Framework_indicator_scores
load(file="../../CaseStudy/data/final/Framework_indicator_scores_reef.RData")

#Snapshot check against figures in C:\Users\kjohns\Desktop\RPI\GBR_Recovery\docs\RPI_peakDensity_final_results
check<-Framework_indicator_scores_reef |> 
  filter(Indicator=="Performance", Metric=="distance.metric", REEF.d=="Snapper South deep slope") |>
  mutate(BelowThreshold=ifelse(upper<0.5, 0, 1),
  YEAR=as.numeric(as.character(fYEAR))) |>
  arrange(REEF.d, YEAR)|>
  dplyr::select(YEAR, REEF.d, BelowThreshold, median, upper, lower)
#The majority are the same. There are some differences and they tend to be when the 'upper' was very close to 0.5.

#Checking Recovery Performance original baseline trajectories versus Case Study
  load(file="C:/Users/kjohns/Desktop/RPI/GBR_Recovery/pipelines/RPI/AB.only/alphaTd/baseline/data/processed/RPI.baseline.AB.only.alphaTd.RData")
RPI.baseline.original=RPI.baseline
original.bioregions<-data.frame(BIOREGION.rpi.agg=unique(RPI.baseline.original$BIOREGION.rpi.agg))
inshore.bioregions<-RPI.baseline.original |> 
  filter(BIOREGION.rpi.agg %in% unique(new.bioregions$BIOREGION.rpi.agg))
inshore.rpids<- inshore.bioregions |>
ungroup() |>
  dplyr::select(BIOREGION.rpi.agg, DEPTH.f, proj.site.rpid) |> unique() |>
  arrange(BIOREGION.rpi.agg, DEPTH.f, proj.site.rpid)

load(file="CaseStudy/data/processed/RPI.baseline.RData")
new.bioregions<-data.frame(BIOREGION.rpi.agg=unique(RPI.baseline$BIOREGION.rpi.agg))
new.rpids<- RPI.baseline |>
  ungroup() |>
  dplyr::select(BIOREGION.rpi.agg, DEPTH.f, proj.site.rpid) |> unique() |>
  arrange(BIOREGION.rpi.agg, DEPTH.f, proj.site.rpid)

#The baseline trajectories in the case study are different to the original, due to a rerun of coral cover model, which is an input for recovery performance
#Rerun of baseline lead to different identification of beginning of recovery trajectories (there seem to be fewer trajectories in new version)
#For inshore bioregions, there were 9 trajectories added and 3 removed.
#I'm assuming there would be differences in other bioregions too.


# framework categories reef
reef_cats<-Framework_indicator_scores_reef |> 
  filter(Metric=="distance.metric") |> 
  mutate(BelowThreshold=ifelse(upper<0.5, 0, 1)) |>
  pivot_wider(id_cols=c(fYEAR, REEF.d),names_from = Indicator, values_from = BelowThreshold) |>   # not here that BelowThreshold=0 
  mutate(
         level2=Juvenile+Macroalgae+Composition,
         Juv_MA=Juvenile+Macroalgae,
         cat_grade=ifelse(CoralCover==1 & Performance==1 & level2==3, 'A',
                      ifelse(CoralCover==1 & Performance==1 & level2>0, 'B',
                             ifelse(CoralCover==1 & Performance==1 & level2==0, 'C',
                                    ifelse(CoralCover==1 & Performance==0 & level2==3, 'B',
                                           ifelse(CoralCover==1 & Performance==0 & level2>0, 'C',
                                                  ifelse(CoralCover==1 & Performance==1 & level2==0, 'D',
                                                         ifelse(CoralCover==0 & Performance==1 & level2==3, 'B',
                                                                ifelse(CoralCover==0 & Performance==1 & level2>0, 'C',
                                                                       ifelse(CoralCover==0 & Performance==1 & level2==0, 'D',
                                                                              ifelse(CoralCover==0 & Performance==0 & level2==3, 'D', 'E'))))))))))) |>
  left_join(spatial_lookup %>% dplyr::select(REEF.d, DEPTH.f, NRM) |>
  distinct()) |>
  rename('Region'='NRM') |>
    mutate(f.score=case_when(cat_grade == "A" ~ 0.9,
                             cat_grade == "B" ~ 0.7,
                             cat_grade == "C" ~ 0.5,
                             cat_grade == "D" ~ 0.3,
                             cat_grade == "E" ~ 0.1,
                             cat_grade == NA ~ NA)) |>
  mutate(Year=as.numeric(as.character(fYEAR)))
                                          

# NRM region coral index scores
mmp.index.reef<-get(load(file="../../CaseStudy/data/processed/boot.index.reef.depth.RData"))

mmp.index.scores<-mmp.index.reef$sum
mmp.index.scores.reef.d<-mmp.index.scores |>
mutate(DEPTH.f=case_when(
  DEPTH==5 ~ "deep slope",
  DEPTH==2 ~ "shallow slope"),
  REEF.d=factor(paste(REEF, DEPTH.f))) 

#check if they contain the same values regardless of order
setequal(mmp.index.scores.reef.d$REEF.d, reef_cats$REEF.d)

# To see which values are not matched
setdiff(mmp.index.scores.reef.d$REEF.d, reef_cats$REEF.d)
setdiff(reef_cats$REEF.d, mmp.index.scores.reef.d$REEF.d)

# The framework is missing Green Island.
#MMP index scores don't
#Camp, Aquila, Farmers, Rat, Holbourne, Henderson, Pine Islets, Pine Peak, Seal Rocks, Temple
#These reefs don't have a composition score in the framework either (but are they contributing to the comparison that excludes composition?)
#Also, Linnet and Martin aren't contributing to MMP regional summaries, are they in the framework regional summary?

mmp.index.scores.reef<- mmp.index.scores.reef.d |>
  mutate(REEF.d=case_when(REEF.d=="Fitzroy West LTMP deep slope" ~ "Fitzroy Island deep slope",
                     REEF.d=="Green deep slope" ~ "Green Island deep slope",
                     REEF.d=="Havannah North deep slope" ~ "Havannah Island deep slope",
                     REEF.d=="Border deep slope" ~ "Border Island deep slope",
                     REEF.d=="Hayman deep slope" ~ "Hayman Island deep slope",
                     REEF.d=="Langford deep slope" ~ "Langford and Bird Isles deep slope",
                     REEF.d=="Middle Rf LTMP shallow slope" ~ "Middle Rf shallow slope",
                     REEF.d=="Pandora North deep slope" ~"Pandora Reef deep slope",
                     TRUE ~ REEF.d))

# To see which values are not matched
setdiff(mmp.index.scores.reef$REEF.d, reef_cats$REEF.d)
setdiff(reef_cats$REEF.d, mmp.index.scores.reef$REEF.d)

framework_index_reef_combined <- reef_cats |> 
  dplyr::select(Region, REEF.d, DEPTH.f, Year, Juvenile, Macroalgae, CoralCover, Composition, Performance, cat_grade, f.score) |> 
  left_join(mmp.index.scores.reef |> 
              dplyr::select(Region, REEF.d, DEPTH.f, Year, Score, Grade))

# Exclude REEF.d levels where both cat_grade and Score are NA for all rows
reef_na_summary <- framework_index_reef_combined |>
  group_by(REEF.d) |>
  summarise(all_cat_grade_NA = all(is.na(cat_grade)),
            all_Score_NA = all(is.na(Score))) |>
  filter(all_cat_grade_NA & all_Score_NA)

compare_reef_grades <- framework_index_reef_combined |>
  filter(!(REEF.d %in% reef_na_summary$REEF.d), Year>2016)

trafficLightPalette <- (c('#FF0000','#FFC000','#FFFF00','#92D050','#00B050'))
lims <- LETTERS[1:5]


# For each REEF.d, Year, check if each variable == 0 and create a label string
# Loop over each Region and generate plots
regions <- unique(compare_reef_grades$Region)

for (reg in regions) {
  region_data <- compare_reef_grades |> filter(Region == reg)
  depths <- unique(region_data$DEPTH.f)
  for (dep in depths) {
    annotation_data <- region_data |>
      filter(DEPTH.f == dep, !is.na(f.score)) |>
      mutate(
        label_J = ifelse(!is.na(Juvenile) & Juvenile == 0, "J", ""),
        label_M = ifelse(!is.na(Macroalgae) & Macroalgae == 0, "M", ""),
        label_CC = ifelse(!is.na(CoralCover) & CoralCover == 0, "CC", ""),
        label_CO = ifelse(!is.na(Composition) & Composition == 0, "CO", ""),
        label_P = ifelse(!is.na(Performance) & Performance == 0, "P", "")
      ) |>
      select(
        REEF.d, Year, Score, f.score, label_J, label_M, label_CC, label_CO, label_P
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
          TRUE ~ Score
        )
      )

    plot_region <- ggplot(region_data |> filter(DEPTH.f == dep), aes(x = Year)) +
      ylim(0, 1.5) +
      geom_line(aes(y = Score, color = "Index")) +
      geom_line(aes(y = f.score, color = "Framework")) +
      scale_color_manual(
        name = "Scoring method",
        values = c("Index" = "black", "Framework" = "grey")
      ) +
      geom_point(aes(y = Score, fill = Grade), shape = 21, size = 3) +
      geom_point(aes(y = f.score, fill = cat_grade), shape = 21, size = 3) +
      scale_fill_manual(values = rev(trafficLightPalette), limits = lims) +
      labs(x = "", y = "Mean indicator score", colour = 'Scoring method') +
      geom_hline(yintercept = 1, colour = 'red', linewidth = 0.5) +
      geom_hline(yintercept = c(0.205, 0.405, 0.605), linetype = "dashed", colour = 'black', linewidth = 0.5) +
      geom_vline(xintercept = c(), linetype = "dashed", colour = 'black', linewidth = 0.5) +
      theme_bw(base_size = 10) +
      theme(legend.position = "right") +
      facet_wrap(vars(REEF.d)) +
      geom_text(
        data = annotation_data,
        aes(x = Year, y = y, label = label),
        colour="red",
        inherit.aes = FALSE,
        size = 3
      ) +
      ggtitle(paste(reg, "-", dep))

    ggsave(
      plot_region,
      file = paste0("../../CaseStudy/output/figures/plot.reefs.", gsub(" ", "_", reg), ".", gsub(" ", "_", dep), ".png"),
      width = 12, height = 10
    )
  }
}