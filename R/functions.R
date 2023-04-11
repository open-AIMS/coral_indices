## ---- loadPackages
library(knitr)
library(tidyverse)
## ----end

## ---- preparePaths
DATA_PATH <<- "../data/"
OUTPUT_PATH <<- "../output/"
FIGS_PATH <<- paste0(OUTPUT_PATH, "figures")
TABS_PATH <<- paste0(OUTPUT_PATH, "tables")

if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
if (!dir.exists(paste0(DATA_PATH,"primary"))) dir.create(paste0(DATA_PATH, "primary"))
if (!dir.exists(paste0(DATA_PATH,"processed"))) dir.create(paste0(DATA_PATH, "processed"))
if (!dir.exists(paste0(DATA_PATH,"modelled"))) dir.create(paste0(DATA_PATH, "modelled"))
if (!dir.exists(paste0(DATA_PATH,"summarised"))) dir.create(paste0(DATA_PATH, "summarised"))
if (!dir.exists(paste0(DATA_PATH,"workspace"))) dir.create(paste0(DATA_PATH, "workspace"))

if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)
if (!dir.exists(TABS_PATH)) dir.create(TABS_PATH)
## ----end
