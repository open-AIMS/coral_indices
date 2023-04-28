CI_clear_models_JU_data <- function() {
    files <- list.files(path = paste0(DATA_PATH, "modelled"),
                        pattern = "JU.*|data_ju.*",
                        full.names = TRUE)
    unlink(files)
}
