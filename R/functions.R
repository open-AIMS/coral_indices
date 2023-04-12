## ---- loadPackages
library(knitr)
library(tidyverse)
library(assertthat)
library(purrr)
library(rlang)
## ----end

source('../R/functions_boxes.R')

#########################################################################
## The following function determines whether the current script is the ##
## parent (directly called from command line etc) or child (sourced    ##
## from another R script.                                              ##
## NOTE.  if we were to run sys.nframe() directly from a parent        ##
## script, it would return a value of 0.  However, since we are        ##
## calling sys.nframe() from a sourced script, then it will be 1       ##
#########################################################################
CI_isParent <- function() {
   ifelse(sys.nframe()==1, TRUE, FALSE)
}

CI_fakeArgs <- function(type) {
    switch(type,
           CI_startMatter(args = c("--final_year=2023",
                                          "--fresh_start=true")),
           CI_startMatter(args = c("--final_year=2023"))
           )
}

########################################################################
## The following function is a wrapper around a number of routines    ##
## that occur once at the start of an analysis run.                   ##
## In order, these routines are:                                      ##
## - initialise a status list.  This list is the basis of information ##
##   presented in the command line interface.                         ##
## - initialise the log file                                          ##
########################################################################
CI_startMatter <- function(args = commandArgs()) {
    CI_initialise()             ## initialise
    CI_parseCLA(args)           ## parse command line arguments
    ## CURRENT_STAGE <<- 1
    ## ## CI__add_stage(stage = paste0("STAGE",CURRENT_STAGE), title = 'Stage 1 - prepare environment')
    CI_generateSettings()       ## generate the rest of the path and naming settings
    CI_preparePaths()           ## prepare paths
    CI_initialise_log()       ## create the log file
    ## source('CI_config.R')     ## create directory structure if it does not yet exist
    ## CI_checkPackages()         ## load required packages
    ## CI_analysis_stage()       ## read in the stage that the analysis is up to
    CI_openingBanner()        ## display an opening banner
    ## ## CI_checkPackages()
    ## ## sf_use_s2(FALSE)
    CI_log(status = "SUCCESS",
           logFile = LOG_FILE,
           Category = "Preparations",
           msg=NULL)
}

CI__add_setting <- function(item, value) {
    if(any(grep(item, names(CI$setting)))) {
        print('This setting already exists, if you would like to modify it, try CI__change_setting()')
    } else {
        CI$setting <- append(CI$setting, setNames(value, item))
        assign('CI', CI, env = globalenv())
    }
}

CI__add_status <- function(stage, item, label, status) {
    CI[[stage]]$items <- c(CI[[stage]]$items, item)
    CI[[stage]]$labels <- c(CI[[stage]]$labels, label)
    CI[[stage]]$status <- c(CI[[stage]]$status, status)
    CI[[stage]]$predicate <- c(CI[[stage]]$predicate, NA)
    CI[[stage]]$predicate_value <- c(CI[[stage]]$predicate_value, NA)
    assign("CI", CI, env = globalenv())
}


CI_initialise <- function() {
    ## setClass('CI', slots = representation(FINAL_YEAR="numeric"))
    ## CI <<- new('CI')
    CI <- vector('list', 2)
    CI <- setNames(CI, c('setting', 'status'))
    CI$setting <- list()
    CI <<- CI
}

####################################################################################
## The following function parses the command line arguments                       ##
## 1. Ensure that a final (maximum) year has been supplied as --final_year=<YEAR> ##
##    and that the final year is a four digit integer                             ##
## 2. Ensure that IF fresh start has been specified, it is specified as either:   ##
##    true or false.  A value of true indicates that at the start of the run      ##
##    all generated data should be cleared completely such that the analyses run  ##
##    completely fresh                                                            ##
####################################################################################
CI_parseCLA <- function(args) {
    valid_cla <- paste0("\n\nThe call must be of the form:\n",
                        "00_main.R --final_year=<YEAR> --fresh_start=<true|false>",
                        "\n<YEAR>:  \ta valid four digit year representing the final",
                        "\n\t\t(maximum) year of reporting",
                        "\n<true|false> \twhether to start by clearing all stored data (true).\n\n")
    ## Ensure that a final year is supplied
    final_year <- grep('--final_year=.*', args)
    assertthat::assert_that(length(final_year)>0,
                            msg = paste0("\n\nFinal year not supplied.", valid_cla))
    assertthat::assert_that(any(str_detect(args, '--final_year=[0-9]{4}')),
                            msg = paste0("\n\nFinal year not supplied as four digit year.",
                                         valid_cla))
    FINAL_YEAR <- as.numeric(gsub('--final_year=(.*)','\\1', args[final_year]))
    CI__add_setting(item = 'FINAL_YEAR', FINAL_YEAR)
    ## check to see whether there should be a fresh start
    fresh_start <- grep('--fresh_start=.*', args)
    if (length(fresh_start)>0) {
        assertthat::assert_that(str_detect(args[fresh_start], '--fresh_start=(true|false)'),
                                msg = paste0("\n\nFresh start must be supplied as either true or false",
                                             valid_cla))
        FRESH_START <- ifelse(gsub('--fresh_start=(.*)','\\1', args[fresh_start]) == 'true',
                               TRUE,
                               FALSE)
    } else {
        FRESH_START <- FALSE
    }
    CI__add_setting(item = 'FRESH_START', FRESH_START)
    
}

CI_generateSettings <- function() {
    ## Location of folder to store R data objects
    DATA_PATH <<- "../data/"
    PRIMARY_DATA_PATH <<- "../data/primary/"
    PARS_DATA_PATH <<- "../data/parameters/"
    ## CI__change_status(stage = "SETTINGS", item = "DATA_PATH",
    ##                          status = "success", update_display = FALSE)
    ## Define the name of the input benthic data
    OUTPUT_PATH <<- "../output/"
    FIGS_PATH <<- paste0(OUTPUT_PATH, "figures")
    TABS_PATH <<- paste0(OUTPUT_PATH, "tables")
    ## purrr::map(.x = c('DATA_PATH', 'PRIMARY_DATA_PATH'),
    ##     .f = ~ CI__add_setting(item = .x, value = eval(sym(.x))))
    runStage <<- 0
    lapply(c('DATA_PATH', 'PRIMARY_DATA_PATH', 'PARS_DATA_PATH',
             'OUTPUT_PATH','FIGS_PATH', 'TABS_PATH', 'runStage'),
           function(x) CI__add_setting(item = x, value = eval(sym(x)))) 
}

## ---- preparePaths
CI_preparePaths <- function() {
    if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
    if (!dir.exists(paste0(DATA_PATH,"primary"))) dir.create(paste0(DATA_PATH, "primary"))
    if (!dir.exists(paste0(DATA_PATH,"processed"))) dir.create(paste0(DATA_PATH, "processed"))
    ## if (!dir.exists(paste0(DATA_PATH,"modelled"))) dir.create(paste0(DATA_PATH, "modelled"))
    ## if (!dir.exists(paste0(DATA_PATH,"summarised"))) dir.create(paste0(DATA_PATH, "summarised"))
    ## if (!dir.exists(paste0(DATA_PATH,"workspace"))) dir.create(paste0(DATA_PATH, "workspace"))

    if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
    if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)
    if (!dir.exists(TABS_PATH)) dir.create(TABS_PATH)

    if (!dir.exists(paste0(DATA_PATH, "log"))) dir.create(paste0(DATA_PATH, "log"))
}
## ----end

########################################################################
## The following function initialises a log file.  This log file is    ##
## placed in the root of the project as it needs to be in a location   ##
## that is guarenteed to exist from a freshly cloned instance of this  ##
## codebase.                                                           ##
#########################################################################
CI_initialise_log <- function() {
    ##Log file
    LOG_FILE <<- paste0(DATA_PATH, 'log/CI.log')
    if (file.exists(LOG_FILE)) unlink(LOG_FILE)
    CI__add_setting(item = "LOG_FILE", value = LOG_FILE)
}

CI_openingBanner <- function() {
    system('clear')
    cat(" ")
    currentTime <- format(Sys.time(),'%d/%m/%Y %H:%M:%S')

    ## CI__add_setting(item = 'currentTime', currentTime)

    box.style <- cli:::box_styles()
    box.width <- 80
    box.margins <- 1

    ## get the width of the path box
    max.settings.width <- 40
    settings.box.nchar <-nchar(
        paste0(names(CI$setting), ": ", CI$setting)
    )
    ## we want to ensure that the settings box has a max
    ## width of 40 characters. We will center truncate the strings
    settings.box.nchar <- pmin(settings.box.nchar, max.settings.width)
    settings.box.width <- max(settings.box.nchar) +
        2 +              # add one for the status character
        box.margins*2    # add the left and right margin

    ## Outer box (top)
    top <- CI__outerBox.top(box.width, settings.box.width)
    ## Settings box
    settings.box.text <- CI__settingsBox(settings = CI$setting,
                                     box.width = settings.box.width,
                                     box.nchar = settings.box.nchar,
                                     box.margins = box.margins,
                                     currentTime,
                                     max.settings.width)
    ## Main box
    main.box.text <- CI__mainBox(settings.box.text,
                                  box.width,
                                  settings.box.width,
                                  box.margins)

     ## Outer box (bottom)
    bottom <- CI__outerBox.bottom(box.width, settings.box.width)

    ## Combine boxes
    combined.boxes.text <- CI__combinedBoxes(
        top,
        settings.box.text,
        main.box.text,
        bottom,
        box.width,
        settings.box.width,
        box.margins)

    cat(combined.boxes.text)

    ## log box
    log.box <- CI__logBox(box.width, box.margins)
    cat(log.box)
}

####################################################################################
## The following function writes out log information to a file named by the       ##
## logFile argument in the log/ folder.                                           ##
## Arguments:                                                                     ##
## - status:     a string indicating either 'FAILURE',  'SUCCESS',  'WARNING' or  ##
##              'INFO                                                             ##
## - logFile:    a character string representation of the log file name           ##
##               (including path relative to the current working director)        ##
## - Category:   a character string with a category to appear verbatim in the log ##
## - success:    boolean or string. One of TRUE (for success), 'WARNING'          ##
##               (for warnings) or anything else for a failure                    ##
## - msg:        the message (as a string) to appear verbatim in the log          ##
####################################################################################
CI_log <- function(status, logFile, Category, msg=NULL) {
    d=dirname(logFile)
    files <- list.files(d)
    if(!any(grepl(paste0('^',logFile,'$'),files))) system(paste0('touch "',logFile,'"'))
    now <- Sys.time()
    options(digits.secs=2)              ## switch to subsecond display
    msg = paste0(now, '|', status, ': ', Category, ' ', msg)
    ## if (!DEBUG_MODE) cat(paste0(msg,'\n'))
    if (!is.null(msg)) {
        write(msg,  file=paste0(logFile), append=TRUE)
    }
    ## if (DEBUG_MODE)
    CI_openingBanner()
}

