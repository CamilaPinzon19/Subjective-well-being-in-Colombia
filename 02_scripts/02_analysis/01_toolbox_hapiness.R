# ==============================================================================
#                        TOOLBOX HAPPINESS PLATFORM - MAIN SCRIPT
# ==============================================================================
# 
# Description: 
# Main platform for coefficient estimation in happiness regression analysis in 
# Colombia using data from the National Quality of Life Survey (ENCV).
#
# ==============================================================================
#                           INITIAL CONFIGURATION
# ==============================================================================
rm(list = ls());gc()

# ==============================================================================
#                           LIBRARY LOADING
# ==============================================================================
packages <- c('here','readr','readxl','dplyr','purrr')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))
invisible(gc())

# ==============================================================================
#                        LOADING PLATFORM
# ==============================================================================
lapply(here("02_scripts/functions/01_happiness_platform_VF.R"), source)

# ==============================================================================
#                        Implementation of the platform
# ==============================================================================
years = 2018:2024
path = '01_data'
run_happiness_platform_VF(path = path,
                       years = years)
