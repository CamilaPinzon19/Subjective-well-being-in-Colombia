# ==============================================================================
#                    01_transformation_databases.R
# ==============================================================================
# 
# Description:
# This script processes ENCV data from 2018-2022 to create municipality codes
# by combining department and municipality information. It handles column name
# variations across years and exports the results for further analysis.
# 
# Input: 
#   - Datos de la vivienda.CSV (per year)
#   - Municipio de aplicacion.CSV (per year)
# 
# Output:
#   - Variables diseno muestral.xlsx (per year)
# ==============================================================================

rm(list = ls())
packages <- c('readr','dplyr','writexl','here')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))
invisible(gc())

# Vector of years to process
years <- 2018:2022

# Path - current working directory where scripts and year folders are located
path_input <- here("01_data", "raw")
path_output <- here("01_data", "processed")

for (year in years) {
  cat("Processing year:", year, "\n")
  
  folder_input <- file.path(path_input, paste0("ENCV_", year))
  folder_output <- file.path(path_output)

  # 1. Define column name according to year
  col_depto <- if (year == 2018) "DPTO" else "P1_DEPARTAMENTO"
  
  # 2. Build complete file names
  vivienda_file <- file.path(folder_input, "Datos de la vivienda.CSV")
  mcpio_file <- file.path(folder_input, "Municipio de aplicacion.CSV")
  
  # 3. Warn and skip to next year if any file is missing
  if (!file.exists(vivienda_file) || !file.exists(mcpio_file)) {
    warning("Missing some files for ", year)
    next
  }
  
  # 4. Load data
  datos_vivienda <- read_delim(vivienda_file,
                               delim=";", escape_double=FALSE, trim_ws=TRUE,
                               col_select = c("DIRECTORIO","SECUENCIA_P","ORDEN",col_depto))
  
  mcpio <- read_delim(mcpio_file,
                      delim=";", escape_double=FALSE, trim_ws=TRUE)
  
  # 5. Create MPIO variable
  Variables_diseño_muestral <- datos_vivienda %>%
    mutate(MPIO = as.numeric(paste0(.data[[col_depto]], mcpio$P1_MUNICIPIO))) %>%
    select(-all_of(col_depto))
  
  # 6. Export Excel file
  write_xlsx(Variables_diseño_muestral,
             file.path(folder_output, paste0("Variables_diseno_muestral_",year,".xlsx")))
  
  cat("✓ Completed year:", year, "\n")
}
