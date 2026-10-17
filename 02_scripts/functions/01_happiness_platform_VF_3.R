# ==============================================================================
#                 HAPPINESS PLATFORM - MAIN ANALYSIS FUNCTION
# ==============================================================================
# Description: Analyzes life satisfaction in Colombia using ENCV data and
#              survey-weighted regression models.
#              Processes each requested year, prepares the individual, household,
#              and dwelling databases, incorporates the sampling design, estimates
#              the model, and saves coefficient and significance summaries.
#
# Parameters:
#   path: string - Directory containing the ENCV data folders by year.
#   years: numeric vector - Years to analyze (e.g., 2018:2024).
#
# Output files:
#   Satisfaction_logit_2026.RData - Estimated coefficients and confidence intervals.
#   Satisfaction_p_vals_2026.RData - Global summary of coefficient significance.
# ==============================================================================

# Main function that executes the happiness/satisfaction analysis pipeline.
run_happiness_platform <- function(path, 
                                   years){
  
  # Initialize containers for the results obtained in each year.
  # list_coef_final stores coefficient estimates and confidence intervals;
  # p_values_list stores the corresponding p-values.
  list_coef_final <- list()
  p_values_list <- list()
  
  ## It runs every year of interests
  # Process each year independently, using the database structure available for that year.
  for (year in years) {
    print(paste("- - - - - - - - - - - - - Processing year:", year, "- - - - - - - - - - - - - - - - \n"))
    
    # Set the path 
    setwd(file.path(path, as.character(year)))
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    ###                   1. Reading the databases                     ####
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # Set the delimiter used by CSV files; the 2021 files use commas, while
    # the other CSV files handled here use semicolons.
    delim_to_use <- if (year == 2021) "," else ";"
    
    educacion <- read_delim("Educacion.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                            col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P8587", "P1088"),
                            show_col_types = FALSE)
    trabajo <- read_delim("Fuerza de trabajo.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                          col_select = c("DIRECTORIO", "SECUENCIA_P","ORDEN", "P6240",
                                         "P6435", "P8624", "P6750", "P6885", "P6886", "P415", "P6920"),
                          show_col_types = FALSE)
    # The dwelling database has a different structure in 2018.
    # For that year, read the CSV and derive the department code from DPTO.
    # In later years, the department variable is already available or the data
    # are supplied in a Stata .DTA file.
    if (year == 2018) {
      datos_vivienda <- read_delim("Datos de la vivienda.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                   col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "DPTO", "CLASE", "FEX_C",
                                                  "P5661S1", "P5661S2", "P5661S3", "P5661S4", "P5661S5", "P5661S6", "P5661S7",
                                                  "P1070", "P8520S1", "P8520S5", "P8520S3"),
                                   show_col_types = FALSE, locale = locale(decimal_mark = ","))
      datos_vivienda <- datos_vivienda %>%
        mutate(P1_DEPARTAMENTO = sprintf("%02d", as.numeric(DPTO))
        )
    } else {
      if (year %in% c(2022, 2023, 2024) ) {
        datos_vivienda <- read_dta("Datos de la vivienda.DTA", 
                                   col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P1_DEPARTAMENTO", "CLASE", "FEX_C",
                                                  "P5661S1", "P5661S2", "P5661S3", "P5661S4", "P5661S5", "P5661S6", "P5661S7",
                                                  "P1070", "P8520S1", "P8520S5", "P8520S3"))
      } else {
        datos_vivienda <- read_delim("Datos de la vivienda.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                     col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P1_DEPARTAMENTO", "CLASE", "FEX_C",
                                                    "P5661S1", "P5661S2", "P5661S3", "P5661S4", "P5661S5", "P5661S6", "P5661S7",
                                                    "P1070", "P8520S1", "P8520S5", "P8520S3"),
                                     show_col_types = FALSE, locale = locale(decimal_mark = ","))
      }
    }
    
    tecnologia <- read_delim("Tecnologias de informacion y comunicacion.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                             col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P1084"),
                             show_col_types = FALSE)
    tenencia_finan <- read_delim("Tenencia y financiacion de la vivienda que ocupa el hogar.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                 col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P5095"),
                                 show_col_types = FALSE)
    condiciones_vida <- read_delim("Condiciones de vida del hogar y tenencia de bienes.CSV", 
                                   delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                                   col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P9010", "P5230", "P784S1", 
                                                  "P1077S1"),
                                   show_col_types = FALSE)
    caracteristicas_hogar <- read_delim("Caracteristicas y composicion del hogar.CSV", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                        col_select =c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6020", "P6040",
                                                      "P756S3", "P6087", "P6088", "P1895", "P1901", "P6080"),
                                        show_col_types = FALSE)
    categorias_ruralidad <- read_delim("Categorias_de_Ruralidad.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                       show_col_types = FALSE)
    
    # Health variables changed structure after 2020, so the selected columns
    # depend on the year being processed.
    if (year %in% 2018:2020) {
      salud <- read_delim("Salud.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE, 
                          col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6100", "P6127",
                                         "P1906S1", "P1906S2", "P1906S3", "P1906S4", "P1906S5", "P1906S6", "P1906S7",
                                         "P1906S8", "P3008S1", "P1707", "P1708"),
                          show_col_types = FALSE)
    } else {
      salud <- read_delim("Salud.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE, 
                          col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6100", "P6127",
                                         "P1906S1", "P1906S2", "P1906S3", "P1906S4", "P1906S5", "P1906S6", "P1906S7",
                                         "P1906S8", "P3008S1", "P1707", "P3335S1"),
                          show_col_types = FALSE)
    }
    
    # Load the sampling-design information using the format available for the year.
    # For 2018-2022, the municipality identifier is taken from an Excel file;
    # in later years, it is read from a CSV and joined using person-level keys.
    if (year %in% 2018:2022){
      Variables_diseno_muestral <- read_excel("Variables diseno muestral.xlsx")
      
      caracteristicas_hogar <- left_join(caracteristicas_hogar, Variables_diseno_muestral[,c("DIRECTORIO", "MPIO")], by = c("DIRECTORIO"))
      caracteristicas_hogar$MPIO <- as.numeric(caracteristicas_hogar$MPIO)
    } else{
      Variables_diseno_muestral <- read_delim("Variables diseno muestral.CSV", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                              col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "MPIO"),
                                              show_col_types = FALSE)
      
      caracteristicas_hogar <- left_join(caracteristicas_hogar, Variables_diseno_muestral, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
      caracteristicas_hogar$MPIO <- as.numeric(caracteristicas_hogar$MPIO)
    }
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    ###               2. Modifications to the database              ####
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # ============================================================================
    #                         2. DATA PREPROCESSING
    # ============================================================================
    # Create a housing-problems indicator from the seven P5661S items.
    # A row sum equal to seven is coded as 1; all other cases are coded as 2.
    datos_vivienda <- datos_vivienda %>% 
      mutate(problemas = case_when(rowSums(across(starts_with("P5661S"), as.numeric), na.rm = TRUE) == 7 ~ 1, 
                                   .default = 2) ) %>% # Problems with their housing
      select(-c("P5661S1", "P5661S2", "P5661S3", "P5661S4", "P5661S5", "P5661S6", "P5661S7"))
    
    # Recode household characteristics, restricting the analysis to people older
    # than 14. The 2018 nationality coding differs from the coding in later years.
    # Age, parental education, life satisfaction, happiness, and ethnicity are
    # converted into analysis categories.
    if (year==2018){
      caracteristicas_hogar <- caracteristicas_hogar %>%
        filter(P6040 > 14)  %>% # Age filter for over 15 years
        mutate(P756S3 = case_when(P756S3==3~3, # Nationality
                                  .default = 1),
               P6040 = case_when(P6040 >= 60 ~ 3, # Age
                                 P6040 < 27 ~ 1,
                                 .default = 2),
               P6087 = case_when(P6087 %in% 1:2 ~ 1, # Father's education
                                 P6087 %in% 3:4 ~ 2,
                                 P6087 %in% 5:7 ~ 3,
                                 P6087==8 ~ 4,
                                 P6087==9 ~ 5,
                                 .default = NA),
               P6088 = case_when(P6088 %in% 1:2 ~ 1, # Mother's education
                                 P6088 %in% 3:4 ~ 2,
                                 P6088 %in% 5:7 ~ 3,
                                 P6088==8 ~ 4,
                                 P6088==9 ~ 5,
                                 .default = NA),
               P1895 = case_when(P1895 %in% 0:6 ~ 0,  # SATISFACCION DE VIDA
                                 .default = 1),
               P1901 = case_when(P1901 %in% 0:6 ~ 1, # Hapiness
                                 .default = 2),
               P6080 = case_when(P6080==1 ~ 1, # Ethnicity
                                 P6080==2 ~ 2,
                                 P6080 %in% 3:5 ~ 3,
                                 .default = 4)
        )
    }else{
      caracteristicas_hogar <- caracteristicas_hogar %>%
        filter(P6040 > 14)  %>% # Age filter for over 15 years
        mutate(P756S3 = case_when(is.na(P756S3) ~ 1, # Nationality
                                  P756S3==3 ~ 2, 
                                  .default = 3),
               P6040 = case_when(P6040 >= 60 ~ 3, # Age
                                 P6040 < 27 ~ 1,
                                 .default = 2),
               P6087 = case_when(P6087 %in% 1:2 ~ 1, # Father's education
                                 P6087 %in% 3:4 ~ 2,
                                 P6087 %in% 5:7 ~ 3,
                                 P6087==8 ~ 4,
                                 P6087==9 ~ 5,
                                 .default = NA),
               P6088 = case_when(P6088 %in% 1:2 ~ 1, # Mother's education
                                 P6088 %in% 3:4 ~ 2,
                                 P6088 %in% 5:7 ~ 3,
                                 P6088==8 ~ 4,
                                 P6088==9 ~ 5,
                                 .default = NA), 
               P1895 = case_when(P1895 %in% 0:6 ~ 0, 
                                 .default = 1),
               P1901 = case_when(P1901 %in% 0:6 ~ 1, # Hapiness
                                 .default = 2),
               P6080 = case_when(P6080==1 ~ 1, # Ethnicity
                                 P6080==2 ~ 2,
                                 P6080 %in% 3:5 ~ 3,
                                 .default = 4)
        )
    }
    
    # Combine the father's and mother's education into one variable.
    # The maximum observed education level is retained; if both values are missing,
    # the combined variable remains missing. The original variables are then removed.
    # Adjustment to parental education to mitigate correlation problem between variables
    caracteristicas_hogar <- caracteristicas_hogar %>% 
      mutate(educacion_padres = apply(caracteristicas_hogar[,c("P6087", "P6088")], 1, function(x) {
        # Parents' highest educational level
        if (all(is.na(x))) NA else max(x, na.rm = TRUE)
      })
      ) %>% 
      select(-c("P6087", "P6088"))
    
    # Recode completed and current education levels and combine them into nivel_edu.
    # The two source variables are removed after the combined indicator is created.
    educacion <- educacion %>% 
      mutate(P8587 = case_when(P8587==1 ~ 1, 
                               P8587 %in% 2:3 ~ 2,
                               P8587 %in% 4:5 ~ 3,
                               P8587 %in% 6:10 ~ 4,
                               P8587==11 ~ 5,
                               P8587 %in% 12:13 ~ 6),
             P1088 = case_when(P1088 %in% 1:2~2,
                               P1088 %in% 3:4~3,
                               P1088 %in% 5:6~4,
                               P1088==7~5,
                               P1088==8~6),
             nivel_edu = rowSums(across(c(P8587, P1088)), na.rm = TRUE) # Educational level
      ) %>% 
      select(-c("P8587", "P1088"))
    
    # Recode employment characteristics and derive grouped measures of commuting
    # time, working hours, and income. Codes 98 and 99 in the income variables are
    # treated as missing before the income quintiles are calculated.
    trabajo <- trabajo %>% 
      mutate(P6435 = case_when(P6435 %in% 1:2 ~ 1,
                               P6435==3 ~ 2,
                               P6435 %in% 4:5 ~ 3,
                               P6435==6 ~ 4,
                               P6435==7|P6435==10 ~ 5,
                               P6435 %in% 8:9 ~ 6),
             P6885 = case_when(P6885==8|P6885==11 ~ 1,
                               P6885==12 ~ 2,
                               .default = 3),
             P6886 = cut(P6886,
                         breaks = quantile(P6886, probs = seq(0, 1, 0.2), na.rm = TRUE),
                         include.lowest = TRUE,
                         labels = FALSE),
             P415 = cut(P415,
                        breaks = quantile(P415, probs = seq(0, 1, 0.2), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = FALSE),
             P8624 = if_else(P8624 %in% c(98, 99), NA_real_, P8624),
             P6750 = if_else(P6750 %in% c(98, 99), NA_real_, P6750),
             Ingresos = if_else(
               if_all(c(P8624, P6750), is.na), NA_real_,  # Asigna NA cuando ambas son NA
               rowSums(across(c(P8624, P6750)), na.rm = TRUE)  # Calcula la suma en los demás casos
             ),
             quintil_ingreso = cut(Ingresos,
                                   breaks = quantile(Ingresos, probs = seq(0, 1, 0.2), na.rm = TRUE),
                                   include.lowest = TRUE,
                                   labels = FALSE)) %>% 
      select(-c("P8624", "P6750", "Ingresos") )
    
    # Collapse internet-use responses into three categories: daily, sometimes,
    # and never/other.
    tecnologia <- tecnologia %>% 
      mutate(P1084 = case_when(P1084==1 ~ 1, # Use of internet
                               P1084 %in% 2:4 ~ 2,
                               .default = 3))
    
    # Collapse the housing-tenure variable into owned and not-owned categories.
    tenencia_finan <- tenencia_finan %>% 
      mutate(P5095 = case_when(P5095==1|P5095==2 ~ 1, # Housing tenure
                               .default = 2)) 
    
    # Recode health-related variables and harmonize the pre-2021 and post-2020
    # data structures. Construct indicators for health problems and having children.
    # After 2020, several variables in the "Salud" database changed their code, 
    # so they are processed differently
    if (year %in% 2018:2020) {
      salud <- salud %>% 
        mutate(P3335S1 = case_when(is.na(P1708)~1, 
                                   P1708==2~1, # Has childs
                                   .default = 2),
               P1906 = case_when(rowSums(across(starts_with("P1906S"), as.numeric), na.rm = TRUE) == 32 ~ 1, 
                                 .default = 2), # Has at least one health problem
               P6127 = case_when(P6127 %in% 1:2 ~ 1, # Health
                                 P6127==3 ~ 2,
                                 .default = 3)
        ) %>% 
        select(-c("P1906S1", "P1906S2", "P1906S3", "P1906S4", "P1906S5", "P1906S6", "P1906S7", "P1906S8",
                  "P1708"))
    } else {
      salud <- salud %>% 
        mutate(P1906 = case_when(rowSums(across(starts_with("P1906S"), as.numeric), na.rm = TRUE) == 32 ~ 1, 
                                 .default = 2), # Has at least one health problem
               P6127 = case_when(P6127 %in% 1:2 ~ 1, # Health
                                 P6127==3 ~ 2,
                                 .default = 3),
               P3335S1 = case_when(is.na(P3335S1)~1,
                                   .default = 2) # Has childs
        ) %>% 
        select(-c("P1906S1", "P1906S2", "P1906S3", "P1906S4", "P1906S5", "P1906S6", "P1906S7", "P1906S8"))
    }
    
    # Standardize column names before merging: in these source files, the order
    # of ORDEN and SECUENCIA_P differs from the expected order.
    # The columns ORDEN y SECUENCIA from the tenencia_finan data base are inverted
    colnames(tenencia_finan) <- c("DIRECTORIO", "ORDEN", "SECUENCIA_P", "P5095")
    colnames(condiciones_vida) <- c("DIRECTORIO", "ORDEN", "SECUENCIA_P", "P9010", "P5230", "P784S1", "P1077S1")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    ###                      3.Join the dataframes                     ####
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # ============================================================================
    #                         3. MERGE THE DATABASES
    # ============================================================================
    # Add rurality categories using the municipality identifier (MPIO).
    caracteristicas_hogar_2 <- left_join(caracteristicas_hogar, categorias_ruralidad, by = "MPIO")
    
    # First, all dataframes that have information by individual must be joined
    dataframes_individuales <- list(caracteristicas_hogar_2, educacion, trabajo, tecnologia, 
                                    salud)
    df_final_individual <- reduce(dataframes_individuales, 
                                  left_join, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
    
    # Then, all dataframes that have information by home must be joined
    df_por_hogares <- left_join(condiciones_vida, tenencia_finan, 
                                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
    
    # Now, we join the individual dataframe with the home one
    df_por_hogares <- df_por_hogares %>% 
      select(-c("ORDEN"))
    df_casi_final <- df_final_individual %>% 
      left_join(df_por_hogares, by = c("DIRECTORIO", "SECUENCIA_P"))
    
    # Finally this new dataframe is joined with the one with information by dwelling
    # ============================================================================
    #                         2. DATA PREPROCESSING
    # ============================================================================
    # Create a housing-problems indicator from the seven P5661S items.
    # A row sum equal to seven is coded as 1; all other cases are coded as 2.
    datos_vivienda <- datos_vivienda %>% 
      select(-c("ORDEN", "SECUENCIA_P"))
    df_final_total <- df_casi_final %>% 
      left_join(datos_vivienda, by = c("DIRECTORIO"))
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    ###                4.  Prepare the sample design                  ####
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    df_final_total <- df_final_total %>% 
      mutate(postestrato = paste0(CLASE, "-", P1_DEPARTAMENTO))
    
    
    # Change the variable of economic activity according to desired categories
    df_final_total <- df_final_total %>% 
      mutate(P6240 = case_when(P6240==1 ~1,
                               P6240 %in% c(2, 5, 6)~2,
                               P6240==3 ~3,
                               .default = 4)
      )# %>% 
    #select(-c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
    
    # Variables only meaningful for employed people are assigned an explicit
    # "No aplica" category for everyone outside the employed group.
    # Variables que solo tienen sentido si la persona está empleada:
    # en vez de excluir filas/columnas por grupo, se marca la ausencia
    # explícitamente como una categoría "No aplica"
    vars_solo_empleados <- c("P6435", "P6885", "P6886", "P415", "quintil_ingreso")
    
    df_final_total <- df_final_total %>%
      mutate(across(all_of(vars_solo_empleados),
                    ~ ifelse(P6240 != 1, "No aplica", as.character(.))))
    
    # Pregnancy/children information is not applicable to men; remaining missing
    # values are also assigned the explicit category "No aplica".
    # Variables que solo aplican a mujeres (embarazo / tenencia de hijos):
    # para hombres, y para cualquier NA remanente, se marca "No aplica"
    df_final_total <- df_final_total %>%
      mutate(P3335S1 = case_when(P6020 == 1 ~ "No aplica",
                                 is.na(P3335S1) ~ "No aplica",
                                 TRUE ~ as.character(P3335S1)))
    
    # Remove identifiers and variables not intended to act as predictors.
    # P1895 is retained as the response variable and is not included among predictors.
    # Se descartan identificadores y P1895 (no se usa como predictor)
    df_final_total <- df_final_total %>%
      select(-c(ORDEN, MPIO, P1901, CLASE, P1_DEPARTAMENTO)) # P1895 o P1901
    
    # Rename predictors explicitly so that adding or reordering columns does not
    # silently assign incorrect names by position.
    # Renombrado EXPLÍCITO (más seguro que colnames<- por posición, ahora que
    # agregamos columnas de diseño muestral que pueden desordenar los índices)
    df_final_total <- df_final_total %>%
      rename(
        P6020.            = P6020,
        P6240.            = P6240,    
        P6040.            = P6040,
        P756S3.           = P756S3,
        P6080.            = P6080,
        educacion_padres. = educacion_padres,
        nivel_edu.        = nivel_edu,
        P6435.            = P6435,
        P6885.            = P6885,
        P6886.            = P6886,
        P415.             = P415,
        P6920.            = P6920,
        quintil_ingreso.  = quintil_ingreso,
        P1084.            = P1084,
        P6100.            = P6100,
        P6127.            = P6127,
        P3008S1.          = P3008S1,
        P1707.            = P1707,
        P3335S1.          = P3335S1,
        P1906.            = P1906,
        P9010.            = P9010,
        P5230.            = P5230,
        P784S1.           = P784S1,
        P1077S1.          = P1077S1,
        P5095.            = P5095,
        P1070.            = P1070,
        P8520S1.          = P8520S1,
        P8520S5.          = P8520S5,
        P8520S3.          = P8520S3,
        problemas.        = problemas
      )
    
    # Preserve sampling-design variables and identifiers in their original types;
    # convert the remaining analysis variables to factors.
    # Columnas de diseño muestral / pesos que NO deben convertirse en factor
    id_diseno <- c("FEX_C", "postestrato", "DIRECTORIO", "SECUENCIA_P")
    
    df_final_total <- df_final_total %>%
      mutate(across(-all_of(id_diseno), as.factor))
    
    # Set life satisfaction (P1895) as an ordered factor for the ordinal model.
    # The model-fitting block below switches to a survey-weighted binary logit if
    # the response has fewer than three observed levels.
    # Respuesta binaria (por ahora) pero tratada como ordinal para usar svyolr
    df_final_total$P1895 <- factor(df_final_total$P1895, ordered = TRUE)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    ###                     5. Logistic model                     ####
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # ============================================================================
    #                    5. SPECIFY AND ESTIMATE THE MODEL
    # ============================================================================
    # Define the predictors and their reference categories for interpretation.
    #### 5.1 Setting reference levels for variables ####
    
    vars <- c("P6020.", "P6240.", "`Rural/Urbano`", "P6040.", "P756S3.", "P6080.",
              "educacion_padres.", "nivel_edu.", "P6435.", "P6885.", "P6886.",
              "P415.", "P6920.", "quintil_ingreso.", "P1084.", "P6100.",
              "P6127.", "P3008S1.", "P1707.", "P3335S1.", "P1906.",
              "P9010.", "P5230.", "P784S1.", "P1077S1.", "P5095.", "P1070.",
              "P8520S1.", "P8520S5.", "P8520S3.", "problemas.")
    
    refs <- c("1", "1", "Urbano", "3", "1", "4",
              "5", "2", "No aplica", "No aplica", "No aplica",
              "No aplica", "2", "No aplica", "3", "3",
              "3", "1", "1", "No aplica", "2",
              "2", "1", "1", "2", "2", "3",
              "2", "2", "2", "2")
    
    vars_clean <- gsub("`", "", vars)  # nombre real de columna sin backticks
    
    for (i in seq_along(vars_clean)) {
      df_final_total[[vars_clean[i]]] <- relevel(df_final_total[[vars_clean[i]]], ref = refs[i])
    }
    
    # Build the model formula from the selected predictors.
    # Define the complex survey design using household/person identifiers,
    # post-strata, and the expansion weight FEX_C.
    #### 5.2 Diseño muestral ####
    
    predictores <- vars # ya incluye sexo/labor/rural
    formula_mod <- as.formula(paste("P1895 ~", paste(predictores, collapse = " + ")))
    
    ecv_design <- svydesign(data = df_final_total,
                            ids = ~DIRECTORIO + SECUENCIA_P,
                            strata = ~postestrato,
                            weights = ~FEX_C,
                            nest = TRUE)
    
    # Choose the model according to the number of observed response categories.
    # With two levels, use a survey-weighted quasibinomial logit; with three or more,
    # use the survey-weighted ordinal logistic model svyolr.
    modelo <- if (nlevels(df_final_total$P1895) < 3) {
      # svyolr EXIGE 3+ niveles en la respuesta y falla con una binaria.
      # Para 2 categorías, el logit ordenado colapsa matemáticamente en un logit binario 
      # ponderado -> se usa svyglm(quasibinomial), que es el equivalente.
      svyglm(formula_mod, design = ecv_design, family = quasibinomial())
    } else {
      svyolr(formula_mod, design = ecv_design)
    }
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    ###                      6. Save the results                       ####
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # ============================================================================
    #                         6. SAVE THE RESULTS
    # ============================================================================
    # Extract estimates, confidence intervals, and p-values from the fitted model.
    # Regression coefficient vector
    coefes <- coef(modelo)
    # Confidence intervals of the estimated coefficients
    ci <- confint.default(modelo)
    # p-values for significance
    pvals <- data.frame(year = year,
                        variable = names(coefes),
                        p_value = summary(modelo)$coefficients[, "Pr(>|t|)"],
                        row.names = NULL)
    p_values_list[[as.character(year)]] <- pvals
    
    # Create a dataframe with the information of variables, their coefficients and IC
    df_tmp <- data.frame(variable = names(coefes),
                         estimate = coefes,
                         odds_ratio = exp(coefes),
                         conf_low = ci[, 1],
                         conf_high = ci[, 2],
                         year = year)
    
    
    # Store this year's coefficient table under its year so the results can be
    # combined or inspected after all years have been processed.
    # Add the temporal dataframe under the year name
    list_coef_final[[as.character(year)]] <- df_tmp
    
  }
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  ###                           7. Outputs                           ####
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  # Backup list that will save the lists of all regressions
  lista_RData_final <- list()
  
  # p-values
  p_values_df <- bind_rows(p_values_list) %>% 
    mutate(significant = p_value < 0.05)
  
  # ---- 1) Global significance ----
  frecuencia_global <- p_values_df %>%
    group_by(variable) %>%
    summarise(veces_sig = sum(significant, na.rm = TRUE),
              total = n(),
              porcentaje = veces_sig / total * 100)
  
  # Save the yearly coefficient results and the global significance summary
  # in separate .RData files in the main project directory.
  # Saves an .RData file with the backup list
  setwd(path)
  save(list_coef_final, file = "Satisfaction_logit_2026.RData")
  save(frecuencia_global, file = "Satisfaction_p_vals_2026.RData")
  
}