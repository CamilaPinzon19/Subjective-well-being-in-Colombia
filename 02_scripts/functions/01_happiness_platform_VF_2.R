# ==============================================================================
#                        HAPPINESS PLATFORM - MAIN ANALYSIS FUNCTION
# ==============================================================================
# Description: Core function for analyzing happiness determinants in Colombia
#              using ENCV (National Quality of Life Survey) data.
#              Performs logistic regression analysis across multiple demographic
#              and socioeconomic subgroups.
# ==============================================================================

# Main function that executes the complete happiness analysis pipeline
# Parameters:
#   path: string - Path to the directory containing ENCV data folders by year
#   years: numeric vector - Years to be analyzed (e.g., 2018:2024)
run_happiness_platform_VF <- function(path, 
                                   years){
  
  # ============================================================================
  #                          1. INITIALIZATION
  # ============================================================================
  
  # Define all possible regression types based on different demographic subgroups
  # Naming convention: [activity][gender][location]
  # t=trabajando(employed), d=desempleado(unemployed), e=estudiando(studying), o=hogar(domestic work)
  # m=mujer(women), h=hombre(men)  
  # r=rural, u=urbano(urban)
  regression_type <- c("tmr", "tmu", "thr", "thu",    # Employed: women/men + rural/urban
                       "dmr", "dmu", "dhr", "dhu",    # Unemployed: women/men + rural/urban
                       "emr", "emu", "ehr", "ehu",    # Studying: women/men + rural/urban
                       "omr", "omu", "ohr", "ohu")    # Domestic work: women/men + rural/urban
  
  # Initialize empty lists to store regression coefficients and adj r2 for each analysis type
  # Each list will contain coefficient and adj r2 results for all years analyzed
  for (type in regression_type) {
    # Dynamically create and initialize lists with names like "list_coeficients_tmr", etc.
    assign(paste0("list_coeficients_", type), list())
    assign(paste0("list_r2_", type), list())
  }
  
  # ============================================================================
  #                          2. YEAR-BY-YEAR PROCESSING
  # ============================================================================
  
  # Main loop: Process each year specified in the years parameter
  for (year in years) {
    cat("\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat("PROCESSING YEAR:", year, "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    
    # Set working directory for current year
    year_path <- here(file.path(path,paste0("raw/ENCV_",as.character(year))))
    if (!dir.exists(year_path)) {
      warning(paste("Directory not found for year", year, ":", year_path))
      next
    }
    setwd(year_path)
    
    # ========================================================================
    #                        2.1 DATABASE LOADING
    # ========================================================================
    
    cat("Loading databases...\n")
    
    # Define delimiter based on year (2021 uses comma, others use semicolon)
    delim_to_use <- if (year == 2021) "," else ";"

    # Read education database
    educacion <- read_delim("Educacion.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                            col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P8587", "P1088"),
                            show_col_types = FALSE)
    
    # Read labor force database  
    trabajo <- read_delim("Fuerza de trabajo.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                          col_select = c("DIRECTORIO", "SECUENCIA_P","ORDEN", "P6240",
                                         "P6435", "P8624", "P6750", "P6885", "P6886", "P415", "P6920"),
                          show_col_types = FALSE)
    
    # Read housing data database
    datos_vivienda <- read_delim("Datos de la vivienda.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                 col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN",
                                                "P5661S1", "P5661S2", "P5661S3", "P5661S4", "P5661S5", "P5661S6", "P5661S7",
                                                "P1070", "P8520S1", "P8520S5", "P8520S3", "P8520S4"),
                                 show_col_types = FALSE)
    
    # Read technology and communication database
    tecnologia <- read_delim("Tecnologias de informacion y comunicacion.CSV", delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                             col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P1084"),
                             show_col_types = FALSE)
    
    # Read housing tenure and financing database
    tenencia_finan <- read_delim("Tenencia y financiacion de la vivienda que ocupa el hogar.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                 col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P5095"),
                                 show_col_types = FALSE)
    
    # Read living conditions and asset ownership database
    condiciones_vida <- read_delim("Condiciones de vida del hogar y tenencia de bienes.CSV", 
                                   delim = delim_to_use, escape_double = FALSE, trim_ws = TRUE,
                                   col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P9010", "P5230", "P784S1", 
                                                  "P1077S1"),
                                   show_col_types = FALSE)
    
    # Read household characteristics and composition database
    caracteristicas_hogar <- read_delim("Caracteristicas y composicion del hogar.CSV", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                        col_select =c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6020", "P6040",
                                                      "P756S3", "P6087", "P6088", "P1895", "P1901", "P6080"),
                                        show_col_types = FALSE)
    
    # Read rurality categories database
    categorias_ruralidad <- read_delim("Categorias_de_Ruralidad.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                       show_col_types = FALSE)
    
    # Load health data (variable structure changes after 2020)
    if (year %in% 2018:2020) {
      salud <- read_delim(
        "Salud.CSV", 
        delim = delim_to_use, 
        escape_double = FALSE, 
        trim_ws = TRUE, 
        col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6100", "P6181", 
                       "P5694", "P6127", "P1906S1", "P1906S2", "P1906S3", 
                       "P1906S4", "P1906S5", "P1906S6", "P1906S7", "P1906S8", 
                       "P3008S1", "P1707", "P1708"),
        show_col_types = FALSE
      )
    } else {
      salud <- read_delim(
        "Salud.CSV", 
        delim = delim_to_use, 
        escape_double = FALSE, 
        trim_ws = TRUE, 
        col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "P6100", "P6181", 
                       "P5694", "P6127", "P1906S1", "P1906S2", "P1906S3", 
                       "P1906S4", "P1906S5", "P1906S6", "P1906S7", "P1906S8", 
                       "P3008S1", "P1707", "P3335S1"),
        show_col_types = FALSE
      )
    }
    
    # Load sampling design variables (format differs between 2018-2022 and later years)
    if (year %in% 2018:2022) {
      Variables_diseno_muestral <- read_excel(here(paste0("01_data/processed/Variables_diseno_muestral_",year,".xlsx")))
      caracteristicas_hogar <- left_join(
        caracteristicas_hogar, 
        Variables_diseno_muestral[, c("DIRECTORIO", "MPIO")], 
        by = "DIRECTORIO"
      )
      caracteristicas_hogar$MPIO <- as.numeric(caracteristicas_hogar$MPIO)
    } else {
      Variables_diseno_muestral <- read_delim(
        "Variables diseno muestral.CSV", 
        delim = ";", 
        escape_double = FALSE, 
        trim_ws = TRUE,
        col_select = c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "MPIO"),
        show_col_types = FALSE
      )
      caracteristicas_hogar <- left_join(
        caracteristicas_hogar, 
        Variables_diseno_muestral, 
        by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN")
      )
      caracteristicas_hogar$MPIO <- as.numeric(caracteristicas_hogar$MPIO)
    }
    
    
    # ========================================================================
    #                        2.2 DATA PREPROCESSING
    # ========================================================================    
    
    cat("Preprocessing data...\n")
    
    # Process housing problems variable
    # Create binary indicator: 1 if no housing problems (all P5661S variables = 1), 2 if any problems exist
    datos_vivienda <- datos_vivienda %>% 
      mutate(problemas = case_when(rowSums(across(starts_with("P5661S"), as.numeric), na.rm = TRUE) == 7 ~ 1, 
                                   .default = 2) ) %>% # Problems with their housing
      select(-c("P5661S1", "P5661S2", "P5661S3", "P5661S4", "P5661S5", "P5661S6", "P5661S7"))
    
    # Process household characteristics with year-specific nationality coding
    if (year==2018){
      # Special processing for 2018 data (different nationality coding structure)
      caracteristicas_hogar <- caracteristicas_hogar %>%
        filter(P6040 > 14)  %>% # Age filter for individuals over 15 years old
        mutate(P756S3 = case_when(P756S3==3~3, # Nationality: 3=Other, everything else=1(Colombian)
                                  .default = 1),
               P6040 = case_when(P6040 >= 60 ~ 3, # Age groups: 1=Young(<27), 2=Adult(27-59), 3=Elderly(60+)
                                 P6040 < 27 ~ 1,
                                 .default = 2),
               P6087 = case_when(P6087 %in% 1:2 ~ 1, # Father's education levels (recoded to 5 categories)
                                 P6087 %in% 3:4 ~ 2,
                                 P6087 %in% 5:7 ~ 3,
                                 P6087==8 ~ 4,
                                 P6087==9 ~ 5,
                                 .default = NA),
               P6088 = case_when(P6088 %in% 1:2 ~ 1, # Mother's education levels (recoded to 5 categories)
                                 P6088 %in% 3:4 ~ 2,
                                 P6088 %in% 5:7 ~ 3,
                                 P6088==8 ~ 4,
                                 P6088==9 ~ 5,
                                 .default = NA),
               P1895 = case_when(P1895 %in% 0:6 ~ 1, # Life satisfaction (binary: 1=satisfied(0-6), 2=unsatisfied(7-10))
                                 .default = 2),
               P1901 = case_when(P1901 %in% 0:6 ~ 1, # Happiness level (binary: 1=happy(0-6), 2=unhappy(7-10))
                                 .default = 2),
               P6080 = case_when(P6080==1 ~ 1, # Ethnicity: 1=Indigenous, 2=Gypsy, 3=Afrodescendant, 4=None
                                 P6080==2 ~ 2,
                                 P6080 %in% 3:5 ~ 3,
                                 .default = 4)
        )
    }else{
      # Processing for years 2019+ (standard nationality coding)
      caracteristicas_hogar <- caracteristicas_hogar %>%
        filter(P6040 > 14)  %>% # Age filter for individuals over 15 years old
        mutate(P756S3 = case_when(is.na(P756S3) ~ 1, # Nationality: 1=Colombian, 2=Venezuelan, 3=Other
                                  P756S3==3 ~ 2, 
                                  .default = 3),
               P6040 = case_when(P6040 >= 60 ~ 3, # Age groups: 1=Young(<27), 2=Adult(27-59), 3=Elderly(60+)
                                 P6040 < 27 ~ 1,
                                 .default = 2),
               P6087 = case_when(P6087 %in% 1:2 ~ 1, # Father's education levels (recoded to 5 categories)
                                 P6087 %in% 3:4 ~ 2,
                                 P6087 %in% 5:7 ~ 3,
                                 P6087==8 ~ 4,
                                 P6087==9 ~ 5,
                                 .default = NA),
               P6088 = case_when(P6088 %in% 1:2 ~ 1, # Mother's education levels (recoded to 5 categories)
                                 P6088 %in% 3:4 ~ 2,
                                 P6088 %in% 5:7 ~ 3,
                                 P6088==8 ~ 4,
                                 P6088==9 ~ 5,
                                 .default = NA), 
               P1895 = case_when(P1895 %in% 0:6 ~ 1, # Life satisfaction (binary: 1=satisfied(0-6), 2=unsatisfied(7-10))
                                 .default = 2),
               P1901 = case_when(P1901 %in% 0:6 ~ 1, # Happiness level (binary: 1=happy(0-6), 2=unhappy(7-10))
                                 .default = 2),
               P6080 = case_when(P6080==1 ~ 1, # Ethnicity: 1=Indigenous, 2=Gypsy, 3=Afrodescendant, 4=None
                                 P6080==2 ~ 2,
                                 P6080 %in% 3:5 ~ 3,
                                 .default = 4)
        )
    }
    
    # Create combined parental education variable to avoid multicollinearity
    # Takes the maximum education level between father and mother
    caracteristicas_hogar <- caracteristicas_hogar %>% 
      mutate(educacion_padres = apply(caracteristicas_hogar[,c("P6087", "P6088")], 1, function(x) {
        # Parents' highest educational level
        if (all(is.na(x))) NA else max(x, na.rm = TRUE)
      })
      ) %>% 
      select(-c("P6087", "P6088"))
    
    # Process education variables
    # Combine different education variables into a comprehensive education level indicator
    educacion <- educacion %>% 
      mutate(P8587 = case_when(P8587==1 ~ 1,        # Recode education completed levels
                               P8587 %in% 2:3 ~ 2,
                               P8587 %in% 4:5 ~ 3,
                               P8587 %in% 6:10 ~ 4,
                               P8587==11 ~ 5,
                               P8587 %in% 12:13 ~ 6),
             P1088 = case_when(P1088 %in% 1:2~2,    # Recode current education levels
                               P1088 %in% 3:4~3,
                               P1088 %in% 5:6~4,
                               P1088==7~5,
                               P1088==8~6),
             nivel_edu = rowSums(across(c(P8587, P1088)), na.rm = TRUE) # Combined educational level
      ) %>% 
      select(-c("P8587", "P1088"))
    
    # Process work/employment variables
    trabajo <- trabajo %>% 
      mutate(P6435 = case_when(P6435 %in% 1:2 ~ 1,       # Work position: 1=Employee, 2=Domestic, 3=Self-employed, 4=Employer, 5=Farm worker, 6=Other
                               P6435==3 ~ 2,
                               P6435 %in% 4:5 ~ 3,
                               P6435==6 ~ 4,
                               P6435==7|P6435==10 ~ 5,
                               P6435 %in% 8:9 ~ 6),
             P6885 = case_when(P6885==8|P6885==11 ~ 1,    # Transportation to work: 1=Walking/bicycle, 2=Bicycle, 3=Other
                               P6885==12 ~ 2,
                               .default = 3),
             P6886 = cut(P6886,                           # Commute time quintiles
                         breaks = quantile(P6886, probs = seq(0, 1, 0.2), na.rm = TRUE),
                         include.lowest = TRUE,
                         labels = FALSE),
             P415 = cut(P415,                             # Working hours quintiles
                        breaks = quantile(P415, probs = seq(0, 1, 0.2), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = FALSE),
             P8624 = if_else(P8624 %in% c(98, 99), NA_real_, P8624), # Clean income variables (remove invalid codes)
             P6750 = if_else(P6750 %in% c(98, 99), NA_real_, P6750),
             Ingresos = if_else(                          # Combined income from multiple sources
               if_all(c(P8624, P6750), is.na), NA_real_,  # Assign NA when both income sources are NA
               rowSums(across(c(P8624, P6750)), na.rm = TRUE)  # Calculate sum for other cases
             ),
             quintil_ingreso = cut(Ingresos,              # Income quintiles
                                   breaks = quantile(Ingresos, probs = seq(0, 1, 0.2), na.rm = TRUE),
                                   include.lowest = TRUE,
                                   labels = FALSE)) %>% 
      select(-c("P8624", "P6750", "Ingresos") )
    
    # Process technology access variables
    tecnologia <- tecnologia %>% 
      mutate(P1084 = case_when(P1084==1 ~ 1,      # Internet use: 1=Daily, 2=Sometimes, 3=Never
                               P1084 %in% 2:4 ~ 2,
                               .default = 3))
    
    # Process housing tenure variables
    tenencia_finan <- tenencia_finan %>% 
      mutate(P5095 = case_when(P5095==1|P5095==2 ~ 1, # Housing tenure: 1=Owned, 2=Not owned
                               .default = 2)) 
    
    # Process health variables with year-specific handling
    # Variable structure changed after 2020
    if (year %in% 2018:2020) {
      # Processing for 2018-2020 data structure
      salud <- salud %>% 
        mutate(P3335S1 = case_when(is.na(P1708)~1,      # Has children indicator (derived from P1708)
                                   P1708==2~1, 
                                   .default = 2),
               P1906 = case_when(rowSums(across(starts_with("P1906S"), as.numeric), na.rm = TRUE) == 32 ~ 1,  # Health problems (binary)
                                 .default = 2), 
               P6181 = case_when(P6181 %in% 1:2 ~ 1,      # Health service rating: 1=Good, 2=Bad
                                 P6181 %in% 2:3 ~ 2,
                                 .default = NA),
               P6127 = case_when(P6127 %in% 1:2 ~ 1,      # Health status: 1=Good, 2=Regular, 3=Poor
                                 P6127==3 ~ 2,
                                 .default = 3),
               P5694 = case_when(is.na(P5694)~2,          # Pregnancy status: 1=Yes, 2=No
                                 P5694==3~2,
                                 .default = 1)
        ) %>% 
        select(-c("P1906S1", "P1906S2", "P1906S3", "P1906S4", "P1906S5", "P1906S6", "P1906S7", "P1906S8",
                  "P1708"))
    } else {
      # Processing for 2021+ data structure
      salud <- salud %>% 
        mutate(P1906 = case_when(rowSums(across(starts_with("P1906S"), as.numeric), na.rm = TRUE) == 32 ~ 1,  # Health problems (binary)
                                 .default = 2), 
               P6181 = case_when(P6181 %in% 1:2 ~ 1,      # Health service rating: 1=Good, 2=Bad
                                 P6181 %in% 2:3 ~ 2,
                                 .default = NA), 
               P6127 = case_when(P6127 %in% 1:2 ~ 1,      # Health status: 1=Good, 2=Regular, 3=Poor
                                 P6127==3 ~ 2,
                                 .default = 3),
               P5694 = case_when(is.na(P5694)~2,          # Pregnancy status: 1=Yes, 2=No
                                 P5694==3~2,
                                 .default = 1),
               P3335S1 = case_when(is.na(P3335S1)~1,      # Has children indicator
                                   .default = 2) 
        ) %>% 
        select(-c("P1906S1", "P1906S2", "P1906S3", "P1906S4", "P1906S5", "P1906S6", "P1906S7", "P1906S8"))
    }
    
    # Fix column order issues in some databases (ORDEN and SECUENCIA_P were swapped)
    colnames(tenencia_finan) <- c("DIRECTORIO", "ORDEN", "SECUENCIA_P", "P5095")
    colnames(condiciones_vida) <- c("DIRECTORIO", "ORDEN", "SECUENCIA_P", "P9010", "P5230", "P784S1", "P1077S1")
    
    # ========================================================================
    #                        2.3 DATA MERGING
    # ========================================================================
    
    cat("Merging databases...\n")
    
    # Add rurality information to household characteristics
    caracteristicas_hogar_2 <- left_join(caracteristicas_hogar, categorias_ruralidad, by = "MPIO")
    
    # Combine all individual-level dataframes (person-specific information)
    # All these dataframes contain information at the individual level
    dataframes_individuales <- list(caracteristicas_hogar_2, educacion, trabajo, tecnologia, 
                                    salud)
    df_final_individual <- reduce(dataframes_individuales, 
                                  left_join, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
    
    # Combine household-level dataframes (household-specific information)
    # These dataframes contain information at the household level
    df_por_hogares <- left_join(condiciones_vida, tenencia_finan, 
                                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
    
    # Join individual data with household data
    # Remove ORDEN from household data to avoid duplication in the join
    df_por_hogares <- df_por_hogares %>% 
      select(-c("ORDEN"))
    df_casi_final <- df_final_individual %>% 
      left_join(df_por_hogares, by = c("DIRECTORIO", "SECUENCIA_P"))
    
    # Finally join with dwelling-level data (dwelling-specific information)
    # Remove individual-level keys as dwelling data is at the dwelling level
    datos_vivienda <- datos_vivienda %>% 
      select(-c("ORDEN", "SECUENCIA_P"))
    df_final_total <- df_casi_final %>% 
      left_join(datos_vivienda, by = c("DIRECTORIO"))
    
    # ========================================================================
    #                        2.4 FINAL DATA PREPARATION
    # ========================================================================
    
    cat("Preparing final datasets...\n")
    
    # Recode economic activity variable and prepare final dataset
    df_final_total <- df_final_total %>% 
      mutate(P6240 = case_when(P6240==1 ~1,           # Economic activity: 1=Employed, 2=Unemployed/Seeking, 3=Studying, 4=Domestic work
                               P6240 %in% c(2, 5, 6)~2,
                               P6240==3 ~3,
                               .default = 4)
      ) %>% 
      select(-c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))  # Remove identifier variables
    
    # Add periods to variable names ending in numbers for better regression interpretation
    # This helps distinguish factor levels in regression output
    colnames(df_final_total) <- c("P6020", "P6040.", "P756S3.", "P1895", "P1901", "P6080.", "MPIO", 
                                  "educacion_padres.", "Rural/Urbano", "nivel_edu.", "P6240", "P6435.", 
                                  "P6885.", "P6886.", "P415.", "P6920.", "quintil_ingreso.", "P1084.",
                                  "P6100.", "P6181.", "P5694.", "P6127.", "P3008S1.", "P1707.", "P3335S1.",
                                  "P1906.", "P9010.", "P5230.", "P784S1.", "P1077S1.", "P5095.", "P1070.", 
                                  "P8520S1." , "P8520S5.", "P8520S3.", "P8520S4.", "problemas.")
    
    # ========================================================================
    #                        2.5 CREATE POPULATION SEGMENTS
    # ========================================================================
    
    cat("Creating population segments...\n")
    
    # Employed women in urban areas
    df_trabajo_mujer_urban <- df_final_total %>% 
      filter(P6240==1 & P6020 == 2 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901"))%>%  # Remove filtering variables and alternative happiness measure
      mutate(across(everything(), as.factor))  # Convert all variables to factors for logistic regression
    
    # Employed women in rural areas
    df_trabajo_mujer_rural <- df_final_total %>% 
      filter(P6240==1 & P6020 == 2 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901"))%>% 
      mutate(across(everything(), as.factor))
    
    # Employed men in urban areas
    df_trabajo_hombre_urban <- df_final_total %>% 
      filter(P6240==1 & P6020 == 1 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1."))%>%  # Also remove pregnancy and children variables (male-specific)
      mutate(across(everything(), as.factor))
    
    # Employed men in rural areas
    df_trabajo_hombre_rural <- df_final_total %>% 
      filter(P6240==1 & P6020 == 1 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1."))%>% 
      mutate(across(everything(), as.factor))
    
    
    #### 4.2 Unemployed datasets ####
    # Create separate datasets for unemployed individuals (excluding employment-specific variables)
    
    # Unemployed women in urban areas
    df_desempleo_mujer_urban <- df_final_total %>% 
      filter(P6240==2 & P6020 == 2 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P6435.", "P6885.", "P6886.", "P415.", 
                "quintil_ingreso."))%>%  # Remove work-related variables not applicable to unemployed
      mutate(across(everything(), as.factor))
    
    # Unemployed women in rural areas
    df_desempleo_mujer_rural <- df_final_total %>% 
      filter(P6240==2 & P6020 == 2 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P6435.", "P6885.", "P6886.", "P415.", 
                "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Unemployed men in urban areas
    df_desempleo_hombre_urban <- df_final_total %>% 
      filter(P6240==2 & P6020 == 1 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1.", "P6435.", "P6885.", 
                "P6886.", "P415.", "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Unemployed men in rural areas
    df_desempleo_hombre_rural <- df_final_total %>% 
      filter(P6240==2 & P6020 == 1 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1.", "P6435.", "P6885.", 
                "P6886.", "P415.", "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    #### 4.3 Studying datasets ####
    # Create separate datasets for students (excluding employment-specific variables)
    
    # Female students in urban areas
    df_estudio_mujer_urban <- df_final_total %>% 
      filter(P6240==3 & P6020 == 2 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P6435.", "P6885.", "P6886.", "P415.", 
                "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Female students in rural areas
    df_estudio_mujer_rural <- df_final_total %>% 
      filter(P6240==3 & P6020 == 2 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P6435.", "P6885.", "P6886.", "P415.", 
                "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Male students in urban areas
    df_estudio_hombre_urban <- df_final_total %>% 
      filter(P6240==3 & P6020 == 1 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1.", "P6435.", "P6885.", 
                "P6886.", "P415.","quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Male students in rural areas
    df_estudio_hombre_rural <- df_final_total %>% 
      filter(P6240==3 & P6020 == 1 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1.", "P6435.", "P6885.", 
                "P6886.", "P415.", "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    ####  4.4 Domestic worker datasets   ####
    # Create separate datasets for individuals doing domestic work (excluding employment-specific variables)
    
    # Women doing domestic work in urban areas
    df_hogar_mujer_urban <- df_final_total %>% 
      filter(P6240==4 & P6020 == 2 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P6435.", "P6885.", "P6886.", "P415.", 
                "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Women doing domestic work in rural areas
    df_hogar_mujer_rural <- df_final_total %>% 
      filter(P6240==4 & P6020 == 2 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P6435.", "P6885.", "P6886.", "P415.", 
                "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Men doing domestic work in urban areas
    df_hogar_hombre_urban <- df_final_total %>% 
      filter(P6240==4 & P6020 == 1 & `Rural/Urbano`=="Urbano") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1.", "P6435.", "P6885.", 
                "P6886.", "P415.", "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # Men doing domestic work in rural areas
    df_hogar_hombre_rural <- df_final_total %>% 
      filter(P6240==4 & P6020 == 1 & `Rural/Urbano`=="Rural") %>% 
      select(-c("P6020", `Rural/Urbano`, "MPIO", "P6240", "P1901", "P5694.", "P3335S1.", "P6435.", "P6885.", 
                "P6886.", "P415.", "quintil_ingreso."))%>% 
      mutate(across(everything(), as.factor))
    
    # ========================================================================
    #                        2.6 SET REFERENCE LEVELS
    # ========================================================================
    
    cat("Setting reference levels for factors...\n")
    
    #### 5.1 Setting reference levels for variables ####
    # Set reference categories for all factor variables to ensure consistent interpretation across models
    
    # Reference levels for employed datasets (includes employment-related variables)
    vars <- c("P6040.", "P756S3.", "P6080.", "educacion_padres.", "nivel_edu.", "P6435.", "P6885.",
              "P6886.", "P415.", "P6920.", "quintil_ingreso.", "P1084.", "P6100.", "P6181.",
              "P5694.", "P6127.", "P3008S1.", "P1707.", "P3335S1.", "P1906.", "P9010.", "P5230.", 
              "P784S1.", "P1077S1.", "P5095.", "P1070.", "P8520S1.", "P8520S5.", "P8520S3.", "P8520S4.","problemas.")
    
    # Corresponding reference categories for each variable
    refs <- c("3", "1", "4", "5", "2", "1", "3", 
              "5", "5", "2", "1", "3", "3", "2",
              "2", "3", "1", "1", "1", "2", "2", "1", 
              "1", "2", "2", "3", "2", "2", "2", "2", "2")
    
    # Apply reference levels to employed women datasets
    for (i in seq_along(vars)) {
      df_trabajo_mujer_rural[[vars[i]]] <- relevel(df_trabajo_mujer_rural[[vars[i]]], ref = refs[i])
      df_trabajo_mujer_urban[[vars[i]]] <- relevel(df_trabajo_mujer_urban[[vars[i]]], ref = refs[i])
    }
    
    # Reference levels for employed men datasets (excludes pregnancy and children variables)
    vars <- c("P6040.", "P756S3.", "P6080.", "educacion_padres.", "nivel_edu.", "P6435.", "P6885.",
              "P6886.", "P415.", "P6920.", "quintil_ingreso.", "P1084.", "P6100.", "P6181.",
              "P6127.", "P3008S1.", "P1707.", "P1906.", "P9010.", "P5230.", 
              "P784S1.", "P1077S1.", "P5095.", "P1070.", "P8520S1.", "P8520S5.", "P8520S3.", "P8520S4.","problemas.")
    
    refs <- c("3", "1", "4", "5", "2", "1", "3", 
              "5", "5", "2", "1", "3", "3", "2",
              "3", "1", "1", "2", "2", "1", 
              "1", "2", "2", "3", "2", "2", "2", "2", "2")
    
    # Apply reference levels to employed men datasets
    for (i in seq_along(vars)) {
      df_trabajo_hombre_rural[[vars[i]]] <- relevel(df_trabajo_hombre_rural[[vars[i]]], ref = refs[i])
      df_trabajo_hombre_urban[[vars[i]]] <- relevel(df_trabajo_hombre_urban[[vars[i]]], ref = refs[i])
    }
    
    # Reference levels for unemployed/studying/domestic work women datasets (excludes employment variables)
    vars <- c("P6040.", "P756S3.", "P6080.", "educacion_padres.", "nivel_edu.", 
              "P6920.", "P1084.", "P6100.", "P6181.", "P5694.",
              "P6127.", "P3008S1.", "P1707.","P3335S1." ,"P1906.", "P9010.", "P5230.", 
              "P784S1.", "P1077S1.", "P5095.", "P1070.", "P8520S1.", "P8520S5.", "P8520S3.", "P8520S4.","problemas.")
    
    refs <- c("3", "1", "4", "5", "2", 
              "2", "3", "3", "2", "2",
              "3", "1", "1", "1", "2", "2", "1", 
              "1", "2", "2", "3", "2", "2", "2", "2", "2")
    
    # Apply reference levels to non-employed women datasets
    for (i in seq_along(vars)) {
      df_desempleo_mujer_rural[[vars[i]]] <- relevel(df_desempleo_mujer_rural[[vars[i]]], ref = refs[i])
      df_desempleo_mujer_urban[[vars[i]]] <- relevel(df_desempleo_mujer_urban[[vars[i]]], ref = refs[i])
      df_estudio_mujer_rural[[vars[i]]] <- relevel(df_estudio_mujer_rural[[vars[i]]], ref = refs[i])
      df_estudio_mujer_urban[[vars[i]]] <- relevel(df_estudio_mujer_urban[[vars[i]]], ref = refs[i])
      df_hogar_mujer_rural[[vars[i]]] <- relevel(df_hogar_mujer_rural[[vars[i]]], ref = refs[i])
      df_hogar_mujer_urban[[vars[i]]] <- relevel(df_hogar_mujer_urban[[vars[i]]], ref = refs[i])
    }
    
    # Reference levels for unemployed/studying/domestic work men datasets (excludes employment and gender-specific variables)
    vars <- c("P6040.", "P756S3.", "P6080.", "educacion_padres.", "nivel_edu.", 
              "P6920.", "P1084.", "P6100.", "P6181.",
              "P6127.", "P3008S1.", "P1707." ,"P1906.", "P9010.", "P5230.", 
              "P784S1.", "P1077S1.", "P5095.", "P1070.", "P8520S1.", "P8520S5.", "P8520S3.", "P8520S4.","problemas.")
    
    refs <- c("3", "1", "4", "5", "2", 
              "2", "3", "3", "2",
              "3", "1", "1", "2", "2", "1", 
              "1", "2", "2", "3", "2", "2", "2", "2", "2")
    
    # Apply reference levels to non-employed men datasets
    for (i in seq_along(vars)) {
      df_desempleo_hombre_rural[[vars[i]]] <- relevel(df_desempleo_hombre_rural[[vars[i]]], ref = refs[i])
      df_desempleo_hombre_urban[[vars[i]]] <- relevel(df_desempleo_hombre_urban[[vars[i]]], ref = refs[i])
      df_estudio_hombre_rural[[vars[i]]] <- relevel(df_estudio_hombre_rural[[vars[i]]], ref = refs[i])
      df_estudio_hombre_urban[[vars[i]]] <- relevel(df_estudio_hombre_urban[[vars[i]]], ref = refs[i])
      df_hogar_hombre_rural[[vars[i]]] <- relevel(df_hogar_hombre_rural[[vars[i]]], ref = refs[i])
      df_hogar_hombre_urban[[vars[i]]] <- relevel(df_hogar_hombre_urban[[vars[i]]], ref = refs[i])
    }
    
    # ========================================================================
    #                        2.7 LOGISTIC REGRESSION MODELS
    # ========================================================================
    
    cat("Running logistic regression models...\n")
    
    # Employed subgroup models
    mod_tmr <- glm(P1895 ~ ., data = df_trabajo_mujer_rural, family = binomial)     # Employed women, rural
    mod_tmu <- glm(P1895 ~ ., data = df_trabajo_mujer_urban, family = binomial)     # Employed women, urban
    mod_thr <- glm(P1895 ~ ., data = df_trabajo_hombre_rural, family = binomial)    # Employed men, rural
    mod_thu <- glm(P1895 ~ ., data = df_trabajo_hombre_urban, family = binomial)    # Employed men, urban
    
    # Unemployed subgroup models
    mod_dmr <- glm(P1895 ~ ., data = df_desempleo_mujer_rural, family = binomial)   # Unemployed women, rural
    mod_dmu <- glm(P1895 ~ ., data = df_desempleo_mujer_urban, family = binomial)   # Unemployed women, urban
    mod_dhr <- glm(P1895 ~ ., data = df_desempleo_hombre_rural, family = binomial)  # Unemployed men, rural
    mod_dhu <- glm(P1895 ~ ., data = df_desempleo_hombre_urban, family = binomial)  # Unemployed men, urban
    
    # Student subgroup models
    mod_emr <- glm(P1895 ~ ., data = df_estudio_mujer_rural, family = binomial)     # Female students, rural
    mod_emu <- glm(P1895 ~ ., data = df_estudio_mujer_urban, family = binomial)     # Female students, urban
    mod_ehr <- glm(P1895 ~ ., data = df_estudio_hombre_rural, family = binomial)    # Male students, rural
    mod_ehu <- glm(P1895 ~ ., data = df_estudio_hombre_urban, family = binomial)    # Male students, urban
    
    # Domestic work subgroup models
    mod_omr <- glm(P1895 ~ ., data = df_hogar_mujer_rural, family = binomial)       # Women in domestic work, rural
    mod_omu <- glm(P1895 ~ ., data = df_hogar_mujer_urban, family = binomial)       # Women in domestic work, urban
    mod_ohr <- glm(P1895 ~ ., data = df_hogar_hombre_rural, family = binomial)      # Men in domestic work, rural
    mod_ohu <- glm(P1895 ~ ., data = df_hogar_hombre_urban, family = binomial)      # Men in domestic work, urban
    
    # ========================================================================
    #                        2.8 EXTRACT AND STORE RESULTS
    # ========================================================================
    
    cat("Extracting results...\n")
    
    # Extract and store regression results for each model type
    # This process creates standardized coefficient dataframes and McFadden's pseudo adjusted r2
    # for all 16 regression models
    
    for (type in regression_type) {
      # Get the corresponding fitted model object
      modelo <- get(paste0("mod_", type))
      
      # Extract regression coefficients
      coefes <- coef(modelo)
      
      # Calculate confidence intervals for coefficients (using normal approximation)
      ci <- confint.default(modelo)
      
      # Create a structured dataframe with coefficient information
      df_tmp <- data.frame(
        variable = names(coefes),       # Variable names
        estimate = coefes,              # Coefficient estimates
        odds_ratio = exp(coefes),       # Odds ratios
        conf_low = ci[, 1],             # Lower confidence interval bound
        conf_high = ci[, 2],            # Upper confidence interval bound
        year = year)                    # Year identifier
      
      # Save McFadden's pseudo adjusted R2 
      r2_adj <- r2_mcfadden(modelo)$R2_adjusted
      
      # Retrieve the existing results lists for this regression type
      lista_actual <- get(paste0("list_coeficients_", type))
      lista_r2_actual <- get(paste0("list_r2_", type))
      
      # Add current year's results to the lists
      lista_actual[[as.character(year)]] <- df_tmp
      lista_r2_actual[[as.character(year)]] <- r2_adj
      
      # Update the lists in the global environment
      assign(paste0("list_coeficients_", type), lista_actual)
      assign(paste0("list_r2_", type), lista_r2_actual)
    }
    
  }
  # ============================================================================
  #                          3. SAVE FINAL RESULTS
  # ============================================================================
  
  cat("\nSaving final results...\n")
  
  # Create master lists to consolidate all regression results
  lista_RData_final <- list()
  lista_r2_final <- list()
  
  # Populate master lists with results from all 16 regression types
  # Each element of the first list contains time series of coefficients across all analyzed years
  # The second one contains adj r2 across all analyzed years
  for (type in regression_type) {
    lista_RData_final[[type]] <- get(paste0("list_coeficients_", type))
    lista_r2_final[[type]] <- get(paste0("list_r2_", type))
  }
  
  # Save comprehensive results to RData file
  setwd(path)  # Return to main project directory
  save(lista_RData_final, file = "Happiness_summary.RData")
  save(lista_r2_final, file = "Hapiness_adj_r2.RData")
  
}