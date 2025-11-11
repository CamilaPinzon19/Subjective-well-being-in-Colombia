# ==============================================================================
#                        PLOT ESTIMATIONS - COEFFICIENT VISUALIZATION
# ==============================================================================
# Description: Script for generating visualization plots of regression coefficients
#              from happiness analysis in Colombia using ENCV data.
#              Creates time series plots showing coefficient evolution over years.
# ==============================================================================

# ------------------------------------------------------------------------------
#                           Load libraries and set path
# ------------------------------------------------------------------------------

# Load required libraries for data manipulation and plotting
packages <- c('here', 'ggplot2', 'dplyr', 'purrr')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))
invisible(gc())

# Load the RData file containing the regression results from happiness analysis
# This file should contain 'lista_RData_final' object with coefficient estimates
load(here("03_outputs/data/Happiness_summary.RData"))

# ------------------------------------------------------------------------------
#                           Modifications to the list
# ------------------------------------------------------------------------------

# Create the main output directory for coefficient plots
# recursive = TRUE creates parent directories if they don't exist
# showWarnings = FALSE suppresses warnings if directory already exists
dir.create(here("03_outputs/figures/Plots_coefs_Proof"), recursive = TRUE, showWarnings = FALSE)

# Main loop: iterate through each analysis type in the results list
# Each 'tipo' represents a different model specification or subset analysis
for (tipo in names(lista_RData_final)) {
  
  # Combine coefficient data from all years for the current analysis type
  # bind_rows() stacks dataframes vertically, combining results across years
  df_coefs_all <- bind_rows(lista_RData_final[[tipo]])
  
  # Remove row names to clean up the combined dataframe
  rownames(df_coefs_all) <- NULL
  
  # Extract unique variable names from the coefficient data
  # This will be used to create variable groups for plotting
  unique_var <- unique(df_coefs_all$variable)
  
  # Create a named vector mapping technical variable codes to human-readable labels
  # This makes the plots more interpretable by showing descriptive names
  label_names <- c("(Intercept)" = "(Intercept)",
                   
                   # Demographic variables
                   "P6040.1"     = "Age: Youth", 
                   "P6040.2"     = "Age: Adulthood", 
                   "P756S3.2"    = "Nationality: Venezuelan",
                   "P756S3.3"    = "Nationality: Other", 
                   "P6080.1"     = "Ethnicity: Indigenous", 
                   "P6080.2"     = "Ethnicity: Gypsy",
                   "P6080.3"     = "Ethnicity: Afrodescendant",
                   
                   # Parental education variables
                   "educacion_padres.1" = "Parents' highest educational level: Elementary school", 
                   "educacion_padres.2" = "Parents' highest educational level: High school",
                   "educacion_padres.3" = "Parents' highest educational level: Technical training", 
                   "educacion_padres.4" = "Parents' highest educational level: University", 
                   
                   # Individual education variables
                   "nivel_edu.1" = "Educational level: No formal education",
                   "nivel_edu.3" = "Educational level: High school", 
                   "nivel_edu.4" = "Educational level: Technical training", 
                   "nivel_edu.5" = "Educational level: University", 
                   "nivel_edu.6" = "Educational level: Post-graduate", 
                   
                   # Employment and work-related variables
                   "P6435.2"     = "Work: Domestic work", 
                   "P6435.3"     = "Work: Self-employed",
                   "P6435.4"     = "Work: Employer", 
                   "P6435.5"     = "Work: Ground/farm worker", 
                   "P6885.1"     = "Transport to work: car",
                   "P6885.2"     = "Transport to work: bicycle",
                   
                   # Commute time quintiles (travel time to work)
                   "P6886.1"     = "Commute time to work: 1st quintile",
                   "P6886.2"     = "Commute time to work: 2nd quintile", 
                   "P6886.3"     = "Commute time to work: 3rd quintile", 
                   "P6886.4"     = "Commute time to work: 4th quintile",
                   
                   # Working hours quintiles
                   "P415.1"      = "Working hours: 1st quintile", 
                   "P415.2"      = "Working hours: 2nd quintile", 
                   "P415.3"      = "Working hours: 3rd quintile", 
                   "P415.4"      = "Working hours: 4th quintile", 
                   
                   # Pension and retirement status
                   "P6920.1"     = "Pension: Contributing", 
                   "P6920.3"     = "Pension: Retired", 
                   
                   # Income quintiles
                   "quintil_ingreso.2" = "Income: 2nd quintile", 
                   "quintil_ingreso.3" = "Income: 3rd quintile", 
                   "quintil_ingreso.4" = "Income: 4th quintile", 
                   "quintil_ingreso.5" = "Income: 5th quintile", 
                   
                   # Technology access variables
                   "P1084.1"    = "Internet: Use it everyday",
                   "P1084.2"    = "Internet: Sometimes uses it", 
                   
                   # Health system and health status variables
                   "P6100.1"    = "Health scheme: Contributed", 
                   "P6100.2"    = "Health scheme: Special",
                   "P6181.1"    = "Rating to the HSSI: Very good or good",  # (health social security institution) 
                   "P5694.1"    = "Pregnancy: Yes",  
                   "P6127.1"    = "Health: Good", 
                   "P6127.2"    = "Health: Regular",
                   
                   # Lifestyle and behavioral variables
                   "P3008S1.2"  = "Smokes cigarettes or tobacco: No", 
                   "P1707.2"    = "Drink sugary drinks: No", 
                   "P3335S1.2"  = "Has childs: Yes", 
                   "P1906.1"    = "Has at least one health problem: No", 
                   
                   # Social and economic perception variables
                   "P9010.1"    = "Security: Feels secure", 
                   "P5230.2"    = "Considers himself poor: No", 
                   "P784S1.2"   = "Subsidies: Not receiving", 
                   "P1077S1.1"  = "Washing machine: Yes", 
                   
                   # Housing characteristics
                   "P5095.1"    = "Housing tenure: Own",  
                   "P1070.1"    = "Type of housing: House", 
                   "P1070.2"    = "Type of housing: Apartment",
                   "P1070.4"    = "Type of housing: Traditional indigenous housing", 
                   "P1070.5"    = "Type of housing: Other",
                   
                   # Utility access variables
                   "P8520S1.1"  = "Electrical power: Yes", 
                   "P8520S5.1"  = "Water supply: Yes", 
                   "P8520S3.1"  = "Sewer: Yes", 
                   "P8520S4.1"  = "Garbage collection: Yes",
                   "problemas.1" = "Problems with their housing: No")
  
  # Add human-readable variable names to the coefficient dataframe
  # This creates a new column 'var_name' with descriptive labels for plotting
  df_coefs_all <- df_coefs_all %>%
    mutate(var_name = label_names[variable])
  
  # Split variables into groups of 9 for creating separate plot panels
  # ceiling(seq_along(unique_var) / 9) creates group indicators (1,1,1...1,2,2,2...2,3,3,3...)
  # This ensures each plot panel has maximum 9 variables for readability
  groups_variables <- split(unique_var, ceiling(seq_along(unique_var) / 9))
  
  # ------------------------------------------------------------------------------
  #                           Plots
  # ------------------------------------------------------------------------------
  
  # Create a list of plots, one for each variable group
  # map() applies the plotting function to each group of variables
  list_plots <- map(groups_variables, function(vars_grupo) {
    
    # Filter the coefficient data to include only variables in the current group
    df_sub <- df_coefs_all %>%
      filter(variable %in% vars_grupo)
    
    # Create the ggplot object with time series visualization
    ggplot(df_sub, aes(x = year, y = estimate, color = var_name)) +
      
      # Add line plot showing coefficient evolution over time
      # group = variable ensures each variable gets its own line
      geom_line(aes(group = variable)) +
      
      # Add confidence interval ribbons around the coefficient estimates
      # alpha = 0.2 makes ribbons semi-transparent
      # color = NA removes ribbon borders for cleaner appearance
      geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = var_name), alpha = 0.2, color = NA) +
      
      # Create separate panels for each variable using facet_wrap
      # scales = "free_y" allows each panel to have its own y-axis range
      facet_wrap(~ var_name, scales = "free_y") +
      
      # Apply minimal theme for clean, professional appearance
      theme_minimal() +
      
      # Add plot labels and titles
      labs(
        title = "Evolution of regression coefficients",
        x = "Year", 
        y = "Estimation of the coefficients"
      ) +
      
      # Remove legend since variable names are shown in panel titles
      theme(legend.position = "none")
  })
  
  # ------------------------------------------------------------------------------
  #                           Outputs
  # ------------------------------------------------------------------------------
  
  # Create a subdirectory for the current analysis type within the main plots folder
  # This organizes plots by analysis type for better file management
  folder_path <- file.path(paste0(here("03_outputs/figures/Plots_coefs_Proof"),"/", tipo))
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
  # Save each plot panel as a PNG file
  # seq_along(list_plots) creates sequence 1, 2, 3... for numbering panels
  for (i in seq_along(list_plots)) {
    ggsave(
      # Generate filename with panel number and analysis type
      filename = file.path(folder_path, paste0("panel_", i, "_type_", tipo, ".png")),
      plot = list_plots[[i]],      # Plot object to save
      width = 12, height = 8       # Plot dimensions in inches
    )
  }
}