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
packages <- c('here', 'ggplot2', 'dplyr', 'purrr', 'ggh4x', 'patchwork')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))
invisible(gc())

# Load the RData file containing the regression results from happiness analysis
# This file should contain 'lista_RData_final' object with coefficient estimates
load(here("03_outputs/data/Satisfaction_logit_2026.RData"))

# ------------------------------------------------------------------------------
#                           Modifications to the list
# ------------------------------------------------------------------------------

# Create the main output directory for coefficient plots
# recursive = TRUE creates parent directories if they don't exist
# showWarnings = FALSE suppresses warnings if directory already exists
dir.create(here("03_outputs/figures/Plots_coefs_Proof"), recursive = TRUE, showWarnings = FALSE)


# Combine coefficient data from all years for the current analysis type
# bind_rows() stacks dataframes vertically, combining results across years
df_coefs_all <- bind_rows(list_coef_final)
  
# Remove row names to clean up the combined dataframe
rownames(df_coefs_all) <- NULL
  
# Extract unique variable names from the coefficient data
# This will be used to create variable groups for plotting
unique_var <- unique(df_coefs_all$variable)
  
# Create a named vector mapping technical variable codes to human-readable labels
# This makes the plots more interpretable by showing descriptive names
label_names <- c("(Intercept)" = "(Intercept)",
                 
                 # Demographic variables
                 "P6020.2"     = "Sex: Female",
                 "P6040.1"     = "Age: Youth", 
                 "P6040.2"     = "Age: Adulthood", 
                 "P756S3.2"    = "Nationality: Venezuelan",
                 "P756S3.3"    = "Nationality: Other", 
                 "P6080.1"     = "Ethnicity: Indigenous", 
                 "P6080.2"     = "Ethnicity: Gypsy",
                 "P6080.3"     = "Ethnicity: Afrodescendant",
                 "`Rural/Urbano`Rural" = "Area: Rural",
                 
                 # Employment status variables
                 "P6240.2"     = "Employment status: Unemployed",
                 "P6240.3"     = "Employment status: Studying",
                 "P6240.4"     = "Employment status: Domestic work",
                 
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
                 "P6435.1"     = "Work: Employee",
                 "P6435.2"     = "Work: Domestic work",
                 "P6435.3"     = "Work: Self-employed",
                 "P6435.4"     = "Work: Employer",
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
                 "quintil_ingreso.1" = "Income: 1st quintile",
                 "quintil_ingreso.2" = "Income: 2nd quintile",
                 "quintil_ingreso.3" = "Income: 3rd quintile",
                 "quintil_ingreso.4" = "Income: 4th quintile",
                 
                 # Technology access variables
                 "P1084.1"     = "Internet: Use it everyday",
                 "P1084.2"     = "Internet: Sometimes uses it",  
                 
                 # Health system and health status variables
                 "P6100.1"     = "Health scheme: Contributed", 
                 "P6100.2"     = "Health scheme: Special",
                 "P6100.9"     = "Health scheme: Doesn't know",
                 "P6127.1"     = "Health: Good", 
                 "P6127.2"     = "Health: Regular",
                 
                 # Lifestyle and behavioral variables
                 "P3008S1.2"   = "Smokes cigarettes or tobacco: No", 
                 "P1707.2"     = "Drink sugary drinks: No", 
                 "P3335S1.1"   = "Has childs: No", 
                 "P1906.1"     = "Has at least one health problem: No", 
                 
                 # Social and economic perception variables
                 "P9010.1"     = "Security: Feels secure", 
                 "P5230.2"     = "Considers him(her)self poor: No", 
                 "P784S1.2"    = "Subsidies: Not receiving", 
                 "P1077S1.1"   = "Washing machine: Yes", 
                 
                 # Housing characteristics
                 "P5095.1"     = "Housing tenure: Own",  
                 "P1070.1"     = "Type of housing: House", 
                 "P1070.2"     = "Type of housing: Apartment",
                 "P1070.4"     = "Type of housing: Traditional indigenous housing", 
                 "P1070.5"     = "Type of housing: Other", 
                 
                 # Utility access variables
                 "P8520S1.1"   = "Electrical power: Yes", 
                 "P8520S5.1"   = "Water supply: Yes", 
                 "P8520S3.1"   = "Sewer: Yes", 
                 "P8520S4.1"   = "Garbage collection: Yes",
                 
                 # Housing problems
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
  
# Create a list of plots, one for each group of variables
# map() applies the plotting function to each group in groups_variables
list_plots <- map(groups_variables, function(vars_grupo) {
  
  # Filter the coefficient data to include only variables belonging to the current group
  df_sub <- df_coefs_all %>%
    filter(variable %in% vars_grupo)
  
  # Extract the unique variable identifiers present in the current group
  vars_reales <- unique(df_sub$variable)
  # Extract the unique variable names used as facet labels
  nombres_reales <- unique(df_sub$var_name)
  
  # --- Define a color palette exclusively for the variables in the current group ---
  paleta_colores <- scales::hue_pal()(length(nombres_reales))
  
  # Assign each variable name to its corresponding color
  names(paleta_colores) <- nombres_reales
  
  # Create a list of individual plots, one for each variable in the group
  # map() iterates over the unique variable identifiers
  plots_individuales <- map(vars_reales, function(v) {
    
    # Filter the data to retain observations corresponding to the current variable
    df_v <- df_sub %>% filter(variable == v)
    
    # Extract the display name of the current variable
    nombre_var <- as.character(unique(df_v$var_name))
    
    # Retrieve the color assigned to the current variable from the palette
    color_var <- paleta_colores[[nombre_var]]
    
    # Create the ggplot object for the current variable
    ggplot(df_v, aes(x = year, y = estimate)) +
      
      # Add a line showing the evolution of the estimated coefficient over time
      # The line color is assigned individually to the current variable
      geom_line(color = color_var) +
      
      # Add a confidence interval ribbon around the coefficient estimates
      # ymin and ymax define the lower and upper confidence limits
      # alpha = 0.2 makes the ribbon semi-transparent
      # fill = color_var assigns the same color as the coefficient line
      geom_ribbon(
        aes(ymin = conf_low, ymax = conf_high),
        alpha = 0.2, fill = color_var
      ) +
      
      # Create a separate facet displaying the name of the current variable
      facet_wrap(~ var_name) +
      
      # Apply a minimal theme for a clean and professional appearance
      theme_minimal() +
      
      # Remove the axis titles to reduce repetition across individual panels
      labs(x = NULL, y = NULL) +
      
      # Remove the legend because each plot represents a single variable
      # and its identity is already indicated by the facet title
      theme(legend.position = "none")
  })
  
  # Calculate the number of empty panels needed to complete a 3 x 3 grid
  # Each group will occupy a layout with nine positions
  n_empty <- 9 - length(plots_individuales)
  
  # Create empty plot objects to fill the unused positions in the grid
  # plot_spacer() generates a blank panel without axes or data
  espacios_vacios <- rep(list(patchwork::plot_spacer()), n_empty)
  
  # Combine the individual plots with the empty panels
  # This ensures that all groups follow the same 3 x 3 layout
  todos_los_paneles <- c(plots_individuales, espacios_vacios)
  
  # Arrange the plots in three columns and three rows
  wrap_plots(todos_los_paneles, ncol = 3, nrow = 3) +
    
    # Add a common title to the combined figure
    plot_annotation(
      title = "Evolution of regression coefficients"
    ) &
    
    # Apply consistent margins to the combined figure
    # margin(5, 5, 5, 5) specifies top, right, bottom, and left margins
    theme(plot.margin = margin(5, 5, 5, 5))
})
  
# ------------------------------------------------------------------------------
#                           Outputs
# ------------------------------------------------------------------------------
  
# Create a subdirectory for the current analysis type within the main plots folder
# This organizes plots by analysis type for better file management
folder_path <- file.path(paste0(here("03_outputs/figures/Plots_coefs"),"/", tipo))
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
# Save each plot panel as a PNG file
# seq_along(list_plots) creates sequence 1, 2, 3... for numbering panels
for (i in seq_along(list_plots)) {
  ggsave(
    # Generate filename with panel number and analysis type
    filename = file.path(folder_path, paste0("panel_", i, ".png")),
    plot = list_plots[[i]],      # Plot object to save
    width = 12, height = 8       # Plot dimensions in inches
  )
}
