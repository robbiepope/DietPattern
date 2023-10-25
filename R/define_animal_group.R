###############################################################################
# FUNCTION: Define animal groups
################################################################################

# Function to assign FFQ meals into animal healthful groups
define_animal_group <- function (df) 
  #' xx
  #
  #' @param df <- DF with amalgamated FFQ groups in a column called 'FoodGroup'
  #' @return animal_df <- DF with additional column 'HealthfulGroup' with healthful group assignment
  #' 
  #' @author Robbie Pope
  #' 
{
  # Define dictionary of healthy and less healthy foods for animal scores
  animal_groups <- list(
    healthy = c('h_dairy', 'h_meat', 'h_egg', 'h_fish'),
    less_healthy = c('u_animalfat', 'u_meat', 'u_dairy', 'u_misc', 'u_fish')
    )
  
  # Create additional column assigining each food group to a animal_group definition 
  animal_df <- df %>%
    mutate(
      HealthfulGroup = case_when(
        FoodGroup %in% animal_groups$healthy ~ "healthy",
        FoodGroup %in% animal_groups$less_healthy ~ "less_healthy",
        TRUE ~ "other"
      )
    )
  return(animal_df)
}
