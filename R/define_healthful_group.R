###############################################################################
# FUNCTION: Define healthful groups
################################################################################
# Function to assign FFQ meals into PDI healthful groups
define_healthful_group <- function (df)
  #' xx
  #
  #' @param df <- DF with amalgamated FFQ groups in a column called 'FoodGroup'
  #' @return healthful_df <- DF with additional column 'HealthfulGroup' with healthful group assignment
  #'
  #' @author Robbie Pope
  #'
{
  # Define dictionary of healthy, less healthy and animal based foods for PDI scores
  health_groups <- list(
    healthy = c('whole_grain', 'fruit', 'vegetables', 'nuts', 'legumes', 'veg_oil', 'tea_coffee'),
    less_healthy = c('fruit_juice', 'refined_grains', 'potatoes', 'sugar_beverage', 'sweet_dessert'),
    animal_foods = c('animal_fat', 'dairy', 'eggs', 'fish', 'meat', 'misc_animal')
  )

  # Create additional column assigining each food group to a helathy_group definition
  healthful_df <- df %>%
    mutate(
      HealthfulGroup = case_when(
        FoodGroup %in% health_groups$healthy ~ "healthy",
        FoodGroup %in% health_groups$less_healthy ~ "less_healthy",
        FoodGroup %in% health_groups$animal_foods ~ "animal_foods",
        TRUE ~ "other"
      )
    )
  return(healthful_df)
}
