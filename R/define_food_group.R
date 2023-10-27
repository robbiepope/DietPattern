################################################################################
# FUNCTION: Create a DF with summed meal portions (g) for a food group
################################################################################
# Function to define the FFQ amalgamated food groups
define_food_group <- function (df, id_list, food_group_name)
  #' Group the DataFrame by ID and calculate the sum of meal_portion for
  #' specified meal_ID codes. Any FFQ IDs with no meal_id data are added as 0g.
  #'
  #' @param df <- FFQ Line FETA processed df of FFQ_IDs, MEAL_IDs and MEAL_Portions
  #' @param id_list <- Vector of numerical meal_ids for a food group
  #' @param food_group_name <- String, name of food group, e.g., "animal_fat"
  #' @return summarized df for food group
  #'
  #' @author Robbie Pope
  #'
{
  # Create df of summed meal portion weights (g) for food group
  summed_df <- df %>%
    group_by(ID) %>%
    summarize(total_meal_portion = sum(ifelse(MEAL_ID %in% id_list, MEAL_PORTION, 0))) %>%
    ungroup()

  # Create column with food group name
  summed_df$FoodGroup <- food_group_name

  return(summed_df) # Return final summed df
}


