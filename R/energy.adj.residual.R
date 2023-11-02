################################################################################
# FUNCTION: Nutrient residual energy adjusted model
################################################################################
# Compute nutrient residual energy adjusted model
energy.adj.residual <- function(df, energy_col, start_col, end_col)
  #' Function to compute nutrient residual energy-adjusted model and return df
  #' with adjusted columns.
  #'
  #' Energy adjusted intake = residual + prediction from mean kcal
  #'
  #' Methodology: Adjustmentfor total energy intake in epidemiologic studies,  Willet et al. 1997
  #'
  #' @param df Dataframe of FFQ processed data containing energy in kcal, all NA should be 0
  #' @param energy_col The index of the energy_kcal column to use as a mean
  #' @param start_col The index of the first nutrient/food group column
  #' @param end_col The index of the final nutrient/food group column
  #' @return Dataframe with nutrient/food group energy adjusted additional columns
  #' @author Robbie Pope
  #'
{
  # Create new df to manipulate
  df_adj <- df
  # Compute mean energy of df
  mean_energy <- mean(df[, energy_col], na.rm=TRUE) # Ignore NA
  # Loop through df columns and adjust
  for (col in start_col:end_col) {
    col_vect <- as.numeric(df[, col]) # Extract nutrient or food group column
    col_reg <- lm(col_vect ~ Energy_kcal, data=df) # Linear regression to predict with kcal
    col_resid <- resid(col_reg) # Extract residual
    col_pred <- as.numeric(predict(col_reg, data.frame(Energy_kcal=mean_energy))) # Predict using mean kcal
    col_adj <- col_resid + col_pred # Compute adjusted by summing residual and prediction
    # Create new column name with "_adj"
    new_col_name <- paste0(names(df)[col], "_adj")
    df_adj[[new_col_name]] <- col_adj # Add adjusted column to the new dataframe
  }
  # Return final df with adjusted nutrient / food group values
  return(df_adj)
}
