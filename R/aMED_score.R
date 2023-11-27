################################################################################
# FUNCTION: Compute aMED score for each food groups
################################################################################
# aMED score Calculation Function
aMED_score <- function(row, median_list)
  #' Comute the aMED score based on the foodgroup median intake.
  #' Fung et al. 2005
  #' doi.org/10.1093/ajcn/82.1.163
  #'
  #' Below median intake is scored 0 for favorable foods
  #' Equal to or above intake is scored 1 for favorable foods
  #'
  #' For alcohol the score is sex specific and based on a range:
  #' Female is scored 1 if between 5-15, otherwise 0
  #' Male is scored 1 if between 5-25, otherwise 0
  #'
  #' @param row A row from the df
  #' @param median_list List of FoodGroups with their corresponding median values
  #' @return Score of 1 or 0 based on median foodgroup intake and individual intake
  #'
  #' @author Robbie Pope
  {
  # Extract relevant information from the input df row
  food_group <- row$FoodGroup
  favour <- row$FavGroup
  median_value <- median_list[food_group]
  sex <- row$SEX
  portion <- row$total_meal_portion

  # Separate alcohol food group due to different scoring appraoch
  if (favour == "alcohol") {
    # Segregate scoring based on participant Sex
    if (sex == "F") {
      if (portion > 5 && portion <= 15) {
        return(1)
      } else {
        return(0)
      }
    } else if (sex == 'M') {
      if (portion > 5 && portion <= 25) {
        return(1)
      } else {
        return(0)
      }
    }
  # For all favorable food groups, score 1 for greater than median
  } else if (favour == 'favourable') {
    if (portion < median_value) {
      return(0)
    } else {
      return(1)
    }
  # For all unfavorable food groups, score 1 for less than median
  } else if (favour == 'unfavourable') {
    if (portion < median_value) {
      return(1)
    } else {
      return(0)
    }
  # If none of the above return NA value
  } else {
    return(NA)
  }
}
