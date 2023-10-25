################################################################################
# FUNCTION: Compute animal score
################################################################################ 
# Required Libraries
library(dplyr)

# Animal Score Calculation Function
animal_score <- function (df) 
  #' Compute animal score based on food group '+ve' or '-ve'. Positive or negative 
  #' groups are assigned points based on quintiles as follows:
  #' +ve Group: quintile 5 = 5, 
  #' -ve Group: quintile 5 = 1
  #'
  #' @param df <- DF with FFQ_ID, and columns 'HealthfulGroup' and 'Quintile'
  #' @return score_df <- Df with additional column with quintile based animal scoring
  #' 
  #' @import dplyr
  #'
  #' @author Robbie Pope
  #' 
{
  # Animal groups: (+ve group = Healthy) & (-ve group = less_healthy)
  posneg <- list(
    positive = c('healthy'),
    negative = c('less_healthy')
    )
  score_df <- df %>% 
    mutate(PosNeg = case_when # Assign healthful group pos / neg
           (
             HealthfulGroup %in% posneg$positive ~ 'positive',
             HealthfulGroup %in% posneg$negative ~ 'negative',
               TRUE ~ NA_character_
             ),
           animalscore = if_else(PosNeg == 'positive', Quintile, 6 - Quintile) # Compute animal score 
           )
  
  return(score_df)
}
