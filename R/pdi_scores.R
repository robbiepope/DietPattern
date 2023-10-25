################################################################################
# FUNCTION: Compute PDI, hPDI and uPDI scores
################################################################################ 
# Required Libraries
library(dplyr)

# PDI Calculation Function
pdi_scores <- function (df, score=c('all', 'pdi', 'hpdi', 'updi')) 
  #' Compute PDI score based on food group '+ve' or '-ve'. Positive or negative 
  #' groups are assigned points based on quintiles as follows:
  #' +ve Group: quintile 5 = 5, 
  #' -ve Group: quintile 5 = 1
  #'
  #' @param df <- DF with FFQ_ID, and columns 'HealthfulGroup' and 'Quintile'
  #' @param score <- Which PDI score to compute, all will compute all three scores. Default is 'all'
  #' @return score_df <- Df with additional column(s) with quintile based scoring
  #' 
  #' @import dplyr
  #'
  #' @author Robbie Pope
{
  if (score == 'pdi') 
  {
    # PDI groups: (+ve groups = healthy and less_Healthy) & (-ve groups = animal_foods)
    posneg <- list(
      positive = c('healthy', 'less_healthy'),
      negative = c('animal_foods')
    )
    score_df <- df %>% 
      mutate(PosNeg = case_when # Assign healthful group pos / neg
             (
               HealthfulGroup %in% posneg$positive ~ 'positive',
               HealthfulGroup %in% posneg$negative ~ 'negative',
               TRUE ~ NA_character_
             ),
             PDI_score = if_else(PosNeg == 'positive', Quintile, 6 - Quintile) # Compute PDI score 
      )
    
  } else if (score == 'hpdi') 
  {
    # hPDI groups: (+ve groups = healthy) & (-ve groups = less_Healthy and animal_foods)
    posneg <- list(
      positive = c('healthy'),
      negative = c('less_healthy', 'animal_foods')
    )
    score_df <- df %>% 
      mutate(h_PosNeg = case_when # Assign healthful group pos / neg
             (
               HealthfulGroup %in% posneg$positive ~ 'positive',
               HealthfulGroup %in% posneg$negative ~ 'negative',
               TRUE ~ NA_character_
             ),
             hPDI_score = if_else(h_PosNeg == 'positive', Quintile, 6 - Quintile) # Compute hPDI score 
      )
    
  } else if (score == 'updi') 
  {
    # uPDI groups: (+ve groups = healthy) & (-ve groups = less_Healthy and animal_foods)
    posneg <- list(
      positive = c('less_healthy'),
      negative = c('healthy', 'animal_foods')
    )
    score_df <- df %>%
      mutate(u_PosNeg = case_when # Assign healthful group pos / neg
             (
               HealthfulGroup %in% posneg$positive ~ 'positive',
               HealthfulGroup %in% posneg$negative ~ 'negative',
               TRUE ~ NA_character_
             ),
             uPDI_score = if_else(u_PosNeg == 'positive', Quintile, 6 - Quintile) # Compute uPDI score
      )
    
  } else if (score == 'all') 
  {
    pdi_df <- pdi_scores(df, score = 'pdi') # Compute PDI and extract column
    pdi_col <- pdi_df$PDI_score
    
    hpdi_df <- pdi_scores(df, score = 'hpdi') # Compute hPDI and extract column
    hpdi_col <- hpdi_df$hPDI_score
    
    updi_df <- pdi_scores(df, score = 'updi') # Compute uPDI and extract column
    updi_col <- updi_df$uPDI_score
    
    score_df <- cbind(df, pdi_col, hpdi_col, updi_col) # Combine final df
    
  } else 
  {
    stop("PDI score type not valid. Choose: 'all', 'pdi', 'hpdi', 'updi'")
  }
  
  return(score_df)
}
