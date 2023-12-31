% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdi_scores.R
\name{pdi_scores}
\alias{pdi_scores}
\title{Compute PDI score based on food group '+ve' or '-ve'. Positive or negative 
groups are assigned points based on quintiles as follows:
+ve Group: quintile 5 = 5, 
-ve Group: quintile 5 = 1}
\usage{
pdi_scores(df, score = c("all", "pdi", "hpdi", "updi"))
}
\arguments{
\item{df}{<- DF with FFQ_ID, and columns 'HealthfulGroup' and 'Quintile'}

\item{score}{<- Which PDI score to compute, all will compute all three scores. Default is 'all'}
}
\value{
score_df <- Df with additional column(s) with quintile based scoring
}
\description{
Compute PDI score based on food group '+ve' or '-ve'. Positive or negative 
groups are assigned points based on quintiles as follows:
+ve Group: quintile 5 = 5, 
-ve Group: quintile 5 = 1
}
\author{
Robbie Pope
}
