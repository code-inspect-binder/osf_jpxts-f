## This code is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
## 
## For a copy of the GNU General Public License, 
## see <http://www.gnu.org/licenses/>.

## (c) 2022 Lara Kroencke


# Set working directory to the folder "07_Manuscript"
setwd("Statistical Code/Final Submission/07_Manuscript")

# Load packages
library(numform)
library(tidyr)
library(dplyr)
library(metafor)

# Read in data
Values_Base <- read.csv("Values_Base.csv")
Values_Personality <- read.csv("Values_Personality.csv")

# Function to format p-values
p_value <- function(est, lower_2.5ci, upper_2.5ci, pval, threshold) {
  # Estimates and CIs with 3 decimal places, no leading zero, bold if p < .01
  ifelse(pval < threshold, paste0(numform::f_num(est, digits = 3), " [", numform::f_num(lower_2.5ci, digits = 3), ", ", numform::f_num(upper_2.5ci, digits = 3), "] *"),
          paste0(numform::f_num(est, digits = 3), " [", numform::f_num(lower_2.5ci, digits = 3), ", ", numform::f_num(upper_2.5ci, digits = 3), "]"))
}

### Analysis 1
## Baseline models
# Extract standardized parameters and N from baseline models
Values_Analysis1_Model1 <- Values_Base[which(Values_Base$analysis == "analysis1" &
                                               ((Values_Base$study %in% c("S1", "S2") & Values_Base$DV == "well_being") |
                                                  (Values_Base$study == "S3" & Values_Base$DV == "affect_balance")) & 
                                               Values_Base$output %in% c("standardized", "N")), ]

# Number of participants
Values_Analysis1_Model1$est[Values_Analysis1_Model1$param == "NClusters" & Values_Analysis1_Model1$study == "S1"] # S1
Values_Analysis1_Model1$est[Values_Analysis1_Model1$param == "NClusters" & Values_Analysis1_Model1$study == "S2"] # S2
Values_Analysis1_Model1$est[Values_Analysis1_Model1$param == "NClusters" & Values_Analysis1_Model1$study == "S3"] # S3

# Number of observations
Values_Analysis1_Model1$est[Values_Analysis1_Model1$param == "Observations" & Values_Analysis1_Model1$study == "S1"] # S1
Values_Analysis1_Model1$est[Values_Analysis1_Model1$param == "Observations" & Values_Analysis1_Model1$study == "S2"] # S2
Values_Analysis1_Model1$est[Values_Analysis1_Model1$param == "Observations" & Values_Analysis1_Model1$study == "S3"] # S3

# Extract within-person variances of social interactions from unstandardized models
variances <- Values_Base$est[which(Values_Base$analysis == "analysis1" &
                                     ((Values_Base$study %in% c("S1", "S2") & Values_Base$DV == "well_being") |
                                        (Values_Base$study == "S3" & Values_Base$DV == "affect_balance")) & 
                                     Values_Base$output == "unstandardized" & 
                                     Values_Base$paramHeader == "Variances" & Values_Base$param == "INT" & Values_Base$BetweenWithin == "Within")]

rows <- which(Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT") # within-person effects

# Divide estimates, posterior sd, lower CI, and upper CI of the within-person effects by the within-person SDs of social interactions
# to obtain coefficients that are standardized with respect to the DV only
Values_Analysis1_Model1$est[rows] <- Values_Analysis1_Model1$est[rows] / sqrt(variances)
Values_Analysis1_Model1$posterior_sd[rows] <- Values_Analysis1_Model1$posterior_sd[rows] / sqrt(variances)
Values_Analysis1_Model1$lower_2.5ci[rows] <- Values_Analysis1_Model1$lower_2.5ci[rows] / sqrt(variances)
Values_Analysis1_Model1$upper_2.5ci[rows] <- Values_Analysis1_Model1$upper_2.5ci[rows] / sqrt(variances)

# Choose within- and between-person effects
coefficients <- Values_Analysis1_Model1[c(which(Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"), 
                                          which(Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT")), ]

# Apply function to format p-values
coefficients$est <- p_value(coefficients$est, coefficients$lower_2.5ci, coefficients$upper_2.5ci, coefficients$pval, 0.005)
# Choose variables
dat1 <- coefficients[, c("study", "paramHeader", "param", "est")]

## Personality models
# Extract standardized parameters and N from personality models
Values_Analysis1_Model1_Pers <- Values_Personality[which(Values_Personality$analysis == "analysis1" &
                                                  ((Values_Personality$study %in% c("S1", "S2") & Values_Personality$DV == "well_being") |
                                                  (Values_Personality$study == "S3" & Values_Personality$DV == "affect_balance")) & 
                                                    Values_Personality$output %in% c("standardized", "N")), ]
  
# Number of participants
unique(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$param == "NClusters" & Values_Analysis1_Model1_Pers$study == "S1"]) # S1
unique(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$param == "NClusters" & Values_Analysis1_Model1_Pers$study == "S2"]) # S2
unique(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$param == "NClusters" & Values_Analysis1_Model1_Pers$study == "S3"]) # S3

# Number of observations
unique(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$param == "Observations" & Values_Analysis1_Model1_Pers$study == "S1"]) # S1
unique(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$param == "Observations" & Values_Analysis1_Model1_Pers$study == "S2"]) # S2
unique(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$param == "Observations" & Values_Analysis1_Model1_Pers$study == "S3"]) # S3

# Extract within-person variances of social interactions from unstandardized models
variances <- Values_Personality$est[which(Values_Personality$analysis == "analysis1" &
                                            ((Values_Personality$study %in% c("S1", "S2") & Values_Personality$DV == "well_being") |
                                               (Values_Personality$study == "S3" & Values_Personality$DV == "affect_balance")) & 
                                            Values_Personality$output == "unstandardized" & 
                                            Values_Personality$paramHeader == "Variances" & Values_Personality$param == "INT" & Values_Personality$BetweenWithin == "Within")]

rows <- which(Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT") # within-person effects

# Divide estimates, posterior sd, lower CI, and upper CI of the within-person effects by the within-person SDs of social interactions
# to obtain coefficients that are standardized with respect to the DV only
Values_Analysis1_Model1_Pers$est[rows] <- Values_Analysis1_Model1_Pers$est[rows] / sqrt(variances)
Values_Analysis1_Model1_Pers$posterior_sd[rows] <- Values_Analysis1_Model1_Pers$posterior_sd[rows] / sqrt(variances)
Values_Analysis1_Model1_Pers$lower_2.5ci[rows] <- Values_Analysis1_Model1_Pers$lower_2.5ci[rows] / sqrt(variances)
Values_Analysis1_Model1_Pers$upper_2.5ci[rows] <- Values_Analysis1_Model1_Pers$upper_2.5ci[rows] / sqrt(variances)

# Choose within- and between-person effects, simple effects of personality traits, and cross-level interactions
coefficients <- Values_Analysis1_Model1_Pers[c(which(Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"), 
                                               which(Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"),
                                               which(Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"),
                                               which(Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P")), ]

# Apply function to format p-values
coefficients$est <- p_value(coefficients$est, coefficients$lower_2.5ci, coefficients$upper_2.5ci, coefficients$pval, 0.005)
# Choose variables
dat2 <- coefficients[, c("study", "paramHeader", "param", "est", "pers")]
# Change to wide format (i.e., one column per personality trait)
dat2 <- tidyr::pivot_wider(dat2, names_from = "pers", values_from = "est")

## Join baseline models and personality models
res1 <- dplyr::full_join(dat2, dat1, by = c("study", "paramHeader", "param"))
# Choose variables
res1 <- res1[, c("est", "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")]

# Upper half of Table 4
res1

## Meta-analysis
simple_effect_wp <- NA
simple_effect_bp <- NA
simple_effect_pers <- NA
interaction_effect <- NA
perso <- c("bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o") # names of personality traits

for(i in 1:6) {
  if(i == 1) {
    
    # Meta-analyze within-person effects from baseline models
    wp <- metafor::rma(yi = c(Values_Analysis1_Model1$est[Values_Analysis1_Model1$study == "S1" & Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"],
                              Values_Analysis1_Model1$est[Values_Analysis1_Model1$study == "S2" & Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"],
                              Values_Analysis1_Model1$est[Values_Analysis1_Model1$study == "S3" & Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"]),
                       sei = c(Values_Analysis1_Model1$posterior_sd[Values_Analysis1_Model1$study == "S1" & Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"],
                               Values_Analysis1_Model1$posterior_sd[Values_Analysis1_Model1$study == "S2" & Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"],
                               Values_Analysis1_Model1$posterior_sd[Values_Analysis1_Model1$study == "S3" & Values_Analysis1_Model1$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1$param == "INT"]),
                       method = "FE")
    simple_effect_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)
    
    # Meta-analyze between-person effects from baseline models
    bp <- metafor::rma(yi = c(Values_Analysis1_Model1$est[Values_Analysis1_Model1$study == "S1" & Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT"],
                              Values_Analysis1_Model1$est[Values_Analysis1_Model1$study == "S2" & Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT"],
                              Values_Analysis1_Model1$est[Values_Analysis1_Model1$study == "S3" & Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT"]),
                       sei = c(Values_Analysis1_Model1$posterior_sd[Values_Analysis1_Model1$study == "S1" & Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT"],
                               Values_Analysis1_Model1$posterior_sd[Values_Analysis1_Model1$study == "S2" & Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT"],
                               Values_Analysis1_Model1$posterior_sd[Values_Analysis1_Model1$study == "S3" & Values_Analysis1_Model1$paramHeader == "WB.ON" & Values_Analysis1_Model1$param == "INT"]),
                       method = "FE")
    simple_effect_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)
  }
  if(i %in% c(2:6)) {
    
    # Meta-analyze within-person effects from personality models
    wp <- metafor::rma(yi = c(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                              Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                              Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"]),
                       sei = c(Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                               Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                               Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"]),
                       method = "FE")
    simple_effect_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)
    
    # Meta-analyze between-person effects from personality models
    bp <- metafor::rma(yi = c(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                              Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                              Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"]),
                       sei = c(Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                               Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"],
                               Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "INT"]),
                       method = "FE")
    simple_effect_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)
    
    # Meta-analyze simple effects of personality traits from personality models
    personality <- metafor::rma(yi = c(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                       Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                       Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"]),
                                sei = c(Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                        Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                        Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model1_Pers$param == "P"]),
                                method = "FE")
    simple_effect_pers[i] <- p_value(personality$b, personality$ci.lb, personality$ci.ub, personality$pval, 0.01)
    
    # Meta-analyze cross-level interactions from personality models
    interaction <- metafor::rma(yi = c(Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                       Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                       Values_Analysis1_Model1_Pers$est[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P"]),
                                sei = c(Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S1" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                        Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S2" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P"],
                                        Values_Analysis1_Model1_Pers$posterior_sd[Values_Analysis1_Model1_Pers$study == "S3" & Values_Analysis1_Model1_Pers$pers == perso[i-1] & Values_Analysis1_Model1_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model1_Pers$param == "P"]),
                                method = "FE")
    interaction_effect[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)
  }
}

# Combine all effects into one data frame
res1_meta <- rbind(simple_effect_wp, simple_effect_bp, simple_effect_pers, interaction_effect)

# Lower half of Table 4
res1_meta


### Analysis 2
## Baseline models
# Extract standardized parameters and N from baseline models
Values_Analysis1_Model2 <- Values_Base[which(Values_Base$analysis == "analysis2" &
                                               ((Values_Base$study %in% c("S1", "S2") & Values_Base$DV == "well_being") |
                                                  (Values_Base$study == "S3" & Values_Base$DV == "affect_balance")) & 
                                               Values_Base$output %in% c("standardized", "N")), ]

# Number of participants
Values_Analysis1_Model2$est[Values_Analysis1_Model2$param == "NClusters" & Values_Analysis1_Model2$study == "S1"] # S1
Values_Analysis1_Model2$est[Values_Analysis1_Model2$param == "NClusters" & Values_Analysis1_Model2$study == "S2"] # S2
Values_Analysis1_Model2$est[Values_Analysis1_Model2$param == "NClusters" & Values_Analysis1_Model2$study == "S3"] # S3

# Number of observations
Values_Analysis1_Model2$est[Values_Analysis1_Model2$param == "Observations" & Values_Analysis1_Model2$study == "S1"] # S1
Values_Analysis1_Model2$est[Values_Analysis1_Model2$param == "Observations" & Values_Analysis1_Model2$study == "S2"] # S2
Values_Analysis1_Model2$est[Values_Analysis1_Model2$param == "Observations" & Values_Analysis1_Model2$study == "S3"] # S3

# Extract within-person variances of FtF interactions, CMC, and mixed episodes from unstandardized models
variances <- Values_Base$est[which(Values_Base$analysis == "analysis2" &
                                     ((Values_Base$study %in% c("S1", "S2") & Values_Base$DV == "well_being") |
                                        (Values_Base$study == "S3" & Values_Base$DV == "affect_balance")) & 
                                     Values_Base$output == "unstandardized" & 
                                     Values_Base$paramHeader == "Variances" & Values_Base$param %in% c("FTF", "CMC", "MIXED") & Values_Base$BetweenWithin == "Within")]

rows <- which(Values_Analysis1_Model2$paramHeader %in% c("S1|WB.ON", "S2|WB.ON", "S3|WB.ON") & Values_Analysis1_Model2$param %in% c("FTF", "CMC", "MIXED")) # within-person effects

# Divide estimates, posterior sd, lower CI, and upper CI of the within-person effects by the within-person SDs of FtF interactions, CMC, and mixed episodes
# to obtain coefficients that are standardized with respect to the DV only
Values_Analysis1_Model2$est[rows] <- Values_Analysis1_Model2$est[rows] / sqrt(variances)
Values_Analysis1_Model2$posterior_sd[rows] <- Values_Analysis1_Model2$posterior_sd[rows] / sqrt(variances)
Values_Analysis1_Model2$lower_2.5ci[rows] <- Values_Analysis1_Model2$lower_2.5ci[rows] / sqrt(variances)
Values_Analysis1_Model2$upper_2.5ci[rows] <- Values_Analysis1_Model2$upper_2.5ci[rows] / sqrt(variances)

# Choose within- and between-person effects
coefficients <- Values_Analysis1_Model2[c(which(Values_Analysis1_Model2$paramHeader == "S1|WB.ON" & Values_Analysis1_Model2$param == "FTF"), 
                                          which(Values_Analysis1_Model2$paramHeader == "S2|WB.ON" & Values_Analysis1_Model2$param == "CMC"),
                                          which(Values_Analysis1_Model2$paramHeader == "S3|WB.ON" & Values_Analysis1_Model2$param == "MIXED"),
                                          which(Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == "FTF"),
                                          which(Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == "CMC"),
                                          which(Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == "MIXED")), ]

# Apply function to format p-values
coefficients$est <- p_value(coefficients$est, coefficients$lower_2.5ci, coefficients$upper_2.5ci, coefficients$pval, 0.005)
# Choose variables
dat1 <- coefficients[, c("study", "paramHeader", "param", "est")]

## Personality models
# Extract standardized parameters and N from personality models
Values_Analysis1_Model2_Pers <- Values_Personality[which(Values_Personality$analysis == "analysis2" &
                                                           ((Values_Personality$study %in% c("S1", "S2") & Values_Personality$DV == "well_being") |
                                                              (Values_Personality$study == "S3" & Values_Personality$DV == "affect_balance")) & 
                                                           Values_Personality$output %in% c("standardized", "N")), ]

# Number of participants
unique(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$param == "NClusters" & Values_Analysis1_Model2_Pers$study == "S1"]) # S1
unique(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$param == "NClusters" & Values_Analysis1_Model2_Pers$study == "S2"]) # S2
unique(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$param == "NClusters" & Values_Analysis1_Model2_Pers$study == "S3"]) # S3

# Number of observations
unique(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$param == "Observations" & Values_Analysis1_Model2_Pers$study == "S1"]) # S1
unique(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$param == "Observations" & Values_Analysis1_Model2_Pers$study == "S2"]) # S2
unique(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$param == "Observations" & Values_Analysis1_Model2_Pers$study == "S3"]) # S3

# Extract within-person variances of FtF interactions, CMC, and mixed episodes from unstandardized models
variances <- Values_Personality$est[which(Values_Personality$analysis == "analysis2" &
                                            ((Values_Personality$study %in% c("S1", "S2") & Values_Personality$DV == "well_being") |
                                               (Values_Personality$study == "S3" & Values_Personality$DV == "affect_balance")) & 
                                            Values_Personality$output == "unstandardized" & 
                                            Values_Personality$paramHeader == "Variances" & Values_Personality$param %in% c("FTF", "CMC", "MIXED") & Values_Personality$BetweenWithin == "Within")]

rows <- which(Values_Analysis1_Model2_Pers$paramHeader %in% c("S1|WB.ON", "S2|WB.ON", "S3|WB.ON") & Values_Analysis1_Model2_Pers$param %in% c("FTF", "CMC", "MIXED")) # within-person effects

# Divide estimates, posterior sd, lower CI, and upper CI of the within-person effects by the within-person SDs of FtF interactions, CMC, and mixed episodes
# to obtain coefficients that are standardized with respect to the DV only
Values_Analysis1_Model2_Pers$est[rows] <- Values_Analysis1_Model2_Pers$est[rows] / sqrt(variances)
Values_Analysis1_Model2_Pers$posterior_sd[rows] <- Values_Analysis1_Model2_Pers$posterior_sd[rows] / sqrt(variances)
Values_Analysis1_Model2_Pers$lower_2.5ci[rows] <- Values_Analysis1_Model2_Pers$lower_2.5ci[rows] / sqrt(variances)
Values_Analysis1_Model2_Pers$upper_2.5ci[rows] <- Values_Analysis1_Model2_Pers$upper_2.5ci[rows] / sqrt(variances)

# Choose within- and between-person effects, simple effects of personality traits, and cross-level interactions
coefficients <- Values_Analysis1_Model2_Pers[c(which(Values_Analysis1_Model2_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model2_Pers$param == "FTF"), 
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "S2|WB.ON" & Values_Analysis1_Model2_Pers$param == "CMC"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "S3|WB.ON" & Values_Analysis1_Model2_Pers$param == "MIXED"), 
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "FTF"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "CMC"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "MIXED"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model2_Pers$param == "P"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "S2.ON" & Values_Analysis1_Model2_Pers$param == "P"),
                                               which(Values_Analysis1_Model2_Pers$paramHeader == "S3.ON" & Values_Analysis1_Model2_Pers$param == "P")), ]

# Apply function to format p-values
coefficients$est <- p_value(coefficients$est, coefficients$lower_2.5ci, coefficients$upper_2.5ci, coefficients$pval, 0.005)
# Choose variables
dat2 <- coefficients[, c("study", "paramHeader", "param", "est", "pers")]
# Change to wide format (i.e., one column per personality trait)
dat2 <- tidyr::pivot_wider(dat2, names_from = "pers", values_from = "est")

## Join baseline models and personality models
res2 <- dplyr::full_join(dat2, dat1, by = c("study", "paramHeader", "param"))
# Choose variables
res2 <- res2[, c("est", "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")]

# Upper half of Table 5
res2

## Meta-analysis
simple_effect_FtF_wp <- NA
simple_effect_CMC_wp <- NA
simple_effect_mixed_wp <- NA
simple_effect_FtF_bp <- NA
simple_effect_CMC_bp <- NA
simple_effect_mixed_bp <- NA
simple_effect_pers <- NA
interaction_effect_FtF_pers <- NA
interaction_effect_CMC_pers <- NA
interaction_effect_mixed_pers <- NA

header = c("S1|WB.ON", "S2|WB.ON", "S3|WB.ON") # header of within-person effects
header2 = c("S1.ON", "S2.ON", "S3.ON") # header of cross-level interactions
par = c("FTF", "CMC", "MIXED") # names of variables in Mplus output files

for(i in 1:6) {
  if(i == 1) {
    for(j in 1:3) {
      
      # Meta-analyze within-person effects from baseline models
      wp <- metafor::rma(yi = c(Values_Analysis1_Model2$est[Values_Analysis1_Model2$study == "S1" & Values_Analysis1_Model2$paramHeader == header[j] & Values_Analysis1_Model2$param == par[j]],
                                Values_Analysis1_Model2$est[Values_Analysis1_Model2$study == "S2" & Values_Analysis1_Model2$paramHeader == header[j] & Values_Analysis1_Model2$param == par[j]],
                                Values_Analysis1_Model2$est[Values_Analysis1_Model2$study == "S3" & Values_Analysis1_Model2$paramHeader == header[j] & Values_Analysis1_Model2$param == par[j]]),
                         sei = c(Values_Analysis1_Model2$posterior_sd[Values_Analysis1_Model2$study == "S1" & Values_Analysis1_Model2$paramHeader == header[j] & Values_Analysis1_Model2$param == par[j]],
                                 Values_Analysis1_Model2$posterior_sd[Values_Analysis1_Model2$study == "S2" & Values_Analysis1_Model2$paramHeader == header[j] & Values_Analysis1_Model2$param == par[j]],
                                 Values_Analysis1_Model2$posterior_sd[Values_Analysis1_Model2$study == "S3" & Values_Analysis1_Model2$paramHeader == header[j] & Values_Analysis1_Model2$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_FtF_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 2) {simple_effect_CMC_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 3) {simple_effect_mixed_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      
      # Meta-analyze between-person effects from baseline models
      bp <- metafor::rma(yi = c(Values_Analysis1_Model2$est[Values_Analysis1_Model2$study == "S1" & Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == par[j]],
                                Values_Analysis1_Model2$est[Values_Analysis1_Model2$study == "S2" & Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == par[j]],
                                Values_Analysis1_Model2$est[Values_Analysis1_Model2$study == "S3" & Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == par[j]]),
                         sei = c(Values_Analysis1_Model2$posterior_sd[Values_Analysis1_Model2$study == "S1" & Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == par[j]],
                                 Values_Analysis1_Model2$posterior_sd[Values_Analysis1_Model2$study == "S2" & Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == par[j]],
                                 Values_Analysis1_Model2$posterior_sd[Values_Analysis1_Model2$study == "S3" & Values_Analysis1_Model2$paramHeader == "WB.ON" & Values_Analysis1_Model2$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_FtF_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 2) {simple_effect_CMC_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 3) {simple_effect_mixed_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
    }
  }
  if(i %in% c(2:6)) {
    for(j in 1:3) {
      
      # Meta-analyze within-person effects from personality models
      wp <- metafor::rma(yi = c(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header[j] & Values_Analysis1_Model2_Pers$param == par[j]],
                                Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header[j] & Values_Analysis1_Model2_Pers$param == par[j]],
                                Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header[j] & Values_Analysis1_Model2_Pers$param == par[j]]),
                         sei = c(Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header[j] & Values_Analysis1_Model2_Pers$param == par[j]],
                                 Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header[j] & Values_Analysis1_Model2_Pers$param == par[j]],
                                 Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header[j] & Values_Analysis1_Model2_Pers$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_FtF_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 2) {simple_effect_CMC_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 3) {simple_effect_mixed_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      
      # Meta-analyze between-person effects from personality models
      bp <- metafor::rma(yi = c(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == par[j]],
                                Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == par[j]],
                                Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == par[j]]),
                         sei = c(Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == par[j]],
                                 Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == par[j]],
                                 Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_FtF_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 2) {simple_effect_CMC_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 3) {simple_effect_mixed_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      
      # Meta-analyze cross-level interactions from personality models
      interaction <- metafor::rma(yi = c(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header2[j] & Values_Analysis1_Model2_Pers$param == "P"],
                                         Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header2[j] & Values_Analysis1_Model2_Pers$param == "P"],
                                         Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header2[j] & Values_Analysis1_Model2_Pers$param == "P"]),
                                  sei = c(Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header2[j] & Values_Analysis1_Model2_Pers$param == "P"],
                                          Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header2[j] & Values_Analysis1_Model2_Pers$param == "P"],
                                          Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == header2[j] & Values_Analysis1_Model2_Pers$param == "P"]),
                                  method = "FE")
      if (j == 1) {interaction_effect_FtF_pers[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)}
      if (j == 2) {interaction_effect_CMC_pers[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)}
      if (j == 3) {interaction_effect_mixed_pers[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)}
    }
    
    # Meta-analyze simple effects of personality traits from personality models
    personality <- metafor::rma(yi = c(Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"],
                                       Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"],
                                       Values_Analysis1_Model2_Pers$est[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"]),
                                sei = c(Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S1" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"],
                                        Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S2" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"],
                                        Values_Analysis1_Model2_Pers$posterior_sd[Values_Analysis1_Model2_Pers$study == "S3" & Values_Analysis1_Model2_Pers$pers == perso[i-1] & Values_Analysis1_Model2_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model2_Pers$param == "P"]),
                                method = "FE")
    simple_effect_pers[i] <- p_value(personality$b, personality$ci.lb, personality$ci.ub, personality$pval, 0.01)
  }
}

# Combine all effects into one data frame
res2_meta <- rbind(simple_effect_FtF_wp, simple_effect_CMC_wp, simple_effect_mixed_wp,
             simple_effect_FtF_bp, simple_effect_CMC_bp, simple_effect_mixed_bp,
             simple_effect_pers,
             interaction_effect_FtF_pers, interaction_effect_CMC_pers, interaction_effect_mixed_pers)

# Lower half of Table 5
res2_meta


### Analysis 3
## Baseline models
# Extract standardized parameters and N from baseline models
Values_Analysis1_Model3 <- Values_Base[which(Values_Base$analysis == "analysis3" &
                                               ((Values_Base$study %in% c("S1", "S2") & Values_Base$DV == "well_being") |
                                                  (Values_Base$study == "S3" & Values_Base$DV == "affect_balance")) & 
                                               Values_Base$output %in% c("standardized", "N")), ]

# Number of participants
Values_Analysis1_Model3$est[Values_Analysis1_Model3$param == "NClusters" & Values_Analysis1_Model3$study == "S1"] # S1
Values_Analysis1_Model3$est[Values_Analysis1_Model3$param == "NClusters" & Values_Analysis1_Model3$study == "S2"] # S2
Values_Analysis1_Model3$est[Values_Analysis1_Model3$param == "NClusters" & Values_Analysis1_Model3$study == "S3"] # S3

# Number of observations
Values_Analysis1_Model3$est[Values_Analysis1_Model3$param == "Observations" & Values_Analysis1_Model3$study == "S1"] # S1
Values_Analysis1_Model3$est[Values_Analysis1_Model3$param == "Observations" & Values_Analysis1_Model3$study == "S2"] # S2
Values_Analysis1_Model3$est[Values_Analysis1_Model3$param == "Observations" & Values_Analysis1_Model3$study == "S3"] # S3

# Extract within-person variances of interactions with close peers, family, and weak ties from unstandardized models
variances <- Values_Base$est[which(Values_Base$analysis == "analysis3" &
                                     ((Values_Base$study %in% c("S1", "S2") & Values_Base$DV == "well_being") |
                                        (Values_Base$study == "S3" & Values_Base$DV == "affect_balance")) & 
                                     Values_Base$output == "unstandardized" & 
                                     Values_Base$paramHeader == "Variances" & Values_Base$param %in% c("PEERS", "FAMILY", "WEAK_TIES") & Values_Base$BetweenWithin == "Within")]

rows <- which(Values_Analysis1_Model3$paramHeader %in% c("S1|WB.ON", "S2|WB.ON", "S3|WB.ON") & Values_Analysis1_Model3$param %in% c("PEERS", "FAMILY", "WEAK_TIES")) # within-person effects

# Divide estimates, posterior sd, lower CI, and upper CI of the within-person effects by the within-person SDs of interactions with close peers, family, and weak ties
# to obtain coefficients that are standardized with respect to the DV only         
Values_Analysis1_Model3$est[rows] <- Values_Analysis1_Model3$est[rows] / sqrt(variances)
Values_Analysis1_Model3$posterior_sd[rows] <- Values_Analysis1_Model3$posterior_sd[rows] / sqrt(variances)
Values_Analysis1_Model3$lower_2.5ci[rows] <- Values_Analysis1_Model3$lower_2.5ci[rows] / sqrt(variances)
Values_Analysis1_Model3$upper_2.5ci[rows] <- Values_Analysis1_Model3$upper_2.5ci[rows] / sqrt(variances)

# Choose within- and between-person effects
coefficients <- Values_Analysis1_Model3[c(which(Values_Analysis1_Model3$paramHeader == "S1|WB.ON" & Values_Analysis1_Model3$param == "PEERS"), 
                                          which(Values_Analysis1_Model3$paramHeader == "S2|WB.ON" & Values_Analysis1_Model3$param == "FAMILY"),
                                          which(Values_Analysis1_Model3$paramHeader == "S3|WB.ON" & Values_Analysis1_Model3$param == "WEAK_TIES"),
                                          which(Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == "PEERS"),
                                          which(Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == "FAMILY"),
                                          which(Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == "WEAK_TIES")), ]

# Apply function to format p-values
coefficients$est <- p_value(coefficients$est, coefficients$lower_2.5ci, coefficients$upper_2.5ci, coefficients$pval, 0.005)
# Choose variables
dat1 <- coefficients[, c("study", "paramHeader", "param", "est")]

## Personality models
# Extract standardized parameters and N from personality models
Values_Analysis1_Model3_Pers <- Values_Personality[which(Values_Personality$analysis == "analysis3" &
                                                           ((Values_Personality$study %in% c("S1", "S2") & Values_Personality$DV == "well_being") |
                                                              (Values_Personality$study == "S3" & Values_Personality$DV == "affect_balance")) & 
                                                           Values_Personality$output %in% c("standardized", "N")), ]

# Number of participants
unique(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$param == "NClusters" & Values_Analysis1_Model3_Pers$study == "S1"]) # S1
unique(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$param == "NClusters" & Values_Analysis1_Model3_Pers$study == "S2"]) # S2
unique(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$param == "NClusters" & Values_Analysis1_Model3_Pers$study == "S3"]) # S3

# Number of observations
unique(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$param == "Observations" & Values_Analysis1_Model3_Pers$study == "S1"]) # S1
unique(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$param == "Observations" & Values_Analysis1_Model3_Pers$study == "S2"]) # S2
unique(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$param == "Observations" & Values_Analysis1_Model3_Pers$study == "S3"]) # S3

# Extract within-person variances of interactions with close peers, family, and weak ties from unstandardized models
variances <- Values_Personality$est[which(Values_Personality$analysis == "analysis3" &
                                            ((Values_Personality$study %in% c("S1", "S2") & Values_Personality$DV == "well_being") |
                                               (Values_Personality$study == "S3" & Values_Personality$DV == "affect_balance")) & 
                                            Values_Personality$output == "unstandardized" & 
                                            Values_Personality$paramHeader == "Variances" & Values_Personality$param %in% c("PEERS", "FAMILY", "WEAK_TIES") & Values_Personality$BetweenWithin == "Within")]

rows <- which(Values_Analysis1_Model3_Pers$paramHeader %in% c("S1|WB.ON", "S2|WB.ON", "S3|WB.ON") & Values_Analysis1_Model3_Pers$param %in% c("PEERS", "FAMILY", "WEAK_TIES")) # within-person effects

# Divide estimates, posterior sd, lower CI, and upper CI of the within-person effects by the within-person SDs of interactions with close peers, family, and weak ties
# to obtain coefficients that are standardized with respect to the DV only 
Values_Analysis1_Model3_Pers$est[rows] <- Values_Analysis1_Model3_Pers$est[rows] / sqrt(variances)
Values_Analysis1_Model3_Pers$posterior_sd[rows] <- Values_Analysis1_Model3_Pers$posterior_sd[rows] / sqrt(variances)
Values_Analysis1_Model3_Pers$lower_2.5ci[rows] <- Values_Analysis1_Model3_Pers$lower_2.5ci[rows] / sqrt(variances)
Values_Analysis1_Model3_Pers$upper_2.5ci[rows] <- Values_Analysis1_Model3_Pers$upper_2.5ci[rows] / sqrt(variances)

# Choose within- and between-person effects, simple effects of personality traits, and cross-level interactions
coefficients <- Values_Analysis1_Model3_Pers[c(which(Values_Analysis1_Model3_Pers$paramHeader == "S1|WB.ON" & Values_Analysis1_Model3_Pers$param == "PEERS"), 
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "S2|WB.ON" & Values_Analysis1_Model3_Pers$param == "FAMILY"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "S3|WB.ON" & Values_Analysis1_Model3_Pers$param == "WEAK_TIES"), 
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "PEERS"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "FAMILY"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "WEAK_TIES"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "S1.ON" & Values_Analysis1_Model3_Pers$param == "P"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "S2.ON" & Values_Analysis1_Model3_Pers$param == "P"),
                                               which(Values_Analysis1_Model3_Pers$paramHeader == "S3.ON" & Values_Analysis1_Model3_Pers$param == "P")), ]

# Apply function to format p-values
coefficients$est <- p_value(coefficients$est, coefficients$lower_2.5ci, coefficients$upper_2.5ci, coefficients$pval, 0.005)
# Choose variables
dat2 <- coefficients[, c("study", "paramHeader", "param", "est", "pers")]
# Change to wide format (i.e., one column per personality trait)
dat2 <- tidyr::pivot_wider(dat2, names_from = "pers", values_from = "est")

## Join baseline models and personality models
res3 <- dplyr::full_join(dat2, dat1, by = c("study", "paramHeader", "param"))
# Choose variables
res3 <- res3[, c("est", "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")]

# Upper half of Table 6
res3

## Meta-analysis
simple_effect_close_peers_wp <- NA
simple_effect_family_wp <- NA
simple_effect_weak_ties_wp <- NA
simple_effect_close_peers_bp <- NA
simple_effect_family_bp <- NA
simple_effect_weak_ties_bp <- NA
simple_effect_pers <- NA
interaction_effect_close_peers_pers <- NA
interaction_effect_family_pers <- NA
interaction_effect_weak_ties_pers <- NA

par = c("PEERS", "FAMILY", "WEAK_TIES") # names of variables in Mplus output files

for(i in 1:6) {
  if(i == 1) {
    for(j in 1:3) {
      
      # Meta-analyze within-person effects from baseline models
      wp <- metafor::rma(yi = c(Values_Analysis1_Model3$est[Values_Analysis1_Model3$study == "S1" & Values_Analysis1_Model3$paramHeader == header[j] & Values_Analysis1_Model3$param == par[j]],
                                Values_Analysis1_Model3$est[Values_Analysis1_Model3$study == "S2" & Values_Analysis1_Model3$paramHeader == header[j] & Values_Analysis1_Model3$param == par[j]],
                                Values_Analysis1_Model3$est[Values_Analysis1_Model3$study == "S3" & Values_Analysis1_Model3$paramHeader == header[j] & Values_Analysis1_Model3$param == par[j]]),
                         sei = c(Values_Analysis1_Model3$posterior_sd[Values_Analysis1_Model3$study == "S1" & Values_Analysis1_Model3$paramHeader == header[j] & Values_Analysis1_Model3$param == par[j]],
                                 Values_Analysis1_Model3$posterior_sd[Values_Analysis1_Model3$study == "S2" & Values_Analysis1_Model3$paramHeader == header[j] & Values_Analysis1_Model3$param == par[j]],
                                 Values_Analysis1_Model3$posterior_sd[Values_Analysis1_Model3$study == "S3" & Values_Analysis1_Model3$paramHeader == header[j] & Values_Analysis1_Model3$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_close_peers_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 2) {simple_effect_family_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 3) {simple_effect_weak_ties_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      
      # Meta-analyze between-person effects from baseline models
      bp <- metafor::rma(yi = c(Values_Analysis1_Model3$est[Values_Analysis1_Model3$study == "S1" & Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == par[j]],
                                Values_Analysis1_Model3$est[Values_Analysis1_Model3$study == "S2" & Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == par[j]],
                                Values_Analysis1_Model3$est[Values_Analysis1_Model3$study == "S3" & Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == par[j]]),
                         sei = c(Values_Analysis1_Model3$posterior_sd[Values_Analysis1_Model3$study == "S1" & Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == par[j]],
                                 Values_Analysis1_Model3$posterior_sd[Values_Analysis1_Model3$study == "S2" & Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == par[j]],
                                 Values_Analysis1_Model3$posterior_sd[Values_Analysis1_Model3$study == "S3" & Values_Analysis1_Model3$paramHeader == "WB.ON" & Values_Analysis1_Model3$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_close_peers_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 2) {simple_effect_family_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 3) {simple_effect_weak_ties_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
    }
  }
  if(i %in% c(2:6)) {
    for(j in 1:3) {
      
      # Meta-analyze within-person effects from personality models
      wp <- metafor::rma(yi = c(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header[j] & Values_Analysis1_Model3_Pers$param == par[j]],
                                Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header[j] & Values_Analysis1_Model3_Pers$param == par[j]],
                                Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header[j] & Values_Analysis1_Model3_Pers$param == par[j]]),
                         sei = c(Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header[j] & Values_Analysis1_Model3_Pers$param == par[j]],
                                 Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header[j] & Values_Analysis1_Model3_Pers$param == par[j]],
                                 Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header[j] & Values_Analysis1_Model3_Pers$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_close_peers_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 2) {simple_effect_family_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      if (j == 3) {simple_effect_weak_ties_wp[i] <- p_value(wp$b, wp$ci.lb, wp$ci.ub, wp$pval, 0.01)}
      
      # Meta-analyze between-person effects from personality models
      bp <- metafor::rma(yi = c(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == par[j]],
                                Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == par[j]],
                                Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == par[j]]),
                         sei = c(Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == par[j]],
                                 Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == par[j]],
                                 Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == par[j]]),
                         method = "FE")
      if (j == 1) {simple_effect_close_peers_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 2) {simple_effect_family_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      if (j == 3) {simple_effect_weak_ties_bp[i] <- p_value(bp$b, bp$ci.lb, bp$ci.ub, bp$pval, 0.01)}
      
      # Meta-analyze cross-level interactions from personality models
      interaction <- metafor::rma(yi = c(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header2[j] & Values_Analysis1_Model3_Pers$param == "P"],
                                         Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header2[j] & Values_Analysis1_Model3_Pers$param == "P"],
                                         Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header2[j] & Values_Analysis1_Model3_Pers$param == "P"]),
                                  sei = c(Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header2[j] & Values_Analysis1_Model3_Pers$param == "P"],
                                          Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header2[j] & Values_Analysis1_Model3_Pers$param == "P"],
                                          Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == header2[j] & Values_Analysis1_Model3_Pers$param == "P"]),
                                  method = "FE")
      if (j == 1) {interaction_effect_close_peers_pers[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)}
      if (j == 2) {interaction_effect_family_pers[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)}
      if (j == 3) {interaction_effect_weak_ties_pers[i] <- p_value(interaction$b, interaction$ci.lb, interaction$ci.ub, interaction$pval, 0.01)}
    }
    
    # Meta-analyze simple effects of personality traits from personality models
    personality <- metafor::rma(yi = c(Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"],
                                       Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"],
                                       Values_Analysis1_Model3_Pers$est[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"]),
                                sei = c(Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S1" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"],
                                        Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S2" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"],
                                        Values_Analysis1_Model3_Pers$posterior_sd[Values_Analysis1_Model3_Pers$study == "S3" & Values_Analysis1_Model3_Pers$pers == perso[i-1] & Values_Analysis1_Model3_Pers$paramHeader == "WB.ON" & Values_Analysis1_Model3_Pers$param == "P"]),
                                method = "FE")
    simple_effect_pers[i] <- p_value(personality$b, personality$ci.lb, personality$ci.ub, personality$pval, 0.01)
  }
}

# Combine all effects into one data frame
res3_meta <- rbind(simple_effect_close_peers_wp, simple_effect_family_wp, simple_effect_weak_ties_wp,
             simple_effect_close_peers_bp, simple_effect_family_bp, simple_effect_weak_ties_bp,
             simple_effect_pers,
             interaction_effect_close_peers_pers, interaction_effect_family_pers, interaction_effect_weak_ties_pers)

# Lower half of Table 6
res3_meta
