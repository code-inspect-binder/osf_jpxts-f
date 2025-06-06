## This code is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
## 
## For a copy of the GNU General Public License, 
## see <http://www.gnu.org/licenses/>.

## (c) 2022 Lara Kroencke


# Set working directory to the folder "02_DataPrep"
setwd("Statistical Code/Final Submission/02_DataPrep")

# Load packages
library(MplusAutomation)

# Read in data
survey1 <- read.csv("data_clean_S1.csv")
survey2 <- read.csv("data_clean_S2.csv")
survey3 <- read.csv("data_clean_S3.csv")

# Standardization of well-being
survey1$well_being_z <- (survey1$well_being - mean(survey1$well_being, na.rm = T)) / sd(survey1$well_being, na.rm = T)
survey1$id <- paste0(survey1$id, "_S1")

survey2$well_being_z <- (survey2$well_being - mean(survey2$well_being, na.rm = T)) / sd(survey2$well_being, na.rm = T)
survey2$id <- paste0(survey2$id, "_S2")

survey3$well_being_z <- (survey3$affect_balance - mean(survey3$affect_balance, na.rm = T)) / sd(survey3$affect_balance, na.rm = T)
survey3$id <- paste0(survey3$id, "_S3")

# Combine all surveys into one data frame
survey <- rbind(survey1[, c("id", "well_being_z", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_n", "gender", "ethnicity", "SES", "weekend")],
                survey2[, c("id", "well_being_z", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_n", "gender", "ethnicity", "SES", "weekend")],
                survey3[, c("id", "well_being_z", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_n", "gender", "ethnicity", "SES", "weekend")])
survey$id <- as.numeric(as.factor(survey$id))


### Plot 1

setwd("../05_Plots/Simple slope/Plot 1") # set working directory to the folder "05_Plots/Simple slope/Plot 1"
survey_plot1 <- survey[, c("id", "well_being_z", "interaction", "bfi_n", "gender", "ethnicity", "SES", "weekend")] # choose variables
MplusAutomation::prepareMplusData(survey_plot1, "analysis1_model1.dat") # prepare Mplus data set

# Well-being
round(min(survey_plot1$well_being_z, na.rm = T), 1) # minimum well-being = -3
round(max(survey_plot1$well_being_z, na.rm = T), 1) # maximum well-being = 1.5

# Neuroticism
survey_plot1$neuroticism.cgm <- survey_plot1$bfi_n - mean(survey_plot1$bfi_n[!duplicated(survey_plot1$id)], na.rm = TRUE) # grand-mean center neuroticism
N <- survey_plot1$neuroticism.cgm[!duplicated(survey_plot1$id)] # extract single value per person
N <- N[!is.na(N)] # delete missing values
round(summary(N), 2) # minimum neuroticism = -2.03, maximum neuroticism = 1.89
round(sd(N), 2) # sd neuroticism = 0.77

# Function to calculate percent ranks
perc.rank <- function(x) {
  length(N[N <= x])/length(N)*100
}

# Calculate percent ranks for -2 SD, -1 SD, 0, 1 SD, 2 SD
percent_ranks <- as.data.frame(cbind(x = N, y = unlist(lapply(N, perc.rank))))
round(percent_ranks[which.min(abs(percent_ranks$x+1.54)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x+0.77)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x-0.77)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x-1.54)),], 2)

### Plot 2 + Plot 3

setwd("../Plot 2") # set working directory to the folder "05_Plots/Simple slope/Plot 2"
survey_plot2 <- survey[, c("id", "well_being_z", "FtF", "CMC", "mixed", "bfi_n", "gender", "ethnicity", "SES", "weekend")] # choose variables
survey_plot2 <- survey_plot2[-which(is.na(survey_plot2$FtF) | is.na(survey_plot2$CMC) | is.na(survey_plot2$mixed)), ] # delete missing values
MplusAutomation::prepareMplusData(survey_plot2, "analysis1_model2.dat") # prepare Mplus data set

# Well-being
round(min(survey_plot2$well_being_z, na.rm = T), 1) # minimum well-being = -3
round(max(survey_plot2$well_being_z, na.rm = T), 1) # maximum well-being = 1.5

# Neuroticism
survey_plot2$neuroticism.cgm <- survey_plot2$bfi_n - mean(survey_plot2$bfi_n[!duplicated(survey_plot2$id)], na.rm = TRUE) # grand-mean center neuroticism
N <- survey_plot2$neuroticism.cgm[!duplicated(survey_plot2$id)] # extract single value per person
N <- N[!is.na(N)] # delete missing values
round(summary(N), 2) # minimum neuroticism = -2.02, maximum neuroticism = 1.89
round(sd(N), 2) # sd neuroticism = 0.77

# Calculate percent ranks for -2 SD, -1 SD, 0, 1 SD, 2 SD
percent_ranks <- as.data.frame(cbind(x = N, y = unlist(lapply(N, perc.rank))))
round(percent_ranks[which.min(abs(percent_ranks$x+1.54)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x+0.77)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x-0.77)),], 2)
round(percent_ranks[which.min(abs(percent_ranks$x-1.54)),], 2)

### Plot 4

setwd("../Plot 4") # set working directory to the folder "05_Plots/Simple slope/Plot 4"
survey_plot4 <- survey[which(survey$interaction != 0), ] # delete social interactions
survey_plot4 <- survey_plot4[, c("id", "well_being_z", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_n", "gender", "ethnicity", "SES", "weekend")] # choose variables
survey_plot4 <- survey_plot4[-which(is.na(survey_plot4$FtF) | is.na(survey_plot4$CMC) | is.na(survey_plot4$mixed) |
                                      is.na(survey_plot4$close_peers) | is.na(survey_plot4$family) | is.na(survey_plot4$weak_ties)), ] # delete missing values
MplusAutomation::prepareMplusData(survey_plot4, "analysis2.dat") # prepare Mplus data set

# Well-being
round(min(survey_plot4$well_being_z, na.rm = T), 1) # minimum well-being = -3
round(max(survey_plot4$well_being_z, na.rm = T), 1) # maximum well-being = 1.5


### Power analysis

setwd("./../../../06_PowerAnalysis") # set working directory to the folder "06_PowerAnalysis"
survey_power <- survey[, c("id", "well_being_z", "interaction", "bfi_n")] # choose variables
MplusAutomation::prepareMplusData(survey_power, "analysis1_model1.dat") # prepare Mplus data set
