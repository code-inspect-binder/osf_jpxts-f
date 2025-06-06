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
library(Lambda4)
library(psych)
library(numform)

# Read in data
survey1 <- read.csv("data_clean_S1.csv")
survey2 <- read.csv("data_clean_S2.csv")
survey3 <- read.csv("data_clean_S3.csv")


#-------------------------------
#  Participants
#-------------------------------

### Create data sets with unique participants
sub1 <- survey1[!duplicated(survey1$id), ]
sub2 <- survey2[!duplicated(survey2$id), ]
sub3 <- survey3[!duplicated(survey3$id), ]

### Gender, age, academic class, and ethnic group (see section "Participants")
sub <- sub1 # choose data set
dim(sub) # number of participants
round(prop.table(table(sub$gender)), 3) # gender (0 = male, 1 = female)
round(mean(sub$age, na.rm = T), 1) # mean age
round(sd(sub$age, na.rm = T), 1) # sd age
round(prop.table(table(sub$academicclass)), 3) # academic class

sum(!is.na(sub$ethnic_group) & sub$ethnic_group == "4") # number of participants who chose option 4
sum(!is.na(sub$ethnic_group) & sub$ethnic_group == "2") # number of participants who chose option 2
sum(!is.na(sub$ethnic_group) & sub$ethnic_group == "3") # number of participants who chose option 3
sum(!is.na(sub$ethnic_group) & sub$ethnic_group == "1") # number of participants who chose option 1
sum(!is.na(sub$ethnic_group) & sub$ethnic_group %in% c("5", "6", "7")) # number of participants who chose option 5, 6, or 7
sum(!(is.na(sub$ethnic_group) | sub$ethnic_group %in% c("4", "2", "3", "1", "5", "6", "7")))  # number of participants who chose multiple options
sum(is.na(sub$ethnic_group))  # number of missing responses

# Number of participants per specific ethnic group in S1, S2, S3
# 4: 354, 136, 292 (Anglo/White)
# 2: 220, 122, 183 (Asian/ Asian American)
# 3: 217, 97, 216 (Hispanic/Latino)
# 1: 44, 22, 43 (African American/Black)
# 5, 6, 7: 1 + 11 = 12, 2 + 6 = 8, 2 + 16 = 18 (Native American, Pacific Islander, other)
# Multi: 111, 41, 110
# Missing: 402, 425, 2

# Calculate proportions
round(c(354, 220, 217, 44, 111) / 958 * 100, 1) # S1
round(c(136, 122, 97, 22, 41) / 426 * 100, 1) # S2
round(c(292, 183, 216, 43, 110) / 862 * 100, 1) # S3


#-------------------------------
#  Procedures
#-------------------------------

### Average time window between surveys (see section "Procedures")
survey <- survey1 # choose data set
survey$date <- as.Date(survey$StartDate) # create date variable
survey$previous <- c(NA, unlist(lapply(1:nrow(survey), function(x) survey$StartDate[x-1]))) # create lagged time stamp
survey$id_date <- paste(survey$id, survey$date, by = " ")
survey$previous <- ifelse(!duplicated(survey$id_date), NA, survey$previous) # set first time stamp of the day to missing
survey$time_diff <- difftime(survey$StartDate, survey$previous, units = "mins") # calculate time difference between consecutive surveys within the same day
round(mean(survey$time_diff, na.rm = T), 0)

### Distribution of surveys across time of day (see section "Procedures")
survey <- survey1 # choose data set
survey$StartDate <- as.POSIXct(survey$StartDate, format = "%Y-%m-%d %H:%M:%S") # create posixct variable
survey$hour <- as.integer(format(survey$StartDate, format= "%H")) # extract hour from time stamp

# Function to categorize time stamps as night (before 9am), morning (9am - noon), midday (noon - 3pm), afternoon (3pm - 6pm), and evening (6pm - 9pm)
time_of_day <- function(x) {
  if (x >= 0 & x < 9) {
    return("night")
  }
  else if (x < 12) {
    return("morning")
  }
  else if (x < 15) {
    return("midday")
  }
  else if (x < 18) {
    return("afternoon")
  }
  else if (x <= 21) {
    return("evening")
  } else {
    return(NA)
  }
}

survey$time_of_day <- unlist(lapply(survey$hour, time_of_day)) # apply function to hour variable 
round(prop.table(table(survey$time_of_day)), 2)


#-------------------------------
#  Measures: Reliabilities
#-------------------------------

### Omega Well-being

# Set working directory to the folder "03_Descriptives/Omega"
setwd("../03_Descriptives/Omega")

# Function to recode variables
reverse <- function(x) {
  x <- 5-x
}

# Study 1
omega <- survey1[, c("id", "content", "stressed", "lonely")] # choose variables
omega$stressed <- reverse(omega$stressed) # recode stressed
omega$lonely <- reverse(omega$lonely) # recode lonely
MplusAutomation::prepareMplusData(omega, "Omega.dat") # save data set

# The code below only works if Mplus is installed
# MplusAutomation::runModels("OMEGA_S1.inp") # run Mplus file (3 items)

res1 <- MplusAutomation::readModels("OMEGA_S1.out")$parameters$unstandardized # extract unstandardized parameters
well_being_omega_within_1 <- res1$est[res1$paramHeader == "New.Additional.Parameters" & res1$param == "OMEGAW"]
well_being_omega_within_1 <- format(round(well_being_omega_within_1, 2), nsmall = 2) # omega  within
well_being_omega_between_1 <- res1$est[res1$paramHeader == "New.Additional.Parameters" & res1$param == "OMEGAB"]
well_being_omega_between_1 <- format(round(well_being_omega_between_1, 2), nsmall = 2) # omega between

# Study 2
omega <- survey2[, c("id", "content", "stressed", "lonely")] # choose variables
omega$stressed <- reverse(omega$stressed) # recode stressed
omega$lonely <- reverse(omega$lonely) # recode lonely
MplusAutomation::prepareMplusData(omega, "Omega.dat") # save data set

# The code below only works if Mplus is installed
# MplusAutomation::runModels("OMEGA_S2.inp") # run Mplus file (3 items)

res2 <- MplusAutomation::readModels("OMEGA_S2.out")$parameters$unstandardized # extract unstandardized parameters
well_being_omega_within_2 <- res2$est[res2$paramHeader == "New.Additional.Parameters" & res2$param == "OMEGAW"]
well_being_omega_within_2 <- format(round(well_being_omega_within_2, 2), nsmall = 2) # omega  within
well_being_omega_between_2 <- res2$est[res2$paramHeader == "New.Additional.Parameters" & res2$param == "OMEGAB"]
well_being_omega_between_2 <- format(round(well_being_omega_between_2, 2), nsmall = 2) # omega between

# Study 3
omega <- survey3[, c("id", "angry", "worried", "happy", "sad")] # choose variables
omega$angry <- reverse(omega$angry) # recode angry
omega$worried <- reverse(omega$worried) # recode worried
omega$sad <- reverse(omega$sad) # recode sad
MplusAutomation::prepareMplusData(omega, "Omega.dat") # save data set

# The code below only works if Mplus is installed
# MplusAutomation::runModels("OMEGA_S3.inp") # run Mplus file (4 items)

res3 <- MplusAutomation::readModels("OMEGA_S3.out")$parameters$unstandardized # extract unstandardized parameters
affect_balance_omega_within_3 <- res3$est[res3$paramHeader == "New.Additional.Parameters" & res3$param == "OMEGAW"]
affect_balance_omega_within_3 <- format(round(affect_balance_omega_within_3, 2), nsmall = 2) # omega  within
affect_balance_omega_between_3 <- res3$est[res3$paramHeader == "New.Additional.Parameters" & res3$param == "OMEGAB"]
affect_balance_omega_between_3 <- format(round(affect_balance_omega_between_3, 2), nsmall = 2) # omega between

# Overview (see section "State Measures: Well-Being")
well_being_omega_within_1
well_being_omega_between_1

well_being_omega_within_2
well_being_omega_between_2

affect_balance_omega_within_3
affect_balance_omega_between_3

### Omega Personality

# Function to recode variables
reverse <- function(x) {
  x <- 6-x
}

# Function to calculate omega between for L2 variables
omega_between <- function(survey, item_numbers, reverse_items) {
  dat <- survey[!duplicated(survey$id), paste0("bfi_", item_numbers)] # choose variables 
  for(i in 1:length(reverse_items)) {
    dat[, paste0("bfi_", reverse_items[i])] <- reverse(dat[, paste0("bfi_", reverse_items[i])]) # recode variables
  }
  format(round(as.numeric(Lambda4::omega.tot(dat, factors = 1)), 2), nsmall = 2) # calculate and format omega between
}

# Apply function to personality variables in S1
extraversion_omega_between_1 <- omega_between(survey1, item_numbers = c(1, 6, 11, 16, 21, 26, 31, 36),  reverse_items = c(6, 21, 31))
agreeableness_omega_between_1 <- omega_between(survey1, item_numbers = c(2, 7, 12, 17, 22, 27, 32, 37, 42), reverse_items = c(2, 12, 27, 37))
conscientiousness_omega_between_1 <- omega_between(survey1, item_numbers = c(3, 8, 13, 18, 23, 28, 33, 38, 43), reverse_items = c(8, 18, 23, 43))
neuroticism_omega_between_1 <- omega_between(survey1, item_numbers = c(4, 9, 14, 19, 24, 29, 34, 39), reverse_items = c(9, 24, 34))
openness_omega_between_1 <- omega_between(survey1, item_numbers = c(5, 10, 15, 20, 25, 30, 35, 40, 41, 44), reverse_items = c(35, 41))

# Apply function to personality variables in S2
extraversion_omega_between_2 <- omega_between(survey2, item_numbers = c(1, 6, 11, 16, 21, 26, 31, 36),  reverse_items = c(6, 21, 31))
agreeableness_omega_between_2 <- omega_between(survey2, item_numbers = c(2, 7, 12, 17, 22, 27, 32, 37, 42), reverse_items = c(2, 12, 27, 37))
conscientiousness_omega_between_2 <- omega_between(survey2, item_numbers = c(3, 8, 13, 18, 23, 28, 33, 38, 43), reverse_items = c(8, 18, 23, 43))
neuroticism_omega_between_2 <- omega_between(survey2, item_numbers = c(4, 9, 14, 19, 24, 29, 34, 39), reverse_items = c(9, 24, 34))
openness_omega_between_2 <- omega_between(survey2, item_numbers = c(5, 10, 15, 20, 25, 30, 35, 40, 41, 44), reverse_items = c(35, 41))

# Apply function to personality variables in S3
extraversion_omega_between_3 <- omega_between(survey3, item_numbers = c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56),  reverse_items = c(11, 16, 26, 31, 36, 51))
agreeableness_omega_between_3 <- omega_between(survey3, item_numbers = c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57), reverse_items = c(12, 17, 22, 37, 42, 47))
conscientiousness_omega_between_3 <- omega_between(survey3, item_numbers = c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58), reverse_items = c(3, 8, 23, 28, 48, 58))
neuroticism_omega_between_3 <- omega_between(survey3, item_numbers = c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59), reverse_items = c(4, 9, 24, 29, 44, 49))
openness_omega_between_3 <- omega_between(survey3, item_numbers = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60), reverse_items = c(5, 25, 30, 45, 50, 55))

# Overview (see section "Trait Measures: Personality")
omega_1 <- as.numeric(c(extraversion_omega_between_1, agreeableness_omega_between_1, conscientiousness_omega_between_1, neuroticism_omega_between_1, openness_omega_between_1))
mean(omega_1)
min(omega_1)
max(omega_1)

omega_2 <- as.numeric(c(extraversion_omega_between_2, agreeableness_omega_between_2, conscientiousness_omega_between_2, neuroticism_omega_between_2, openness_omega_between_2))
mean(omega_2)
min(omega_2)
max(omega_2)

omega_3 <- as.numeric(c(extraversion_omega_between_3, agreeableness_omega_between_3, conscientiousness_omega_between_3, neuroticism_omega_between_3, openness_omega_between_3))
mean(omega_3)
min(omega_3)
max(omega_3)


#-------------------------------
#  Compliance
#-------------------------------

### Observations per participant (see section "Preprocessing of Experience Sampling Data and Compliance")
survey <- survey1 # choose data set
dim(survey) # total number of observations
length(unique(survey$id)) # total number of participants
summary <- aggregate(survey$id, by = list(id = survey$id), length) # calculate number of observations per participant
round(mean(summary$x), 1) # mean number of observations per participant
round(sd(summary$x), 1) # sd number of observations per participant


#-------------------------------
#  Statistical Analyses
#-------------------------------

# Observations for Analysis 2 (see section "Statistical Analyses/Analysis 2")
survey <- survey1 # choose data set
sub <- survey[survey$interaction != 0 & !(is.na(survey$FtF) | is.na(survey$CMC) | is.na(survey$mixed) | is.na(survey$close_peers) | is.na(survey$family) | is.na(survey$weak_ties)), ]
nrow(sub) # number of observations
length(unique(sub$id)) # number of participants


#---------------------------------
#  Number of Interactions Per Type
#---------------------------------

# See Table 2

table1 <- NULL
table2 <- NULL
table3 <- NULL

for (k in 1:3) {
  survey <- eval(parse(text = paste0("survey", k))) # choose survey
  # choose variables
  survey <- survey[, c("interaction", "FtF", "CMC", "TVC", "TCE", "SN", "mixed_CMC", "mixed", "close_peers", "family", "weak_ties", "mixed_partner")]
  
  table <- data.frame(matrix(ncol = 7, nrow = 0))
  mode_communication <- c("survey$FtF == 1", "survey$CMC == 1",
                          "survey$TVC == 1", "survey$TCE == 1", "survey$SN == 1", "survey$mixed_CMC == 1",
                          "survey$mixed == 1",
                          "is.na(survey$FtF)",
                          "survey$interaction == 0",
                          "!is.na(survey$interaction)")
  # rows refer to: FtF, CMC, TVC, TCE, SNS, Multiple, Mixed, Unknown, No interaction, Total
  
  for (i in 1:length(mode_communication)) {
    row <- c(nrow(survey[which(eval(parse(text = mode_communication[i])) & survey$close_peers == 1), ]), # Close peers
             nrow(survey[which(eval(parse(text = mode_communication[i])) & survey$family == 1), ]), # Family
             nrow(survey[which(eval(parse(text = mode_communication[i])) & survey$weak_ties == 1), ]), # Weak ties
             nrow(survey[which(eval(parse(text = mode_communication[i])) & survey$mixed_partner == 1), ]), # Multiple
             nrow(survey[which(eval(parse(text = mode_communication[i])) & is.na(survey$mixed_partner)), ]), # Unknown
             nrow(survey[which(eval(parse(text = mode_communication[i])) & survey$interaction == 0), ]), # No interaction
             nrow(survey[which(eval(parse(text = mode_communication[i]))), ])) # Total
    row <- format(row, big.mark = ",", trim = TRUE)
    table <- rbind(table, row)
  }
  
  # Save data frames
  if (k == 1) {
    table1 <- table
  }
  
  if (k == 2) {
    table2 <- table
  }
  
  if (k == 3) {
    table3 <- table
  }
}

# Create new data frame
table <- data.frame(matrix(ncol = 7, nrow = 10))
names(table) <- c("close_peers", "family", "weak_ties", "multiple", "unknown", "no_interaction", "total")
row.names(table) <- c("FtF", "CMC", "TVC", "TCE", "SNS", "multiple", "mixed", "unknown", "no_interaction", "total")

# Fill the data frame with results from S1 / S2 / S3
for(i in 1:nrow(table)) {
  for(j in 1:ncol(table)) {
    table[i,j] <- paste(table1[i,j], table2[i,j], table3[i,j], sep = "/")
  }
}
table

# Calculate percentages
survey <- survey1 # choose data set

# Share of different modes of communication (within social interactions)
round(length(which(survey$FtF == 1)) / length(which(survey$interaction == 1))*100, 0)
round(length(which(survey$CMC == 1)) / length(which(survey$interaction == 1))*100, 0)
round(length(which(survey$mixed == 1)) / length(which(survey$interaction == 1))*100, 0)

# Share of different CMC channels (within computer-mediated interactions)
round(length(which(survey$TVC == 1)) / length(which(survey$CMC == 1))*100, 0)
round(length(which(survey$TCE == 1)) / length(which(survey$CMC == 1))*100, 0)
round(length(which(survey$SN == 1)) / length(which(survey$CMC == 1))*100, 0)
round(length(which(survey$mixed_CMC == 1)) / length(which(survey$CMC == 1))*100, 0)


#-------------------------------
#  Summary table
#-------------------------------

# See Table 3

# Function to calculate mean for for L2 variables
mean_L2 <- function(x) {mean(x[!duplicated(survey$id)], na.rm = T)}

# Function to calculate sd for for L2 variables
sd_L2 <- function(x) {sd(x[!duplicated(survey$id)], na.rm = T)}

correlations1 <- NULL
correlations2 <- NULL
correlations3 <- NULL

for (k in 1:3) {
  survey <- eval(parse(text = paste0("survey", k))) # choose survey
  # choose variables
  if (k %in% c(1, 2)) {
    survey <- survey[, c("well_being", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o", "id")]
  }
  if (k == 3) {
    survey <- survey[, c("affect_balance", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o", "id")]
    names(survey)[1] <- "well_being"
  }
  
  survey$id <- as.character(survey$id)
  summary <- psych::statsBy(data = survey, "id")
  rbg <- matrix(numform::f_num(summary$rbg, digits = 2), 13, 13) # between-person correlations
  rwg <- matrix(numform::f_num(summary$rwg, digits = 2), 13, 13) # within-person correlations
  pbg <- summary$pbg # p-values for between-person correlations
  pwg <- summary$pwg # p-values for within-person correlations
  
  # Create new data frame
  correlations <- data.frame(matrix(ncol = 13, nrow = 13))
  
  # Fill the data frame with between- and within-person correlations
  for(i in 1:nrow(correlations)) {
    for(j in 1:ncol(correlations)) {
      if(upper.tri(correlations)[i,j] == TRUE) {
        correlations[i,j] <- ifelse(pbg[i,j] < 0.01, paste0(rbg[i,j], "*"), rbg[i,j]) # rows above the diagonal = between-person correlations
      }
      if(lower.tri(correlations)[i,j] == TRUE){
        correlations[i,j] <- ifelse(pwg[i,j] < 0.01, paste0(rwg[i,j], "*"), rwg[i,j]) # rows below the diagonal = within-person correlations
      }
      if(upper.tri(correlations)[i,j] == FALSE & lower.tri(correlations)[i,j] == FALSE){
        correlations[i,j] <- "" # diagonal = empty
      }
    }
  }
  
  # Calculate means for all variables
  means <- c(apply(survey[c("well_being", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties")], 2, mean, na.rm = T),
             apply(survey[c("bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")], 2, mean_L2))
  means <- format(round(means, 2), nsmall = 2)
  
  # Calculate within variables
  survey$well_being.wp <- EMAtools::pcenter(survey$id, survey$well_being)
  survey$interaction.wp <- EMAtools::pcenter(survey$id, survey$interaction)
  survey$FtF.wp <- EMAtools::pcenter(survey$id, survey$FtF)
  survey$CMC.wp <- EMAtools::pcenter(survey$id, survey$CMC)
  survey$mixed.wp <- EMAtools::pcenter(survey$id, survey$mixed)
  survey$close_peers.wp <- EMAtools::pcenter(survey$id, survey$close_peers)
  survey$family.wp <- EMAtools::pcenter(survey$id, survey$family)
  survey$weak_ties.wp <- EMAtools::pcenter(survey$id, survey$weak_ties)
  
  # Calculate between variables
  survey$well_being.bp <- EMAtools::pmean(survey$id, survey$well_being)
  survey$interaction.bp <- EMAtools::pmean(survey$id, survey$interaction)
  survey$FtF.bp <- EMAtools::pmean(survey$id, survey$FtF)
  survey$CMC.bp <- EMAtools::pmean(survey$id, survey$CMC)
  survey$mixed.bp <- EMAtools::pmean(survey$id, survey$mixed)
  survey$close_peers.bp <- EMAtools::pmean(survey$id, survey$close_peers)
  survey$family.bp <- EMAtools::pmean(survey$id, survey$family)
  survey$weak_ties.bp <- EMAtools::pmean(survey$id, survey$weak_ties)
  
  # Calculate sds for all variables
  sds_within <- c(apply(survey[c("well_being.wp", "interaction.wp", "FtF.wp", "CMC.wp", "mixed.wp", "close_peers.wp", "family.wp", "weak_ties.wp")], 2, sd, na.rm = T),
                  rep(NA, 5))
  sds_within <- format(round(sds_within, 2), nsmall = 2)
  sds_between <- c(apply(survey[c("well_being.bp", "interaction.bp", "FtF.bp", "CMC.bp", "mixed.bp", "close_peers.bp", "family.bp", "weak_ties.bp",
                                  "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")], 2, sd_L2))
  sds_between <- format(round(sds_between, 2), nsmall = 2)
  
  # Combine correlations, means, and sds into one data frame
  correlations <- rbind(correlations, means, sds_within, sds_between)
  
  # Save data frames
  if (k == 1) {
    correlations1 <- correlations
  }
  
  if (k == 2) {
    correlations2 <- correlations
  }
  
  if (k == 3) {
    correlations3 <- correlations
  }
}

# Create new data frame
correlations <- data.frame(matrix(ncol = 13, nrow = 16))
names(correlations) <- c("well_being", "interaction", "FtF", "CMC", "mixed", "close_peers", "family", "weak_ties", "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")

# Fill the data frame with results from S1 / S2 / S3
for(i in 1:nrow(correlations)) {
  for(j in 1:ncol(correlations)) {
    correlations[i,j] <- paste(correlations1[i,j], correlations2[i,j], correlations3[i,j], sep = "/")
  }
}
correlations
