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


##########CHOOSE ONE############

# Data from S3
survey <- read.csv("data3-reproduce analyses.csv")

dim(survey) # number of observations: 67415 (S3)
length(unique(survey$id)) # number of participants: 888 (S3)


################################
#########PREPROCESSING##########
################################

# The original data set included included 74097 observations. 77 surveys were completed by test users, resulting in 74020 valid observations.
# 736 surveys were removed because the anonymous participant IDs were missing due to technical problems.
# In addition, the following reports and participants were already removed from the data set “data3-reproduce analyses.csv” as part of the CoCo project data cleaning (see section "Preprocessing of Experience Sampling Data and Compliance"):
# Reports that were completed too soon after the previous report (i.e., started a new report less than 60 min after previous report was started)
# Reports that were completed too quickly (i.e., less than 0.5 seconds per question)
# Reports that took too long to complete (>60 minutes)
# Participants who responded "strongly disagree" or “disagree” to the following question at the end of the writing assignment: "I responded as truthfully as possible to the daily surveys during Step 2 of self-tracking. Note: Your response to this question will NOT affect your grade."
# Participants who were under 18 at the start of the assignment.
# During this process, 1207 observations were removed because of age and 4038 observations were removed based on the other criteria listed above.
# In addition, 624 observations were removed for this project because they were completed after the end of the assignment.


### Delete participants who are too young (<18 years) or too old (>24 years)

survey <- survey[-which(survey$age < 18 | survey$age > 24), ] # delete all participants who are younger than 18 or older than 24

dim(survey) # number of observations: 65591 (S3)
length(unique(survey$id)) # number of participants: 864 (S3)

# Number of observations that were removed because of age: 1824 (S3)


### Remove partial reports

table(survey$Finished, useNA = "ifany") # number of surveys to exclude: 95 (S3)
survey <- survey[survey$Finished == 1,]  # keep only finished surveys (Finished == 1)

dim(survey) # number of observations: 65496 (S3)
length(unique(survey$id)) # number of participants: 864 (S3)

# Number of observations removed during data cleaning: 4133 (S3)
round(4133/74020*100, 0) # 6%


################################
###########VARIABLES############
################################

### Well-being

# Calculate affect balance
survey$negative_affect <- apply(survey[c("angry", "worried", "sad")], 1, function(x) {mean(x, na.rm = TRUE)})
survey$affect_balance <- survey$happy - survey$negative_affect

# Calculate well-being variable (stressed and lonely reverse coded)
survey$stressed_r <- 5 - survey$stressed
survey$lonely_r <- 5 - survey$lonely

survey$well_being <- apply(survey[c("happy", "stressed_r", "lonely_r")], 1, function(x) {mean(x, na.rm = TRUE)})


### Mode of communication

# Initiate variables

survey$TiP <- 0
survey$ToP <- 0
survey$Texting <- 0
survey$ChattingDATING <- 0
survey$Emailing <- 0
survey$Videochatting <- 0
survey$Socialmedia <- 0
survey$OTHER <- 0
survey$SKIP <- 0

# Participants could select all response options that applied to them in any given situation. Here, we create one dummy variable for every response option.

for (i in 1:length(survey$interaction_mode)) { 
  v <- unlist(strsplit(survey$interaction_mode[i], ","))
  if (is.element("1", v)) {
    survey$TiP[i] <- 1
  }
  if (is.element("2", v)) {
    survey$ToP[i] <- 1
  }
  if (is.element("3", v)) {
    survey$Texting[i] <- 1
  }
  if (is.element("4", v)) {
    survey$ChattingDATING[i] <- 1
  }
  if (is.element("5", v)) {
    survey$Emailing[i] <- 1
  }
  if (is.element("6", v)) {
    survey$Videochatting[i] <- 1
  }
  if (is.element("7", v)) {
    survey$Socialmedia[i] <- 1
  }
  if (is.element("8", v)) {
    survey$OTHER[i] <- 1
  }
  if (is.na(survey$interaction_mode[i]) & survey$interaction[i] == 1) {
    survey$SKIP[i] <- 1
  }
}


### Type of interaction partner

# Initiate variables

survey$Classmates <- 0
survey$Coworkers <- 0
survey$Family <- 0
survey$Friends <- 0
survey$Roommates <- 0
survey$Significant_other <- 0
survey$Strangers <- 0
survey$OTHER2 <- 0
survey$SKIP2 <- 0

# Participants could select all response options that applied to them in any given situation. Here, we create one dummy variable for every response option.

for (i in 1:length(survey$interaction_partner)){ 
  v <- unlist(strsplit(survey$interaction_partner[i], ","))
  if (is.element("1", v)) {
    survey$Classmates[i] <- 1
  }
  if (is.element("2", v)) {
    survey$Coworkers[i] <- 1
  }
  if (is.element("3", v)) {
    survey$Family[i] <- 1
  }
  if (is.element("4", v)) {
    survey$Friends[i] <- 1
  }
  if (is.element("5", v)) {
    survey$Roommates[i] <- 1
  }
  if (is.element("6", v)) {
    survey$Significant_other[i] <- 1
  }
  if (is.element("7", v)) {
    survey$Strangers[i] <- 1
  }
  if (is.element("8", v)) {
    survey$OTHER2[i] <- 1
  }
  if (is.na(survey$interaction_partner[i]) & survey$interaction[i] == 1) {
    survey$SKIP2[i] <- 1
  }
}


### Demographics

# Recode gender

table(survey$gender[!duplicated(survey$id)], useNA = "ifany")
# 264 male, 591 female, 9 other (S3)
survey$gender <- ifelse(survey$gender == 1, 0, ifelse(survey$gender == 2, 1, NA))
table(survey$gender[!duplicated(survey$id)], useNA = "ifany")

# Create dummy variable for ethnicity: 0 = option 4 only (Anglo/White), 1 = any other option / combination of options

table(survey$ethnic_group[!duplicated(survey$id)], useNA = "ifany")
survey$ethnicity <- ifelse(survey$ethnic_group == "4", 0, 1)
table(survey$ethnicity[!duplicated(survey$id)], useNA = "ifany")

# Create dummy variable for SES: 1 = mother or father completed at least some college (4 = some college), 0 = otherwise

table(survey$mother_educationlevel[!duplicated(survey$id)], useNA = "ifany")
table(survey$father_educationlevel[!duplicated(survey$id)], useNA = "ifany")
survey$SES <- ifelse(survey$mother_educationlevel >= 4 | survey$father_educationlevel >= 4, 1, 0)
table(survey$SES[!duplicated(survey$id)], useNA = "ifany")


### Weekend

# Create dummy variable for weekend: 1 = weekend, 0 = weekday

survey$StartDate <- as.POSIXct(survey$StartDate, format = "%Y-%m-%d %H:%M:%S")
survey$weekend <- ifelse(weekdays(survey$StartDate, abbr = TRUE) %in% c("Sat", "Sun"), 1, 0)


################################
############DATASETS############
################################

# Check response scales

table(survey$interaction, useNA = "ifany")
table(survey$interaction_mode, useNA = "ifany") 
table(survey$interaction_partner, useNA = "ifany")

nrow(survey) # number of observations: 65496 (S3)
length(unique(survey$id)) # number of participants: 864 (S3)

### Social interactions

# Create dummy variable for social interactions

survey$interaction <- ifelse(survey$interaction == 1, 1, 0)
table(survey$interaction, useNA = "ifany") # number of observations with social interactions: 31128 (S3)

### Mode of communication

survey$sum_CMC <- apply(survey[c("ToP", "Texting", "ChattingDATING", "Emailing", "Videochatting", "Socialmedia")], 1, sum)

# Create dummy variable for FtF interactions

survey$FtF <- ifelse(survey$SKIP == 1 | survey$OTHER == 1, NA,
                     ifelse(survey$TiP == 1 & survey$sum_CMC == 0, 1, 0))

table(survey$FtF, useNA = "ifany") # number of observations with FtF interactions ONLY: 13411 (S3)
table(survey$interaction_mode[survey$FtF == 1])

# Create dummy variable for CMC

survey$CMC <- ifelse(survey$SKIP == 1 | survey$OTHER == 1, NA,
                     ifelse(survey$TiP == 0 & survey$sum_CMC >= 1, 1, 0))

table(survey$CMC, useNA = "ifany") # number of observations with CMC ONLY: 8394 (S3)
table(survey$interaction_mode[survey$CMC == 1])

# Create dummy variable for mixed episodes

survey$mixed <- ifelse(survey$SKIP == 1 | survey$OTHER == 1, NA,
                       ifelse(survey$TiP == 1 & survey$sum_CMC >= 1, 1, 0))

table(survey$mixed, useNA = "ifany") # number of observations with mixed episodes: 9040 (S3)
table(survey$interaction_mode[survey$mixed == 1])

table(apply(survey[c("FtF", "CMC", "mixed")], 1, sum), useNA = "ifany") # 30845 (S3)
length(which(survey$SKIP == 1 | survey$OTHER == 1))

### Interaction partner

# Create dummy variable for close peers

survey$close_peers <- ifelse(survey$SKIP2 == 1 | survey$OTHER2 == 1, NA,
                             ifelse(survey$Friends == 1 | survey$Roommates == 1 | survey$Significant_other == 1, 1, 0))

table(survey$close_peers, useNA = "ifany") # number of observations with interactions with close peers: 21903 (S3)
table(survey$interaction_partner[survey$close_peers == 1])

# Create dummy variable for family

survey$family <- ifelse(survey$SKIP2 == 1 | survey$OTHER2 == 1, NA,
                        ifelse(survey$Family == 1, 1, 0))

table(survey$family, useNA = "ifany") # number of observations with interactions with family: 12220 (S3)
table(survey$interaction_partner[survey$family == 1])

# Create dummy variable for weak ties

survey$weak_ties <- ifelse(survey$SKIP2 == 1 | survey$OTHER2 == 1, NA,
                           ifelse(survey$Classmates == 1 | survey$Coworkers == 1 | survey$Strangers == 1, 1, 0))

table(survey$weak_ties, useNA = "ifany") # number of observations with interactions with weak ties: 6580 (S3)
table(survey$interaction_partner[survey$weak_ties == 1])

# Create dummy variable for mixed interaction partners

survey$no_partners <- apply(survey[c("close_peers", "family", "weak_ties")], 1, sum)
survey$mixed_partner <- ifelse(survey$no_partners > 1, 1, 0)

table(survey$mixed_partner, useNA = "ifany") # number of observations with mixed interaction partners: 9005 (S3)
table(apply(survey[c("close_peers", "family", "weak_ties")], 1, sum), useNA = "ifany")

survey$close_peers_all <- survey$close_peers
survey$family_all <- survey$family
survey$weak_ties_all <- survey$weak_ties

survey$close_peers <- ifelse(survey$mixed_partner == 1, NA, survey$close_peers)
survey$family <- ifelse(survey$mixed_partner == 1, NA, survey$family)
survey$weak_ties <- ifelse(survey$mixed_partner == 1, NA, survey$weak_ties)

table(survey$close_peers, useNA = "ifany") # number of observations with interactions with close peers ONLY: 13413 (S3)
table(survey$interaction_partner[survey$close_peers == 1])

table(survey$family, useNA = "ifany") # number of observations with interactions with family ONLY: 5196 (S3)
table(survey$interaction_partner[survey$family == 1])

table(survey$weak_ties, useNA = "ifany") # number of observations with interactions with weak ties ONLY: 2971 (S3)
table(survey$interaction_partner[survey$weak_ties == 1])

table(apply(survey[c("close_peers", "family", "weak_ties")], 1, sum), useNA = "ifany") # 21580 (S3)
length(which(survey$SKIP2 == 1 | survey$OTHER2 == 1 | survey$mixed_partner == 1))

table(apply(survey[c("close_peers_all", "family_all", "weak_ties_all")], 1, sum), useNA = "ifany") # 21580, 7892, 1113 (S3)
length(which(survey$SKIP2 == 1 | survey$OTHER2 == 1))

# Friends / roommates vs. significant others

# Create dummy variable for friends and roommates

survey$friends_roommates <- ifelse(survey$SKIP2 == 1 | survey$OTHER2 == 1, NA,
                                   ifelse(survey$Friends == 1 | survey$Roommates == 1, 1, 0))

table(survey$friends_roommates, useNA = "ifany") # number of observations with interactions with friends and roommates: 17750 (S3)
table(survey$interaction_partner[survey$friends_roommates == 1])

# Create dummy variable for significant others

survey$significant_other <- ifelse(survey$SKIP2 == 1 | survey$OTHER2 == 1, NA,
                                   ifelse(survey$Significant_other == 1, 1, 0))

table(survey$significant_other, useNA = "ifany") # number of observations with interactions with significant others: 6958 (S3)
table(survey$interaction_partner[survey$significant_other == 1])

# Create dummy variable for mixed interaction partners

survey$no_partners2 <- apply(survey[c("friends_roommates", "significant_other", "family_all", "weak_ties_all")], 1, sum)
survey$mixed_partner2 <- ifelse(survey$no_partners2 > 1, 1, 0)

table(survey$mixed_partner2, useNA = "ifany") # number of observations with mixed interaction partners: 10518 (S3)
table(apply(survey[c("friends_roommates", "significant_other", "family_all", "weak_ties_all")], 1, sum), useNA = "ifany")

survey$friends_roommates2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$friends_roommates)
survey$significant_other2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$significant_other)
survey$family2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$family_all)
survey$weak_ties2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$weak_ties_all)

table(survey$friends_roommates2, useNA = "ifany") # number of observations with interactions with friends and roommates ONLY: 9298 (S3)
table(survey$interaction_partner[survey$friends_roommates2 == 1])

table(survey$significant_other2, useNA = "ifany") # number of observations with interactions with significant others ONLY: 2602 (S3)
table(survey$interaction_partner[survey$significant_other2 == 1])

table(survey$family2, useNA = "ifany") # number of observations with interactions with family ONLY: 5196 (S3)
table(survey$interaction_partner[survey$family2 == 1])

table(survey$weak_ties2, useNA = "ifany") # number of observations with interactions with weak ties ONLY: 2971 (S3)
table(survey$interaction_partner[survey$weak_ties2 == 1])

table(apply(survey[c("friends_roommates2", "significant_other2", "family2", "weak_ties2")], 1, sum), useNA = "ifany") # 20067 (S3)
length(which(survey$SKIP2 == 1 | survey$OTHER2 == 1 | survey$mixed_partner2 == 1))

### TVC vs. TCE vs. SN

# Create dummy variable for TVC

survey$TVC <- ifelse(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1, NA,
                     ifelse(survey$ToP == 1 | survey$Videochatting == 1, 1, 0))

table(survey$TVC, useNA = "ifany") # number of observations with TVC: 4529 (S3)
table(survey$interaction_mode[survey$TVC == 1])

# Create dummy variable for TCE

survey$TCE <- ifelse(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1, NA,
                     ifelse(survey$Texting == 1 | survey$ChattingDATING == 1 | survey$Emailing == 1, 1, 0))

table(survey$TCE, useNA = "ifany") # number of observations with TCE: 4206 (S3)
table(survey$interaction_mode[survey$TCE == 1])

# Create dummy variable for SN

survey$SN <- ifelse(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1, NA,
                    ifelse(survey$Socialmedia == 1, 1, 0))

table(survey$SN, useNA = "ifany") # number of observations with SN: 1617 (S3)
table(survey$interaction_mode[survey$SN == 1])

# Create dummy variable for mixed CMC

survey$no_cmc <- apply(survey[c("TVC", "TCE", "SN")], 1, sum)
survey$mixed_CMC <- ifelse(survey$no_cmc > 1, 1, 0)

table(survey$mixed_CMC, useNA = "ifany") # number of observations with mixed CMC: 1727 (S3)
table(apply(survey[c("TVC", "TCE", "SN")], 1, sum), useNA = "ifany")

survey$TVC <- ifelse(survey$mixed_CMC == 1, NA, survey$TVC)
survey$TCE <- ifelse(survey$mixed_CMC == 1, NA, survey$TCE)
survey$SN <- ifelse(survey$mixed_CMC == 1, NA, survey$SN)

table(survey$TVC, useNA = "ifany") # number of observations with TVC ONLY: 3444 (S3)
table(survey$interaction_mode[survey$TVC == 1])

table(survey$TCE, useNA = "ifany") # number of observations with TCE ONLY: 2636 (S3)
table(survey$interaction_mode[survey$TCE == 1])

table(survey$SN, useNA = "ifany") # number of observations with SN ONLY: 587 (S3)
table(survey$interaction_mode[survey$SN == 1])

table(apply(survey[c("TVC", "TCE", "SN")], 1, sum), useNA = "ifany") # 6667 (S3)
length(which(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1 | survey$mixed_CMC == 1))

### Create final data set

data_clean <- survey[, c("id", "StartDate", "affect_balance", "angry", "worried", "happy", "sad", "well_being", "stressed", "lonely",
                     "interaction",
                     "FtF", "CMC", "mixed",
                     "close_peers", "family", "weak_ties", "mixed_partner",
                     "close_peers_all", "family_all", "weak_ties_all",
                     "friends_roommates2", "significant_other2", "family2", "weak_ties2", "mixed_partner2",
                     "TVC", "TCE", "SN", "mixed_CMC",
                     "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o",
                     paste0("bfi_", 1:60),
                     "gender", "ethnicity", "SES", "age", "academicclass", "ethnic_group",
                     "weekend")]

dim(data_clean)
sum(is.na(!is.na(data_clean$interaction)))

table(data_clean$affect_balance, useNA = "ifany")
table(data_clean$interaction, useNA = "ifany")
table(apply(survey[c("FtF", "CMC", "mixed")], 1, sum), useNA = "ifany")
table(apply(survey[c("close_peers", "family", "weak_ties")], 1, sum), useNA = "ifany")

write.csv(data_clean, "data_clean_S3.csv", row.names = FALSE)
