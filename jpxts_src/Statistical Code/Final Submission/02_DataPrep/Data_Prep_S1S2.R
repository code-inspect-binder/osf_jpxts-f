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

# Data from S1
survey <- read.csv("data1-reproduce analyses.csv")

# Data from S2
survey <- read.csv("data2-reproduce analyses.csv")

dim(survey) # number of observations: 57408 (S1) / 32634 (S2)
length(unique(survey$id)) # number of participants: 1397 (S1) / 857 (S2)


################################
#########PREPROCESSING##########
################################

# The original data set for S1 included 57500 observations. 92 surveys were completed by test users, resulting in 57408 valid observations.
# The original data set for S2 included 32677 observations. 9 surveys were completed by test users, resulting in 32668 valid observations. 34 observations were removed for this project because they were completed after the end of the assignment.


### Delete participants who are too young (<18 years) or too old (>24 years)

survey <- survey[-which(survey$age < 18 | survey$age > 24), ] # delete all participants who are younger than 18 or older than 24

dim(survey) # number of observations: 55923 (S1) / 32428 (S2)
length(unique(survey$id)) # number of participants: 1360 (S1) / 851 (S2)

# Number of observations that were removed because of age: 1485 (S1) / 206 (S2)


### Remove partial reports

table(survey$Finished, useNA = "ifany") # number of surveys to exclude: 111 (S1) / 55 (S2)
survey <- survey[survey$Finished == 1,]  # keep only finished surveys (Finished == 1)

dim(survey) # number of observations: 55812 (S1) / 32373 (S2)
length(unique(survey$id)) # number of participants: 1360 (S1) / 851 (S2)


### Remove surveys that were completed too close to each other

survey$StartDate <- as.POSIXct(survey$StartDate, format = "%Y-%m-%d %H:%M:%S")
survey$EndDate <- as.POSIXct(survey$EndDate, format = "%Y-%m-%d %H:%M:%S")
survey$exclude <- 0 # create variable 'exclude'

# Split data set

data <- split(survey, survey$id, drop = T) # split data set using id
number <- length(unique(survey$id))
combine <- list()

# The following code loops over all participants.
# If the participant has completed more than one survey, the differences in StartDates between all consecutive surveys are computed (i.e., StartDate survey 2 - StartDate survey 1, StartDate survey 3 - StartDate survey 2, etc.).
# If the difference is smaller than 0.25 hours (i.e., 15 minutes), all surveys after the first that fall within the subsequent 3 hours are excluded.

for (i in 1:number) {
  sub <- data[[i]]
  if (nrow(sub) > 1) {
    for (j in 1:(length(sub$StartDate)-1)) {
      if (difftime(sub$StartDate[j+1], sub$StartDate[j], units = "hours") <= 0.25) {
        for (k in (j+1):length(sub$StartDate)) {
          sub$exclude[k] <- ifelse(difftime(sub$StartDate[k], sub$StartDate[j], units = "hours") <= 3, 1, 0)
        }
      }
    }
  }
  combine[[i]] <- sub
}

survey <- do.call(rbind, combine)

table(survey$exclude, useNA = "ifany") # number of surveys to exclude: 8543 (S1) / 4842 (S2)

survey <- survey[which(survey$exclude != 1), ] # remove surveys
survey <- survey[, !names(survey) %in% c("exclude")]  # remove variable 'exclude'

dim(survey) # number of observations: 47269 (S1) / 27531 (S2)
length(unique(survey$id)) # number of participants: 1360 (S1) / 851 (S2)

# Number of observations removed during data cleaning: 8654 (S1) / 4897 (S2)
round(8654/57408*100, 0) # 15%
round(4897/32668*100, 0) # 15%


################################
###########VARIABLES############
################################

### Well-being

survey$content <- as.numeric(ifelse(survey$content == "999", NA, survey$content)) # set 999 (SKIP QUESTION) to NA
table(survey$content, useNA = "ifany")

survey$stressed <- as.numeric(ifelse(survey$stressed == "999", NA, survey$stressed)) # set 999 (SKIP QUESTION) to NA
table(survey$stressed, useNA = "ifany")

survey$lonely <- as.numeric(ifelse(survey$lonely == "999", NA, survey$lonely)) # set 999 (SKIP QUESTION) to NA
table(survey$lonely, useNA = "ifany")

# Calculate well-being variable (stressed and lonely reverse coded)
survey$stressed_r <- 5 - survey$stressed
survey$lonely_r <- 5 - survey$lonely

survey$well_being <- apply(survey[c("content", "stressed_r", "lonely_r")], 1, function(x) {mean(x, na.rm = TRUE)})


### Mode of communication

# Initiate variables

survey$TiP <- 0
survey$ToP <- 0
survey$TM <- 0
survey$ChattingWHATSAPP <- 0
survey$ChattingDATING <- 0
survey$Emailing <- 0
survey$Videochatting <- 0
survey$Facebook <- 0
survey$Instagram <- 0
survey$Snapchat <- 0
survey$Twitter <- 0
survey$OTHER <- 0
survey$NoInteraction <- 0
survey$SKIP <- 0

# Participants could select all response options that applied to them in any given situation. Here, we create one dummy variable for every response option.

for (i in 1:length(survey$interacting_by)) { 
  v <- unlist(strsplit(survey$interacting_by[i], ","))
  if (is.element("1", v)) {
    survey$TiP[i] <- 1
  }
  if (is.element("2", v)) {
    survey$ToP[i] <- 1
  }
  if (is.element("3", v)) {
    survey$TM[i] <- 1
  }
  if (is.element("4", v)) {
    survey$ChattingWHATSAPP[i] <- 1
  }
  if (is.element("5", v)) {
    survey$ChattingDATING[i] <- 1
  }
  if (is.element("6", v)) {
    survey$Emailing[i] <- 1
  }
  if (is.element("7", v)) {
    survey$Videochatting[i] <- 1
  }
  if (is.element("8", v)) {
    survey$Facebook[i] <- 1
  }
  if (is.element("9", v)) {
    survey$Instagram[i] <- 1
  }
  if (is.element("10", v)) {
    survey$Snapchat[i] <- 1
  }
  if (is.element("11", v)) {
    survey$Twitter[i] <- 1
  }
  if (is.element("12", v)) {
    survey$OTHER[i] <- 1
  }
  if (is.element("0", v)) {
    survey$NoInteraction[i] <- 1
  }
  if (is.element("999", v)) {
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

for (i in 1:length(survey$interacting_people)){ 
  v <- unlist(strsplit(survey$interacting_people[i], ","))
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
  if (is.element("999", v)) {
    survey$SKIP2[i] <- 1
  }
}


### Demographics

# Recode gender

table(survey$gender[!duplicated(survey$id)], useNA = "ifany")
# 357 male, 594 female, 8 other, 401 missing (S1)
# 170 male, 258 female, 1 other, 422 missing (S2)
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

survey$weekend <- ifelse(weekdays(survey$StartDate, abbr = TRUE) %in% c("Sat", "Sun"), 1, 0)


################################
############DATASETS############
################################

# Check response scales
table(survey$interacting_by, useNA = "ifany")
table(survey$interacting_people, useNA = "ifany")
table(survey$interacting_people[survey$interacting_by == "0"], useNA = "ifany")

# Set no social interaction (interacting_by == "0") to 0
survey$interacting_people <- ifelse(survey$interacting_by == "0", "0", survey$interacting_people)
table(survey$interacting_people, useNA = "ifany")

nrow(survey) # number of observations: 47269 (S1) / 27531 (S2)
length(unique(survey$id)) # number of participants: 1360 (S1) / 851 (S2)

### Social interactions

# Create dummy variable for social interactions

survey$mixed_mode <- ifelse(survey$NoInteraction == 1 &
                            (survey$TiP == 1 | survey$ToP == 1 | survey$TM == 1 | survey$ChattingWHATSAPP == 1 | survey$ChattingDATING == 1 |
                            survey$Emailing == 1 | survey$Videochatting == 1 |
                            survey$Facebook == 1 | survey$Instagram == 1 | survey$Snapchat == 1 | survey$Twitter == 1 |
                            survey$OTHER == 1 | survey$SKIP == 1), 1, 0)

table(survey$mixed_mode, useNA = "ifany")

survey$interaction <- ifelse(survey$mixed_mode == 1 | (survey$SKIP == 1 & (survey$SKIP2 == 1 | is.na(survey$interacting_people))), NA,
                             ifelse(survey$interacting_by != "0", 1, 0))

table(survey$interaction, useNA = "ifany") # number of observations with social interactions: 36374 (S1) / 20474 (S2)
length(which(survey$mixed_mode == 1 | (survey$SKIP == 1 & (survey$SKIP2 == 1 | is.na(survey$interacting_people)))))

### Mode of communication

survey$sum_CMC <- apply(survey[c("ToP", "TM", "ChattingWHATSAPP", "ChattingDATING", "Emailing", "Videochatting", "Facebook", "Instagram", "Snapchat", "Twitter")], 1, sum)

# Create dummy variable for FtF interactions

survey$FtF <- ifelse(survey$mixed_mode == 1 | survey$SKIP == 1 | survey$OTHER == 1, NA,
                     ifelse(survey$TiP == 1 & survey$sum_CMC == 0, 1, 0))

table(survey$FtF, useNA = "ifany") # number of observations with FtF interactions ONLY: 14703 (S1) / 8931 (S2)
table(survey$interacting_by[survey$FtF == 1])

# Create dummy variable for CMC

survey$CMC <- ifelse(survey$mixed_mode == 1 | survey$SKIP == 1 | survey$OTHER == 1, NA,
                     ifelse(survey$TiP == 0 & survey$sum_CMC >= 1, 1, 0))

table(survey$CMC, useNA = "ifany") # number of observations with CMC ONLY: 11610 (S1) / 6691 (S2)
table(survey$interacting_by[survey$CMC == 1])

# Create dummy variable for mixed episodes

survey$mixed <- ifelse(survey$mixed_mode == 1 | survey$SKIP == 1 | survey$OTHER == 1, NA,
                       ifelse(survey$TiP == 1 & survey$sum_CMC >= 1, 1, 0))

table(survey$mixed, useNA = "ifany") # number of observations with mixed episodes: 9753 (S1) / 4620 (S2)
table(survey$interacting_by[survey$mixed == 1])

table(apply(survey[c("FtF", "CMC", "mixed")], 1, sum), useNA = "ifany") # 36066 (S1) / 20242 (S2)
length(which(survey$mixed_mode == 1 | survey$SKIP == 1 | survey$OTHER == 1))

### Interaction partner

# Create dummy variable for close peers

survey$close_peers <- ifelse(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people), NA,
                             ifelse(survey$Friends == 1 | survey$Roommates == 1 | survey$Significant_other == 1, 1, 0))

table(survey$close_peers, useNA = "ifany") # number of observations with interactions with close peers: 28370 (S1) / 15038 (S2)
table(survey$interacting_people[survey$close_peers == 1])

# Create dummy variable for family

survey$family <- ifelse(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people), NA,
                        ifelse(survey$Family == 1, 1, 0))

table(survey$family, useNA = "ifany") # number of observations with interactions with family: 8591 (S1) / 5103 (S2)
table(survey$interacting_people[survey$family == 1])

# Create dummy variable for weak ties

survey$weak_ties <- ifelse(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people), NA,
                           ifelse(survey$Classmates == 1 | survey$Coworkers == 1 | survey$Strangers == 1, 1, 0))

table(survey$weak_ties, useNA = "ifany") # number of observations with interactions with weak ties: 11280 (S1) / 6309 (S2)
table(survey$interacting_people[survey$weak_ties == 1])

# Create dummy variable for mixed interaction partners

survey$no_partners <- apply(survey[c("close_peers", "family", "weak_ties")], 1, sum)
survey$mixed_partner <- ifelse(survey$no_partners > 1, 1, 0)

table(survey$mixed_partner, useNA = "ifany") # number of observations with mixed interaction partners: 11479 (S1) / 5797 (S2)
table(apply(survey[c("close_peers", "family", "weak_ties")], 1, sum), useNA = "ifany")

survey$close_peers_all <- survey$close_peers
survey$family_all <- survey$family
survey$weak_ties_all <- survey$weak_ties

survey$close_peers <- ifelse(survey$mixed_partner == 1, NA, survey$close_peers)
survey$family <- ifelse(survey$mixed_partner == 1, NA, survey$family)
survey$weak_ties <- ifelse(survey$mixed_partner == 1, NA, survey$weak_ties)

table(survey$close_peers, useNA = "ifany") # number of observations with interactions with close peers ONLY: 17194 (S1) / 9406 (S2)
table(survey$interacting_people[survey$close_peers == 1])

table(survey$family, useNA = "ifany") # number of observations with interactions with family ONLY: 2450 (S1) / 1946 (S2)
table(survey$interacting_people[survey$family == 1])

table(survey$weak_ties, useNA = "ifany") # number of observations with interactions with weak ties ONLY: 4336 (S1) / 2823 (S2)
table(survey$interacting_people[survey$weak_ties == 1])

table(apply(survey[c("close_peers", "family", "weak_ties")], 1, sum), useNA = "ifany") # 23980 (S1) / 14175 (S2)
length(which(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people) | survey$mixed_partner == 1))

table(apply(survey[c("close_peers_all", "family_all", "weak_ties_all")], 1, sum), useNA = "ifany") # 23980, 10176, 1303 (S1) / 14175, 5116, 681 (S2)
length(which(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people)))

# Friends / roommates vs. significant others

# Create dummy variable for friends and roommates

survey$friends_roommates <- ifelse(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people), NA,
                                   ifelse(survey$Friends == 1 | survey$Roommates == 1, 1, 0))

table(survey$friends_roommates, useNA = "ifany") # number of observations with interactions with friends and roommates: 25037 (S1) / 12940 (S2)
table(survey$interacting_people[survey$friends_roommates == 1])

# Create dummy variable for significant others

survey$significant_other <- ifelse(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people), NA,
                                   ifelse(survey$Significant_other == 1, 1, 0))

table(survey$significant_other, useNA = "ifany") # number of observations with interactions with significant others: 6681 (S1) / 3822 (S2)
table(survey$interacting_people[survey$significant_other == 1])

# Create dummy variable for mixed interaction partners

survey$no_partners2 <- apply(survey[c("friends_roommates", "significant_other", "family_all", "weak_ties_all")], 1, sum)
survey$mixed_partner2 <- ifelse(survey$no_partners2 > 1, 1, 0)

table(survey$mixed_partner2, useNA = "ifany") # number of observations with mixed interaction partners: 13456 (S1) / 6847 (S2)
table(apply(survey[c("friends_roommates", "significant_other", "family_all", "weak_ties_all")], 1, sum), useNA = "ifany")

survey$friends_roommates2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$friends_roommates)
survey$significant_other2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$significant_other)
survey$family2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$family_all)
survey$weak_ties2 <- ifelse(survey$mixed_partner2 == 1, NA, survey$weak_ties_all)

table(survey$friends_roommates2, useNA = "ifany") # number of observations with interactions with friends and roommates ONLY: 12882 (S1) / 6865 (S2)
table(survey$interacting_people[survey$friends_roommates2 == 1])

table(survey$significant_other2, useNA = "ifany") # number of observations with interactions with significant others ONLY: 2335 (S1) / 1491 (S2)
table(survey$interacting_people[survey$significant_other2 == 1])

table(survey$family2, useNA = "ifany") # number of observations with interactions with family ONLY: 2450 (S1) / 1946 (S2)
table(survey$interacting_people[survey$family2 == 1])

table(survey$weak_ties2, useNA = "ifany") # number of observations with interactions with weak ties ONLY: 4336 (S1) / 2823 (S2)
table(survey$interacting_people[survey$weak_ties2 == 1])

table(apply(survey[c("friends_roommates2", "significant_other2", "family2", "weak_ties2")], 1, sum), useNA = "ifany") # 22003 (S1) / 13125 (S2)
length(which(survey$mixed_mode == 1 | survey$SKIP2 == 1 | survey$OTHER2 == 1 | is.na(survey$interacting_people) | survey$mixed_partner2 == 1))

### TVC vs. TCE vs. SN

# Create dummy variable for TVC

survey$TVC <- ifelse(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1, NA,
                     ifelse(survey$ToP == 1 | survey$Videochatting == 1, 1, 0))

table(survey$TVC, useNA = "ifany") # number of observations with TVC: 2333 (S1) / 1433 (S2)
table(survey$interacting_by[survey$TVC == 1])

# Create dummy variable for TCE

survey$TCE <- ifelse(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1, NA,
                     ifelse(survey$TM == 1 | survey$ChattingWHATSAPP == 1 | survey$ChattingDATING == 1 | survey$Emailing == 1, 1, 0))

table(survey$TCE, useNA = "ifany") # number of observations with TCE: 7768 (S1) / 4374 (S2)
table(survey$interacting_by[survey$TCE == 1])

# Create dummy variable for SN

survey$SN <- ifelse(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1, NA,
                    ifelse(survey$Facebook == 1 | survey$Instagram == 1 | survey$Snapchat == 1 | survey$Twitter == 1, 1, 0))

table(survey$SN, useNA = "ifany") # number of observations with SN: 4736 (S1) / 2599 (S2)
table(survey$interacting_by[survey$SN == 1])

# Create dummy variable for mixed CMC

survey$no_cmc <- apply(survey[c("TVC", "TCE", "SN")], 1, sum)
survey$mixed_CMC <- ifelse(survey$no_cmc > 1, 1, 0)

table(survey$mixed_CMC, useNA = "ifany") # number of observations with mixed CMC: 2946 (S1) / 1565 (S2)
table(apply(survey[c("TVC", "TCE", "SN")], 1, sum), useNA = "ifany")

survey$TVC <- ifelse(survey$mixed_CMC == 1, NA, survey$TVC)
survey$TCE <- ifelse(survey$mixed_CMC == 1, NA, survey$TCE)
survey$SN <- ifelse(survey$mixed_CMC == 1, NA, survey$SN)

table(survey$TVC, useNA = "ifany") # number of observations with TVC ONLY: 1501 (S1) / 954 (S2)
table(survey$interacting_by[survey$TVC == 1])

table(survey$TCE, useNA = "ifany") # number of observations with TCE ONLY: 4983 (S1) / 2902 (S2)
table(survey$interacting_by[survey$TCE == 1])

table(survey$SN, useNA = "ifany") # number of observations with SN ONLY: 2180 (S1) / 1270 (S2)
table(survey$interacting_by[survey$SN == 1])

table(apply(survey[c("TVC", "TCE", "SN")], 1, sum), useNA = "ifany") # 8664 (S1) / 5126 (S2)
length(which(is.na(survey$CMC) | survey$FtF == 1 | survey$mixed == 1 | survey$mixed_CMC == 1))

### Create final data set

data_clean <- survey[, c("id", "StartDate", "well_being", "content", "stressed", "lonely",
                     "interaction",
                     "FtF", "CMC", "mixed",
                     "close_peers", "family", "weak_ties", "mixed_partner",
                     "close_peers_all", "family_all", "weak_ties_all",
                     "friends_roommates2", "significant_other2", "family2", "weak_ties2", "mixed_partner2",
                     "TVC", "TCE", "SN", "mixed_CMC",
                     "bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o",
                     paste0("bfi_", 1:44),
                     "gender", "ethnicity", "SES", "age", "academicclass", "ethnic_group",
                     "weekend")]

dim(data_clean)
data_clean <- data_clean[!is.na(data_clean$interaction), ]

table(data_clean$well_being, useNA = "ifany")
table(data_clean$interaction, useNA = "ifany")
table(apply(survey[c("FtF", "CMC", "mixed")], 1, sum), useNA = "ifany")
table(apply(survey[c("close_peers", "family", "weak_ties")], 1, sum), useNA = "ifany")

##########CHOOSE ONE############
write.csv(data_clean, "data_clean_S1.csv", row.names = FALSE)
write.csv(data_clean, "data_clean_S2.csv", row.names = FALSE)
