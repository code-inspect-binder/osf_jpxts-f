library(tidyverse)
library(chron)

##########CHOOSE ONE############

survey <- read.csv("data1-reproduce analyses.csv") # data from S1
#survey <- read.csv("data2-reproduce analyses.csv") # data from S2


################################
#########PREPROCESSING##########
################################


### Remove partial reports

survey <- subset(survey,survey$Finished=="1") # keep only finished surveys (Finished==1)

length(unique(survey$ID)) # number of participants: 1397 (S1) / 857 (S2)


### Remove surveys that were completed too close to each other

survey <- survey[order(survey$EndDate),] # order surveys by EndDate
survey <- survey[order(survey$StartDate),] # order surveys by StartDate
survey$exclude <- 0 # create new variable

# Split dataset

data <- split(survey,survey$ID,drop=T)
number <- length(unique(survey$ID))
combine <- list()

# The following code loops over all participants
# If the participant has completed more than one survey, the differences in StartDates between all consecutive surveys are computed (i.e., StartDate survey 2 - StartDate survey 1, StartDate survey 3 - StartDate survey 2, etc.)
# If the difference is smaller than 0.25 hours (i.e., 15 minutes), all surveys after the first that fall within the subsequent 3 hours are excluded

for (i in 1:number){
	sub <- data[[i]]
	if(nrow(sub)>1){
		for (j in 1:(length(sub$StartDate)-1)){
			if(difftime(sub$StartDate[j+1],sub$StartDate[j],units="hours") <= 0.25){
				for (k in (j+1):length(sub$StartDate)){
					sub$exclude[k] <- ifelse(difftime(sub$StartDate[k],sub$StartDate[j],units="hours") <= 3,1,0)
					}
				}
			}
		}
	combine[[i]] <- sub
	}

survey <- do.call(rbind,combine)

table(survey$exclude) # number of surveys to exclude: 8738 (S1) / 4861 (S2)

survey <- survey[-which(survey$exclude==1),] # remove surveys

survey <- select(survey,-exclude) # remove variable 'exclude'

length(unique(survey$ID)) # number of participants: 1397 (S1) / 857 (S2)


### Delete participants who are too young (<18 years) or too old (>24 years)

survey <- survey[-which(survey$age<18|survey$age>24),] # delete all participants who are younger than 18 or older than 24

dim(survey) # number of observations: 47269 (S1) / 27531 (S1)
length(unique(survey$ID)) # number of participants: 1360 (S1) / 851 (S1)


################################
###########VARIABLES############
################################


### Well-being

survey$content <- as.numeric(ifelse(survey$content=="999",NA,survey$content)) # set 999 (SKIP QUESTION ) to NA
table(survey$content,exclude=FALSE)

survey$stressed <- as.numeric(ifelse(survey$stressed=="999",NA,survey$stressed)) # set 999 (SKIP QUESTION ) to NA
table(survey$stressed,exclude=FALSE)

survey$lonely <- as.numeric(ifelse(survey$lonely=="999",NA,survey$lonely)) # set 999 (SKIP QUESTION ) to NA
table(survey$lonely,exclude=FALSE)

survey <- survey %>% rowwise() %>% 
  dplyr::mutate(well_being = mean(c(content, 5-stressed, 5-lonely),na.rm=TRUE)) # calculate well-being variable (stressed and lonely reverse coded)


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

# Participants could select all response options that applied to them in any given situation. Here, we create one dummy variable for every response option

survey$interacting_by <- as.character(survey$interacting_by)

for (i in 1:length(survey$interacting_by)){ 
v <- unlist(strsplit(survey$interacting_by[i],","))
if (is.element('1',v)){
	survey$TiP[i] <- 1
}
if (is.element('2',v)){
	survey$ToP[i] <- 1
}
if (is.element('3',v)){
	survey$TM[i] <- 1
}
if (is.element('4',v)){
	survey$ChattingWHATSAPP[i] <- 1
}
if (is.element('5',v)){
	survey$ChattingDATING[i] <- 1
}
if (is.element('6',v)){
	survey$Emailing[i] <- 1
}
if (is.element('7',v)){
	survey$Videochatting[i] <- 1
}
if (is.element('8',v)){
	survey$Facebook[i] <- 1
}
if (is.element('9',v)){
	survey$Instagram[i] <- 1
}
if (is.element('10',v)){
	survey$Snapchat[i] <- 1
}
if (is.element('11',v)){
	survey$Twitter[i] <- 1
}
if (is.element('12',v)){
	survey$OTHER[i] <- 1
}
if (is.element('0',v)){
	survey$NoInteraction[i] <- 1
}
if (is.element('999',v)){
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

# Participants could select all response options that applied to them in any given situation. Here, we create one dummy variable for every response option

survey$interacting_people <- as.character(survey$interacting_people)

for (i in 1:length(survey$interacting_people)){ 
v <- unlist(strsplit(survey$interacting_people[i],","))
if (is.element('1',v)){
	survey$Classmates[i] <- 1
}
if (is.element('2',v)){
	survey$Coworkers[i] <- 1
}
if (is.element('3',v)){
	survey$Family[i] <- 1
}
if (is.element('4',v)){
	survey$Friends[i] <- 1
}
if (is.element('5',v)){
	survey$Roommates[i] <- 1
}
if (is.element('6',v)){
	survey$Significant_other[i] <- 1
}
if (is.element('7',v)){
	survey$Strangers[i] <- 1
}
if (is.element('8',v)){
	survey$OTHER2[i] <- 1
}
if (is.element('999',v)){
	survey$SKIP2[i] <- 1
}
}


### Demographics

# Create dummy variable for ethnicity: 0 = option 4 only (Anglo/White), 1 = any other option / combination of options

survey$ethnicity <- ifelse(survey$ethnic_group==4,0,ifelse(survey$ethnic_group=="",NA,1))
table(survey$ethnicity,exclude=FALSE)

# Create dummy variable for SES: 1 = mother or father completed at least some college (4 = some college), 0 = otherwise

survey$SES <- ifelse(survey$mother_educationlevel>=4|survey$father_educationlevel>=4,1,ifelse(survey$mother_educationlevel==""&survey$father_educationlevel=="",NA,0)) 
table(survey$SES,exclude=FALSE)


### Weekend

# Create dummy variable for weekend: 1 = weekend, 0 = weekday

survey$weekend <- ifelse(is.weekend(survey$StartDate)==TRUE,1,0)
table(substr(survey$StartDate[survey$weekend==1],1,10))
table(substr(survey$StartDate[survey$weekend==0],1,10))


### Personality

# Standardize personality variables

survey$bfi_e_z <- scale(survey$bfi_e,center=TRUE,scale=TRUE)
survey$bfi_a_z <- scale(survey$bfi_a,center=TRUE,scale=TRUE)
survey$bfi_c_z <- scale(survey$bfi_c,center=TRUE,scale=TRUE)
survey$bfi_n_z <- scale(survey$bfi_n,center=TRUE,scale=TRUE)
survey$bfi_o_z <- scale(survey$bfi_o,center=TRUE,scale=TRUE)


################################
############DATASETS############
################################


#-------------------------------
#  Model 1
#-------------------------------

# Exclude all surveys when participants chose "Other form of interaction" OR "Not applicable, was not interacting with anyone" in combination with another response option OR "SKIP QUESTION"

survey <- survey[-which(survey$OTHER==1|(survey$NoInteraction==1&(survey$TiP==1|survey$ToP==1|survey$TM==1|survey$ChattingWHATSAPP==1|survey$ChattingDATING==1|survey$Emailing==1|survey$Videochatting==1|survey$Facebook==1|survey$Instagram==1|survey$Snapchat==1|survey$Twitter==1))|survey$SKIP==1),]

dim(survey) # number of observations: 46409 (S1) / 26918 (S2)
length(unique(survey$ID)) # number of participants: 1360 (S1) /849 (S2)

# Create dummy variable for no interaction

survey$no_interaction <- ifelse(survey$NoInteraction==1,1,0)
table(survey$no_interaction) # number of observations with no interaction: 10343 (S1) / 6676 (S2)
table(survey$interacting_by[survey$no_interaction==1])

# Create dummy variable for FtF interactions

survey$FtF <- ifelse(survey$TiP==1&survey$ToP==0&survey$TM==0&survey$ChattingWHATSAPP==0&survey$ChattingDATING==0&survey$Emailing==0&survey$Videochatting==0&survey$Facebook==0&survey$Instagram==0&survey$Snapchat==0&survey$Twitter==0,1,0)
table(survey$FtF) # number of observations with FtF interaction ONLY: 14703 (S1) / 8931 (S2)
table(survey$interacting_by[survey$FtF==1])

# Create dummy variable for CMC

survey$CMC <- ifelse(survey$TiP==0&(survey$ToP==1|survey$TM==1|survey$ChattingWHATSAPP==1|survey$ChattingDATING==1|survey$Emailing==1|survey$Videochatting==1|survey$Facebook==1|survey$Instagram==1|survey$Snapchat==1|survey$Twitter==1),1,0)
table(survey$CMC) # number of observations with CMC ONLY: 11610 (S1) / 6691 (S2)
table(survey$interacting_by[survey$CMC==1])

# Create dummy variable for mixed interactions

survey$mixed <- ifelse(survey$TiP==1&(survey$ToP==1|survey$TM==1|survey$ChattingWHATSAPP==1|survey$ChattingDATING==1|survey$Emailing==1|survey$Videochatting==1|survey$Facebook==1|survey$Instagram==1|survey$Snapchat==1|survey$Twitter==1),1,0)
table(survey$mixed) # number of observations with mixed interactions: 9753 (S1) / 4620 (S2)
table(survey$interacting_by[survey$mixed==1])

table(rowSums(select(survey,no_interaction:mixed))) # 46409 (S1) / 26918 (S2)
length(unique(survey$ID)) # number of participants: 1360 (S1) / 849 (S2)

# Create final dataset

model_one_final <- select(survey,ID,StartDate,well_being,no_interaction:mixed,bfi_e_z:bfi_o_z,sex,ethnicity,SES,weekend)

##########CHOOSE ONE############

write.csv(model_one_final,"Model1_S1.csv",na="",row.names=FALSE)
#write.csv(model_one_final,"Model1_S2.csv",na="",row.names=FALSE)


#-------------------------------
#  Model 2
#-------------------------------

# Exclude all surveys when no interaction took place

survey <- survey[-which(survey$no_interaction==1),]

# Exclude all surveys when participants chose "None of the above, Other" OR "SKIP QUESTION"

survey <- survey[-which(survey$OTHER2==1|survey$SKIP2==1),]

dim(survey) # number of observations: 35208 (S1) / 19783 (S2)
length(unique(survey$ID)) # number of participants: 1360 (S1) / 847 (S2)

# Create dummy variable for family

survey$family <- ifelse(survey$Family==1&survey$Friends==0&survey$Roommates==0&survey$Significant_other==0&survey$Classmates==0&survey$Coworkers==0&survey$Strangers==0,1,0)
table(survey$family) # number of observations with interactions with family: 2441 (S1) / 1941 (S2)
table(survey$interacting_people[survey$family==1])

# Create dummy variable for peers

survey$peers <- ifelse((survey$Friends==1|survey$Roommates==1|survey$Significant_other==1)&survey$Family==0&survey$Classmates==0&survey$Coworkers==0&survey$Strangers==0,1,0)
table(survey$peers) # number of observations with interactions with peers: 17080 (S1) / 9338 (S2)
table(survey$interacting_people[survey$peers==1])

# Create dummy variable for others

survey$others <- ifelse((survey$Classmates==1|survey$Coworkers==1|survey$Strangers==1)&survey$Family==0&survey$Friends==0&survey$Roommates==0&survey$Significant_other==0,1,0)
table(survey$others) # number of observations with interactions with others: 4257 (S1) / 2740 (S2)
table(survey$interacting_people[survey$others==1])

# Exclude all surveys when participants selected interaction partners from multiple categories 

survey <- survey[-which(survey$family==0&survey$peers==0&survey$others==0),]

dim(survey) # number of observations: 23778 (S1) / 14019 (S2)
length(unique(survey$ID)) # number of participants: 1347 (S1) / 834 (S2)

# Create final dataset

model_two_final <- select(survey,ID,StartDate,well_being,FtF:others,bfi_e_z:bfi_o_z,sex,ethnicity,SES,weekend)

##########CHOOSE ONE############

write.csv(model_two_final,"Model2_S1.csv",na="",row.names=FALSE)
#write.csv(model_two_final,"Model2_S2.csv",na="",row.names=FALSE)


#-------------------------------
#  Model 3
#-------------------------------

# Keep only surveys with CMC

survey <- survey[which(survey$CMC==1),]

dim(survey) # number of observations: 8101 (S1) / 4727 (S2)
length(unique(survey$ID)) # number of participants: 1237 (S1) / 753 (S2)

# Create dummy variable for talking on the phone/video-chatting (TVC)

survey$TVC <- ifelse((survey$ToP==1|survey$Videochatting==1)&survey$TM==0&survey$ChattingWHATSAPP==0&survey$ChattingDATING==0&survey$Emailing==0&survey$Facebook==0&survey$Instagram==0&survey$Snapchat==0&survey$Twitter==0,1,0)
table(survey$TVC) # number of observations with TVC: 1254 (S1) / 803 (S2)
table(survey$interacting_by[survey$TVC==1])

# Create dummy variable for texting/chatting/emailing (TCE)

survey$TCE <- ifelse((survey$TM==1|survey$ChattingWHATSAPP==1|survey$ChattingDATING==1|survey$Emailing==1)&survey$ToP==0&survey$Videochatting==0&survey$Facebook==0&survey$Instagram==0&survey$Snapchat==0&survey$Twitter==0,1,0)
table(survey$TCE) # number of observations with TCE: 3779 (S1) / 2155 (S2)
table(survey$interacting_by[survey$TCE==1])

# Create dummy variable for interactions on social networking sites (SNS)

survey$SNS <- ifelse((survey$Facebook==1|survey$Instagram==1|survey$Snapchat==1|survey$Twitter==1)&survey$ToP==0&survey$Videochatting==0&survey$TM==0&survey$ChattingWHATSAPP==0&survey$ChattingDATING==0&survey$Emailing==0,1,0)
table(survey$SNS) # number of observations with SNS: 1669 (S1) / 977 (S2)
table(survey$interacting_by[survey$SNS==1])

# Exclude all surveys when participants used multiple CMC modes simultaneously

survey <- survey[-which(survey$TVC==0&survey$TCE==0&survey$SNS==0),]

dim(survey) # number of observations: 6702 (S1) / 3935 (S2)
length(unique(survey$ID)) # number of participants: 1204 (S1) / 729 (S2)

# Create final dataset

model_three_final  <- select(survey,ID,StartDate,well_being,TVC:SNS,family:others,bfi_e_z:bfi_o_z,sex,ethnicity,SES,weekend)

##########CHOOSE ONE############

write.csv(model_three_final,"Model3_S1.csv",na="",row.names=FALSE)
#write.csv(model_three_final,"Model3_S2.csv",na="",row.names=FALSE)
