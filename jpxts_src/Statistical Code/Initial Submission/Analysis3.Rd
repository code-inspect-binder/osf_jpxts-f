library(lme4)
library(lmerTest)
library(numform)

##########CHOOSE ONE############

model_three <- read.csv("Model3_S1.csv") # data from S1
#model_three <- read.csv("Model3_S2.csv") # data from S2


################################
###########STEP ONE#############
################################

# Main effects of mode of communication (TCE = texting/chatting/emailing, SNS = interactions on social networking sites; reference category: talking on the phone/video-chatting), type of interaction partner (family, others; reference category: peers), and personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z) on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M3aE <- lmer(well_being ~
TCE + SNS + family + others + weekend +
bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3aA <- lmer(well_being ~
TCE + SNS + family + others + weekend +
bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3aC <- lmer(well_being ~
TCE + SNS + family + others + weekend +
bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3aN <- lmer(well_being ~
TCE + SNS + family + others + weekend +
bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3aO <- lmer(well_being ~
TCE + SNS + family + others + weekend +
bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

# Create output table

table_one <- data.frame(matrix(ncol=0,nrow=9)) # empty table with 9 rows
models <- c(M3aE,M3aA,M3aC,M3aN,M3aO) # list of all multilevel models

# Extract estimates, standard errors, p-values, random intercept variances, and residual variances from the multilevel models and add them to the output table

for (i in 1:length(models)){
dat <- rbind(as.data.frame(summary(models[[i]])$coefficients[,c(1,2,5)]),c(as.data.frame(VarCorr(models[[i]]))[1,4],"",""),c(as.data.frame(VarCorr(models[[i]]))[2,4],"",""))
table_one <- cbind(table_one,dat)
}

# Format estimates and standard errors

for (j in c(1,2,4,5,7,8,10,11,13,14)){
  for(i in 1:nrow(table_one)){
    table_one[i,j] <- ifelse(table_one[i,j]=="","",format(round(as.numeric(table_one[i,j]),3),nsmall=3))
    }
}

# Format p-values

for (j in c(3,6,9,12,15)){
  for(i in 1:nrow(table_one)){
    table_one[i,j] <- ifelse(table_one[i,j]=="","",f_num(as.numeric(table_one[i,j]),digits=3))
    table_one[i,j] <- ifelse(table_one[i,j]==".000","<.001",table_one[i,j])
  }
}

# Rename columns and rows

names(table_one) <- rep(c("Est.", "SE", "p"), times=5)
row.names(table_one) <- c("Intercept", "TCE", "SNS", "Family", "Others", "Weekend", "Personality", "Variance intercept", "Residual variance")


################################
###########STEP TWO#############
################################

# Effects of mode of communication (TCE = texting/chatting/emailing, SNS = interactions on social networking sites; reference category: talking on the phone/video-chatting), type of interaction partner (family, others; reference category: peers), personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z), and all two-way interactions  on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M3bE <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_e_z +
TCE*bfi_e_z + SNS*bfi_e_z +
family*bfi_e_z + others*bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3bA <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_a_z +
TCE*bfi_a_z + SNS*bfi_a_z +
family*bfi_a_z + others*bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3bC <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_c_z +
TCE*bfi_c_z + SNS*bfi_c_z +
family*bfi_c_z + others*bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3bN <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_n_z +
TCE*bfi_n_z + SNS*bfi_n_z +
family*bfi_n_z + others*bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3bO <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_o_z +
TCE*bfi_o_z + SNS*bfi_o_z +
family*bfi_o_z + others*bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

# Create output table

table_two <- data.frame(matrix(ncol=0,nrow=17)) # empty table with 17 rows
models <- c(M3bE,M3bA,M3bC,M3bN,M3bO) # list of all multilevel models

# Extract estimates, standard errors, p-values, random intercept variances, and residual variances from the multilevel models and add them to the output table

for (i in 1:length(models)){
dat <- rbind(as.data.frame(summary(models[[i]])$coefficients[,c(1,2,5)]),c(as.data.frame(VarCorr(models[[i]]))[1,4],"",""),c(as.data.frame(VarCorr(models[[i]]))[2,4],"",""))
table_two <- cbind(table_two,dat)
}

# Format estimates and standard errors

for (j in c(1,2,4,5,7,8,10,11,13,14)){
  for(i in 1:nrow(table_two)){
    table_two[i,j] <- ifelse(table_two[i,j]=="","",format(round(as.numeric(table_two[i,j]),3),nsmall=3))
    }
}

# Format p-values

for (j in c(3,6,9,12,15)){
  for(i in 1:nrow(table_two)){
    table_two[i,j] <- ifelse(table_two[i,j]=="","",f_num(as.numeric(table_two[i,j]),digits=3))
    table_two[i,j] <- ifelse(table_two[i,j]==".000","<.001",table_two[i,j])
  }
}

# Rename columns and rows

names(table_two) <- rep(c("Est.", "SE", "p"), times=5)
row.names(table_two) <- c("Intercept", "TCE", "SNS", "Family", "Others", "Weekend", "Personality", "TCE x Family", "TCE x Others", "SNS x Family", "SNS x Others", "Personality x TCE", "Personality x SNS", "Personality x Family", "Personality x Others", "Variance intercept", "Residual variance")


################################
##########STEP THREE############
################################

# Effects of mode of communication (TCE = texting/chatting/emailing, SNS = interactions on social networking sites; reference category: talking on the phone/video-chatting), type of interaction partner (family, others; reference category: peers), personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z), and all two-way and three-way interactions  on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M3cE <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_e_z +
TCE*bfi_e_z + SNS*bfi_e_z +
family*bfi_e_z + others*bfi_e_z +
TCE*family*bfi_e_z + TCE*others*bfi_e_z +
SNS*family*bfi_e_z + SNS*others*bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3cA <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_a_z +
TCE*bfi_a_z + SNS*bfi_a_z +
family*bfi_a_z + others*bfi_a_z +
TCE*family*bfi_a_z + TCE*others*bfi_a_z +
SNS*family*bfi_a_z + SNS*others*bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3cC <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_c_z +
TCE*bfi_c_z + SNS*bfi_c_z +
family*bfi_c_z + others*bfi_c_z +
TCE*family*bfi_c_z + TCE*others*bfi_c_z +
SNS*family*bfi_c_z + SNS*others*bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3cN <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_n_z +
TCE*bfi_n_z + SNS*bfi_n_z +
family*bfi_n_z + others*bfi_n_z +
TCE*family*bfi_n_z + TCE*others*bfi_n_z +
SNS*family*bfi_n_z + SNS*others*bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

M3cO <- lmer(well_being ~
TCE + SNS + family + others + weekend +
TCE*family + TCE*others +
SNS*family + SNS*others +
bfi_o_z +
TCE*bfi_o_z + SNS*bfi_o_z +
family*bfi_o_z + others*bfi_o_z +
TCE*family*bfi_o_z + TCE*others*bfi_o_z +
SNS*family*bfi_o_z + SNS*others*bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_three)

# Create output table

table_three <- data.frame(matrix(ncol=0,nrow=21)) # empty table with 21 rows
models <- c(M3cE,M3cA,M3cC,M3cN,M3cO) # list of all multilevel models

# Extract estimates, standard errors, p-values, random intercept variances, and residual variances from the multilevel models and add them to the output table

for (i in 1:length(models)){
dat <- rbind(as.data.frame(summary(models[[i]])$coefficients[,c(1,2,5)]),c(as.data.frame(VarCorr(models[[i]]))[1,4],"",""),c(as.data.frame(VarCorr(models[[i]]))[2,4],"",""))
table_three <- cbind(table_three,dat)
}

# Format estimates and standard errors

for (j in c(1,2,4,5,7,8,10,11,13,14)){
  for(i in 1:nrow(table_three)){
    table_three[i,j] <- ifelse(table_three[i,j]=="","",format(round(as.numeric(table_three[i,j]),3),nsmall=3))
    }
}

# Format p-values

for (j in c(3,6,9,12,15)){
  for(i in 1:nrow(table_three)){
    table_three[i,j] <- ifelse(table_three[i,j]=="","",f_num(as.numeric(table_three[i,j]),digits=3))
    table_three[i,j] <- ifelse(table_three[i,j]==".000","<.001",table_three[i,j])
  }
}

# Rename columns and rows

names(table_three) <- rep(c("Est.", "SE", "p"), times=5)
row.names(table_three) <- c("Intercept", "TCE", "SNS", "Family", "Others", "Weekend", "Personality", "TCE x Family", "TCE x Others", "SNS x Family", "SNS x Others", "Personality x TCE", "Personality x SNS", "Personality x Family", "Personality x Others", "Personality x TCE x Family", "Personality x TCE x Others", "Personality x SNS x Family", "Personality x SNS x Others", "Variance intercept", "Residual variance")