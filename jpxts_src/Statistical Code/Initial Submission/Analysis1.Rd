library(lme4)
library(lmerTest)
library(numform)

##########CHOOSE ONE############

model_one <- read.csv("Model1_S1.csv") # data from S1
#model_one <- read.csv("Model1_S2.csv") # data from S2


################################
###########STEP ONE#############
################################

# Main effects of mode of communication (FtF, CMC, mixed; reference category: no social interaction) and personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z) on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M1aE <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1aA <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1aC <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1aN <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1aO <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

# Create output table

table_one <- data.frame(matrix(ncol=0,nrow=8)) # empty table with 8 rows
models <- c(M1aE,M1aA,M1aC,M1aN,M1aO) # list of all multilevel models

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
row.names(table_one) <- c("Intercept", "FtF", "CMC", "Mixed", "Weekend", "Personality", "Variance intercept", "Residual variance")


################################
###########STEP TWO#############
################################

# Effects of mode of communication (FtF, CMC, mixed; reference category: no social interaction), personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z), and their cross-level interactions on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M1bE <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_e_z +
FtF*bfi_e_z + CMC*bfi_e_z + mixed*bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1bA <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_a_z +
FtF*bfi_a_z + CMC*bfi_a_z + mixed*bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1bC <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_c_z +
FtF*bfi_c_z + CMC*bfi_c_z + mixed*bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1bN <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_n_z +
FtF*bfi_n_z + CMC*bfi_n_z + mixed*bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

M1bO <- lmer(well_being ~
FtF + CMC + mixed + weekend +
bfi_o_z +
FtF*bfi_o_z + CMC*bfi_o_z + mixed*bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_one)

# Create output table

table_two <- data.frame(matrix(ncol=0,nrow=11)) # empty table with 11 rows
models <- c(M1bE,M1bA,M1bC,M1bN,M1bO) # list of all multilevel models

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
row.names(table_two) <- c("Intercept", "FtF", "CMC", "Mixed", "Weekend", "Personality", "Personality x FtF", "Personality x CMC", "Personality x Mixed", "Variance intercept", "Residual variance")