library(lme4)
library(lmerTest)
library(numform)

##########CHOOSE ONE############

model_two <- read.csv("Model2_S1.csv") # data from S1
#model_two <- read.csv("Model2_S2.csv") # data from S2


################################
###########STEP ONE#############
################################

# Main effects of mode of communication (CMC, mixed; reference category: FtF), type of interaction partner (family, others; reference category: peers), and personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z) on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M2aE <- lmer(well_being ~
CMC + mixed + family + others + weekend +
bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2aA <- lmer(well_being ~
CMC + mixed + family + others + weekend +
bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2aC <- lmer(well_being ~
CMC + mixed + family + others + weekend +
bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2aN <- lmer(well_being ~
CMC + mixed + family + others + weekend +
bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2aO <- lmer(well_being ~
CMC + mixed + family + others + weekend +
bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

# Create output table

table_one <- data.frame(matrix(ncol=0,nrow=9)) # empty table with 9 rows
models <- c(M2aE,M2aA,M2aC,M2aN,M2aO) # list of all multilevel models

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
row.names(table_one) <- c("Intercept", "CMC", "Mixed", "Family", "Others", "Weekend", "Personality", "Variance intercept", "Residual variance")


################################
###########STEP TWO#############
################################

# Effects of mode of communication (CMC, mixed; reference category: FtF), type of interaction partner (family, others; reference category: peers), personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z), and all two-way interactions  on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M2bE <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_e_z +
CMC*bfi_e_z + mixed*bfi_e_z +
family*bfi_e_z + others*bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2bA <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_a_z +
CMC*bfi_a_z + mixed*bfi_a_z +
family*bfi_a_z + others*bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2bC <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_c_z +
CMC*bfi_c_z + mixed*bfi_c_z +
family*bfi_c_z + others*bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2bN <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_n_z +
CMC*bfi_n_z + mixed*bfi_n_z +
family*bfi_n_z + others*bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2bO <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_o_z +
CMC*bfi_o_z + mixed*bfi_o_z +
family*bfi_o_z + others*bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

# Create output table

table_two <- data.frame(matrix(ncol=0,nrow=17)) # empty table with 17 rows
models <- c(M2bE,M2bA,M2bC,M2bN,M2bO) # list of all multilevel models

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
row.names(table_two) <- c("Intercept", "CMC", "Mixed", "Family", "Others", "Weekend", "Personality", "CMC x Family", "CMC x Others", "Mixed x Family", "Mixed x Others", "Personality x CMC", "Personality x Mixed", "Personality x Family", "Personality x Others", "Variance intercept", "Residual variance")


################################
##########STEP THREE############
################################

# Effects of mode of communication (CMC, mixed; reference category: FtF), type of interaction partner (family, others; reference category: peers), personality traits (bfi_e_z, bfi_a_z, bfi_c_z, bfi_n_z, bfi_o_z), and all two-way and three-way interactions  on momentary well-being
# Control variable: weekend
# Intercepts were allowed to vary randomly across participants (1 | ID)

M2cE <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_e_z +
CMC*bfi_e_z + mixed*bfi_e_z +
family*bfi_e_z + others*bfi_e_z +
CMC*family*bfi_e_z + CMC*others*bfi_e_z +
mixed*family*bfi_e_z + mixed*others*bfi_e_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2cA <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_a_z +
CMC*bfi_a_z + mixed*bfi_a_z +
family*bfi_a_z + others*bfi_a_z +
CMC*family*bfi_a_z + CMC*others*bfi_a_z +
mixed*family*bfi_a_z + mixed*others*bfi_a_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2cC <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_c_z +
CMC*bfi_c_z + mixed*bfi_c_z +
family*bfi_c_z + others*bfi_c_z +
CMC*family*bfi_c_z + CMC*others*bfi_c_z +
mixed*family*bfi_c_z + mixed*others*bfi_c_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2cN <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_n_z +
CMC*bfi_n_z + mixed*bfi_n_z +
family*bfi_n_z + others*bfi_n_z +
CMC*family*bfi_n_z + CMC*others*bfi_n_z +
mixed*family*bfi_n_z + mixed*others*bfi_n_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

M2cO <- lmer(well_being ~
CMC + mixed + family + others + weekend +
CMC*family + CMC*others +
mixed*family + mixed*others +
bfi_o_z +
CMC*bfi_o_z + mixed*bfi_o_z +
family*bfi_o_z + others*bfi_o_z +
CMC*family*bfi_o_z + CMC*others*bfi_o_z +
mixed*family*bfi_o_z + mixed*others*bfi_o_z +
(1 | ID), 
na.action = na.exclude, data=model_two)

# Create output table

table_three <- data.frame(matrix(ncol=0,nrow=21)) # empty table with 21 rows
models <- c(M2cE,M2cA,M2cC,M2cN,M2cO) # list of all multilevel models

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
row.names(table_three) <- c("Intercept", "CMC", "Mixed", "Family", "Others", "Weekend", "Personality", "CMC x Family", "CMC x Others", "Mixed x Family", "Mixed x Others", "Personality x CMC", "Personality x Mixed", "Personality x Family", "Personality x Others", "Personality x CMC x Family", "Personality x CMC x Others", "Personality x Mixed x Family", "Personality x Mixed x Others", "Variance intercept", "Residual variance")