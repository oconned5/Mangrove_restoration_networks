###############################################################
## FINAL MODELS SELECTED FOR ANALYSES OF VEGETATION NETWORK INDICES
## INDICIES RESPONSE VARIABLES, MANGROVE FOREST TREATMENT AND VEGETATION STRUCTURE EXPLANATORY VARIABLES


# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Network_indices_vegetation_search.csv

dfTiw <- subset(dat1, Site == "Tiwoho" & network_size_sufficient_for_analysis == "YES")

dfTiw$Treatment <- factor(dfTiw$Treatment, levels = c("Reference Forest","Mixed Species Regeneration","Monoculture Reforestation"))


#############
## PACKAGES #
#############

#install.packages("betareg")
#install.packages("MASS")


#################################
##### Analysis ##################
#################################

#### BETA GLM  for 0-1 proportion data
library(betareg)

############################
### weighted_connectance ###
############################


#### BETA GLM, logit link, identity link.phi

weighted_connectanceM1a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "identity", 
                                    data = dfTiw)
summary(weighted_connectanceM1a)
AIC(weighted_connectanceM1a)
plot(weighted_connectanceM1a)

########################################################
###########  weighted_NODF  ############################
########################################################

### gaussian GLM, identity link function
### 
weighted_NODFM7a <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfTiw)
summary(weighted_NODFM7a)
AIC(weighted_NODFM7a)
Model_R2weighted_NODFM7a <- with(summary(weighted_NODFM7a), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7a

plot(weighted_NODFM7a)

########################################################
###########  generality.HL  ############################
########################################################

### gaussian GLM, identity link functions
### 
generality.HLM7a <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfTiw)
summary(generality.HLM7a)
AIC(generality.HLM7a)

Model_R2generality.HLM7a <- with(summary(generality.HLM7a), 1 - deviance/null.deviance)
Model_R2generality.HLM7a

plot(generality.HLM7a)

#########################################################
########### robustness.HL  ##############################
#########################################################

#### BETA GLM, logit link, identity link.phi

robustness.HLM1a <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "logit",
                             link.phi = "identity", 
                             data = dfTiw)
summary(robustness.HLM1a)
AIC(robustness.HLM1a)
plot(robustness.HLM1a)

#########################################################
########### interaction_evenness  ##############################
#########################################################

#### BETA GLM, logit link, identity link.phi

interaction_evennessM1a <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "identity", 
                                    data = dfTiw)
summary(interaction_evennessM1a)
AIC(interaction_evennessM1a)
plot(interaction_evennessM1a)


########################################################
###########  vulnerability.LL  ############################
########################################################

### gaussian GLM, identity link functions

vulnerability.LLM7a <- glm(vulnerability.LL  ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "identity"),
                           data = dfTiw)
summary(vulnerability.LLM7a)
AIC(vulnerability.LLM7a)

Model_R2vulnerability.LLM7a <- with(summary(vulnerability.LLM7a), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM7a

plot(vulnerability.LLM7a)


####################################################################
################   H2   ############################################
####################################################################

#### BETA GLM with logit link, identity link.phi


H22 <- dfTiw$H2 - 0.00001 # put H2 between 0-1 for beta glm
## 
H22M1a <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "logit",
                   link.phi = "identity", 
                   data = dfTiw)
summary(H22M1a)
AIC(H22M1a)
plot(H22M1a)


############################
### modularity_index #######
############################


### gaussian for modularity_index, trial different link functions
### identity

modularity_indexM7a <- glm(modularity_index ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "identity"),
                           data = dfTiw)
summary(modularity_indexM7a)
AIC(modularity_indexM7a)

Model_R2modularity_indexM7a <- with(summary(modularity_indexM7a), 1 - deviance/null.deviance)
Model_R2modularity_indexM7a

plot(modularity_indexM7a)


#########################################################
###### Number of species higher level ###################
#########################################################



### Poisson for number of species higher level, trial different link functions
### identity
number.of.species.HLM7a <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = poisson (link = "identity"),
                               data = dfTiw)
summary(number.of.species.HLM7a)
AIC(number.of.species.HLM7a)
plot(number.of.species.HLM7a)

# R-squared
Model_R2number.of.species.HLM7a <- with(summary(number.of.species.HLM7a), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM7a


# overdispersion
overdispersion_number.of.species.HLM7a <- number.of.species.HLM7a$deviance / number.of.species.HLM7a$df.residual
overdispersion_number.of.species.HLM7a

# deviance residuals
devresid_number.of.species.HLM7a <- resid(number.of.species.HLM7a, type = "deviance")
plot(devresid_number.of.species.HLM7a ~ dfTiw$CaCo_index)


# Housekeeping
graphics.off() 
rm(list=ls())