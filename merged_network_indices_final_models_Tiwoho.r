###############################################################
## FINAL MODELS SELECTED FOR ANALYSES OF MERGED NETWORK INDICES
## INDICIES RESPONSE VARIABLES, MANGROVE FOREST TREATMENT AND VEGETATION STRUCTURE EXPLANATORY VARIABLES



# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Merged_Network_indices.csv

dfTiw <- subset(dat1, Site == "Tiwoho")

### reset reference level of Treatment variable to Reference Forest forest

dfTiw$Treatment <- factor(dfTiw$Treatment, levels = c("Reference Forest","Mixed Species Regeneration","Monoculture Reforestation"))

#############
## PACKAGES #
#############

#install.packages("betareg")
#install.packages("MASS")

library(betareg)
library(MASS)

############################
### weighted_connectance####
############################

## Beta GLM, cauchit link, identitylink.phi

weighted_connectanceM4a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "identity", 
                                    data = dfTiw)
summary(weighted_connectanceM4a)
AIC(weighted_connectanceM4a)
plot(weighted_connectanceM4a)


########################################################
###########  weighted_NODF  ############################
########################################################

### gaussian GLM for weighted_NODF
### identity 
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


### Inverse Gaussian for generality.HL
### identity link function

generality.HLM8c <- glm(generality.HL ~ Treatment 
                        + CaCo_index,
                        family = inverse.gaussian (link = "identity"),
                        data = dfTiw)
summary(generality.HLM8c)
AIC(generality.HLM8c)

Model_R2generality.HLM8c <- with(summary(generality.HLM8c), 1 - deviance/null.deviance)
Model_R2generality.HLM8c

plot(generality.HLM8c)


#########################################################
########### robustness.HL  ##############################
#########################################################

### beta GLM, log link, identity link.phi
robustness.HLM5a <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "log",
                             link.phi = "identity", 
                             data = dfTiw)
summary(robustness.HLM5a)
AIC(robustness.HLM5a)
plot(robustness.HLM5a)

#########################################################
########### interaction_evenness  ##############################
#########################################################
## beta glm
## logit link identity link.phi

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

### Inverse Gaussian for vulnerability.LL, 1/mu^2 link function


vulnerability.LLM8a <- glm(vulnerability.LL ~ Treatment 
                           + CaCo_index,
                           family = inverse.gaussian (link = "1/mu^2"),
                           data = dfTiw)
summary(vulnerability.LLM8a)
summary.lm(vulnerability.LLM8a)
AIC(vulnerability.LLM8a)

Model_R2vulnerability.LLM8a <- with(summary(vulnerability.LLM8a), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8a

plot(vulnerability.LLM8a)


###########################################################################
######   H2 ###############################################################
##########################################################################

### Gaussuan with identity link

H2M7a <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = gaussian (link = "identity"),
             data = dfTiw)
summary(H2M7a)
summary.lm(H2M7a)
AIC(H2M7a)
Model_R2H2M7a <- with(summary(H2M7a), 1 - deviance/null.deviance)
Model_R2H2M7a


plot(H2M7a)


##########################################################
###### Number of interactions ############################
#########################################################

### quasipoisson for number of interactions, log link function
no._interactionsM8b <- glm(no._interactions ~ Treatment
                           + CaCo_index 
                           ,
                           family = quasipoisson (link = "log"),
                           data = dfTiw)
summary(no._interactionsM8b)
AIC(no._interactionsM8b)
plot(no._interactionsM8b)

# R-squared
Model_R2no._interactionsM8b <- with(summary(no._interactionsM8b), 1 - deviance/null.deviance)
Model_R2no._interactionsM8b

##########################################################
###### Number of video interactions ######################
##########################################################

### quasipoisson for number of video interactions, identity link function
no._interactions_videoM8a <- glm(no._interactions_video ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = quasipoisson (link = "identity"),
                                 data = dfTiw)
summary(no._interactions_videoM8a)
AIC(no._interactions_videoM8a)
plot(no._interactions_videoM8a)

# R-squared
Model_R2no._interactions_videoM8a <- with(summary(no._interactions_videoM8a), 1 - deviance/null.deviance)
Model_R2no._interactions_videoM8a

##########################################################
###### Number of vegetation interactions #################
##########################################################

### poisson GLM with log link function

no._interactions_all_vegM7b <- glm(no._interactions_all_veg ~ Treatment
                                   + CaCo_index,
                                   family = poisson (link = "log"),
                                   data = dfTiw)
summary(no._interactions_all_vegM7b)
AIC(no._interactions_all_vegM7b)
plot(no._interactions_all_vegM7b)

# R-squared
Model_R2no._interactions_all_vegM7b <- with(summary(no._interactions_all_vegM7b), 1 - deviance/null.deviance)
Model_R2no._interactions_all_vegM7b

# overdispersion
overdispersion_no._interactions_all_vegM7b <- no._interactions_all_vegM7b$deviance / no._interactions_all_vegM7b$df.residual
overdispersion_no._interactions_all_vegM7b

# deviance residuals
devresid_no._interactions_all_vegM7b <- resid(no._interactions_all_vegM7b, type = "deviance")
plot(devresid_no._interactions_all_vegM7b ~ dfTiw$CaCo_index)


############################
### modularity_index #######
############################

### Gaussian for modularity_index, identity link function
modularity_indexM7a <- glm(modularity_index ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "identity"),
                           data = dfTiw)
summary(modularity_indexM7a)
summary.lm(modularity_indexM7a)
AIC(modularity_indexM7a)

Model_R2modularity_indexM7a <- with(summary(modularity_indexM7a), 1 - deviance/null.deviance)
Model_R2modularity_indexM7a

plot(modularity_indexM7a)

### also very close
### Gammafor modularity_index, identity link function
modularity_indexM9c <- glm(modularity_index ~ Treatment
                           + CaCo_index,
                           family = Gamma (link = "identity"),
                           data = dfTiw)
summary(modularity_indexM9c)
AIC(modularity_indexM9c)

Model_R2modularity_indexM9c <- with(summary(modularity_indexM9c), 1 - deviance/null.deviance)
Model_R2modularity_indexM9c

plot(modularity_indexM9c)


#########################################################
###### Number of species higher level ###################
#########################################################

### Poisson for number of species higher level, identity link functions
### 
number.of.species.HLM7a <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = poisson (link = "identity"),
                               data = dfTiw)
summary(number.of.species.HLM7a)
summary.lm(number.of.species.HLM7a)
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