###############################################################
## FINAL MODELS SELECTED FOR ANALYSES OF MERGED NETWORK INDICES
## INDICIES RESPONSE VARIABLES, MANGROVE FOREST TREATMENT AND VEGETATION STRUCTURE EXPLANATORY VARIABLES



# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Merged_Network_indices.csv

dfLik <- subset(dat1, Site == "Likupang")

### reset reference level of Treatment variable to Reference Forest forest

dfLik$Treatment <- factor(dfLik$Treatment, levels = c("Reference Forest","Mixed Species Regeneration","Monoculture Reforestation"))

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

## Beta GLM, cloglog link, sqrt link.phi

weighted_connectanceM3c <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM3c)
AIC(weighted_connectanceM3c)
plot(weighted_connectanceM3c)


########################################################
###########  weighted_NODF  ############################
########################################################

### gaussian GLM for weighted_NODF
### identity 
weighted_NODFM7a <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfLik)
summary(weighted_NODFM7a)
summary.lm(weighted_NODFM7a)
AIC(weighted_NODFM7a)
plot(weighted_NODFM7a)

Model_R2weighted_NODFM7a <- with(summary(weighted_NODFM7a), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7a


########################################################
###########  generality.HL  ############################
########################################################


### Inverse Gaussian for generality.HL
### identity link function


generality.HLM8c <- glm(generality.HL ~ Treatment 
                        + CaCo_index,
                        family = inverse.gaussian (link = "identity"),
                        data = dfLik)
summary(generality.HLM8c)
summary.lm(generality.HLM8c)

AIC(generality.HLM8c)
plot(generality.HLM8c)

Model_R2generality.HLM8c <- with(summary(generality.HLM8c), 1 - deviance/null.deviance)
Model_R2generality.HLM8c


#########################################################
########### robustness.HL  ##############################
#########################################################

### beta GLM, cauchi link, identity link.phi

robustness.HLM4a <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "cauchit",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM4a)
AIC(robustness.HLM4a)
plot(robustness.HLM4a)


#########################################################
########### interaction_evenness  ##############################
#########################################################
## beta glm
## cauchit link identity link.phi

interaction_evennessM4a <- betareg (interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM4a)
AIC(interaction_evennessM4a)
plot(interaction_evennessM4a)


########################################################
###########  vulnerability.LL  ############################
########################################################
### an inverse transformation for Vulnerability 
InverseVulnerability <- 1/dat1$vulnerability.LL

## Inverse Gaussian GLM with inverse link

# 
vulnerability.LLM8b <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = inverse.gaussian (link = "inverse"),
                           data = dfLik)
summary(vulnerability.LLM8b)
summary.lm(vulnerability.LLM8b)
AIC(vulnerability.LLM8b)
plot(vulnerability.LLM8b)

Model_R2vulnerability.LLM8b <- with(summary(vulnerability.LLM8b), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8b


###########################################################################
######   H2 ###############################################################
##########################################################################

H22 <- dfLik$H2 - 0.00001 # put H2 between 0-1 for beta glm


## loglog link, trial link.phi

H22M6a <- betareg (H22 ~ Treatment 
                   + CaCo_index,
                   link = "loglog",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M6a)
AIC(H22M6a)
plot(H22M6a)



##########################################################
###### Number of interactions ############################
#########################################################

### Negative binomial for number of interactions, trial different link functions

### identity
no._interactionsM9a <- glm.nb(no._interactions ~ Treatment
                              + CaCo_index 
                              , link = "identity",
                              data = dfLik)
summary(no._interactionsM9a, cor = FALSE)
summary.lm(no._interactionsM9a)
AIC(no._interactionsM9a)
plot(no._interactionsM9a)

# R-squared
Model_R2no._interactionsM9a <- with(summary(no._interactionsM9a), 1 - deviance/null.deviance)
Model_R2no._interactionsM9a

# overdispersion
overdispersion_no._interactionsM9a <- no._interactionsM9a$deviance / no._interactionsM9a$df.residual
overdispersion_no._interactionsM9a

# deviance residuals
devresid_no._interactionsM9a <- resid(no._interactionsM9a, type = "deviance")
plot(devresid_no._interactionsM9a ~ dfLik$CaCo_index)


##########################################################
###### Number of video interactions ######################
##########################################################

### Negative binomial for number of video interactions

### Negative binomial for number of video interactions, trial different link functions

### identity
no._interactions_videoM9a <- glm.nb(no._interactions_video ~ Treatment
                                    + CaCo_index 
                                    , link = "identity",
                                    data = dfLik)
summary(no._interactions_videoM9a, cor = FALSE)
summary.lm(no._interactions_videoM9a)
AIC(no._interactions_videoM9a)
plot(no._interactions_videoM9a)

# R-squared
Model_R2no._interactions_videoM9a <- with(summary(no._interactions_videoM9a), 1 - deviance/null.deviance)
Model_R2no._interactions_videoM9a

# overdispersion
overdispersion_no._interactions_videoM9a <- no._interactions_videoM9a$deviance / no._interactions_videoM9a$df.residual
overdispersion_no._interactions_videoM9a

# deviance residuals
devresid_no._interactions_videoM9a <- resid(no._interactions_videoM9a, type = "deviance")
plot(devresid_no._interactions_videoM9a ~ dfLik$CaCo_index)


##########################################################
###### Number of vegetation interactions #################
##########################################################

### Negative binomial for number of vegetation interactions


no._interactions_all_vegM9b <- glm.nb(no._interactions_all_veg ~ Treatment
                                      + CaCo_index 
                                      , link = "log",
                                      data = dfLik)
summary(no._interactions_all_vegM9b, cor = FALSE)
summary.lm(no._interactions_all_vegM9b)
AIC(no._interactions_all_vegM9b)
plot(no._interactions_all_vegM9b)

# R-squared
Model_R2no._interactions_all_vegM9b <- with(summary(no._interactions_all_vegM9b), 1 - deviance/null.deviance)
Model_R2no._interactions_all_vegM9b

# overdispersion
overdispersion_no._interactions_all_vegM9b <- no._interactions_all_vegM9b$deviance / no._interactions_all_vegM9b$df.residual
overdispersion_no._interactions_all_vegM9b

# deviance residuals
devresid_no._interactions_all_vegM9b <- resid(no._interactions_all_vegM9b, type = "deviance")
plot(devresid_no._interactions_all_vegM9b ~ dfLik$CaCo_index)


############################
### modularity_index #######
############################

### Inverse Gaussian for modularity_index, trial different link functions
### 1/mu^2
modularity_indexM8a <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = inverse.gaussian (link = "1/mu^2"),
                           data = dfLik)
summary(modularity_indexM8a)
AIC(modularity_indexM8a)

Model_R2modularity_indexM8a <- with(summary(modularity_indexM8a), 1 - deviance/null.deviance)
Model_R2modularity_indexM8a

plot(modularity_indexM8a)


### log of modularity index
## inverse_gaussian inverse link

Log10_modularity_indexM8b <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = inverse.gaussian (link = "inverse"),
                                 data = dfLik)
summary(Log10_modularity_indexM8b)
AIC(Log10_modularity_indexM8b)

Model_R2Log10_modularity_indexM8b <- with(summary(Log10_modularity_indexM8b), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM8b

plot(Log10_modularity_indexM8b)


#########################################################
###### Number of species higher level ###################
#########################################################

### Poisson for number of species higher level, identity link function
### 
number.of.species.HLM7a <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = poisson (link = "identity"),
                               data = dfLik)
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
plot(devresid_number.of.species.HLM7a ~ dfLik$CaCo_index)

# Housekeeping
graphics.off() 
rm(list=ls())