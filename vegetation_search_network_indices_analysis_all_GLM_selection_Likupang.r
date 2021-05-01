###############################################################
## TRIALING DIFFERENT MODEL FITS FOR THE ANALYSES OF VEGETATION NETWORK INDICES
## INDICIES RESPONSE VARIABLES, MANGROVE FOREST TREATMENT AND VEGETATION STRUCTURE EXPLANATORY VARIABLES



# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Network_indices_vegetation_search.csv

dfLik <- subset(dat1, Site == "Likupang" & network_size_sufficient_for_analysis == "YES")

dfLik$Treatment <- factor(dfLik$Treatment, levels = c("Reference Forest","Mixed Species Regeneration","Monoculture Reforestation"))


#############
## PACKAGES #
#############

#install.packages("betareg")
#install.packages("MASS")



#################################
#### which vegetation indices? ##
#################################

cor.test(dfLik$CaCo_index, dfLik$Foliage_projective_cover)

### use CaCo_index as it has the greatest coverage


#################################
##### Analysis ##################
#################################


#### BETA GLM  for 0-1 proportion data
library(betareg)


############################
### weighted_connectance ###
############################


#### BETA GLM  for 0-1 proportion data

## logit link, trial link.phi
weighted_connectanceM1a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM1a)
AIC(weighted_connectanceM1a)
plot(weighted_connectanceM1a)

weighted_connectanceM1b <- betareg (weighted_connectance ~ Treatment 
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "log", 
                                    data = dfLik)
summary(weighted_connectanceM1b)
AIC(weighted_connectanceM1b)
plot(weighted_connectanceM1b)

weighted_connectanceM1c <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM1c)
AIC(weighted_connectanceM1c)
plot(weighted_connectanceM1c)


## probit link, trial link.phi

weighted_connectanceM2a <- betareg (weighted_connectance ~ Treatment 
                                    + CaCo_index,
                                    link = "probit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM2a)
AIC(weighted_connectanceM2a)
plot(weighted_connectanceM2a)

weighted_connectanceM2b <- betareg (weighted_connectance ~ Treatment 
                                    + CaCo_index,
                                    link = "probit",
                                    link.phi = "log", 
                                    data = dfLik)
summary(weighted_connectanceM2b)
AIC(weighted_connectanceM2b)
plot(weighted_connectanceM2b)

weighted_connectanceM2c <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "probit",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM2c)
AIC(weighted_connectanceM2c)
plot(weighted_connectanceM2c)

## cloglog link, trial link.phi

weighted_connectanceM3a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM3a)
AIC(weighted_connectanceM3a)
plot(weighted_connectanceM3a)

weighted_connectanceM3b <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "log", 
                                    data = dfLik)
summary(weighted_connectanceM3b)
AIC(weighted_connectanceM3b)
plot(weighted_connectanceM3b)

weighted_connectanceM3c <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM3c)
AIC(weighted_connectanceM3c)
plot(weighted_connectanceM3c)


## cauchit link, trial link.phi

weighted_connectanceM4a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM4a)
AIC(weighted_connectanceM4a)
plot(weighted_connectanceM4a)

weighted_connectanceM4b <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "log", 
                                    data = dfLik)
summary(weighted_connectanceM4b)
AIC(weighted_connectanceM4b)
plot(weighted_connectanceM4b)

weighted_connectanceM4c <- betareg (weighted_connectance ~ Treatment 
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM4c)
AIC(weighted_connectanceM4c)
plot(weighted_connectanceM4c)



## log link, trial link.phi

weighted_connectanceM5a <- betareg (weighted_connectance ~ Treatment 
                                    + CaCo_index,
                                    link = "log",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM5a)
AIC(weighted_connectanceM5a)
plot(weighted_connectanceM5a)

weighted_connectanceM5b <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "log",
                                    link.phi = "log", 
                                    data = dfLik)
summary(weighted_connectanceM5b)
AIC(weighted_connectanceM5b)
plot(weighted_connectanceM5b)

weighted_connectanceM5c <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "log",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM5c)
AIC(weighted_connectanceM5c)
plot(weighted_connectanceM5c)


## loglog link, trial link.phi

weighted_connectanceM6a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "loglog",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM6a)
AIC(weighted_connectanceM6a)
plot(weighted_connectanceM6a)

weighted_connectanceM6b <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "loglog",
                                    link.phi = "log", 
                                    data = dfLik)
summary(weighted_connectanceM6b)
AIC(weighted_connectanceM6b)
plot(weighted_connectanceM6b)

weighted_connectanceM6c <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "loglog",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(weighted_connectanceM6c)
AIC(weighted_connectanceM6c)
plot(weighted_connectanceM6c)

### gaussian for weighted_connectance, trial different link functions
### identity
weighted_connectanceM7a <- glm(weighted_connectance ~ Treatment
                               + CaCo_index ,
                               family = gaussian (link = "identity"),
                               data = dfLik)
summary(weighted_connectanceM7a)
AIC(weighted_connectanceM7a)
plot(weighted_connectanceM7a)

Model_R2weighted_connectanceM7a <- with(summary(weighted_connectanceM7a), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM7a

## log
weighted_connectanceM7b <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "log"),
                               data = dfLik)
summary(weighted_connectanceM7b)
AIC(weighted_connectanceM7b)
plot(weighted_connectanceM7b)

Model_R2weighted_connectanceM7b <- with(summary(weighted_connectanceM7b), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM7b


## inverse
weighted_connectanceM7c <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "inverse"),
                               data = dfLik)
summary(weighted_connectanceM7c)
AIC(weighted_connectanceM7c)
plot(weighted_connectanceM7c)

Model_R2weighted_connectanceM7c <- with(summary(weighted_connectanceM7c), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM7c


## sqrt
weighted_connectanceM7d <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "sqrt"),
                               data = dfLik)
summary(weighted_connectanceM7d)
AIC(weighted_connectanceM7d)
plot(weighted_connectanceM7d)

Model_R2weighted_connectanceM7d <- with(summary(weighted_connectanceM7d), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM7d

### Inverse Gaussian for weighted_connectance, trial different link functions
### 1/mu^2
weighted_connectanceM8a <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "1/mu^2"),
                               data = dfLik)
summary(weighted_connectanceM8a)
AIC(weighted_connectanceM8a)
plot(weighted_connectanceM8a)

Model_R2weighted_connectanceM8a <- with(summary(weighted_connectanceM8a), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM8a


## inverse
weighted_connectanceM8b <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "inverse"),
                               data = dfLik)
summary(weighted_connectanceM8b)
AIC(weighted_connectanceM8b)
plot(weighted_connectanceM8b)

Model_R2weighted_connectanceM8b <- with(summary(weighted_connectanceM8b), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM8b


## identity
weighted_connectanceM8c <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "identity"),
                               data = dfLik)
summary(weighted_connectanceM8c)
AIC(weighted_connectanceM8c)
plot(weighted_connectanceM8c)

Model_R2weighted_connectanceM8c <- with(summary(weighted_connectanceM8c), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM8c


## log
weighted_connectanceM8d <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "log"),
                               data = dfLik)
summary(weighted_connectanceM8d)
AIC(weighted_connectanceM8d)
plot(weighted_connectanceM8d)

Model_R2weighted_connectanceM8d <- with(summary(weighted_connectanceM8d), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM8d

### Gamma for weighted_connectance, trial different link functions
### sqrt
weighted_connectanceM9a <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = Gamma (link = "sqrt"),
                               data = dfLik)
summary(weighted_connectanceM9a)
AIC(weighted_connectanceM9a)
plot(weighted_connectanceM9a)

Model_R2weighted_connectanceM9a <- with(summary(weighted_connectanceM9a), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM9a


## inverse
weighted_connectanceM9b <- glm(weighted_connectance ~ Treatment 
                               + CaCo_index,
                               family = Gamma (link = "inverse"),
                               data = dfLik)
summary(weighted_connectanceM9b)
AIC(weighted_connectanceM9b)
plot(weighted_connectanceM9b)

Model_R2weighted_connectanceM9b <- with(summary(weighted_connectanceM9b), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM9b


## identity
weighted_connectanceM9c <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = Gamma (link = "identity"),
                               data = dfLik)
summary(weighted_connectanceM9c)
AIC(weighted_connectanceM9c)
plot(weighted_connectanceM9c)

Model_R2weighted_connectanceM9c <- with(summary(weighted_connectanceM9c), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM9c


## log
weighted_connectanceM9d <- glm(weighted_connectance ~ Treatment
                               + CaCo_index,
                               family = Gamma (link = "log"),
                               data = dfLik)
summary(weighted_connectanceM9d)
AIC(weighted_connectanceM9d)
plot(weighted_connectanceM9d)

Model_R2weighted_connectanceM9d <- with(summary(weighted_connectanceM9d), 1 - deviance/null.deviance)
Model_R2weighted_connectanceM9d



########################################################
###########  weighted_NODF  ############################
########################################################



### gaussian for weighted_NODF, trial different link functions
### identity
weighted_NODFM7a <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfLik, na.action = na.omit)
summary(weighted_NODFM7a)
AIC(weighted_NODFM7a)
plot(weighted_NODFM7a)

Model_R2weighted_NODFM7a <- with(summary(weighted_NODFM7a), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7a

## data transformations
## small value added to weighted_NODF for transformations
## to get rid of zeros

weighted_NODF_plus <- dfLik$weighted_NODF + 0.0001

## log
log_w_NODF <- log10(weighted_NODF_plus)

### gaussian for weighted_NODF, with logged response variable
### identity

log_w_NODFM7a <- glm(log_w_NODF ~ Treatment
                     + CaCo_index,
                     family = gaussian (link = "identity"),
                     data = dfLik, na.action = na.omit)
summary(log_w_NODFM7a)
AIC(log_w_NODFM7a)
plot(log_w_NODFM7a)

Model_R2log_w_NODFM7a <- with(summary(log_w_NODFM7a), 1 - deviance/null.deviance)
Model_R2log_w_NODFM7a

## cubic
cube_w_NODF <-  weighted_NODF_plus ^(1/3)

### gaussian for weighted_NODF, with cubic response variable
### identity

cube_w_NODFM7a <- glm(cube_w_NODF ~ Treatment
                      + CaCo_index,
                      family = gaussian (link = "identity"),
                      data = dfLik, na.action = na.omit)
summary(cube_w_NODFM7a)
AIC(cube_w_NODFM7a)
plot(cube_w_NODFM7a)

Model_R2cube_w_NODFM7a <- with(summary(cube_w_NODFM7a), 1 - deviance/null.deviance)
Model_R2cube_w_NODFM7a

## sqrt
Sqrt_w_NODF <-  sqrt(weighted_NODF_plus)

### gaussian for weighted_NODF, with sqrt response variable
### identity

Sqrt_w_NODFM7a <- glm(Sqrt_w_NODF ~ Treatment
                      + CaCo_index,
                      family = gaussian (link = "identity"),
                      data = dfLik, na.action = na.omit)
summary(Sqrt_w_NODFM7a)
AIC(Sqrt_w_NODFM7a)
plot(Sqrt_w_NODFM7a)

Model_R2Sqrt_w_NODFM7a <- with(summary(Sqrt_w_NODFM7a), 1 - deviance/null.deviance)
Model_R2Sqrt_w_NODFM7a


## inverse
Inverse_w_NODFM <- 1/weighted_NODF_plus

### gaussian for weighted_NODF, with inverse transformed response variable
### identity

Inverse_w_NODFMM7a <- glm(Inverse_w_NODFM ~ Treatment
                          + CaCo_index,
                          family = gaussian (link = "identity"),
                          data = dfLik, na.action = na.omit)
summary(Inverse_w_NODFMM7a)
AIC(Inverse_w_NODFMM7a)
plot(Inverse_w_NODFMM7a)

Model_R2Inverse_w_NODFMM7a <- with(summary(Inverse_w_NODFMM7a), 1 - deviance/null.deviance)
Model_R2Inverse_w_NODFMM7a


## log  - error messages
weighted_NODFM7b <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "log"),
                        data = dfLik, na.action = na.omit)
summary(weighted_NODFM7b)
AIC(weighted_NODFM7b)
plot(weighted_NODFM7b)

Model_R2weighted_NODFM7b <- with(summary(weighted_NODFM7b), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7b



## inverse - error messages 
weighted_NODFM7c <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "inverse"),
                        data = dfLik)
summary(weighted_NODFM7c)
AIC(weighted_NODFM7c)
plot(weighted_NODFM7c)

Model_R2weighted_NODFM7c <- with(summary(weighted_NODFM7c), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7c


## sqrt  - error messages
weighted_NODFM7d <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "sqrt"),
                        data = dfLik)
summary(weighted_NODFM7d)
AIC(weighted_NODFM7d)
plot(weighted_NODFM7d)

Model_R2weighted_NODFM7d <- with(summary(weighted_NODFM7d), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7d

### Inverse Gaussian for weighted_NODF, trial different link functions

## small value added to weighted_NODF for Inverse Gaussian and Gamma
## to get rid of zeros

weighted_NODF_plus <- dfLik$weighted_NODF + 0.0001

### 1/mu^2 #### useless fit
weighted_NODF_plusM8a <- glm(weighted_NODF_plus ~ Treatment 
                             + CaCo_index,
                             family = inverse.gaussian (link = "1/mu^2"),
                             data = dfLik)
summary(weighted_NODF_plusM8a)
AIC(weighted_NODF_plusM8a)
plot(weighted_NODF_plusM8a)

Model_R2weighted_NODF_plusM8a <- with(summary(weighted_NODF_plusM8a), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM8a


## inverse

weighted_NODF_plusM8b <- glm(weighted_NODF_plus ~ Treatment
                             + CaCo_index,
                             family = inverse.gaussian (link = "inverse"),
                             data = dfLik)
summary(weighted_NODF_plusM8b)
AIC(weighted_NODF_plusM8b)
plot(weighted_NODF_plusM8b)

Model_R2weighted_NODF_plusM8b <- with(summary(weighted_NODF_plusM8b), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM8b


## identity

weighted_NODF_plusM8c <- glm(weighted_NODF_plus ~ Treatment
                             + CaCo_index,
                             family = inverse.gaussian (link = "identity"),
                             data = dfLik)
summary(weighted_NODF_plusM8c)
AIC(weighted_NODF_plusM8c)
plot(weighted_NODF_plusM8c)

Model_R2weighted_NODF_plusM8c <- with(summary(weighted_NODF_plusM8c), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM8c


## log

weighted_NODF_plusM8d <- glm(weighted_NODF_plus ~ Treatment
                             + CaCo_index,
                             family = inverse.gaussian (link = "log"),
                             data = dfLik)
summary(weighted_NODF_plusM8d)
AIC(weighted_NODF_plusM8d)
plot(weighted_NODF_plusM8d)

Model_R2weighted_NODF_plusM8d <- with(summary(weighted_NODF_plusM8d), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM8d

### Gamma for weighted_NODF_plus, trial different link functions
### sqrt
weighted_NODF_plusM9a <- glm(weighted_NODF_plus ~ Treatment 
                             + CaCo_index,
                             family = Gamma (link = "sqrt"),
                             data = dfLik)
summary(weighted_NODF_plusM9a)
AIC(weighted_NODF_plusM9a)
plot(weighted_NODF_plusM9a)

Model_R2weighted_NODF_plusM9a <- with(summary(weighted_NODF_plusM9a), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM9a


## inverse
weighted_NODF_plusM9b <- glm(weighted_NODF_plus ~ Treatment
                             + CaCo_index,
                             family = Gamma (link = "inverse"),
                             data = dfLik)
summary(weighted_NODF_plusM9b)
AIC(weighted_NODF_plusM9b)
plot(weighted_NODF_plusM9b)

Model_R2weighted_NODF_plusM9b <- with(summary(weighted_NODF_plusM9b), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM9b


## identity
weighted_NODF_plusM9c <- glm(weighted_NODF_plus ~ Treatment 
                             + CaCo_index,
                             family = Gamma (link = "identity"),
                             data = dfLik)
summary(weighted_NODF_plusM9c)
AIC(weighted_NODF_plusM9c)
plot(weighted_NODF_plusM9c)

Model_R2weighted_NODF_plusM9c <- with(summary(weighted_NODF_plusM9c), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM9c


## log
# fails
weighted_NODF_plusM9d <- glm(weighted_NODF_plus ~ Treatment 
                             + CaCo_index,
                             family = Gamma (link = "log"),
                             data = dfLik)
summary(weighted_NODF_plusM9d)
AIC(weighted_NODF_plusM9d)
plot(weighted_NODF_plusM9d)

Model_R2weighted_NODF_plusM9d <- with(summary(weighted_NODF_plusM9d), 1 - deviance/null.deviance)
Model_R2weighted_NODF_plusM9d


########################################################
###########  generality.HL  ############################
########################################################

### gaussian for generality.HL, trial different link functions
### identity
generality.HLM7a <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfLik)
summary(generality.HLM7a)
AIC(generality.HLM7a)
plot(generality.HLM7a)

Model_R2generality.HLM7a <- with(summary(generality.HLM7a), 1 - deviance/null.deviance)
Model_R2generality.HLM7a


### trial logged generality for identitiy

log_generality <- log10(dfLik$generality.HL)

LogGenerality.HLM8c <- glm(log_generality ~ Treatment 
                           + CaCo_index,
                           family = gaussian (link = "identity"),
                           data = dfLik)
summary(LogGenerality.HLM8c)
AIC(LogGenerality.HLM8c)
plot(LogGenerality.HLM8c)

Model_R2LogGenerality.HLM8c <- with(summary(LogGenerality.HLM8c), 1 - deviance/null.deviance)
Model_R2LogGenerality.HLM8c

## log
generality.HLM7b <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "log"),
                        data = dfLik)
summary(generality.HLM7b)
AIC(generality.HLM7b)
plot(generality.HLM7b)

Model_R2generality.HLM7b <- with(summary(generality.HLM7b), 1 - deviance/null.deviance)
Model_R2generality.HLM7b


## inverse
generality.HLM7c <- glm(generality.HL ~ Treatment 
                        + CaCo_index,
                        family = gaussian (link = "inverse"),
                        data = dfLik)
summary(generality.HLM7c)
AIC(generality.HLM7c)
plot(generality.HLM7c)

Model_R2generality.HLM7c <- with(summary(generality.HLM7c), 1 - deviance/null.deviance)
Model_R2generality.HLM7c


## sqrt
generality.HLM7d <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "sqrt"),
                        data = dfLik)
summary(generality.HLM7d)
AIC(generality.HLM7d)
plot(generality.HLM7d)

Model_R2generality.HLM7d <- with(summary(generality.HLM7d), 1 - deviance/null.deviance)
Model_R2generality.HLM7d

### Inverse Gaussian for generality.HL, trial different link functions


### 1/mu^2
generality.HLM8a <- glm(generality.HL ~ Treatment 
                        + CaCo_index,
                        family = inverse.gaussian (link = "1/mu^2"),
                        data = dfLik)
summary(generality.HLM8a)
AIC(generality.HLM8a)
plot(generality.HLM8a)

Model_R2generality.HLM8a <- with(summary(generality.HLM8a), 1 - deviance/null.deviance)
Model_R2generality.HLM8a


## inverse
#  
generality.HLM8b <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = inverse.gaussian (link = "inverse"),
                        data = dfLik)
summary(generality.HLM8b)
AIC(generality.HLM8b)
plot(generality.HLM8b)

Model_R2generality.HLM8b <- with(summary(generality.HLM8b), 1 - deviance/null.deviance)
Model_R2generality.HLM8b


## identity
# 
generality.HLM8c <- glm(generality.HL ~ Treatment 
                        + CaCo_index,
                        family = inverse.gaussian (link = "identity"),
                        data = dfLik)
summary(generality.HLM8c)
AIC(generality.HLM8c)
plot(generality.HLM8c)

Model_R2generality.HLM8c <- with(summary(generality.HLM8c), 1 - deviance/null.deviance)
Model_R2generality.HLM8c


## log
# 
generality.HLM8d <- glm(generality.HL ~ Treatment 
                        + CaCo_index,
                        family = inverse.gaussian (link = "log"),
                        data = dfLik)
summary(generality.HLM8d)
AIC(generality.HLM8d)
plot(generality.HLM8d)

Model_R2generality.HLM8d <- with(summary(generality.HLM8d), 1 - deviance/null.deviance)
Model_R2generality.HLM8d

### Gamma for generality.HL, trial different link functions
### sqrt
generality.HLM9a <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = Gamma (link = "sqrt"),
                        data = dfLik)
summary(generality.HLM9a)
AIC(generality.HLM9a)
plot(generality.HLM9a)

Model_R2generality.HLM9a <- with(summary(generality.HLM9a), 1 - deviance/null.deviance)
Model_R2generality.HLM9a


## inverse
generality.HLM9b <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = Gamma (link = "inverse"),
                        data = dfLik)
summary(generality.HLM9b)
AIC(generality.HLM9b)
plot(generality.HLM9b)

Model_R2generality.HLM9b <- with(summary(generality.HLM9b), 1 - deviance/null.deviance)
Model_R2generality.HLM9b


## identity
generality.HLM9c <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = Gamma (link = "identity"),
                        data = dfLik)
summary(generality.HLM9c)
AIC(generality.HLM9c)
plot(generality.HLM9c)

Model_R2generality.HLM9c <- with(summary(generality.HLM9c), 1 - deviance/null.deviance)
Model_R2generality.HLM9c


## log
generality.HLM9d <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = Gamma (link = "log"),
                        data = dfLik)
summary(generality.HLM9d)
AIC(generality.HLM9d)
plot(generality.HLM9d)

Model_R2generality.HLM9d <- with(summary(generality.HLM9d), 1 - deviance/null.deviance)
Model_R2generality.HLM9d

#########################################################
########### robustness.HL  ##############################
#########################################################

#### BETA GLM  for 0-1 proportion data

## logit link, trial link.phi
robustness.HLM1a <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "logit",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM1a)
AIC(robustness.HLM1a)
plot(robustness.HLM1a)

robustness.HLM1b <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "logit",
                             link.phi = "log", 
                             data = dfLik)
summary(robustness.HLM1b)
AIC(robustness.HLM1b)
plot(robustness.HLM1b)

robustness.HLM1c <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "logit",
                             link.phi = "sqrt", 
                             data = dfLik)
summary(robustness.HLM1c)
AIC(robustness.HLM1c)
plot(robustness.HLM1c)


## probit link, trial link.phi

robustness.HLM2a <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "probit",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM2a)
AIC(robustness.HLM2a)
plot(robustness.HLM2a)

robustness.HLM2b <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "probit",
                             link.phi = "log", 
                             data = dfLik)
summary(robustness.HLM2b)
AIC(robustness.HLM2b)
plot(robustness.HLM2b)

robustness.HLM2c <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "probit",
                             link.phi = "sqrt", 
                             data = dfLik)
summary(robustness.HLM2c)
AIC(robustness.HLM2c)
plot(robustness.HLM2c)

## cloglog link, trial link.phi

robustness.HLM3a <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "cloglog",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM3a)
AIC(robustness.HLM3a)
plot(robustness.HLM3a)

robustness.HLM3b <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "cloglog",
                             link.phi = "log", 
                             data = dfLik)
summary(robustness.HLM3b)
AIC(robustness.HLM3b)
plot(robustness.HLM3b)

robustness.HLM3c <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "cloglog",
                             link.phi = "sqrt", 
                             data = dfLik)
summary(robustness.HLM3c)
AIC(robustness.HLM3c)
plot(robustness.HLM3c)


## cauchit link, trial link.phi

robustness.HLM4a <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "cauchit",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM4a)
AIC(robustness.HLM4a)
plot(robustness.HLM4a)

robustness.HLM4b <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "cauchit",
                             link.phi = "log", 
                             data = dfLik)
summary(robustness.HLM4b)
AIC(robustness.HLM4b)
plot(robustness.HLM4b)

robustness.HLM4c <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "cauchit",
                             link.phi = "sqrt", 
                             data = dfLik)
summary(robustness.HLM4c)
AIC(robustness.HLM4c)
plot(robustness.HLM4c)



## log link, trial link.phi

robustness.HLM5a <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "log",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM5a)
AIC(robustness.HLM5a)
plot(robustness.HLM5a)

robustness.HLM5b <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "log",
                             link.phi = "log", 
                             data = dfLik)
summary(robustness.HLM5b)
AIC(robustness.HLM5b)
plot(robustness.HLM5b)

robustness.HLM5c <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "log",
                             link.phi = "sqrt", 
                             data = dfLik)
summary(robustness.HLM5c)
AIC(robustness.HLM5c)
plot(robustness.HLM5c)


## loglog link, trial link.phi

robustness.HLM6a <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "loglog",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM6a)
AIC(robustness.HLM6a)
plot(robustness.HLM6a)

robustness.HLM6b <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "loglog",
                             link.phi = "log", 
                             data = dfLik)
summary(robustness.HLM6b)
AIC(robustness.HLM6b)
plot(robustness.HLM6b)

robustness.HLM6c <- betareg (robustness.HL ~ Treatment
                             + CaCo_index,
                             link = "loglog",
                             link.phi = "sqrt", 
                             data = dfLik)
summary(robustness.HLM6c)
AIC(robustness.HLM6c)
plot(robustness.HLM6c)

### gaussian for robustness.HL, trial different link functions
### identity
robustness.HLM7a <- glm(robustness.HL ~ Treatment 
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfLik)
summary(robustness.HLM7a)
AIC(robustness.HLM7a)
plot(robustness.HLM7a)

Model_R2robustness.HLM7a <- with(summary(robustness.HLM7a), 1 - deviance/null.deviance)
Model_R2robustness.HLM7a

## log
robustness.HLM7b <- glm(robustness.HL ~ Treatment 
                        + CaCo_index,
                        family = gaussian (link = "log"),
                        data = dfLik)
summary(robustness.HLM7b)
AIC(robustness.HLM7b)
plot(robustness.HLM7b)

Model_R2robustness.HLM7b <- with(summary(robustness.HLM7b), 1 - deviance/null.deviance)
Model_R2robustness.HLM7b


## log
robustness.HLM7b <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "log"),
                        data = dfLik)
summary(robustness.HLM7b)
AIC(robustness.HLM7b)
plot(robustness.HLM7b)

Model_R2robustness.HLM7b <- with(summary(robustness.HLM7b), 1 - deviance/null.deviance)
Model_R2robustness.HLM7b


## inverse
robustness.HLM7c <- glm(robustness.HL ~ Treatment 
                        + CaCo_index,
                        family = gaussian (link = "inverse"),
                        data = dfLik)
summary(robustness.HLM7c)
AIC(robustness.HLM7c)
plot(robustness.HLM7c)

Model_R2robustness.HLM7c <- with(summary(robustness.HLM7c), 1 - deviance/null.deviance)
Model_R2robustness.HLM7c


## sqrt
robustness.HLM7d <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "sqrt"),
                        data = dfLik)
summary(robustness.HLM7d)
AIC(robustness.HLM7d)
plot(robustness.HLM7d)

Model_R2robustness.HLM7d <- with(summary(robustness.HLM7d), 1 - deviance/null.deviance)
Model_R2robustness.HLM7d



### Inverse Gaussian for robustness.HL, trial different link functions


### 1/mu^2
robustness.HLM8a <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = inverse.gaussian (link = "1/mu^2"),
                        data = dfLik)
summary(robustness.HLM8a)
AIC(robustness.HLM8a)
plot(robustness.HLM8a)

Model_R2robustness.HLM8a <- with(summary(robustness.HLM8a), 1 - deviance/null.deviance)
Model_R2robustness.HLM8a


## inverse
robustness.HLM8b <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = inverse.gaussian (link = "inverse"),
                        data = dfLik)
summary(robustness.HLM8b)
AIC(robustness.HLM8b)
plot(robustness.HLM8b)

Model_R2robustness.HLM8b <- with(summary(robustness.HLM8b), 1 - deviance/null.deviance)
Model_R2robustness.HLM8b


## identity
robustness.HLM8c <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = inverse.gaussian (link = "identity"),
                        data = dfLik)
summary(robustness.HLM8c)
AIC(robustness.HLM8c)
plot(robustness.HLM8c)

Model_R2robustness.HLM8c <- with(summary(robustness.HLM8c), 1 - deviance/null.deviance)
Model_R2robustness.HLM8c


## log
# 
robustness.HLM8d <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = inverse.gaussian (link = "log"),
                        data = dfLik)
summary(robustness.HLM8d)
AIC(robustness.HLM8d)
plot(robustness.HLM8d)

Model_R2robustness.HLM8d <- with(summary(robustness.HLM8d), 1 - deviance/null.deviance)
Model_R2robustness.HLM8d

### Gamma for robustness.HL, trial different link functions
### sqrt
robustness.HLM9a <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = Gamma (link = "sqrt"),
                        data = dfLik)
summary(robustness.HLM9a)
AIC(robustness.HLM9a)
plot(robustness.HLM9a)

Model_R2robustness.HLM9a <- with(summary(robustness.HLM9a), 1 - deviance/null.deviance)
Model_R2robustness.HLM9a


## inverse
robustness.HLM9b <- glm(robustness.HL ~ Treatment
                        + CaCo_index,
                        family = Gamma (link = "inverse"),
                        data = dfLik)
summary(robustness.HLM9b)
AIC(robustness.HLM9b)
plot(robustness.HLM9b)

Model_R2robustness.HLM9b <- with(summary(robustness.HLM9b), 1 - deviance/null.deviance)
Model_R2robustness.HLM9b


## identity
robustness.HLM9c <- glm(robustness.HL ~ Treatment 
                        + CaCo_index,
                        family = Gamma (link = "identity"),
                        data = dfLik)
summary(robustness.HLM9c)
AIC(robustness.HLM9c)
plot(robustness.HLM9c)

Model_R2robustness.HLM9c <- with(summary(robustness.HLM9c), 1 - deviance/null.deviance)
Model_R2robustness.HLM9c


## log
robustness.HLM9d <- glm(robustness.HL ~ Treatment 
                        + CaCo_index,
                        family = Gamma (link = "log"),
                        data = dfLik)
summary(robustness.HLM9d)
AIC(robustness.HLM9d)
plot(robustness.HLM9d)

Model_R2robustness.HLM9d <- with(summary(robustness.HLM9d), 1 - deviance/null.deviance)
Model_R2robustness.HLM9d



#########################################################
########### interaction_evenness  ##############################
#########################################################

#### BETA GLM  for 0-1 proportion data

## logit link, trial link.phi
interaction_evennessM1a <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM1a)
AIC(interaction_evennessM1a)
plot(interaction_evennessM1a)

interaction_evennessM1b <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "log", 
                                    data = dfLik)
summary(interaction_evennessM1b)
AIC(interaction_evennessM1b)
plot(interaction_evennessM1b)

interaction_evennessM1c <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(interaction_evennessM1c)
AIC(interaction_evennessM1c)
plot(interaction_evennessM1c)


## probit link, trial link.phi

interaction_evennessM2a <- betareg (interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    link = "probit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM2a)
AIC(interaction_evennessM2a)
plot(interaction_evennessM2a)

interaction_evennessM2b <- betareg (interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    link = "probit",
                                    link.phi = "log", 
                                    data = dfLik)
summary(interaction_evennessM2b)
AIC(interaction_evennessM2b)
plot(interaction_evennessM2b)

interaction_evennessM2c <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "probit",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(interaction_evennessM2c)
AIC(interaction_evennessM2c)
plot(interaction_evennessM2c)

## cloglog link, trial link.phi

interaction_evennessM3a <- betareg (interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM3a)
AIC(interaction_evennessM3a)
plot(interaction_evennessM3a)

interaction_evennessM3b <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "log", 
                                    data = dfLik)
summary(interaction_evennessM3b)
AIC(interaction_evennessM3b)
plot(interaction_evennessM3b)

interaction_evennessM3c <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "cloglog",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(interaction_evennessM3c)
AIC(interaction_evennessM3c)
plot(interaction_evennessM3c)


## cauchit link, trial link.phi

interaction_evennessM4a <- betareg (interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM4a)
AIC(interaction_evennessM4a)
plot(interaction_evennessM4a)

interaction_evennessM4b <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "log", 
                                    data = dfLik)
summary(interaction_evennessM4b)
AIC(interaction_evennessM4b)
plot(interaction_evennessM4b)

interaction_evennessM4c <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "cauchit",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(interaction_evennessM4c)
AIC(interaction_evennessM4c)
plot(interaction_evennessM4c)



## log link, trial link.phi

interaction_evennessM5a <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "log",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM5a)
AIC(interaction_evennessM5a)
plot(interaction_evennessM5a)

interaction_evennessM5b <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "log",
                                    link.phi = "log", 
                                    data = dfLik)
summary(interaction_evennessM5b)
AIC(interaction_evennessM5b)
plot(interaction_evennessM5b)

interaction_evennessM5c <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "log",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(interaction_evennessM5c)
AIC(interaction_evennessM5c)
plot(interaction_evennessM5c)


## loglog link, trial link.phi
interaction_evennessM6a <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "loglog",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM6a)
AIC(interaction_evennessM6a)
plot(interaction_evennessM6a)

interaction_evennessM6b <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "loglog",
                                    link.phi = "log", 
                                    data = dfLik)
summary(interaction_evennessM6b)
AIC(interaction_evennessM6b)
plot(interaction_evennessM6b)

interaction_evennessM6c <- betareg (interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    link = "loglog",
                                    link.phi = "sqrt", 
                                    data = dfLik)
summary(interaction_evennessM6c)
AIC(interaction_evennessM6c)
plot(interaction_evennessM6c)

### gaussian for interaction_evenness, trial different link functions
### identity
interaction_evennessM7a <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "identity"),
                               data = dfLik)
summary(interaction_evennessM7a)
AIC(interaction_evennessM7a)
plot(interaction_evennessM7a)

Model_R2interaction_evennessM7a <- with(summary(interaction_evennessM7a), 1 - deviance/null.deviance)
Model_R2interaction_evennessM7a

### log response variable

log10interaction_evenness <- log10(dfLik$interaction_evenness)

### identity with logged response variable
log10interaction_evennessM7a <- glm(log10interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    family = gaussian (link = "identity"),
                                    data = dfLik)
summary(log10interaction_evennessM7a)
AIC(log10interaction_evennessM7a)
plot(log10interaction_evennessM7a)

Model_R2log10interaction_evennessM7a <- with(summary(log10interaction_evennessM7a), 1 - deviance/null.deviance)
Model_R2log10interaction_evennessM7a


## log
interaction_evennessM7b <- glm(interaction_evenness ~ Treatment 
                               + CaCo_index,
                               family = gaussian (link = "log"),
                               data = dfLik)
summary(interaction_evennessM7b)
AIC(interaction_evennessM7b)
plot(interaction_evennessM7b)

Model_R2interaction_evennessM7b <- with(summary(interaction_evennessM7b), 1 - deviance/null.deviance)
Model_R2interaction_evennessM7b

### identity with logged response variable
# fails
log10interaction_evennessM7b <- glm(log10interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    family = gaussian (link = "log"),
                                    data = dfLik)
summary(log10interaction_evennessM7b)
AIC(log10interaction_evennessM7b)
plot(log10interaction_evennessM7b)

Model_R2log10interaction_evennessM7b <- with(summary(log10interaction_evennessM7b), 1 - deviance/null.deviance)
Model_R2log10interaction_evennessM7b



## inverse
interaction_evennessM7c <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "inverse"),
                               data = dfLik)
summary(interaction_evennessM7c)
AIC(interaction_evennessM7c)
plot(interaction_evennessM7c)

Model_R2interaction_evennessM7c <- with(summary(interaction_evennessM7c), 1 - deviance/null.deviance)
Model_R2interaction_evennessM7c

### inverse with logged response variable
log10interaction_evennessM7c <- glm(log10interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    family = gaussian (link = "inverse"),
                                    data = dfLik)
summary(log10interaction_evennessM7c)
AIC(log10interaction_evennessM7c)
plot(log10interaction_evennessM7c)

Model_R2log10interaction_evennessM7c <- with(summary(log10interaction_evennessM7c), 1 - deviance/null.deviance)
Model_R2log10interaction_evennessM7c


## sqrt
interaction_evennessM7d <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "sqrt"),
                               data = dfLik)
summary(interaction_evennessM7d)
AIC(interaction_evennessM7d)
plot(interaction_evennessM7d)

Model_R2interaction_evennessM7d <- with(summary(interaction_evennessM7d), 1 - deviance/null.deviance)
Model_R2interaction_evennessM7d

### response variable logged
# fails
log10interaction_evennessM7d <- glm(log10interaction_evenness ~ Treatment
                                    + CaCo_index,
                                    family = gaussian (link = "sqrt"),
                                    data = dfLik)
summary(log10interaction_evennessM7d)
AIC(log10interaction_evennessM7d)
plot(log10interaction_evennessM7d)

Model_R2log10interaction_evennessM7d <- with(summary(log10interaction_evennessM7d), 1 - deviance/null.deviance)
Model_R2log10interaction_evennessM7d



### Inverse Gaussian for interaction_evenness, trial different link functions


### 1/mu^2
interaction_evennessM8a <- glm(interaction_evenness ~ Treatment 
                               + CaCo_index,
                               family = inverse.gaussian (link = "1/mu^2"),
                               data = dfLik)
summary(interaction_evennessM8a)
AIC(interaction_evennessM8a)
plot(interaction_evennessM8a)

Model_R2interaction_evennessM8a <- with(summary(interaction_evennessM8a), 1 - deviance/null.deviance)
Model_R2interaction_evennessM8a


## inverse

interaction_evennessM8b <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "inverse"),
                               data = dfLik)
summary(interaction_evennessM8b)
AIC(interaction_evennessM8b)
plot(interaction_evennessM8b)

Model_R2interaction_evennessM8b <- with(summary(interaction_evennessM8b), 1 - deviance/null.deviance)
Model_R2interaction_evennessM8b


## identity
interaction_evennessM8c <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "identity"),
                               data = dfLik)
summary(interaction_evennessM8c)
AIC(interaction_evennessM8c)
plot(interaction_evennessM8c)

Model_R2interaction_evennessM8c <- with(summary(interaction_evennessM8c), 1 - deviance/null.deviance)
Model_R2interaction_evennessM8c


## log
interaction_evennessM8d <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = inverse.gaussian (link = "log"),
                               data = dfLik)
summary(interaction_evennessM8d)
AIC(interaction_evennessM8d)
plot(interaction_evennessM8d)

Model_R2interaction_evennessM8d <- with(summary(interaction_evennessM8d), 1 - deviance/null.deviance)
Model_R2interaction_evennessM8d

### Gamma for interaction_evenness, trial different link functions
### sqrt
interaction_evennessM9a <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = Gamma (link = "sqrt"),
                               data = dfLik)
summary(interaction_evennessM9a)
AIC(interaction_evennessM9a)
plot(interaction_evennessM9a)

Model_R2interaction_evennessM9a <- with(summary(interaction_evennessM9a), 1 - deviance/null.deviance)
Model_R2interaction_evennessM9a


## inverse
interaction_evennessM9b <- glm(interaction_evenness ~ Treatment 
                               + CaCo_index,
                               family = Gamma (link = "inverse"),
                               data = dfLik)
summary(interaction_evennessM9b)
AIC(interaction_evennessM9b)
plot(interaction_evennessM9b)

Model_R2interaction_evennessM9b <- with(summary(interaction_evennessM9b), 1 - deviance/null.deviance)
Model_R2interaction_evennessM9b


## identity
interaction_evennessM9c <- glm(interaction_evenness ~ Treatment
                               + CaCo_index,
                               family = Gamma (link = "identity"),
                               data = dfLik)
summary(interaction_evennessM9c)
AIC(interaction_evennessM9c)
plot(interaction_evennessM9c)

Model_R2interaction_evennessM9c <- with(summary(interaction_evennessM9c), 1 - deviance/null.deviance)
Model_R2interaction_evennessM9c


## log
interaction_evennessM9d <- glm(interaction_evenness ~ Treatment 
                               + CaCo_index,
                               family = Gamma (link = "log"),
                               data = dfLik)
summary(interaction_evennessM9d)
AIC(interaction_evennessM9d)
plot(interaction_evennessM9d)

Model_R2interaction_evennessM9d <- with(summary(interaction_evennessM9d), 1 - deviance/null.deviance)
Model_R2interaction_evennessM9d

########################################################
###########  vulnerability.LL  #########################
########################################################


### gaussian for vulnerability.LL, trial different link functions
### identity
vulnerability.LLM7a <- glm(vulnerability.LL  ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "identity"),
                           data = dfLik)
summary(vulnerability.LLM7a)
AIC(vulnerability.LLM7a)
plot(vulnerability.LLM7a)

Model_R2vulnerability.LLM7a <- with(summary(vulnerability.LLM7a), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM7a


## log
vulnerability.LLM7b <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "log"),
                           data = dfLik)
summary(vulnerability.LLM7b)
AIC(vulnerability.LLM7b)
plot(vulnerability.LLM7b)

Model_R2vulnerability.LLM7b <- with(summary(vulnerability.LLM7b), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM7b


## inverse
vulnerability.LLM7c <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "inverse"),
                           data = dfLik)
summary(vulnerability.LLM7c)
AIC(vulnerability.LLM7c)
plot(vulnerability.LLM7c)

Model_R2vulnerability.LLM7c <- with(summary(vulnerability.LLM7c), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM7c


## sqrt
vulnerability.LLM7d <- glm(vulnerability.LL ~ Treatment 
                           + CaCo_index,
                           family = gaussian (link = "sqrt"),
                           data = dfLik)
summary(vulnerability.LLM7d)
AIC(vulnerability.LLM7d)
plot(vulnerability.LLM7d)

Model_R2vulnerability.LLM7d <- with(summary(vulnerability.LLM7d), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM7d


### Inverse Gaussian for vulnerability.LL, trial different link functions

### 1/mu^2
vulnerability.LLM8a <- glm(vulnerability.LL ~ Treatment 
                           + CaCo_index,
                           family = inverse.gaussian (link = "1/mu^2"),
                           data = dfLik)
summary(vulnerability.LLM8a)
AIC(vulnerability.LLM8a)
plot(vulnerability.LLM8a)

Model_R2vulnerability.LLM8a <- with(summary(vulnerability.LLM8a), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8a


## inverse

vulnerability.LLM8b <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = inverse.gaussian (link = "inverse"),
                           data = dfLik)
summary(vulnerability.LLM8b)
AIC(vulnerability.LLM8b)
plot(vulnerability.LLM8b)

Model_R2vulnerability.LLM8b <- with(summary(vulnerability.LLM8b), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8b


## identity
vulnerability.LLM8c <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = inverse.gaussian (link = "identity"),
                           data = dfLik)
summary(vulnerability.LLM8c)
AIC(vulnerability.LLM8c)
plot(vulnerability.LLM8c)

Model_R2vulnerability.LLM8c <- with(summary(vulnerability.LLM8c), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8c


## log
vulnerability.LLM8d <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = inverse.gaussian (link = "log"),
                           data = dfLik)
summary(vulnerability.LLM8d)
AIC(vulnerability.LLM8d)
plot(vulnerability.LLM8d)

Model_R2vulnerability.LLM8d <- with(summary(vulnerability.LLM8d), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8d

### Gamma for vulnerability.LL, trial different link functions
### sqrt
vulnerability.LLM9a <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = Gamma (link = "sqrt"),
                           data = dfLik)
summary(vulnerability.LLM9a)
AIC(vulnerability.LLM9a)
plot(vulnerability.LLM9a)

Model_R2vulnerability.LLM9a <- with(summary(vulnerability.LLM9a), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM9a


## inverse
vulnerability.LLM9b <- glm(vulnerability.LL ~ Treatment 
                           + CaCo_index,
                           family = Gamma (link = "inverse"),
                           data = dfLik)
summary(vulnerability.LLM9b)
AIC(vulnerability.LLM9b)
plot(vulnerability.LLM9b)

Model_R2vulnerability.LLM9b <- with(summary(vulnerability.LLM9b), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM9b


## identity
vulnerability.LLM9c <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = Gamma (link = "identity"),
                           data = dfLik)
summary(vulnerability.LLM9c)
AIC(vulnerability.LLM9c)
plot(vulnerability.LLM9c)

Model_R2vulnerability.LLM9c <- with(summary(vulnerability.LLM9c), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM9c


## log
vulnerability.LLM9d <- glm(vulnerability.LL ~ Treatment 
                           + CaCo_index,
                           family = Gamma (link = "log"),
                           data = dfLik)
summary(vulnerability.LLM9d)
AIC(vulnerability.LLM9d)
plot(vulnerability.LLM9d)

Model_R2vulnerability.LLM9d <- with(summary(vulnerability.LLM9d), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM9d

##############################################
### Try Vulnerbility with Log10 transformation
##############################################
LogVulnerability <- log10(dfLik$vulnerability.LL)


### gaussian for LogVulnerability, trial different link functions
### identity
LogVulnerabilityM7a <- glm(LogVulnerability  ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "identity"),
                           data = dfLik)
summary(LogVulnerabilityM7a)
AIC(LogVulnerabilityM7a)
plot(LogVulnerabilityM7a)

Model_R2LogVulnerabilityM7a <- with(summary(LogVulnerabilityM7a), 1 - deviance/null.deviance)
Model_R2LogVulnerabilityM7a


## log - error message
LogVulnerabilityM7b <- glm(LogVulnerability ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "log"),
                           data = dfLik)
summary(LogVulnerabilityM7b)
AIC(LogVulnerabilityM7b)
plot(LogVulnerabilityM7b)

Model_R2LogVulnerabilityM7b <- with(summary(LogVulnerabilityM7b), 1 - deviance/null.deviance)
Model_R2LogVulnerabilityM7b


## inverse - error message
LogVulnerabilityM7c <- glm(LogVulnerability ~ Treatment
                           + CaCo_index,
                           family = gaussian (link = "inverse"),
                           data = dfLik)
summary(LogVulnerabilityM7c)
AIC(LogVulnerabilityM7c)
plot(LogVulnerabilityM7c)

Model_R2LogVulnerabilityM7c <- with(summary(LogVulnerabilityM7c), 1 - deviance/null.deviance)
Model_R2LogVulnerabilityM7c


## sqrt - error message
LogVulnerabilityM7d <- glm(LogVulnerability ~ Treatment 
                           + CaCo_index,
                           family = gaussian (link = "sqrt"),
                           data = dfLik)
summary(LogVulnerabilityM7d)
AIC(LogVulnerabilityM7d)
plot(LogVulnerabilityM7d)

Model_R2LogVulnerabilityM7d <- with(summary(LogVulnerabilityM7d), 1 - deviance/null.deviance)
Model_R2LogVulnerabilityM7d


##############################################
### Try Vulnerbility with Inverse transformation
##############################################

InverseVulnerability <- 1/dfLik$vulnerability.LL

### gaussian for InverseVulnerability, trial different link functions
### identity
InverseVulnerabilityM7a <- glm(InverseVulnerability  ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "identity"),
                               data = dfLik)
summary(InverseVulnerabilityM7a)
AIC(InverseVulnerabilityM7a)
plot(InverseVulnerabilityM7a)

Model_R2InverseVulnerabilityM7a <- with(summary(InverseVulnerabilityM7a), 1 - deviance/null.deviance)
Model_R2InverseVulnerabilityM7a


## log
InverseVulnerabilityM7b <- glm(InverseVulnerability ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "log"),
                               data = dfLik)
summary(InverseVulnerabilityM7b)
AIC(InverseVulnerabilityM7b)
plot(InverseVulnerabilityM7b)

Model_R2InverseVulnerabilityM7b <- with(summary(InverseVulnerabilityM7b), 1 - deviance/null.deviance)
Model_R2InverseVulnerabilityM7b


## inverse
InverseVulnerabilityM7c <- glm(InverseVulnerability ~ Treatment
                               + CaCo_index,
                               family = gaussian (link = "inverse"),
                               data = dfLik)
summary(InverseVulnerabilityM7c)
AIC(InverseVulnerabilityM7c)
plot(InverseVulnerabilityM7c)

Model_R2InverseVulnerabilityM7c <- with(summary(InverseVulnerabilityM7c), 1 - deviance/null.deviance)
Model_R2InverseVulnerabilityM7c


## sqrt
InverseVulnerabilityM7d <- glm(InverseVulnerability ~ Treatment 
                               + CaCo_index,
                               family = gaussian (link = "sqrt"),
                               data = dfLik)
summary(InverseVulnerabilityM7d)
AIC(InverseVulnerabilityM7d)
plot(InverseVulnerabilityM7d)

Model_R2InverseVulnerabilityM7d <- with(summary(InverseVulnerabilityM7d), 1 - deviance/null.deviance)
Model_R2InverseVulnerabilityM7d


#############################################################################
################   H2   ############################################
#####################################################################



hist(dfLik$H2)


#### BETA GLM  for 0-1 proportion data


H22 <- dfLik$H2 - 0.00001 # put H2 between 0-1 for beta glm


## logit link, trial link.phi
H22M1a <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "logit",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M1a)
AIC(H22M1a)
plot(H22M1a)

H22M1b <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "logit",
                   link.phi = "log", 
                   data = dfLik)
summary(H22M1b)
AIC(H22M1b)
plot(H22M1b)

H22M1c <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "logit",
                   link.phi = "sqrt", 
                   data = dfLik)
summary(H22M1c)
AIC(H22M1c)
plot(H22M1c)


## probit link, trial link.phi

H22M2a <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "probit",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M2a)
AIC(H22M2a)
plot(H22M2a)

H22M2b <- betareg (H22 ~ Treatment
                   + CaCo_index 
                   ,
                   link = "probit",
                   link.phi = "log", 
                   data = dfLik)
summary(H22M2b)
AIC(H22M2b)
plot(H22M2b)

H22M2c <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "probit",
                   link.phi = "sqrt", 
                   data = dfLik)
summary(H22M2c)
AIC(H22M2c)
plot(H22M2c)

## cloglog link, trial link.phi

H22M3a <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "cloglog",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M3a)
AIC(H22M3a)
plot(H22M3a)

H22M3b <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "cloglog",
                   link.phi = "log", 
                   data = dfLik)
summary(H22M3b)
AIC(H22M3b)
plot(H22M3b)

H22M3c <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "cloglog",
                   link.phi = "sqrt", 
                   data = dfLik)
summary(H22M3c)
AIC(H22M3c)
plot(H22M3c)


## cauchit link, trial link.phi
### doesn't work
H22M4a <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "cauchit",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M4a)
AIC(H22M4a)
plot(H22M4a)

## doesn't work
H22M4b <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "cauchit",
                   link.phi = "log", 
                   data = dfLik)
summary(H22M4b)
AIC(H22M4b)
plot(H22M4b)

## doesn't work
H22M4c <- betareg (H22 ~ Treatment 
                   + CaCo_index,
                   link = "cauchit",
                   link.phi = "sqrt", 
                   data = dfLik)
summary(H22M4c)
AIC(H22M4c)
plot(H22M4c)



## log link, trial link.phi
# fails
H22M5a <- betareg (H22 ~ Treatment 
                   + CaCo_index,
                   link = "log",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M5a)
AIC(H22M5a)
plot(H22M5a)


H22M5b <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "log",
                   link.phi = "log", 
                   data = dfLik)
summary(H22M5b)
AIC(H22M5b)
plot(H22M5b)


H22M5c <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "log",
                   link.phi = "sqrt", 
                   data = dfLik)
summary(H22M5c)
AIC(H22M5c)
plot(H22M5c)


## loglog link, trial link.phi

H22M6a <- betareg (H22 ~ Treatment 
                   + CaCo_index,
                   link = "loglog",
                   link.phi = "identity", 
                   data = dfLik)
summary(H22M6a)
AIC(H22M6a)
plot(H22M6a)

H22M6b <- betareg (H22 ~ Treatment
                   + CaCo_index,
                   link = "loglog",
                   link.phi = "log", 
                   data = dfLik)
summary(H22M6b)
AIC(H22M6b)
plot(H22M6b)

H22M6c <- betareg (H22 ~ Treatment
                   + CaCo_index 
                   ,
                   link = "loglog",
                   link.phi = "sqrt", 
                   data = dfLik)
summary(H22M6c)
AIC(H22M6c)
plot(H22M6c)

### gaussian for H22, trial different link functions
### identity

H2M7a <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = gaussian (link = "identity"),
             data = dfLik)
summary(H2M7a)
AIC(H2M7a)
Model_R2H2M7a <- with(summary(H2M7a), 1 - deviance/null.deviance)
Model_R2H2M7a


plot(H2M7a)



## log

H2M7b <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = gaussian (link = "log"),
             data = dfLik)
summary(H2M7b)
AIC(H2M7b)

Model_R2H2M7b <- with(summary(H2M7b), 1 - deviance/null.deviance)
Model_R2H2M7b

plot(H2M7b)



## inverse

H2M7c <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = gaussian (link = "inverse"),
             data = dfLik)
summary(H2M7c)
AIC(H2M7c)
Model_R2H2M7c <- with(summary(H2M7c), 1 - deviance/null.deviance)
Model_R2H2M7c

plot(H2M7c)


## sqrt
H2M7d <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = gaussian (link = "sqrt"),
             data = dfLik)
summary(H2M7d)
AIC(H2M7d)
Model_R2H2M7d <- with(summary(H2M7d), 1 - deviance/null.deviance)
Model_R2H2M7d

plot(H2M7d)



### Inverse Gaussian for H2, trial different link functions


### 1/mu^2 - error message
H2M8a <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLik)
summary(H2M8a)
AIC(H2M8a)

Model_R2H2M8a <- with(summary(H2M8a), 1 - deviance/null.deviance)
Model_R2H2M8a

plot(H2M8a)

## inverse
H2M8b <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = inverse.gaussian (link = "inverse"),
             data = dfLik)
summary(H2M8b)
AIC(H2M8b)

Model_R2H2M8b <- with(summary(H2M8b), 1 - deviance/null.deviance)
Model_R2H2M8b

plot(H2M8b)

## identity
H2M8c <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = inverse.gaussian (link = "identity"),
             data = dfLik)
summary(H2M8c)
AIC(H2M8c)

Model_R2H2M8c <- with(summary(H2M8c), 1 - deviance/null.deviance)
Model_R2H2M8c

plot(H2M8c)

## log
H2M8d <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = inverse.gaussian (link = "log"),
             data = dfLik)
summary(H2M8d)
AIC(H2M8d)

Model_R2H2M8d <- with(summary(H2M8d), 1 - deviance/null.deviance)
Model_R2H2M8d

plot(H2M8d)


### Gamma for H2, trial different link functions
### sqrt
H2M9a <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = Gamma (link = "sqrt"),
             data = dfLik)
summary(H2M9a)
AIC(H2M9a)

Model_R2H2M9a <- with(summary(H2M9a), 1 - deviance/null.deviance)
Model_R2H2M9a


plot(H2M9a)

## inverse
H2M9b <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = Gamma (link = "inverse"),
             data = dfLik)
summary(H2M9b)
AIC(H2M9b)


Model_R2H2M9b <- with(summary(H2M9b), 1 - deviance/null.deviance)
Model_R2H2M9b

plot(H2M9b)

## identity
H2M9c <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = Gamma (link = "identity"),
             data = dfLik)
summary(H2M9c)
AIC(H2M9c)

Model_R2H2M9c <- with(summary(H2M9c), 1 - deviance/null.deviance)
Model_R2H2M9c


plot(H2M9c)

## log
H2M9d <- glm(H2 ~ Treatment
             + CaCo_index 
             ,
             family = Gamma (link = "log"),
             data = dfLik)
summary(H2M9d)
AIC(H2M9d)

Model_R2H2M9d <- with(summary(H2M9d), 1 - deviance/null.deviance)
Model_R2H2M9d


plot(H2M9d)

###############################
### try Gaussian with H2 logged 
###############################

LogH2 <- log10(dfLik$H2)

### gaussian for LogH2, trial different link functions
### identity

LogH2M7a <- glm(LogH2 ~ Treatment
                + CaCo_index 
                ,
                family = gaussian (link = "identity"),
                data = dfLik)
summary(LogH2M7a)
AIC(LogH2M7a)
Model_R2LogH2M7a <- with(summary(LogH2M7a), 1 - deviance/null.deviance)
Model_R2LogH2M7a


plot(LogH2M7a)



## log
# failed
LogH2M7b <- glm(LogH2 ~ Treatment
                + CaCo_index 
                ,
                family = gaussian (link = "log"),
                data = dfLik)
summary(LogH2M7b)
AIC(LogH2M7b)

Model_R2LogH2M7b <- with(summary(LogH2M7b), 1 - deviance/null.deviance)
Model_R2LogH2M7b

plot(LogH2M7b)



## inverse
# failed
LogH2M7c <- glm(LogH2 ~ Treatment
                + CaCo_index 
                ,
                family = gaussian (link = "inverse"),
                data = dfLik)
summary(LogH2M7c)
AIC(LogH2M7c)
Model_R2LogH2M7c <- with(summary(LogH2M7c), 1 - deviance/null.deviance)
Model_R2LogH2M7c

plot(LogH2M7c)


## sqrt
# failed
LogH2M7d <- glm(LogH2 ~ Treatment
                + CaCo_index 
                ,
                family = gaussian (link = "sqrt"),
                data = dfLik)
summary(LogH2M7d)
AIC(LogH2M7d)
Model_R2LogH2M7d <- with(summary(LogH2M7d), 1 - deviance/null.deviance)
Model_R2LogH2M7d

plot(LogH2M7d)


#########################################################
### try Gaussian with H2 Log10 transformation, negative skew
#########################################################

log10_Neg_H2 <-  log10(max(dfLik$H2+1) - dfLik$H2)

### gaussian for log10_Neg_H2, trial different link functions
### identity
log10_Neg_H2M7a <- glm(log10_Neg_H2 ~ Treatment
                       + CaCo_index 
                       ,
                       family = gaussian (link = "identity"),
                       data = dfLik)
summary(log10_Neg_H2M7a)
AIC(log10_Neg_H2M7a)
Model_R2log10_Neg_H2M7a <- with(summary(log10_Neg_H2M7a), 1 - deviance/null.deviance)
Model_R2log10_Neg_H2M7a


plot(log10_Neg_H2M7a)



## log
# failed
log10_Neg_H2M7b <- glm(log10_Neg_H2 ~ Treatment
                       + CaCo_index 
                       ,
                       family = gaussian (link = "log"),
                       data = dfLik)
summary(log10_Neg_H2M7b)
AIC(log10_Neg_H2M7b)

Model_R2log10_Neg_H2M7b <- with(summary(log10_Neg_H2M7b), 1 - deviance/null.deviance)
Model_R2log10_Neg_H2M7b

plot(log10_Neg_H2M7b)



## inverse
# failed
log10_Neg_H2M7c <- glm(log10_Neg_H2 ~ Treatment
                       + CaCo_index 
                       ,
                       family = gaussian (link = "inverse"),
                       data = dfLik)
summary(log10_Neg_H2M7c)
AIC(log10_Neg_H2M7c)
Model_R2log10_Neg_H2M7c <- with(summary(log10_Neg_H2M7c), 1 - deviance/null.deviance)
Model_R2log10_Neg_H2M7c

plot(log10_Neg_H2M7c)


## log10
# failed
log10_Neg_H2M7d <- glm(log10_Neg_H2 ~ Treatment
                       + CaCo_index 
                       ,
                       family = gaussian (link = "log10"),
                       data = dfLik)
summary(log10_Neg_H2M7d)
AIC(log10_Neg_H2M7d)
Model_R2log10_Neg_H2M7d <- with(summary(log10_Neg_H2M7d), 1 - deviance/null.deviance)
Model_R2log10_Neg_H2M7d

plot(log10_Neg_H2M7d)


###############################
### try Gaussian with H2 Inverse transformed 
###############################

InverseH2 <- 1/dfLik$H2

### gaussian for InverseH2, trial different link functions
### identity
InverseH2M7a <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = gaussian (link = "identity"),
                    data = dfLik)
summary(InverseH2M7a)
AIC(InverseH2M7a)
Model_R2InverseH2M7a <- with(summary(InverseH2M7a), 1 - deviance/null.deviance)
Model_R2InverseH2M7a


plot(InverseH2M7a)



## log
InverseH2M7b <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = gaussian (link = "log"),
                    data = dfLik)
summary(InverseH2M7b)
AIC(InverseH2M7b)

Model_R2InverseH2M7b <- with(summary(InverseH2M7b), 1 - deviance/null.deviance)
Model_R2InverseH2M7b

plot(InverseH2M7b)



## inverse
InverseH2M7c <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = gaussian (link = "inverse"),
                    data = dfLik)
summary(InverseH2M7c)
AIC(InverseH2M7c)
Model_R2InverseH2M7c <- with(summary(InverseH2M7c), 1 - deviance/null.deviance)
Model_R2InverseH2M7c

plot(InverseH2M7c)


## sqrt - error message
InverseH2M7d <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = gaussian (link = "sqrt"),
                    data = dfLik)
summary(InverseH2M7d)
AIC(InverseH2M7d)
Model_R2InverseH2M7d <- with(summary(InverseH2M7d), 1 - deviance/null.deviance)
Model_R2InverseH2M7d

plot(InverseH2M7d)


### Inverse Gaussian for InverseH2, trial different link functions


### 1/mu^2

InverseH2M8a <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = inverse.gaussian (link = "1/mu^2"),
                    data = dfLik)
summary(InverseH2M8a)
AIC(InverseH2M8a)
plot(InverseH2M8a)

Model_R2InverseH2M8a <- with(summary(InverseH2M8a), 1 - deviance/null.deviance)
Model_R2InverseH2M8a


## inverse

InverseH2M8b <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = inverse.gaussian (link = "inverse"),
                    data = dfLik)
summary(InverseH2M8b)
AIC(InverseH2M8b)
plot(InverseH2M8b)

Model_R2InverseH2M8b <- with(summary(InverseH2M8b), 1 - deviance/null.deviance)
Model_R2InverseH2M8b


## identity
# fails
InverseH2M8c <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = inverse.gaussian (link = "identity"),
                    data = dfLik)
summary(InverseH2M8c)
AIC(InverseH2M8c)


Model_R2InverseH2M8c <- with(summary(InverseH2M8c), 1 - deviance/null.deviance)
Model_R2InverseH2M8c

plot(InverseH2M8c)

## log
# fails
InverseH2M8d <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = inverse.gaussian (link = "log"),
                    data = dfLik)
summary(InverseH2M8d)
AIC(InverseH2M8d)


Model_R2InverseH2M8d <- with(summary(InverseH2M8d), 1 - deviance/null.deviance)
Model_R2InverseH2M8d

plot(InverseH2M8d)

### Gamma for InverseH2, trial different link functions
### sqrt
InverseH2M9a <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = Gamma (link = "sqrt"),
                    data = dfLik)
summary(InverseH2M9a)
AIC(InverseH2M9a)

Model_R2InverseH2M9a <- with(summary(InverseH2M9a), 1 - deviance/null.deviance)
Model_R2InverseH2M9a


plot(InverseH2M9a)

## inverse
InverseH2M9b <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = Gamma (link = "inverse"),
                    data = dfLik)
summary(InverseH2M9b)
AIC(InverseH2M9b)

Model_R2InverseH2M9b <- with(summary(InverseH2M9b), 1 - deviance/null.deviance)
Model_R2InverseH2M9b


plot(InverseH2M9b)

## identity
InverseH2M9c <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = Gamma (link = "identity"),
                    data = dfLik)
summary(InverseH2M9c)
AIC(InverseH2M9c)

Model_R2InverseH2M9c <- with(summary(InverseH2M9c), 1 - deviance/null.deviance)
Model_R2InverseH2M9c

plot(InverseH2M9c)


## log
InverseH2M9d <- glm(InverseH2 ~ Treatment
                    + CaCo_index 
                    ,
                    family = Gamma (link = "log"),
                    data = dfLik)
summary(InverseH2M9d)
AIC(InverseH2M9d)

Model_R2InverseH2M9d <- with(summary(InverseH2M9d), 1 - deviance/null.deviance)
Model_R2InverseH2M9d


plot(InverseH2M9d)

#########################################################
### try with H2 Inverse transformation, negative skew
#########################################################

Inverse_Neg_H2 <-  1/(max(dfLik$H2+1) - dfLik$H2)

### gaussian for Inverse_Neg_H2, trial different link functions
### identity
## better
Inverse_Neg_H2M7a <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = gaussian (link = "identity"),
                         data = dfLik)
summary(Inverse_Neg_H2M7a)
AIC(Inverse_Neg_H2M7a)
Model_R2Inverse_Neg_H2M7a <- with(summary(Inverse_Neg_H2M7a), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M7a


plot(Inverse_Neg_H2M7a)



## log
Inverse_Neg_H2M7b <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = gaussian (link = "log"),
                         data = dfLik)
summary(Inverse_Neg_H2M7b)
AIC(Inverse_Neg_H2M7b)

Model_R2Inverse_Neg_H2M7b <- with(summary(Inverse_Neg_H2M7b), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M7b

plot(Inverse_Neg_H2M7b)



## inverse

Inverse_Neg_H2M7c <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = gaussian (link = "inverse"),
                         data = dfLik)
summary(Inverse_Neg_H2M7c)
AIC(Inverse_Neg_H2M7c)
Model_R2Inverse_Neg_H2M7c <- with(summary(Inverse_Neg_H2M7c), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M7c

plot(Inverse_Neg_H2M7c)


### Inverse Gaussian for Inverse_Neg_H2, trial different link functions


### 1/mu^2
Inverse_Neg_H2M8a <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = inverse.gaussian (link = "1/mu^2"),
                         data = dfLik)
summary(Inverse_Neg_H2M8a)
AIC(Inverse_Neg_H2M8a)

Model_R2Inverse_Neg_H2M8a <- with(summary(Inverse_Neg_H2M8a), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M8a


plot(Inverse_Neg_H2M8a)

## inverse
Inverse_Neg_H2M8b <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = inverse.gaussian (link = "inverse"),
                         data = dfLik)
summary(Inverse_Neg_H2M8b)
AIC(Inverse_Neg_H2M8b)

Model_R2Inverse_Neg_H2M8b <- with(summary(Inverse_Neg_H2M8b), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M8b


plot(Inverse_Neg_H2M8b)


## identity
Inverse_Neg_H2M8c <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = inverse.gaussian (link = "identity"),
                         data = dfLik)
summary(Inverse_Neg_H2M8c)
AIC(Inverse_Neg_H2M8c)

Model_R2Inverse_Neg_H2M8c <- with(summary(Inverse_Neg_H2M8c), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M8c


plot(Inverse_Neg_H2M8c)

## log
Inverse_Neg_H2M8d <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = inverse.gaussian (link = "log"),
                         data = dfLik)
summary(Inverse_Neg_H2M8d)
AIC(Inverse_Neg_H2M8d)

Model_R2Inverse_Neg_H2M8d <- with(summary(Inverse_Neg_H2M8d), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M8d


plot(Inverse_Neg_H2M8d)

### Gamma for Inverse_Neg_H2, trial different link functions
### sqrt
Inverse_Neg_H2M9a <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = Gamma (link = "sqrt"),
                         data = dfLik)
summary(Inverse_Neg_H2M9a)
AIC(Inverse_Neg_H2M9a)

Model_R2Inverse_Neg_H2M9a <- with(summary(Inverse_Neg_H2M9a), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M9a


plot(Inverse_Neg_H2M9a)



## inverse
Inverse_Neg_H2M9b <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = Gamma (link = "inverse"),
                         data = dfLik)
summary(Inverse_Neg_H2M9b)
AIC(Inverse_Neg_H2M9b)

Model_R2Inverse_Neg_H2M9b <- with(summary(Inverse_Neg_H2M9b), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M9b


plot(Inverse_Neg_H2M9b)


## identity
Inverse_Neg_H2M9c <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = Gamma (link = "identity"),
                         data = dfLik)
summary(Inverse_Neg_H2M9c)
AIC(Inverse_Neg_H2M9c)

Model_R2Inverse_Neg_H2M9c <- with(summary(Inverse_Neg_H2M9c), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M9c


plot(Inverse_Neg_H2M9c)


## log
Inverse_Neg_H2M9d <- glm(Inverse_Neg_H2 ~ Treatment
                         + CaCo_index 
                         ,
                         family = Gamma (link = "log"),
                         data = dfLik)
summary(Inverse_Neg_H2M9d)
AIC(Inverse_Neg_H2M9d)

Model_R2Inverse_Neg_H2M9d <- with(summary(Inverse_Neg_H2M9d), 1 - deviance/null.deviance)
Model_R2Inverse_Neg_H2M9d


plot(Inverse_Neg_H2M9d)

###############################
### try with H2 Cubic transformed 
###############################

CubeH2 <-  dfLik$H2 ^(1/3)

### gaussian for CubeH2, trial different link functions
### identity
CubeH2M7a <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "identity"),
                 data = dfLik)
summary(CubeH2M7a)
AIC(CubeH2M7a)
Model_R2CubeH2M7a <- with(summary(CubeH2M7a), 1 - deviance/null.deviance)
Model_R2CubeH2M7a


plot(CubeH2M7a)



## log
CubeH2M7b <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "log"),
                 data = dfLik)
summary(CubeH2M7b)
AIC(CubeH2M7b)

Model_R2CubeH2M7b <- with(summary(CubeH2M7b), 1 - deviance/null.deviance)
Model_R2CubeH2M7b

plot(CubeH2M7b)



## inverse
CubeH2M7c <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "inverse"),
                 data = dfLik)
summary(CubeH2M7c)
AIC(CubeH2M7c)
Model_R2CubeH2M7c <- with(summary(CubeH2M7c), 1 - deviance/null.deviance)
Model_R2CubeH2M7c

plot(CubeH2M7c)


## sqrt
CubeH2M7d <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "sqrt"),
                 data = dfLik)
summary(CubeH2M7d)
AIC(CubeH2M7d)
Model_R2CubeH2M7d <- with(summary(CubeH2M7d), 1 - deviance/null.deviance)
Model_R2CubeH2M7d

plot(CubeH2M7d)



### Inverse Gaussian for CubeH2, trial different link functions


### 1/mu^2
CubeH2M8a <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "1/mu^2"),
                 data = dfLik)
summary(CubeH2M8a)
AIC(CubeH2M8a)

Model_R2CubeH2M8a <- with(summary(CubeH2M8a), 1 - deviance/null.deviance)
Model_R2CubeH2M8a


plot(CubeH2M8a)

## inverse
CubeH2M8b <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "inverse"),
                 data = dfLik)
summary(CubeH2M8b)
AIC(CubeH2M8b)

Model_R2CubeH2M8b <- with(summary(CubeH2M8b), 1 - deviance/null.deviance)
Model_R2CubeH2M8b


plot(CubeH2M8b)

## identity
CubeH2M8c <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "identity"),
                 data = dfLik)
summary(CubeH2M8c)
AIC(CubeH2M8c)

Model_R2CubeH2M8c <- with(summary(CubeH2M8c), 1 - deviance/null.deviance)
Model_R2CubeH2M8c


plot(CubeH2M8c)

## log
CubeH2M8d <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "log"),
                 data = dfLik)
summary(CubeH2M8d)
AIC(CubeH2M8d)

Model_R2CubeH2M8d <- with(summary(CubeH2M8d), 1 - deviance/null.deviance)
Model_R2CubeH2M8d

plot(CubeH2M8d)

### Gamma for CubeH2, trial different link functions
### sqrt
CubeH2M9a <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "sqrt"),
                 data = dfLik)
summary(CubeH2M9a)
AIC(CubeH2M9a)

Model_R2CubeH2M9a <- with(summary(CubeH2M9a), 1 - deviance/null.deviance)
Model_R2CubeH2M9a

plot(CubeH2M9a)

## inverse
CubeH2M9b <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "inverse"),
                 data = dfLik)
summary(CubeH2M9b)
AIC(CubeH2M9b)

Model_R2CubeH2M9b <- with(summary(CubeH2M9b), 1 - deviance/null.deviance)
Model_R2CubeH2M9b

plot(CubeH2M9b)

## identity
CubeH2M9c <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "identity"),
                 data = dfLik)
summary(CubeH2M9c)
AIC(CubeH2M9c)

Model_R2CubeH2M9c <- with(summary(CubeH2M9c), 1 - deviance/null.deviance)
Model_R2CubeH2M9c

plot(CubeH2M9c)

## log
CubeH2M9d <- glm(CubeH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "log"),
                 data = dfLik)
summary(CubeH2M9d)
AIC(CubeH2M9d)

Model_R2CubeH2M9d <- with(summary(CubeH2M9d), 1 - deviance/null.deviance)
Model_R2CubeH2M9d

plot(CubeH2M9d)



###############################
### try Gaussian with H2 Sqrt transformed 
###############################

SqrtH2 <-  sqrt(dfLik$H2)

### gaussian for SqrtH2, trial different link functions
### identity
SqrtH2M7a <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "identity"),
                 data = dfLik)
summary(SqrtH2M7a)
AIC(SqrtH2M7a)
Model_R2SqrtH2M7a <- with(summary(SqrtH2M7a), 1 - deviance/null.deviance)
Model_R2SqrtH2M7a


plot(SqrtH2M7a)



## log
SqrtH2M7b <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "log"),
                 data = dfLik)
summary(SqrtH2M7b)
AIC(SqrtH2M7b)

Model_R2SqrtH2M7b <- with(summary(SqrtH2M7b), 1 - deviance/null.deviance)
Model_R2SqrtH2M7b

plot(SqrtH2M7b)



## inverse
SqrtH2M7c <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "inverse"),
                 data = dfLik)
summary(SqrtH2M7c)
AIC(SqrtH2M7c)
Model_R2SqrtH2M7c <- with(summary(SqrtH2M7c), 1 - deviance/null.deviance)
Model_R2SqrtH2M7c

plot(SqrtH2M7c)


## sqrt
SqrtH2M7d <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = gaussian (link = "sqrt"),
                 data = dfLik)
summary(SqrtH2M7d)
AIC(SqrtH2M7d)
Model_R2SqrtH2M7d <- with(summary(SqrtH2M7d), 1 - deviance/null.deviance)
Model_R2SqrtH2M7d

plot(SqrtH2M7d)



### Inverse Gaussian for SqrtH2, trial different link functions


### 1/mu^2
SqrtH2M8a <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "1/mu^2"),
                 data = dfLik)
summary(SqrtH2M8a)
AIC(SqrtH2M8a)

Model_R2SqrtH2M8a <- with(summary(SqrtH2M8a), 1 - deviance/null.deviance)
Model_R2SqrtH2M8a

plot(SqrtH2M8a)

## inverse
SqrtH2M8b <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "inverse"),
                 data = dfLik)
summary(SqrtH2M8b)
AIC(SqrtH2M8b)

Model_R2SqrtH2M8b <- with(summary(SqrtH2M8b), 1 - deviance/null.deviance)
Model_R2SqrtH2M8b

plot(SqrtH2M8b)

## identity
SqrtH2M8c <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "identity"),
                 data = dfLik)
summary(SqrtH2M8c)
AIC(SqrtH2M8c)

Model_R2SqrtH2M8c <- with(summary(SqrtH2M8c), 1 - deviance/null.deviance)
Model_R2SqrtH2M8c

plot(SqrtH2M8c)

## log
SqrtH2M8d <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = inverse.gaussian (link = "log"),
                 data = dfLik)
summary(SqrtH2M8d)
AIC(SqrtH2M8d)

Model_R2SqrtH2M8d <- with(summary(SqrtH2M8d), 1 - deviance/null.deviance)
Model_R2SqrtH2M8d

plot(SqrtH2M8d)

### Gamma for SqrtH2, trial different link functions
### sqrt
SqrtH2M9a <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "sqrt"),
                 data = dfLik)
summary(SqrtH2M9a)
AIC(SqrtH2M9a)

Model_R2SqrtH2M9a <- with(summary(SqrtH2M9a), 1 - deviance/null.deviance)
Model_R2SqrtH2M9a


plot(SqrtH2M9a)


## inverse
SqrtH2M9b <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "inverse"),
                 data = dfLik)
summary(SqrtH2M9b)
AIC(SqrtH2M9b)

Model_R2SqrtH2M9b <- with(summary(SqrtH2M9b), 1 - deviance/null.deviance)
Model_R2SqrtH2M9b


plot(SqrtH2M9b)


## identity
SqrtH2M9c <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "identity"),
                 data = dfLik)
summary(SqrtH2M9c)
AIC(SqrtH2M9c)

Model_R2SqrtH2M9c <- with(summary(SqrtH2M9c), 1 - deviance/null.deviance)
Model_R2SqrtH2M9c


plot(SqrtH2M9c)



## log
SqrtH2M9d <- glm(SqrtH2 ~ Treatment
                 + CaCo_index 
                 ,
                 family = Gamma (link = "log"),
                 data = dfLik)
summary(SqrtH2M9d)
AIC(SqrtH2M9d)

Model_R2SqrtH2M9d <- with(summary(SqrtH2M9d), 1 - deviance/null.deviance)
Model_R2SqrtH2M9d


plot(SqrtH2M9d)


#########################################################
### try Gaussian with H2 Sqrt transformed, negative skew
#########################################################

Sqrt_Neg_H2 <-  sqrt(max(dfLik$H2+1) - dfLik$H2)

### gaussian for Sqrt_Neg_H2, trial different link functions
### identity
Sqrt_Neg_H2M7a <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = gaussian (link = "identity"),
                      data = dfLik)
summary(Sqrt_Neg_H2M7a)
AIC(Sqrt_Neg_H2M7a)
Model_R2Sqrt_Neg_H2M7a <- with(summary(Sqrt_Neg_H2M7a), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M7a


plot(Sqrt_Neg_H2M7a)



## log
Sqrt_Neg_H2M7b <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = gaussian (link = "log"),
                      data = dfLik)
summary(Sqrt_Neg_H2M7b)
AIC(Sqrt_Neg_H2M7b)

Model_R2Sqrt_Neg_H2M7b <- with(summary(Sqrt_Neg_H2M7b), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M7b

plot(Sqrt_Neg_H2M7b)



## inverse
Sqrt_Neg_H2M7c <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = gaussian (link = "inverse"),
                      data = dfLik)
summary(Sqrt_Neg_H2M7c)
AIC(Sqrt_Neg_H2M7c)
Model_R2Sqrt_Neg_H2M7c <- with(summary(Sqrt_Neg_H2M7c), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M7c

plot(Sqrt_Neg_H2M7c)


## sqrt
Sqrt_Neg_H2M7d <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = gaussian (link = "sqrt"),
                      data = dfLik)
summary(Sqrt_Neg_H2M7d)
AIC(Sqrt_Neg_H2M7d)
Model_R2Sqrt_Neg_H2M7d <- with(summary(Sqrt_Neg_H2M7d), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M7d

plot(Sqrt_Neg_H2M7d)



### Inverse Gaussian for Sqrt_Neg_H2, trial different link functions


### 1/mu^2
Sqrt_Neg_H2M8a <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = inverse.gaussian (link = "1/mu^2"),
                      data = dfLik)
summary(Sqrt_Neg_H2M8a)
AIC(Sqrt_Neg_H2M8a)
plot(Sqrt_Neg_H2M8a)

Model_R2Sqrt_Neg_H2M8a <- with(summary(Sqrt_Neg_H2M8a), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M8a


## inverse

Sqrt_Neg_H2M8b <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = inverse.gaussian (link = "inverse"),
                      data = dfLik)
summary(Sqrt_Neg_H2M8b)
AIC(Sqrt_Neg_H2M8b)
Model_R2Sqrt_Neg_H2M8b <- with(summary(Sqrt_Neg_H2M8b), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M8b

plot(Sqrt_Neg_H2M8b)


## identity
Sqrt_Neg_H2M8c <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = inverse.gaussian (link = "identity"),
                      data = dfLik)
summary(Sqrt_Neg_H2M8c)
AIC(Sqrt_Neg_H2M8c)
plot(Sqrt_Neg_H2M8c)

Model_R2Sqrt_Neg_H2M8c <- with(summary(Sqrt_Neg_H2M8c), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M8c


## log
Sqrt_Neg_H2M8d <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = inverse.gaussian (link = "log"),
                      data = dfLik)
summary(Sqrt_Neg_H2M8d)
AIC(Sqrt_Neg_H2M8d)
plot(Sqrt_Neg_H2M8d)

Model_R2Sqrt_Neg_H2M8d <- with(summary(Sqrt_Neg_H2M8d), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M8d

### Gamma for Sqrt_Neg_H2, trial different link functions
### sqrt
Sqrt_Neg_H2M9a <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = Gamma (link = "sqrt"),
                      data = dfLik)
summary(Sqrt_Neg_H2M9a)
AIC(Sqrt_Neg_H2M9a)
Model_R2Sqrt_Neg_H2M9a <- with(summary(Sqrt_Neg_H2M9a), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M9a

plot(Sqrt_Neg_H2M9a)


## inverse
Sqrt_Neg_H2M9b <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = Gamma (link = "inverse"),
                      data = dfLik)
summary(Sqrt_Neg_H2M9b)
AIC(Sqrt_Neg_H2M9b)
Model_R2Sqrt_Neg_H2M9b <- with(summary(Sqrt_Neg_H2M9b), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M9b

plot(Sqrt_Neg_H2M9b)


## identity
Sqrt_Neg_H2M9c <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = Gamma (link = "identity"),
                      data = dfLik)
summary(Sqrt_Neg_H2M9c)
AIC(Sqrt_Neg_H2M9c)
Model_R2Sqrt_Neg_H2M9c <- with(summary(Sqrt_Neg_H2M9c), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M9c

plot(Sqrt_Neg_H2M9c)


## log
Sqrt_Neg_H2M9d <- glm(Sqrt_Neg_H2 ~ Treatment
                      + CaCo_index 
                      ,
                      family = Gamma (link = "log"),
                      data = dfLik)
summary(Sqrt_Neg_H2M9d)
AIC(Sqrt_Neg_H2M9d)

Model_R2Sqrt_Neg_H2M9d <- with(summary(Sqrt_Neg_H2M9d), 1 - deviance/null.deviance)
Model_R2Sqrt_Neg_H2M9d

plot(Sqrt_Neg_H2M9d)


############################
### modularity_index #######
############################


### gaussian for modularity_index, trial different link functions
### identity
modularity_indexM7a <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = gaussian (link = "identity"),
                           data = dfLik)
summary(modularity_indexM7a)
AIC(modularity_indexM7a)

Model_R2modularity_indexM7a <- with(summary(modularity_indexM7a), 1 - deviance/null.deviance)
Model_R2modularity_indexM7a

plot(modularity_indexM7a)

## log
modularity_indexM7b <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = gaussian (link = "log"),
                           data = dfLik)
summary(modularity_indexM7b)
AIC(modularity_indexM7b)

Model_R2modularity_indexM7b <- with(summary(modularity_indexM7b), 1 - deviance/null.deviance)
Model_R2modularity_indexM7b

plot(modularity_indexM7b)

## inverse

modularity_indexM7c <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = gaussian (link = "inverse"),
                           data = dfLik)
summary(modularity_indexM7c)
AIC(modularity_indexM7c)

Model_R2modularity_indexM7c <- with(summary(modularity_indexM7c), 1 - deviance/null.deviance)
Model_R2modularity_indexM7c

plot(modularity_indexM7c)

## sqrt
modularity_indexM7d <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = gaussian (link = "sqrt"),
                           data = dfLik)
summary(modularity_indexM7d)
AIC(modularity_indexM7d)

Model_R2modularity_indexM7d <- with(summary(modularity_indexM7d), 1 - deviance/null.deviance)
Model_R2modularity_indexM7d

plot(modularity_indexM7d)

### Inverse Gaussian for modularity_index, trial different link functions
### 1/mu^2 - fails

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

## inverse
modularity_indexM8b <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = inverse.gaussian (link = "inverse"),
                           data = dfLik)
summary(modularity_indexM8b)
AIC(modularity_indexM8b)

Model_R2modularity_indexM8b <- with(summary(modularity_indexM8b), 1 - deviance/null.deviance)
Model_R2modularity_indexM8b

plot(modularity_indexM8b)

## identity
# terrible plots
modularity_indexM8c <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = inverse.gaussian (link = "identity"),
                           data = dfLik)
summary(modularity_indexM8c)
AIC(modularity_indexM8c)

Model_R2modularity_indexM8c <- with(summary(modularity_indexM8c), 1 - deviance/null.deviance)
Model_R2modularity_indexM8c

plot(modularity_indexM8c)

## log
modularity_indexM8d <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = inverse.gaussian (link = "log"),
                           data = dfLik)
summary(modularity_indexM8d)
AIC(modularity_indexM8d)

Model_R2modularity_indexM8d <- with(summary(modularity_indexM8d), 1 - deviance/null.deviance)
Model_R2modularity_indexM8d

plot(modularity_indexM8d)

### Gamma for modularity_index, trial different link functions
### sqrt
modularity_indexM9a <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = Gamma (link = "sqrt"),
                           data = dfLik)
summary(modularity_indexM9a)
AIC(modularity_indexM9a)

Model_R2modularity_indexM9a <- with(summary(modularity_indexM9a), 1 - deviance/null.deviance)
Model_R2modularity_indexM9a

plot(modularity_indexM9a)


## inverse
modularity_indexM9b <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = Gamma (link = "inverse"),
                           data = dfLik)
summary(modularity_indexM9b)
AIC(modularity_indexM9b)

Model_R2modularity_indexM9b <- with(summary(modularity_indexM9b), 1 - deviance/null.deviance)
Model_R2modularity_indexM9b

plot(modularity_indexM9b)

## identity
modularity_indexM9c <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = Gamma (link = "identity"),
                           data = dfLik)
summary(modularity_indexM9c)
AIC(modularity_indexM9c)

Model_R2modularity_indexM9c <- with(summary(modularity_indexM9c), 1 - deviance/null.deviance)
Model_R2modularity_indexM9c

plot(modularity_indexM9c)

## log
modularity_indexM9d <- glm(modularity_index ~ Treatment
                           + CaCo_index 
                           ,
                           family = Gamma (link = "log"),
                           data = dfLik)
summary(modularity_indexM9d)
AIC(modularity_indexM9d)

Model_R2modularity_indexM9d <- with(summary(modularity_indexM9d), 1 - deviance/null.deviance)
Model_R2modularity_indexM9d

plot(modularity_indexM9d)

############################
### Log10_modularity_index #######
############################

Log10_modularity_index <- log10(dfLik$modularity_index)

### gaussian for Log10_modularity_index, trial different link functions
### identity
Log10_modularity_indexM7a <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = gaussian (link = "identity"),
                                 data = dfLik)
summary(Log10_modularity_indexM7a)
AIC(Log10_modularity_indexM7a)

Model_R2Log10_modularity_indexM7a <- with(summary(Log10_modularity_indexM7a), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM7a

plot(Log10_modularity_indexM7a)

## log
Log10_modularity_indexM7b <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = gaussian (link = "log"),
                                 data = dfLik)
summary(Log10_modularity_indexM7b)
AIC(Log10_modularity_indexM7b)

Model_R2Log10_modularity_indexM7b <- with(summary(Log10_modularity_indexM7b), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM7b

plot(Log10_modularity_indexM7b)


## inverse

Log10_modularity_indexM7c <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = gaussian (link = "inverse"),
                                 data = dfLik)
summary(Log10_modularity_indexM7c)
AIC(Log10_modularity_indexM7c)

Model_R2Log10_modularity_indexM7c <- with(summary(Log10_modularity_indexM7c), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM7c

plot(Log10_modularity_indexM7c)

## sqrt
Log10_modularity_indexM7d <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = gaussian (link = "sqrt"),
                                 data = dfLik)
summary(Log10_modularity_indexM7d)
AIC(Log10_modularity_indexM7d)

Model_R2Log10_modularity_indexM7d <- with(summary(Log10_modularity_indexM7d), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM7d

plot(Log10_modularity_indexM7d)

### Inverse Gaussian for Log10_modularity_index, trial different link functions
### 1/mu^2
Log10_modularity_indexM8a <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = inverse.gaussian (link = "1/mu^2"),
                                 data = dfLik)
summary(Log10_modularity_indexM8a)
AIC(Log10_modularity_indexM8a)

Model_R2Log10_modularity_indexM8a <- with(summary(Log10_modularity_indexM8a), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM8a

plot(Log10_modularity_indexM8a)

## inverse

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

## identity
Log10_modularity_indexM8c <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = inverse.gaussian (link = "identity"),
                                 data = dfLik)
summary(Log10_modularity_indexM8c)
AIC(Log10_modularity_indexM8c)

Model_R2Log10_modularity_indexM8c <- with(summary(Log10_modularity_indexM8c), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM8c

plot(Log10_modularity_indexM8c)

## log
Log10_modularity_indexM8d <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = inverse.gaussian (link = "log"),
                                 data = dfLik)
summary(Log10_modularity_indexM8d)
AIC(Log10_modularity_indexM8d)

Model_R2Log10_modularity_indexM8d <- with(summary(Log10_modularity_indexM8d), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM8d

plot(Log10_modularity_indexM8d)

### Gamma for Log10_modularity_index, trial different link functions
### sqrt
Log10_modularity_indexM9a <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = Gamma (link = "sqrt"),
                                 data = dfLik)
summary(Log10_modularity_indexM9a)
AIC(Log10_modularity_indexM9a)

Model_R2Log10_modularity_indexM9a <- with(summary(Log10_modularity_indexM9a), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM9a

plot(Log10_modularity_indexM9a)

## inverse
Log10_modularity_indexM9b <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = Gamma (link = "inverse"),
                                 data = dfLik)
summary(Log10_modularity_indexM9b)
AIC(Log10_modularity_indexM9b)

Model_R2Log10_modularity_indexM9b <- with(summary(Log10_modularity_indexM9b), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM9b

plot(Log10_modularity_indexM9b)

## identity
Log10_modularity_indexM9c <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = Gamma (link = "identity"),
                                 data = dfLik)
summary(Log10_modularity_indexM9c)
AIC(Log10_modularity_indexM9c)

Model_R2Log10_modularity_indexM9c <- with(summary(Log10_modularity_indexM9c), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM9c

plot(Log10_modularity_indexM9c)

## log
Log10_modularity_indexM9d <- glm(Log10_modularity_index ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = Gamma (link = "log"),
                                 data = dfLik)
summary(Log10_modularity_indexM9d)
AIC(Log10_modularity_indexM9d)

Model_R2Log10_modularity_indexM9d <- with(summary(Log10_modularity_indexM9d), 1 - deviance/null.deviance)
Model_R2Log10_modularity_indexM9d

plot(Log10_modularity_indexM9d)



#########################################################
###### Number of species higher level ###################
#########################################################



### Poisson for number of species higher level, trial different link functions
### identity
number.of.species.HLM7a <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = poisson (link = "identity"),
                               data = dfLik)
summary(number.of.species.HLM7a)
AIC(number.of.species.HLM7a)
plot(number.of.species.HLM7a)

# R-squared
Model_R2number.of.species.HLM7a <- with(summary(number.of.species.HLM7a), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM7a

pseudo.R2_number.of.species.HLM7a <- (number.of.species.HLM7a$null.deviance - number.of.species.HLM7a$deviance) /
  number.of.species.HLM7a$null.deviance
pseudo.R2_number.of.species.HLM7a

# overdispersion
overdispersion_number.of.species.HLM7a <- number.of.species.HLM7a$deviance / number.of.species.HLM7a$df.residual
overdispersion_number.of.species.HLM7a

# deviance residuals
devresid_number.of.species.HLM7a <- resid(number.of.species.HLM7a, type = "deviance")
plot(devresid_number.of.species.HLM7a ~ dfLik$CaCo_index)

### log
number.of.species.HLM7b <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = poisson (link = "log"),
                               data = dfLik)
summary(number.of.species.HLM7b)
AIC(number.of.species.HLM7b)
plot(number.of.species.HLM7b)

# R-squared
Model_R2number.of.species.HLM7b <- with(summary(number.of.species.HLM7b), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM7b

pseudo.R2_number.of.species.HLM7b <- (number.of.species.HLM7b$null.deviance - number.of.species.HLM7b$deviance) /
  number.of.species.HLM7b$null.deviance
pseudo.R2_number.of.species.HLM7b

# overdispersion
overdispersion_number.of.species.HLM7b <- number.of.species.HLM7b$deviance / number.of.species.HLM7b$df.residual
overdispersion_number.of.species.HLM7b

# deviance residuals
devresid_number.of.species.HLM7b <- resid(number.of.species.HLM7b, type = "deviance")
plot(devresid_number.of.species.HLM7b ~ dfLik$CaCo_index)

## sqrt
number.of.species.HLM7c <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = poisson (link = "sqrt"),
                               data = dfLik)
summary(number.of.species.HLM7c)
AIC(number.of.species.HLM7c)
plot(number.of.species.HLM7c)

# R-squared
Model_R2number.of.species.HLM7c <- with(summary(number.of.species.HLM7c), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM7c

pseudo.R2_number.of.species.HLM7c <- (number.of.species.HLM7c$null.deviance - number.of.species.HLM7c$deviance) /
  number.of.species.HLM7c$null.deviance
pseudo.R2_number.of.species.HLM7c

# overdispersion
overdispersion_number.of.species.HLM7c <- number.of.species.HLM7c$deviance / number.of.species.HLM7c$df.residual
overdispersion_number.of.species.HLM7c

# deviance residuals
devresid_number.of.species.HLM7c <- resid(number.of.species.HLM7c, type = "deviance")
plot(devresid_number.of.species.HLM7c ~ dfLik$CaCo_index)




### quasipoisson for number of species higher level, trial different link functions
### identity
# 
number.of.species.HLM8a <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = quasipoisson (link = "identity"),
                               data = dfLik)
summary(number.of.species.HLM8a)
AIC(number.of.species.HLM8a)
plot(number.of.species.HLM8a)

# R-squared
Model_R2number.of.species.HLM8a <- with(summary(number.of.species.HLM8a), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM8a

pseudo.R2_number.of.species.HLM8a <- (number.of.species.HLM8a$null.deviance - number.of.species.HLM8a$deviance) /
  number.of.species.HLM8a$null.deviance
pseudo.R2_number.of.species.HLM8a

# overdispersion
overdispersion_number.of.species.HLM8a <- number.of.species.HLM8a$deviance / number.of.species.HLM8a$df.residual
overdispersion_number.of.species.HLM8a

# deviance residuals
devresid_number.of.species.HLM8a <- resid(number.of.species.HLM8a, type = "deviance")
plot(devresid_number.of.species.HLM8a ~ dfLik$CaCo_index)

### log
number.of.species.HLM8b <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = quasipoisson (link = "log"),
                               data = dfLik)
summary(number.of.species.HLM8b)
AIC(number.of.species.HLM8b)
plot(number.of.species.HLM8b)

# R-squared
Model_R2number.of.species.HLM8b <- with(summary(number.of.species.HLM8b), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM8b

pseudo.R2_number.of.species.HLM8b <- (number.of.species.HLM8b$null.deviance - number.of.species.HLM8b$deviance) /
  number.of.species.HLM8b$null.deviance
pseudo.R2_number.of.species.HLM8b

# overdispersion
overdispersion_number.of.species.HLM8b <- number.of.species.HLM8b$deviance / number.of.species.HLM8b$df.residual
overdispersion_number.of.species.HLM8b

# deviance residuals
devresid_number.of.species.HLM8b <- resid(number.of.species.HLM8b, type = "deviance")
plot(devresid_number.of.species.HLM8b ~ dfLik$CaCo_index)

### logit
# fails
number.of.species.HLM8c <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = quasipoisson (link = "logit"),
                               data = dfLik)
summary(number.of.species.HLM8c)
AIC(number.of.species.HLM8c)
plot(number.of.species.HLM8c)

# R-squared
Model_R2number.of.species.HLM8c <- with(summary(number.of.species.HLM8c), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM8c

pseudo.R2_number.of.species.HLM8c <- (number.of.species.HLM8c$null.deviance - number.of.species.HLM8c$deviance) /
  number.of.species.HLM8c$null.deviance
pseudo.R2_number.of.species.HLM8c

# overdispersion
overdispersion_number.of.species.HLM8c <- number.of.species.HLM8c$deviance / number.of.species.HLM8c$df.residual
overdispersion_number.of.species.HLM8c

# deviance residuals
devresid_number.of.species.HLM8c <- resid(number.of.species.HLM8c, type = "deviance")
plot(devresid_number.of.species.HLM8c ~ dfLik$CaCo_index)

### probit
# fails
number.of.species.HLM8d <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = quasipoisson (link = "probit"),
                               data = dfLik)
summary(number.of.species.HLM8d)
AIC(number.of.species.HLM8d)
plot(number.of.species.HLM8d)

# R-squared
Model_R2number.of.species.HLM8d <- with(summary(number.of.species.HLM8d), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM8d

pseudo.R2_number.of.species.HLM8d <- (number.of.species.HLM8d$null.deviance - number.of.species.HLM8d$deviance) /
  number.of.species.HLM8d$null.deviance
pseudo.R2_number.of.species.HLM8d

# overdispersion
overdispersion_number.of.species.HLM8d <- number.of.species.HLM8d$deviance / number.of.species.HLM8d$df.residual
overdispersion_number.of.species.HLM8d

# deviance residuals
devresid_number.of.species.HLM8d <- resid(number.of.species.HLM8d, type = "deviance")
plot(devresid_number.of.species.HLM8d ~ dfLik$CaCo_index)


### cloglog
# fails
number.of.species.HLM8e <- glm(number.of.species.HL ~ Treatment
                               + CaCo_index 
                               ,
                               family = quasipoisson (link = "cloglog"),
                               data = dfLik)
summary(number.of.species.HLM8e)
AIC(number.of.species.HLM8e)
plot(number.of.species.HLM8e)

# R-squared
Model_R2number.of.species.HLM8e <- with(summary(number.of.species.HLM8e), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM8e

pseudo.R2_number.of.species.HLM8e <- (number.of.species.HLM8e$null.deviance - number.of.species.HLM8e$deviance) /
  number.of.species.HLM8e$null.deviance
pseudo.R2_number.of.species.HLM8e

# overdispersion
overdispersion_number.of.species.HLM8e <- number.of.species.HLM8e$deviance / number.of.species.HLM8e$df.residual
overdispersion_number.of.species.HLM8e

# deviance residuals
devresid_number.of.species.HLM8e <- resid(number.of.species.HLM8e, type = "deviance")
plot(devresid_number.of.species.HLM8e ~ dfLik$CaCo_index)



### Negative binomial for number of species higher level, trial different link functions
library(MASS)

### identity
number.of.species.HLM9a <- glm.nb(number.of.species.HL ~ Treatment
                                  + CaCo_index 
                                  , link = "identity",
                                  data = dfLik)
summary(number.of.species.HLM9a, cor = FALSE)
AIC(number.of.species.HLM9a)
plot(number.of.species.HLM9a)

# R-squared
Model_R2number.of.species.HLM9a <- with(summary(number.of.species.HLM9a), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM9a

pseudo.R2_number.of.species.HLM9a <- (number.of.species.HLM9a$null.deviance - number.of.species.HLM9a$deviance) /
  number.of.species.HLM9a$null.deviance
pseudo.R2_number.of.species.HLM9a

# overdispersion
overdispersion_number.of.species.HLM9a <- number.of.species.HLM9a$deviance / number.of.species.HLM9a$df.residual
overdispersion_number.of.species.HLM9a

# deviance residuals
devresid_number.of.species.HLM9a <- resid(number.of.species.HLM9a, type = "deviance")
plot(devresid_number.of.species.HLM9a ~ dfLik$CaCo_index)

### log
number.of.species.HLM9b <- glm.nb(number.of.species.HL ~ Treatment
                                  + CaCo_index 
                                  , link = "log",
                                  data = dfLik)
summary(number.of.species.HLM9b, cor = FALSE)
AIC(number.of.species.HLM9b)
plot(number.of.species.HLM9b)

# R-squared
Model_R2number.of.species.HLM9b <- with(summary(number.of.species.HLM9b), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM9b

pseudo.R2_number.of.species.HLM9b <- (number.of.species.HLM9b$null.deviance - number.of.species.HLM9b$deviance) /
  number.of.species.HLM9b$null.deviance
pseudo.R2_number.of.species.HLM9b

# overdispersion
overdispersion_number.of.species.HLM9b <- number.of.species.HLM9b$deviance / number.of.species.HLM9b$df.residual
overdispersion_number.of.species.HLM9b

# deviance residuals
devresid_number.of.species.HLM9b <- resid(number.of.species.HLM9b, type = "deviance")
plot(devresid_number.of.species.HLM9b ~ dfLik$CaCo_index)

## sqrt
number.of.species.HLM9c <- glm.nb(number.of.species.HL ~ Treatment
                                  + CaCo_index 
                                  , link = "sqrt",
                                  data = dfLik)
summary(number.of.species.HLM9c, cor = FALSE)
AIC(number.of.species.HLM9c)
plot(number.of.species.HLM9c)

# R-squared
Model_R2number.of.species.HLM9c <- with(summary(number.of.species.HLM9c), 1 - deviance/null.deviance)
Model_R2number.of.species.HLM9c

pseudo.R2_number.of.species.HLM9c <- (number.of.species.HLM9c$null.deviance - number.of.species.HLM9c$deviance) /
  number.of.species.HLM9c$null.deviance
pseudo.R2_number.of.species.HLM9c

# overdispersion
overdispersion_number.of.species.HLM9c <- number.of.species.HLM9c$deviance / number.of.species.HLM9c$df.residual
overdispersion_number.of.species.HLM9c

# deviance residuals
devresid_number.of.species.HLM9c <- resid(number.of.species.HLM9c, type = "deviance")
plot(devresid_number.of.species.HLM9c ~ dfLik$CaCo_index)


# Housekeeping
graphics.off() 
rm(list=ls())