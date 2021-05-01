###############################################################
## FINAL MODELS SELECTED FOR ANALYSES OF VEGETATION NETWORK INDICES
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


############################
### weighted_connectance ###
############################

#### BETA GLM logit link, identity link.phi
weighted_connectanceM1a <- betareg (weighted_connectance ~ Treatment
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(weighted_connectanceM1a)
AIC(weighted_connectanceM1a)
plot(weighted_connectanceM1a)


########################################################
###########  weighted_NODF  ############################
########################################################

### gaussian for weighted_NODF, identity link function
# poor fit, but couldn't be improved

weighted_NODFM7a <- glm(weighted_NODF ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfLik, na.action = na.omit)
summary(weighted_NODFM7a)
AIC(weighted_NODFM7a)
plot(weighted_NODFM7a)

Model_R2weighted_NODFM7a <- with(summary(weighted_NODFM7a), 1 - deviance/null.deviance)
Model_R2weighted_NODFM7a

########################################################
###########  generality.HL  ############################
########################################################

### gaussian GLM, identity link functions
### 
generality.HLM7a <- glm(generality.HL ~ Treatment
                        + CaCo_index,
                        family = gaussian (link = "identity"),
                        data = dfLik)
summary(generality.HLM7a)
AIC(generality.HLM7a)
plot(generality.HLM7a)

Model_R2generality.HLM7a <- with(summary(generality.HLM7a), 1 - deviance/null.deviance)
Model_R2generality.HLM7a

#########################################################
########### robustness.HL  ##############################
#########################################################

#### BETA GLM, logit link, identity link.phi
robustness.HLM1a <- betareg (robustness.HL ~ Treatment 
                             + CaCo_index,
                             link = "logit",
                             link.phi = "identity", 
                             data = dfLik)
summary(robustness.HLM1a)
AIC(robustness.HLM1a)
plot(robustness.HLM1a)


#########################################################
########### interaction_evenness  ##############################
#########################################################

#### BETA GLM  logit link, identity link.phi

interaction_evennessM1a <- betareg (interaction_evenness ~ Treatment 
                                    + CaCo_index,
                                    link = "logit",
                                    link.phi = "identity", 
                                    data = dfLik)
summary(interaction_evennessM1a)
AIC(interaction_evennessM1a)
plot(interaction_evennessM1a)


########################################################
###########  vulnerability.LL  #########################
########################################################

##### Inverse Gaussian, identity link
vulnerability.LLM8c <- glm(vulnerability.LL ~ Treatment
                           + CaCo_index,
                           family = inverse.gaussian (link = "identity"),
                           data = dfLik)
summary(vulnerability.LLM8c)
AIC(vulnerability.LLM8c)
plot(vulnerability.LLM8c)

Model_R2vulnerability.LLM8c <- with(summary(vulnerability.LLM8c), 1 - deviance/null.deviance)
Model_R2vulnerability.LLM8c


##########################################
################   H2   ##################
##########################################

## Gaussian GLM, identity link

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


############################
### modularity_index #######
############################

# poor fit but couldn't be improved
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

