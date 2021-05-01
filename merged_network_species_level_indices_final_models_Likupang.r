########################################################
### Final models for species level indicies merged networks lower level Likupang


# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
df1<-read.csv(file.choose()) # Species_level_indicies.csv

## Normalised degree modified for beta glm
ND <- df1$normalised.degree - 0.00001 # put normalised degree between 0-1 for beta glm


dat1 <- cbind(df1, ND)

dfLik <- subset(dat1, Site == "Likupang")

#install.packages("betareg")
#install.packages("MASS")


library(betareg)
library(MASS)



################################################
####### Likupang ###############################
################################################

###########################
## Bruguiera gymnorrhiza ##
###########################

dfLikBru <- subset(dfLik, network_lower_level_shorthand == "P02") 


### reset reference level of Treatment variable to Reference Forest forest
dfLikBru$Treatment <- factor(dfLikBru$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration"))

####### Normalised Degree #####
## log link, identity link.phi

NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "log",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

###### Proportional generality ######
### 
## Gamma GLM, identity link function

NDM9c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "identity"),
             data = dfLikBru)
summary(NDM9c)
AIC(NDM9c)
plot(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c



###########################
## Rhizophora apiculata  ##
###########################

dfLikRhAp <- subset(dfLik, network_lower_level_shorthand == "P06") 


### reset reference level of Treatment variable to Reference Forest forest
dfLikRhAp$Treatment <- factor(dfLikRhAp$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))

####### Normalised Degree #####

## logit link, identity link.phi

NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)



###### Proportional generality ######
## Inverse Gaussian GLM, identity link function
# 

NDM8c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "identity"),
             data = dfLikRhAp)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


##################################################################
##### Rhizorphora mucronata ######################################
##################################################################

dfLikRhMu <- subset(dfLik, network_lower_level_shorthand == "P07") 


### reset reference level of Treatment variable to Reference Forest forest
dfLikRhMu$Treatment <- factor(dfLikRhMu$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))

####### Normalised Degree #####

## Beta GLM, probit link, identity link.phi

NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)


NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_R.mucronata,
             family = gaussian (link = "identity"),
             data = dfLikRhMu)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a


###### Proportional generality ######
### Gaussian GLM, identity link functions

NDM7a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.mucronata,
             family = gaussian (link = "identity"),
             data = dfLikRhMu)
summary(NDM7a)
summary.lm(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a



#######################################
###### Sediment #######################
#######################################


dfLikSed <- subset(dfLik, network_lower_level_shorthand == "S") 


### reset reference level of Treatment variable to Reference Forest forest
dfLikSed$Treatment <- factor(dfLikSed$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))

####### Normalised Degree #####

## logit link, trial link.phi
NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfLikSed)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

##### Proportional generality

### gaussian for ND, identity link function
### 

proportional.generality7a <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "identity"),
                                 data = dfLikSed)
summary(proportional.generality7a)
AIC(proportional.generality7a)

Model_R2proportional.generality7a <- with(summary(proportional.generality7a), 1 - deviance/null.deviance)
Model_R2proportional.generality7a

plot(proportional.generality7a)


# Housekeeping
graphics.off() 
rm(list=ls())
