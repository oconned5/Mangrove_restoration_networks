########################################################
### Model selection for species level indicies merged networks lower level Likupang


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
## logit link, trial link.phi
NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "logit",
                  link.phi = "log", 
                  data = dfLikBru)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfLikBru)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi


NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "probit",
                  link.phi = "log", 
                  data = dfLikBru)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfLikBru)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)

NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfLikBru)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfLikBru)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi

NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfLikBru)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfLikBru)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi
# 
NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "log",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "log",
                  link.phi = "log", 
                  data = dfLikBru)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfLikBru)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi
## 
NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfLikBru)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfLikBru)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_B.gymnorrhiza,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfLikBru)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity
###
NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "identity"),
             data = dfLikBru)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a


## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "log"),
             data = dfLikBru)
summary(NDM7b)
AIC(NDM7b)
plot(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b


## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "inverse"),
             data = dfLikBru)
summary(NDM7c)
AIC(NDM7c)
plot(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "sqrt"),
             data = dfLikBru)
summary(NDM7d)
AIC(NDM7d)
plot(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLikBru)
summary(NDM8a)
AIC(NDM8a)
plot(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a


## inverse
## 
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "inverse"),
             data = dfLikBru)
summary(NDM8b)
AIC(NDM8b)
plot(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b


## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "identity"),
             data = dfLikBru)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "log"),
             data = dfLikBru)
summary(NDM8d)
AIC(NDM8d)
plot(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "sqrt"),
             data = dfLikBru)
summary(NDM9a)
AIC(NDM9a)
plot(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a


## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "inverse"),
             data = dfLikBru)
summary(NDM9b)
AIC(NDM9b)
plot(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b


## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "identity"),
             data = dfLikBru)
summary(NDM9c)
AIC(NDM9c)
plot(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c


## log
NDM9d <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza
             ,
             family = Gamma (link = "log"),
             data = dfLikBru)
summary(NDM9d)
AIC(NDM9d)
plot(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d


###### Proportional generality ######

### gaussian for proportional.generality, trial different link functions
### identity
### 
NDM7a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "identity"),
             data = dfLikBru)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

## log

NDM7b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "log"),
             data = dfLikBru)
summary(NDM7b)
AIC(NDM7b)
plot(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b


## inverse
NDM7c <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "inverse"),
             data = dfLikBru)
summary(NDM7c)
AIC(NDM7c)
plot(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


## sqrt
NDM7d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = gaussian (link = "sqrt"),
             data = dfLikBru)
summary(NDM7d)
AIC(NDM7d)
plot(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLikBru)
summary(NDM8a)
AIC(NDM8a)
plot(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a


## inverse

NDM8b <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "inverse"),
             data = dfLikBru)
summary(NDM8b)
AIC(NDM8b)
plot(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b


## identity
## 
NDM8c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "identity"),
             data = dfLikBru)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


## log
#
NDM8d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = inverse.gaussian (link = "log"),
             data = dfLikBru)
summary(NDM8d)
AIC(NDM8d)
plot(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "sqrt"),
             data = dfLikBru)
summary(NDM9a)
AIC(NDM9a)
plot(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a


## inverse
NDM9b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "inverse"),
             data = dfLikBru)
summary(NDM9b)
AIC(NDM9b)
plot(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b


## identity
NDM9c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_B.gymnorrhiza,
             family = Gamma (link = "identity"),
             data = dfLikBru)
summary(NDM9c)
AIC(NDM9c)
plot(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c


## log
NDM9d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_B.gymnorrhiza
             ,
             family = Gamma (link = "log"),
             data = dfLikBru)
summary(NDM9d)
AIC(NDM9d)
plot(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d



###########################
## Rhizophora apiculata  ##
###########################

dfLikRhAp <- subset(dfLik, network_lower_level_shorthand == "P06") 


### reset reference level of Treatment variable to Reference Forest forest
dfLikRhAp$Treatment <- factor(dfLikRhAp$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))


####### Normalised Degree #####

## logit link, trial link.phi


NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "log", 
                  data = dfLikRhAp)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfLikRhAp)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi

NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "probit",
                  link.phi = "log", 
                  data = dfLikRhAp)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfLikRhAp)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)

NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfLikRhAp)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfLikRhAp)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi

NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfLikRhAp)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfLikRhAp)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi

NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "log",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "log",
                  link.phi = "log", 
                  data = dfLikRhAp)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfLikRhAp)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi
##
NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfLikRhAp)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfLikRhAp)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfLikRhAp)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity

NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "identity"),
             data = dfLikRhAp)
summary(NDM7a)
AIC(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

plot(NDM7a)

## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "log"),
             data = dfLikRhAp)
summary(NDM7b)
AIC(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b

plot(NDM7b)

## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "inverse"),
             data = dfLikRhAp)
summary(NDM7c)
AIC(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


plot(NDM7c)

## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "sqrt"),
             data = dfLikRhAp)
summary(NDM7d)
AIC(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d


plot(NDM7d)

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLikRhAp)
summary(NDM8a)
AIC(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a

plot(NDM8a)

## inverse
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "inverse"),
             data = dfLikRhAp)
summary(NDM8b)
AIC(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b

plot(NDM8b)

## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "identity"),
             data = dfLikRhAp)
summary(NDM8c)
AIC(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c

plot(NDM8c)

## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "log"),
             data = dfLikRhAp)
summary(NDM8d)
AIC(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

plot(NDM8d)

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = Gamma (link = "sqrt"),
             data = dfLikRhAp)
summary(NDM9a)
AIC(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a

plot(NDM9a)

## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = Gamma (link = "inverse"),
             data = dfLikRhAp)
summary(NDM9b)
AIC(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b

plot(NDM9b)

## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = Gamma (link = "identity"),
             data = dfLikRhAp)
summary(NDM9c)
AIC(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c

plot(NDM9c)

## log
NDM9d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata 
             ,
             family = Gamma (link = "log"),
             data = dfLikRhAp)
summary(NDM9d)
AIC(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d

plot(NDM9d)

##### Proportional generality



### gaussian for proportional.generality, trial different link functions
### identity
### 
NDM7a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "identity"),
             data = dfLikRhAp)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

## log

NDM7b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "log"),
             data = dfLikRhAp)
summary(NDM7b)
AIC(NDM7b)
plot(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b


## inverse
NDM7c <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "inverse"),
             data = dfLikRhAp)
summary(NDM7c)
AIC(NDM7c)
plot(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


## sqrt
NDM7d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "sqrt"),
             data = dfLikRhAp)
summary(NDM7d)
AIC(NDM7d)
plot(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLikRhAp)
summary(NDM8a)
AIC(NDM8a)
plot(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a


## inverse

NDM8b <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "inverse"),
             data = dfLikRhAp)
summary(NDM8b)
AIC(NDM8b)
plot(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b


## identity
## 
NDM8c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "identity"),
             data = dfLikRhAp)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


## log
# 
NDM8d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "log"),
             data = dfLikRhAp)
summary(NDM8d)
AIC(NDM8d)
plot(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = Gamma (link = "sqrt"),
             data = dfLikRhAp)
summary(NDM9a)
AIC(NDM9a)
plot(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a


## inverse
NDM9b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = Gamma (link = "inverse"),
             data = dfLikRhAp)
summary(NDM9b)
AIC(NDM9b)
plot(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b


## identity
NDM9c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = Gamma (link = "identity"),
             data = dfLikRhAp)
summary(NDM9c)
AIC(NDM9c)
plot(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c


## log
NDM9d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata
             ,
             family = Gamma (link = "log"),
             data = dfLikRhAp)
summary(NDM9d)
AIC(NDM9d)
plot(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d




##################################################################
##### Rhizorphora mucronata ######################################
##################################################################


dfLikRhMu <- subset(dfLik, network_lower_level_shorthand == "P07") 


### reset reference level of Treatment variable to Reference Forest forest
dfLikRhMu$Treatment <- factor(dfLikRhMu$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))

####### Normalised Degree #####


## logit link, trial link.phi
NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "logit",
                  link.phi = "log", 
                  data = dfLikRhMu)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfLikRhMu)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi

NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "probit",
                  link.phi = "log", 
                  data = dfLikRhMu)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfLikRhMu)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)


NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfLikRhMu)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfLikRhMu)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi
#

NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

#fails
NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfLikRhMu)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfLikRhMu)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi
#

NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "log",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "log",
                  link.phi = "log", 
                  data = dfLikRhMu)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfLikRhMu)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi

NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfLikRhMu)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfLikRhMu)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.mucronata,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfLikRhMu)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity

NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = gaussian (link = "identity"),
             data = dfLikRhMu)
summary(NDM7a)
AIC(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

plot(NDM7a)

## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = gaussian (link = "log"),
             data = dfLikRhMu)
summary(NDM7b)
AIC(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b

plot(NDM7b)

## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = gaussian (link = "inverse"),
             data = dfLikRhMu)
summary(NDM7c)
AIC(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


plot(NDM7c)

## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = gaussian (link = "sqrt"),
             data = dfLikRhMu)
summary(NDM7d)
AIC(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d


plot(NDM7d)

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLikRhMu)
summary(NDM8a)
AIC(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a

plot(NDM8a)

## inverse
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.mucronata,
             family = inverse.gaussian (link = "inverse"),
             data = dfLikRhMu)
summary(NDM8b)
AIC(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b

plot(NDM8b)

## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.mucronata,
             family = inverse.gaussian (link = "identity"),
             data = dfLikRhMu)
summary(NDM8c)
AIC(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c

plot(NDM8c)

## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = inverse.gaussian (link = "log"),
             data = dfLikRhMu)
summary(NDM8d)
AIC(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

plot(NDM8d)

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.mucronata,
             family = Gamma (link = "sqrt"),
             data = dfLikRhMu)
summary(NDM9a)
AIC(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a

plot(NDM9a)

## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata,
             family = Gamma (link = "inverse"),
             data = dfLikRhMu)
summary(NDM9b)
AIC(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b

plot(NDM9b)

## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.mucronata,
             family = Gamma (link = "identity"),
             data = dfLikRhMu)
summary(NDM9c)
AIC(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c

plot(NDM9c)

## log
NDM9d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.mucronata 
             ,
             family = Gamma (link = "log"),
             data = dfLikRhMu)
summary(NDM9d)
AIC(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d

plot(NDM9d)



##### Proportional generality

### Poisson for Proportional generality, trial different link functions
### identity
no._interactions_totalM7a <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = poisson (link = "identity"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM7a)
AIC(no._interactions_totalM7a)
plot(no._interactions_totalM7a)

# R-squared
Model_R2no._interactions_totalM7a <- with(summary(no._interactions_totalM7a), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM7a


# overdispersion
overdispersion_no._interactions_totalM7a <- no._interactions_totalM7a$deviance / no._interactions_totalM7a$df.residual
overdispersion_no._interactions_totalM7a

# deviance residuals
devresid_no._interactions_totalM7a <- resid(no._interactions_totalM7a, type = "deviance")
plot(devresid_no._interactions_totalM7a ~ dfLikRhMu$CaCo_index)



### log
no._interactions_totalM7b <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = poisson (link = "log"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM7b)
AIC(no._interactions_totalM7b)
plot(no._interactions_totalM7b)

# R-squared
Model_R2no._interactions_totalM7b <- with(summary(no._interactions_totalM7b), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM7b

pseudo.R2_no._interactions_totalM7b <- (no._interactions_totalM7b$null.deviance - no._interactions_totalM7b$deviance) /
  no._interactions_totalM7b$null.deviance
pseudo.R2_no._interactions_totalM7b

# overdispersion
overdispersion_no._interactions_totalM7b <- no._interactions_totalM7b$deviance / no._interactions_totalM7b$df.residual
overdispersion_no._interactions_totalM7b

# deviance residuals
devresid_no._interactions_totalM7b <- resid(no._interactions_totalM7b, type = "deviance")
plot(devresid_no._interactions_totalM7b ~ dfLikRhMu$CaCo_index)



## sqrt
no._interactions_totalM7c <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = poisson (link = "sqrt"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM7c)
AIC(no._interactions_totalM7c)
plot(no._interactions_totalM7c)

# R-squared
Model_R2no._interactions_totalM7c <- with(summary(no._interactions_totalM7c), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM7c

pseudo.R2_no._interactions_totalM7c <- (no._interactions_totalM7c$null.deviance - no._interactions_totalM7c$deviance) /
  no._interactions_totalM7c$null.deviance
pseudo.R2_no._interactions_totalM7c

# overdispersion
overdispersion_no._interactions_totalM7c <- no._interactions_totalM7c$deviance / no._interactions_totalM7c$df.residual
overdispersion_no._interactions_totalM7c

# deviance residuals
devresid_no._interactions_totalM7c <- resid(no._interactions_totalM7c, type = "deviance")
plot(devresid_no._interactions_totalM7c ~ dfLikRhMu$CaCo_index)




### quasipoisson for Proportional generality, trial different link functions
### identity

no._interactions_totalM8a <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = quasipoisson (link = "identity"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM8a)
AIC(no._interactions_totalM8a)
plot(no._interactions_totalM8a)

# R-squared
Model_R2no._interactions_totalM8a <- with(summary(no._interactions_totalM8a), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM8a

pseudo.R2_no._interactions_totalM8a <- (no._interactions_totalM8a$null.deviance - no._interactions_totalM8a$deviance) /
  no._interactions_totalM8a$null.deviance
pseudo.R2_no._interactions_totalM8a

# overdispersion
overdispersion_no._interactions_totalM8a <- no._interactions_totalM8a$deviance / no._interactions_totalM8a$df.residual
overdispersion_no._interactions_totalM8a

# deviance residuals
devresid_no._interactions_totalM8a <- resid(no._interactions_totalM8a, type = "deviance")
plot(devresid_no._interactions_totalM8a ~ dfLikRhMu$CaCo_index)

### log
no._interactions_totalM8b <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = quasipoisson (link = "log"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM8b)
AIC(no._interactions_totalM8b)
plot(no._interactions_totalM8b)

# R-squared
Model_R2no._interactions_totalM8b <- with(summary(no._interactions_totalM8b), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM8b

pseudo.R2_no._interactions_totalM8b <- (no._interactions_totalM8b$null.deviance - no._interactions_totalM8b$deviance) /
  no._interactions_totalM8b$null.deviance
pseudo.R2_no._interactions_totalM8b

# overdispersion
overdispersion_no._interactions_totalM8b <- no._interactions_totalM8b$deviance / no._interactions_totalM8b$df.residual
overdispersion_no._interactions_totalM8b

# deviance residuals
devresid_no._interactions_totalM8b <- resid(no._interactions_totalM8b, type = "deviance")
plot(devresid_no._interactions_totalM8b ~ dfLikRhMu$CaCo_index)

### logit
# fails
no._interactions_totalM8c <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = quasipoisson (link = "logit"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM8c)
AIC(no._interactions_totalM8c)
plot(no._interactions_totalM8c)

# R-squared
Model_R2no._interactions_totalM8c <- with(summary(no._interactions_totalM8c), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM8c

pseudo.R2_no._interactions_totalM8c <- (no._interactions_totalM8c$null.deviance - no._interactions_totalM8c$deviance) /
  no._interactions_totalM8c$null.deviance
pseudo.R2_no._interactions_totalM8c

# overdispersion
overdispersion_no._interactions_totalM8c <- no._interactions_totalM8c$deviance / no._interactions_totalM8c$df.residual
overdispersion_no._interactions_totalM8c

# deviance residuals
devresid_no._interactions_totalM8c <- resid(no._interactions_totalM8c, type = "deviance")
plot(devresid_no._interactions_totalM8c ~ dfLikRhMu$CaCo_index)

### probit
# fails
no._interactions_totalM8d <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = quasipoisson (link = "probit"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM8d)
AIC(no._interactions_totalM8d)
plot(no._interactions_totalM8d)

# R-squared
Model_R2no._interactions_totalM8d <- with(summary(no._interactions_totalM8d), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM8d

pseudo.R2_no._interactions_totalM8d <- (no._interactions_totalM8d$null.deviance - no._interactions_totalM8d$deviance) /
  no._interactions_totalM8d$null.deviance
pseudo.R2_no._interactions_totalM8d

# overdispersion
overdispersion_no._interactions_totalM8d <- no._interactions_totalM8d$deviance / no._interactions_totalM8d$df.residual
overdispersion_no._interactions_totalM8d

# deviance residuals
devresid_no._interactions_totalM8d <- resid(no._interactions_totalM8d, type = "deviance")
plot(devresid_no._interactions_totalM8d ~ dfLikRhMu$CaCo_index)


### cloglog
# fails
no._interactions_totalM8e <- glm(no._interactions_total ~ Treatment
                                 + CaCo_index  + Biomass_index_R.mucronata 
                                 ,
                                 family = quasipoisson (link = "cloglog"),
                                 data = dfLikRhMu)
summary(no._interactions_totalM8e)
AIC(no._interactions_totalM8e)
plot(no._interactions_totalM8e)

# R-squared
Model_R2no._interactions_totalM8e <- with(summary(no._interactions_totalM8e), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM8e

pseudo.R2_no._interactions_totalM8e <- (no._interactions_totalM8e$null.deviance - no._interactions_totalM8e$deviance) /
  no._interactions_totalM8e$null.deviance
pseudo.R2_no._interactions_totalM8e

# overdispersion
overdispersion_no._interactions_totalM8e <- no._interactions_totalM8e$deviance / no._interactions_totalM8e$df.residual
overdispersion_no._interactions_totalM8e

# deviance residuals
devresid_no._interactions_totalM8e <- resid(no._interactions_totalM8e, type = "deviance")
plot(devresid_no._interactions_totalM8e ~ dfLikRhMu$CaCo_index)



### Negative binomial for Proportional generality, trial different link functions
library(MASS)

### identity
no._interactions_totalM9a <- glm.nb(no._interactions_total ~ Treatment
                                    + CaCo_index  + Biomass_index_R.mucronata 
                                    , link = "identity",
                                    data = dfLikRhMu)
summary(no._interactions_totalM9a, cor = FALSE)
AIC(no._interactions_totalM9a)
plot(no._interactions_totalM9a)

# R-squared
Model_R2no._interactions_totalM9a <- with(summary(no._interactions_totalM9a), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM9a

pseudo.R2_no._interactions_totalM9a <- (no._interactions_totalM9a$null.deviance - no._interactions_totalM9a$deviance) /
  no._interactions_totalM9a$null.deviance
pseudo.R2_no._interactions_totalM9a

# overdispersion
overdispersion_no._interactions_totalM9a <- no._interactions_totalM9a$deviance / no._interactions_totalM9a$df.residual
overdispersion_no._interactions_totalM9a

# deviance residuals
devresid_no._interactions_totalM9a <- resid(no._interactions_totalM9a, type = "deviance")
plot(devresid_no._interactions_totalM9a ~ dfLikRhMu$CaCo_index)

### log
no._interactions_totalM9b <- glm.nb(no._interactions_total ~ Treatment
                                    + CaCo_index  + Biomass_index_R.mucronata 
                                    , link = "log",
                                    data = dfLikRhMu)
summary(no._interactions_totalM9b, cor = FALSE)
AIC(no._interactions_totalM9b)
plot(no._interactions_totalM9b)

# R-squared
Model_R2no._interactions_totalM9b <- with(summary(no._interactions_totalM9b), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM9b

pseudo.R2_no._interactions_totalM9b <- (no._interactions_totalM9b$null.deviance - no._interactions_totalM9b$deviance) /
  no._interactions_totalM9b$null.deviance
pseudo.R2_no._interactions_totalM9b

# overdispersion
overdispersion_no._interactions_totalM9b <- no._interactions_totalM9b$deviance / no._interactions_totalM9b$df.residual
overdispersion_no._interactions_totalM9b

# deviance residuals
devresid_no._interactions_totalM9b <- resid(no._interactions_totalM9b, type = "deviance")
plot(devresid_no._interactions_totalM9b ~ dfLikRhMu$CaCo_index)


## sqrt
no._interactions_totalM9c <- glm.nb(no._interactions_total ~ Treatment
                                    + CaCo_index  + Biomass_index_R.mucronata 
                                    , link = "sqrt",
                                    data = dfLikRhMu)
summary(no._interactions_totalM9c, cor = FALSE)
AIC(no._interactions_totalM9c)
plot(no._interactions_totalM9c)

# R-squared
Model_R2no._interactions_totalM9c <- with(summary(no._interactions_totalM9c), 1 - deviance/null.deviance)
Model_R2no._interactions_totalM9c

pseudo.R2_no._interactions_totalM9c <- (no._interactions_totalM9c$null.deviance - no._interactions_totalM9c$deviance) /
  no._interactions_totalM9c$null.deviance
pseudo.R2_no._interactions_totalM9c

# overdispersion
overdispersion_no._interactions_totalM9c <- no._interactions_totalM9c$deviance / no._interactions_totalM9c$df.residual
overdispersion_no._interactions_totalM9c

# deviance residuals
devresid_no._interactions_totalM9c <- resid(no._interactions_totalM9c, type = "deviance")
plot(devresid_no._interactions_totalM9c ~ dfLikRhMu$CaCo_index)



#######################################
###### Sediment #######################
#######################################


dfLikSed <- subset(dfLik, network_lower_level_shorthand == "S") 


### reset reference level of Treatment variable to Natural forest
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

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index,
                  link = "logit",
                  link.phi = "log", 
                  data = dfLikSed)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfLikSed)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi

NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfLikSed)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "probit",
                  link.phi = "log", 
                  data = dfLikSed)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfLikSed)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfLikSed)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)


NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfLikSed)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfLikSed)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi

NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfLikSed)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfLikSed)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfLikSed)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi


NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "log",
                  link.phi = "identity", 
                  data = dfLikSed)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "log",
                  link.phi = "log", 
                  data = dfLikSed)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfLikSed)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi

NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfLikSed)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfLikSed)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfLikSed)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity

NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "identity"),
             data = dfLikSed)
summary(NDM7a)
AIC(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

plot(NDM7a)

## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "log"),
             data = dfLikSed)
summary(NDM7b)
AIC(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b

plot(NDM7b)

## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "inverse"),
             data = dfLikSed)
summary(NDM7c)
AIC(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


plot(NDM7c)

## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "sqrt"),
             data = dfLikSed)
summary(NDM7d)
AIC(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d


plot(NDM7d)

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfLikSed)
summary(NDM8a)
AIC(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a

plot(NDM8a)

## inverse
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = inverse.gaussian (link = "inverse"),
             data = dfLikSed)
summary(NDM8b)
AIC(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b

plot(NDM8b)

## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = inverse.gaussian (link = "identity"),
             data = dfLikSed)
summary(NDM8c)
AIC(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c

plot(NDM8c)

## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = inverse.gaussian (link = "log"),
             data = dfLikSed)
summary(NDM8d)
AIC(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

plot(NDM8d)

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = Gamma (link = "sqrt"),
             data = dfLikSed)
summary(NDM9a)
AIC(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a

plot(NDM9a)

## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = Gamma (link = "inverse"),
             data = dfLikSed)
summary(NDM9b)
AIC(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b

plot(NDM9b)

## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = Gamma (link = "identity"),
             data = dfLikSed)
summary(NDM9c)
AIC(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c

plot(NDM9c)

## log
NDM9d <- glm(normalised.degree ~ Treatment
             + CaCo_index 
             ,
             family = Gamma (link = "log"),
             data = dfLikSed)
summary(NDM9d)
AIC(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d

plot(NDM9d)





##### Proportional generality

### gaussian for ND, trial different link functions
### identity

proportional.generality7a <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "identity"),
                                 data = dfLikSed)
summary(proportional.generality7a)
AIC(proportional.generality7a)

Model_R2proportional.generality7a <- with(summary(proportional.generality7a), 1 - deviance/null.deviance)
Model_R2proportional.generality7a

plot(proportional.generality7a)

## log

proportional.generality7b <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "log"),
                                 data = dfLikSed)
summary(proportional.generality7b)
AIC(proportional.generality7b)

Model_R2proportional.generality7b <- with(summary(proportional.generality7b), 1 - deviance/null.deviance)
Model_R2proportional.generality7b

plot(proportional.generality7b)

## inverse
proportional.generality7c <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "inverse"),
                                 data = dfLikSed)
summary(proportional.generality7c)
AIC(proportional.generality7c)

Model_R2proportional.generality7c <- with(summary(proportional.generality7c), 1 - deviance/null.deviance)
Model_R2proportional.generality7c


plot(proportional.generality7c)

## sqrt
proportional.generality7d <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "sqrt"),
                                 data = dfLikSed)
summary(proportional.generality7d)
AIC(proportional.generality7d)

Model_R2proportional.generality7d <- with(summary(proportional.generality7d), 1 - deviance/null.deviance)
Model_R2proportional.generality7d


plot(proportional.generality7d)

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
proportional.generality8a <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "1/mu^2"),
                                 data = dfLikSed)
summary(proportional.generality8a)
AIC(proportional.generality8a)

Model_R2proportional.generality8a <- with(summary(proportional.generality8a), 1 - deviance/null.deviance)
Model_R2proportional.generality8a

plot(proportional.generality8a)

## inverse
proportional.generality8b <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "inverse"),
                                 data = dfLikSed)
summary(proportional.generality8b)
AIC(proportional.generality8b)

Model_R2proportional.generality8b <- with(summary(proportional.generality8b), 1 - deviance/null.deviance)
Model_R2proportional.generality8b

plot(proportional.generality8b)

## identity
proportional.generality8c <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "identity"),
                                 data = dfLikSed)
summary(proportional.generality8c)
AIC(proportional.generality8c)

Model_R2proportional.generality8c <- with(summary(proportional.generality8c), 1 - deviance/null.deviance)
Model_R2proportional.generality8c

plot(proportional.generality8c)

## log
proportional.generality8d <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "log"),
                                 data = dfLikSed)
summary(proportional.generality8d)
AIC(proportional.generality8d)

Model_R2proportional.generality8d <- with(summary(proportional.generality8d), 1 - deviance/null.deviance)
Model_R2proportional.generality8d

plot(proportional.generality8d)

### Gamma for ND, trial different link functions
### sqrt
proportional.generality9a <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = Gamma (link = "sqrt"),
                                 data = dfLikSed)
summary(proportional.generality9a)
AIC(proportional.generality9a)

Model_R2proportional.generality9a <- with(summary(proportional.generality9a), 1 - deviance/null.deviance)
Model_R2proportional.generality9a

plot(proportional.generality9a)

## inverse
proportional.generality9b <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = Gamma (link = "inverse"),
                                 data = dfLikSed)
summary(proportional.generality9b)
AIC(proportional.generality9b)

Model_R2proportional.generality9b <- with(summary(proportional.generality9b), 1 - deviance/null.deviance)
Model_R2proportional.generality9b

plot(proportional.generality9b)

## identity
proportional.generality9c <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = Gamma (link = "identity"),
                                 data = dfLikSed)
summary(proportional.generality9c)
AIC(proportional.generality9c)

Model_R2proportional.generality9c <- with(summary(proportional.generality9c), 1 - deviance/null.deviance)
Model_R2proportional.generality9c

plot(proportional.generality9c)

## log
proportional.generality9d <- glm(proportional.generality ~ Treatment
                                 + CaCo_index 
                                 ,
                                 family = Gamma (link = "log"),
                                 data = dfLikSed)
summary(proportional.generality9d)
AIC(proportional.generality9d)

Model_R2proportional.generality9d <- with(summary(proportional.generality9d), 1 - deviance/null.deviance)
Model_R2proportional.generality9d

plot(proportional.generality9d)



# Housekeeping
graphics.off() 
rm(list=ls())