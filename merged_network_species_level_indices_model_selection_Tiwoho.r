########################################################
### Model selection for species level indicies merged networks lower level Tiwoho



# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
df1<-read.csv(file.choose()) # Species_level_indicies.csv

## Normalised degree modified for beta glm
ND <- df1$normalised.degree - 0.00001 # put normalised degree between 0-1 for beta glm


dat1 <- cbind(df1, ND)

dfTiw <- subset(dat1, Site == "Tiwoho")

#install.packages("betareg")
#install.packages("MASS")


library(betareg)
library(MASS)



################################################
####### Tiwoho ###############################
################################################

###########################
## Ceriops tagal ##
###########################


dfTiwCer <- subset(dfTiw, network_lower_level_shorthand == "P05" & Treatment != "Mixed Species Regeneration") 


### reset reference level of Treatment variable to Natural forest
dfTiwCer$Treatment <- factor(dfTiwCer$Treatment, levels = c("Reference Forest", "Monoculture Reforestation"))


####### Normalised Degree #####
## logit link, trial link.phi

NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "logit",
                  link.phi = "log", 
                  data = dfTiwCer)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfTiwCer)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi


NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "probit",
                  link.phi = "log", 
                  data = dfTiwCer)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfTiwCer)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi
#

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)

NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfTiwCer)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfTiwCer)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi

NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfTiwCer)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfTiwCer)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi
# 
NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "log",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "log",
                  link.phi = "log", 
                  data = dfTiwCer)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfTiwCer)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi
## 
NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfTiwCer)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfTiwCer)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity
##
NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "identity"),
             data = dfTiwCer)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a


## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "log"),
             data = dfTiwCer)
summary(NDM7b)
AIC(NDM7b)
plot(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b


## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "inverse"),
             data = dfTiwCer)
summary(NDM7c)
AIC(NDM7c)
plot(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "sqrt"),
             data = dfTiwCer)
summary(NDM7d)
AIC(NDM7d)
plot(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfTiwCer)
summary(NDM8a)
AIC(NDM8a)
plot(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a


## inverse
## 
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "inverse"),
             data = dfTiwCer)
summary(NDM8b)
AIC(NDM8b)
plot(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b


## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "identity"),
             data = dfTiwCer)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "log"),
             data = dfTiwCer)
summary(NDM8d)
AIC(NDM8d)
plot(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = Gamma (link = "sqrt"),
             data = dfTiwCer)
summary(NDM9a)
AIC(NDM9a)
plot(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a


## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = Gamma (link = "inverse"),
             data = dfTiwCer)
summary(NDM9b)
AIC(NDM9b)
plot(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b


## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = Gamma (link = "identity"),
             data = dfTiwCer)
summary(NDM9c)
AIC(NDM9c)
plot(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c


## log
NDM9d <- glm(normalised.degree ~ Treatment
             + CaCo_index + Biomass_index_C.tagal
             ,
             family = Gamma (link = "log"),
             data = dfTiwCer)
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
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "identity"),
             data = dfTiwCer)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

## log

NDM7b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "log"),
             data = dfTiwCer)
summary(NDM7b)
AIC(NDM7b)
plot(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b


## inverse
NDM7c <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "inverse"),
             data = dfTiwCer)
summary(NDM7c)
AIC(NDM7c)
plot(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


## sqrt
NDM7d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = gaussian (link = "sqrt"),
             data = dfTiwCer)
summary(NDM7d)
AIC(NDM7d)
plot(NDM7d)

Model_R2NDM7d <- with(summary(NDM7d), 1 - deviance/null.deviance)
Model_R2NDM7d

### Inverse Gaussian for ND, trial different link functions
### 1/mu^2
NDM8a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "1/mu^2"),
             data = dfTiwCer)
summary(NDM8a)
AIC(NDM8a)
plot(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a


## inverse

NDM8b <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "inverse"),
             data = dfTiwCer)
summary(NDM8b)
AIC(NDM8b)
plot(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b


## identity
## 
NDM8c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "identity"),
             data = dfTiwCer)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


## log
# 
NDM8d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = inverse.gaussian (link = "log"),
             data = dfTiwCer)
summary(NDM8d)
AIC(NDM8d)
plot(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d

### Gamma for ND, trial different link functions
### sqrt
NDM9a <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = Gamma (link = "sqrt"),
             data = dfTiwCer)
summary(NDM9a)
AIC(NDM9a)
plot(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a


## inverse
NDM9b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal,
             family = Gamma (link = "inverse"),
             data = dfTiwCer)
summary(NDM9b)
AIC(NDM9b)
plot(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b


## identity
NDM9c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_C.tagal,
             family = Gamma (link = "identity"),
             data = dfTiwCer)
summary(NDM9c)
AIC(NDM9c)
plot(NDM9c)

Model_R2NDM9c <- with(summary(NDM9c), 1 - deviance/null.deviance)
Model_R2NDM9c


## log
NDM9d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_C.tagal
             ,
             family = Gamma (link = "log"),
             data = dfTiwCer)
summary(NDM9d)
AIC(NDM9d)
plot(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d




###########################
## Rhizophora apiculata  ##
###########################



dfTiwRhAp <- subset(dfTiw, network_lower_level_shorthand == "P06") 


### reset reference level of Treatment variable to Natural forest
dfTiwRhAp$Treatment <- factor(dfTiwRhAp$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration"))



####### Normalised Degree #####

## logit link, trial link.phi

NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "log", 
                  data = dfTiwRhAp)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfTiwRhAp)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi
# fails
NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "probit",
                  link.phi = "log", 
                  data = dfTiwRhAp)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfTiwRhAp)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)

NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfTiwRhAp)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfTiwRhAp)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi
# fails
NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

#fails
NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfTiwRhAp)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

#fails 
NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfTiwRhAp)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi

NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "log",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "log",
                  link.phi = "log", 
                  data = dfTiwRhAp)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfTiwRhAp)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi
## best
NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfTiwRhAp)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfTiwRhAp)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity

NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "identity"),
             data = dfTiwRhAp)
summary(NDM7a)
AIC(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

plot(NDM7a)

## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "log"),
             data = dfTiwRhAp)
summary(NDM7b)
AIC(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b

plot(NDM7b)

## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "inverse"),
             data = dfTiwRhAp)
summary(NDM7c)
AIC(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


plot(NDM7c)

## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = gaussian (link = "sqrt"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM8a)
AIC(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a

plot(NDM8a)

## inverse
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "inverse"),
             data = dfTiwRhAp)
summary(NDM8b)
AIC(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b

plot(NDM8b)

## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "identity"),
             data = dfTiwRhAp)
summary(NDM8c)
AIC(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c

plot(NDM8c)

## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "log"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM9a)
AIC(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a

plot(NDM9a)

## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index  + Biomass_index_R.apiculata,
             family = Gamma (link = "inverse"),
             data = dfTiwRhAp)
summary(NDM9b)
AIC(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b

plot(NDM9b)

## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index  + Biomass_index_R.apiculata,
             family = Gamma (link = "identity"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM9d)
AIC(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d

plot(NDM9d)

###### Proportional generality ######

### gaussian for proportional.generality, trial different link functions
### identity
### 
NDM7a <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "identity"),
             data = dfTiwRhAp)
summary(NDM7a)
AIC(NDM7a)
plot(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

## log

NDM7b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "log"),
             data = dfTiwRhAp)
summary(NDM7b)
AIC(NDM7b)
plot(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b


## inverse
NDM7c <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "inverse"),
             data = dfTiwRhAp)
summary(NDM7c)
AIC(NDM7c)
plot(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


## sqrt
NDM7d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = gaussian (link = "sqrt"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM8a)
AIC(NDM8a)
plot(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a


## inverse

NDM8b <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "inverse"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM8c)
AIC(NDM8c)
plot(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c


## log
# best so far
NDM8d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "log"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM9a)
AIC(NDM9a)
plot(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a


## inverse
NDM9b <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = Gamma (link = "inverse"),
             data = dfTiwRhAp)
summary(NDM9b)
AIC(NDM9b)
plot(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b


## identity
NDM9c <- glm(proportional.generality ~ Treatment 
             + CaCo_index + Biomass_index_R.apiculata,
             family = Gamma (link = "identity"),
             data = dfTiwRhAp)
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
             data = dfTiwRhAp)
summary(NDM9d)
AIC(NDM9d)
plot(NDM9d)

Model_R2NDM9d <- with(summary(NDM9d), 1 - deviance/null.deviance)
Model_R2NDM9d





#######################################
###### Sediment #######################
#######################################

dfTiwSed <- subset(dfTiw, network_lower_level_shorthand == "S") 


### reset reference level of Treatment variable to Natural forest
dfTiwSed$Treatment <- factor(dfTiwSed$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))

####### Normalised Degree #####

## logit link, trial link.phi
NDM1a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "logit",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM1a)
AIC(NDM1a)
plot(NDM1a)

NDM1b <- betareg (ND ~ Treatment 
                  + CaCo_index,
                  link = "logit",
                  link.phi = "log", 
                  data = dfTiwSed)
summary(NDM1b)
AIC(NDM1b)
plot(NDM1b)

NDM1c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "logit",
                  link.phi = "sqrt", 
                  data = dfTiwSed)
summary(NDM1c)
AIC(NDM1c)
plot(NDM1c)


## probit link, trial link.phi

NDM2a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "probit",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM2a)
AIC(NDM2a)
plot(NDM2a)

NDM2b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "probit",
                  link.phi = "log", 
                  data = dfTiwSed)
summary(NDM2b)
AIC(NDM2b)
plot(NDM2b)

NDM2c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "probit",
                  link.phi = "sqrt", 
                  data = dfTiwSed)
summary(NDM2c)
AIC(NDM2c)
plot(NDM2c)

## cloglog link, trial link.phi

NDM3a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cloglog",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM3a)
AIC(NDM3a)
plot(NDM3a)


NDM3b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cloglog",
                  link.phi = "log", 
                  data = dfTiwSed)
summary(NDM3b)
AIC(NDM3b)
plot(NDM3b)

NDM3c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cloglog",
                  link.phi = "sqrt", 
                  data = dfTiwSed)
summary(NDM3c)
AIC(NDM3c)
plot(NDM3c)


## cauchit link, trial link.phi
# best 
NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)

NDM4b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "log", 
                  data = dfTiwSed)
summary(NDM4b)
AIC(NDM4b)
plot(NDM4b)

NDM4c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "sqrt", 
                  data = dfTiwSed)
summary(NDM4c)
AIC(NDM4c)
plot(NDM4c)


## log link, trial link.phi


NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "log",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

NDM5b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "log",
                  link.phi = "log", 
                  data = dfTiwSed)
summary(NDM5b)
AIC(NDM5b)
plot(NDM5b)

NDM5c <- betareg (ND ~ Treatment 
                  + CaCo_index,
                  link = "log",
                  link.phi = "sqrt", 
                  data = dfTiwSed)
summary(NDM5c)
AIC(NDM5c)
plot(NDM5c)


## loglog link, trial link.phi

NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)

NDM6b <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "loglog",
                  link.phi = "log", 
                  data = dfTiwSed)
summary(NDM6b)
AIC(NDM6b)
plot(NDM6b)

NDM6c <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "loglog",
                  link.phi = "sqrt", 
                  data = dfTiwSed)
summary(NDM6c)
AIC(NDM6c)
plot(NDM6c)

### gaussian for ND, trial different link functions
### identity

NDM7a <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "identity"),
             data = dfTiwSed)
summary(NDM7a)
AIC(NDM7a)

Model_R2NDM7a <- with(summary(NDM7a), 1 - deviance/null.deviance)
Model_R2NDM7a

plot(NDM7a)

## log

NDM7b <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "log"),
             data = dfTiwSed)
summary(NDM7b)
AIC(NDM7b)

Model_R2NDM7b <- with(summary(NDM7b), 1 - deviance/null.deviance)
Model_R2NDM7b

plot(NDM7b)

## inverse
NDM7c <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "inverse"),
             data = dfTiwSed)
summary(NDM7c)
AIC(NDM7c)

Model_R2NDM7c <- with(summary(NDM7c), 1 - deviance/null.deviance)
Model_R2NDM7c


plot(NDM7c)

## sqrt
NDM7d <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = gaussian (link = "sqrt"),
             data = dfTiwSed)
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
             data = dfTiwSed)
summary(NDM8a)
AIC(NDM8a)

Model_R2NDM8a <- with(summary(NDM8a), 1 - deviance/null.deviance)
Model_R2NDM8a

plot(NDM8a)

## inverse
NDM8b <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = inverse.gaussian (link = "inverse"),
             data = dfTiwSed)
summary(NDM8b)
AIC(NDM8b)

Model_R2NDM8b <- with(summary(NDM8b), 1 - deviance/null.deviance)
Model_R2NDM8b

plot(NDM8b)

## identity
NDM8c <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = inverse.gaussian (link = "identity"),
             data = dfTiwSed)
summary(NDM8c)
AIC(NDM8c)

Model_R2NDM8c <- with(summary(NDM8c), 1 - deviance/null.deviance)
Model_R2NDM8c

plot(NDM8c)

## log
NDM8d <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = inverse.gaussian (link = "log"),
             data = dfTiwSed)
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
             data = dfTiwSed)
summary(NDM9a)
AIC(NDM9a)

Model_R2NDM9a <- with(summary(NDM9a), 1 - deviance/null.deviance)
Model_R2NDM9a

plot(NDM9a)

## inverse
NDM9b <- glm(normalised.degree ~ Treatment
             + CaCo_index,
             family = Gamma (link = "inverse"),
             data = dfTiwSed)
summary(NDM9b)
AIC(NDM9b)

Model_R2NDM9b <- with(summary(NDM9b), 1 - deviance/null.deviance)
Model_R2NDM9b

plot(NDM9b)

## identity
NDM9c <- glm(normalised.degree ~ Treatment 
             + CaCo_index,
             family = Gamma (link = "identity"),
             data = dfTiwSed)
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
             data = dfTiwSed)
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
                                 data = dfTiwSed)
summary(proportional.generality7a)
AIC(proportional.generality7a)

Model_R2proportional.generality7a <- with(summary(proportional.generality7a), 1 - deviance/null.deviance)
Model_R2proportional.generality7a

plot(proportional.generality7a)

## log

proportional.generality7b <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "log"),
                                 data = dfTiwSed)
summary(proportional.generality7b)
AIC(proportional.generality7b)

Model_R2proportional.generality7b <- with(summary(proportional.generality7b), 1 - deviance/null.deviance)
Model_R2proportional.generality7b

plot(proportional.generality7b)

## inverse
proportional.generality7c <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "inverse"),
                                 data = dfTiwSed)
summary(proportional.generality7c)
AIC(proportional.generality7c)

Model_R2proportional.generality7c <- with(summary(proportional.generality7c), 1 - deviance/null.deviance)
Model_R2proportional.generality7c


plot(proportional.generality7c)

## sqrt
proportional.generality7d <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "sqrt"),
                                 data = dfTiwSed)
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
                                 data = dfTiwSed)
summary(proportional.generality8a)
AIC(proportional.generality8a)

Model_R2proportional.generality8a <- with(summary(proportional.generality8a), 1 - deviance/null.deviance)
Model_R2proportional.generality8a

plot(proportional.generality8a)

## inverse
proportional.generality8b <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "inverse"),
                                 data = dfTiwSed)
summary(proportional.generality8b)
AIC(proportional.generality8b)

Model_R2proportional.generality8b <- with(summary(proportional.generality8b), 1 - deviance/null.deviance)
Model_R2proportional.generality8b

plot(proportional.generality8b)

## identity
proportional.generality8c <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "identity"),
                                 data = dfTiwSed)
summary(proportional.generality8c)
AIC(proportional.generality8c)

Model_R2proportional.generality8c <- with(summary(proportional.generality8c), 1 - deviance/null.deviance)
Model_R2proportional.generality8c

plot(proportional.generality8c)

## log
proportional.generality8d <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = inverse.gaussian (link = "log"),
                                 data = dfTiwSed)
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
                                 data = dfTiwSed)
summary(proportional.generality9a)
AIC(proportional.generality9a)

Model_R2proportional.generality9a <- with(summary(proportional.generality9a), 1 - deviance/null.deviance)
Model_R2proportional.generality9a

plot(proportional.generality9a)

## inverse
proportional.generality9b <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = Gamma (link = "inverse"),
                                 data = dfTiwSed)
summary(proportional.generality9b)
AIC(proportional.generality9b)

Model_R2proportional.generality9b <- with(summary(proportional.generality9b), 1 - deviance/null.deviance)
Model_R2proportional.generality9b

plot(proportional.generality9b)

## identity
proportional.generality9c <- glm(proportional.generality ~ Treatment 
                                 + CaCo_index,
                                 family = Gamma (link = "identity"),
                                 data = dfTiwSed)
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
                                 data = dfTiwSed)
summary(proportional.generality9d)
AIC(proportional.generality9d)

Model_R2proportional.generality9d <- with(summary(proportional.generality9d), 1 - deviance/null.deviance)
Model_R2proportional.generality9d

plot(proportional.generality9d)




# Housekeeping
graphics.off() 
rm(list=ls())