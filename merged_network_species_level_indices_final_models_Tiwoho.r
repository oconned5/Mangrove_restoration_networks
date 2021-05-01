########################################################
### Final models for species level indicies merged networks lower level Tiwoho



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

### reset reference level of Treatment variable to Reference Forest forest
dfTiwCer$Treatment <- factor(dfTiwCer$Treatment, levels = c("Reference Forest", "Monoculture Reforestation"))

####### Normalised Degree #####

## Beta GLM, log link, identity link.phi
 
NDM5a <- betareg (ND ~ Treatment
                  + CaCo_index + Biomass_index_C.tagal,
                  link = "log",
                  link.phi = "identity", 
                  data = dfTiwCer)
summary(NDM5a)
AIC(NDM5a)
plot(NDM5a)

######### Proportional generality #############

### Gaussian GLM, identity link function
### poor enough fit, best of a bad lot
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



###########################
## Rhizophora apiculata  ##
###########################

dfTiwRhAp <- subset(dfTiw, network_lower_level_shorthand == "P06") 

### reset reference level of Treatment variable to Reference Forest forest
dfTiwRhAp$Treatment <- factor(dfTiwRhAp$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration"))


####### Normalised Degree #####

## Beta GLM,  loglog link, identity link.phi
##
NDM6a <- betareg (ND ~ Treatment 
                  + CaCo_index  + Biomass_index_R.apiculata,
                  link = "loglog",
                  link.phi = "identity", 
                  data = dfTiwRhAp)
summary(NDM6a)
AIC(NDM6a)
plot(NDM6a)


######### Proportional generality #############
## Inverse Gaussian GLM, log link

NDM8d <- glm(proportional.generality ~ Treatment
             + CaCo_index + Biomass_index_R.apiculata,
             family = inverse.gaussian (link = "log"),
             data = dfTiwRhAp)
summary(NDM8d)
AIC(NDM8d)
plot(NDM8d)

Model_R2NDM8d <- with(summary(NDM8d), 1 - deviance/null.deviance)
Model_R2NDM8d



#######################################
###### Sediment #######################
#######################################


dfTiwSed <- subset(dfTiw, network_lower_level_shorthand == "S") 


### reset reference level of Treatment variable to Reference Forest forest
dfTiwSed$Treatment <- factor(dfTiwSed$Treatment, levels = c("Reference Forest", "Mixed Species Regeneration", "Monoculture Reforestation"))

####### Normalised Degree #####

## Beta GLM, cauchit link, identity link.phi
# 
NDM4a <- betareg (ND ~ Treatment
                  + CaCo_index,
                  link = "cauchit",
                  link.phi = "identity", 
                  data = dfTiwSed)
summary(NDM4a)
AIC(NDM4a)
plot(NDM4a)


##### Proportional generality

### gaussian for ND, identity link function

proportional.generality7a <- glm(proportional.generality ~ Treatment
                                 + CaCo_index,
                                 family = gaussian (link = "identity"),
                                 data = dfTiwSed)
summary(proportional.generality7a)
AIC(proportional.generality7a)

Model_R2proportional.generality7a <- with(summary(proportional.generality7a), 1 - deviance/null.deviance)
Model_R2proportional.generality7a

plot(proportional.generality7a)


# Housekeeping
graphics.off() 
rm(list=ls())