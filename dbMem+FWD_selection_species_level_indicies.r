# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Species_level_indicies.csv


## packages 
#install.packages("spatialreg")
#install.packages("vegan")
#install.packages("adespatial")
#install.packages("spdep")
#install.packages("sp")

library(spatialreg)
library(vegan)
library(adespatial)
library(spdep)
library(sp)


#######################################################################
################## Likupang  ##########################################
######################################################################

#### Brugeria Likupang

### coordinates
xyLikBru <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "P02",
                       select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesLikBru <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "P02",
                     select=c(proportional.generality, normalised.degree))


xyLikBru <- as.matrix(xyLikBru)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyLikBru), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesLikBru, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesLikBru, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesLikBru, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesLikBru, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesLikBru, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesLikBru, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}




### Rhizophora apiculata Likupang

### coordinates
xyLikAp <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "P06",
                  select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesLikAp <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "P06",
                       select=c(proportional.generality, normalised.degree))


xyLikAp <- as.matrix(xyLikAp)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyLikAp), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesLikAp, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesLikAp, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesLikAp, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesLikAp, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesLikAp, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesLikAp, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}




### Rhizophora mucronata Likupang



### coordinates
xyLikMu <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "P07",
                  select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesLikMu <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "P07",
                       select=c(proportional.generality, normalised.degree))


xyLikMu <- as.matrix(xyLikMu)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyLikMu), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesLikMu, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesLikMu, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesLikMu, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesLikMu, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesLikMu, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesLikMu, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}



### Sediment Likupang



### coordinates
xyLikSed <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "S",
                   select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesLikSed <- subset(dat1, Site == "Likupang" & network_lower_level_shorthand == "S",
                        select=c(proportional.generality, normalised.degree))




xyLikSed <- as.matrix(xyLikSed)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyLikSed), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesLikSed, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesLikSed, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesLikSed, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesLikSed, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesLikSed, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesLikSed, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}


#######################################################################
################## Tiwoho  ##########################################
######################################################################

### Ceriops taga Tiwoho

### coordinates
xyTiwCer <- subset(dat1, Site == "Tiwoho" & network_lower_level_shorthand == "P05",
                   select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesTiwCer <- subset(dat1, Site == "Tiwoho" & network_lower_level_shorthand == "P05",
                        select=c(proportional.generality, normalised.degree))


xyTiwCer <- as.matrix(xyTiwCer)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyTiwCer), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesTiwCer, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesTiwCer, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesTiwCer, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesTiwCer, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesTiwCer, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesTiwCer, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}




### Rhizophora apiculata Tiwoho



### coordinates
xyTiwAp <- subset(dat1, Site == "Tiwoho" & network_lower_level_shorthand == "P06",
                  select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesTiwAp <- subset(dat1, Site == "Tiwoho" & network_lower_level_shorthand == "P06",
                       select=c(proportional.generality, normalised.degree))


xyTiwAp <- as.matrix(xyTiwAp)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyTiwAp), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesTiwAp, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesTiwAp, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesTiwAp, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesTiwAp, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesTiwAp, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesTiwAp, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}




### Sediment Tiwoho

### coordinates
xyTiwSed <- subset(dat1, Site == "Tiwoho" & network_lower_level_shorthand == "S",
                   select=c(longitude_x, latitude_y))

### potentially interesting network indices
indicesTiwSed <- subset(dat1, Site == "Tiwoho" & network_lower_level_shorthand == "S",
                        select=c(proportional.generality, normalised.degree))




xyTiwSed <- as.matrix(xyTiwSed)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyTiwSed), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# 

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesTiwSed, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesTiwSed, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesTiwSed, MEM, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM)[, c(sorted_sel)]
  } else print("No significant spatial autocorrelation was detected in the response")
} else { # We consider both positively and negatively autocorrelated predictors
  # List to save the positively and negatively autocorrelated MEM separately
  mem.sign <- vector("list", 2)
  signif <- c("FALSE", "FALSE")
  # We select the positive and negative MEM separately after testing the global
  # significance of both models at a corrected threshold value of null hypothesis
  # rejection (Sidak correction)
  for (i in 1:2) {
    if (i == 1) { # Positive MEM
      mem <- MEM[, which(attributes(MEM)$values > 0)]
    } else { # Negative MEM
      mem <- MEM[, which(attributes(MEM)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesTiwSed, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesTiwSed, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesTiwSed, mem, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem)[, c(sorted_sel)]
      signif[i] <- "TRUE"
    }
  }
  #  MEM.select will contain both positive and negative MEM, only positive or only
  # negative MEM, depending on the significance of the global tests.
  if (length(which(signif == "FALSE")) != 2) {
    if (length(which(signif == "TRUE")) == 2) {
      MEM.select <- cbind(mem.sign[[1]], mem.sign[[2]])
    } else if (signif[1] == "TRUE") {
      MEM.select <- mem.sign[[1]]
    } else MEM.select <- mem.sign[[2]]
  } else print("No significant spatial autocorrelation was detected in the response")
}



########################################################################################
########################################################################################


# Housekeeping
graphics.off() 
rm(list=ls())
