# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) #Network_indices _vegetation_search.csv
                              #choose indices data for just vegetation search data


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

### coordinates
# just looking at Likupang, exclude quadrats which gave an error message when producing bipartite indices
xyLik <- subset(dat1, Site == "Likupang" & network_size_sufficient_for_analysis == "YES", 
                       select=c(longitude_x, latitude_y))

### potentially interesting network indices
# just looking at Likupang, exclude quadrats which gave an error message when producing bipartite indices
indicesLik <- subset(dat1, Site == "Likupang" & network_size_sufficient_for_analysis == "YES",
                     select=c(weighted_connectance, weighted_NODF,
                              generality.HL, robustness.HL,
                              interaction_evenness, vulnerability.LL,
                              H2, modularity_index))


xyLik <- as.matrix(xyLik)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyLik), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# raw variables

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesLik, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesLik, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesLik, MEM, adjR2thresh = R2adj, nperm = 999)
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
    if (anova.cca(rda(indicesLik, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesLik, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesLik, mem, adjR2thresh = R2adj, nperm = 999)
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


### do again but re-scale all variables so mean = 0 and SD = 1

scaled.indicesLik <- scale(indicesLik)

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(scaled.indicesLik, MEM), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(scaled.indicesLik, MEM))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(scaled.indicesLik, MEM, adjR2thresh = R2adj, nperm = 999)
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
    if (anova.cca(rda(scaled.indicesLik, mem), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(scaled.indicesLik, mem))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(scaled.indicesLik, mem, adjR2thresh = R2adj, nperm = 999)
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

### coordinates
# just looking at Tiwoho, exclude quadrats which gave an error message when producing bipartite indices
# also exclude TW3, as even though it didn't produce an error message, it's full of NAs which stop this running
xyTiw <- subset(dat1, Site == "Tiwoho" & network_size_sufficient_for_analysis == "YES" & Quadrat != "TW3",
                select=c(longitude_x, latitude_y))

### potentially interesting network indices
# just looking at Tiwoho, exclude quadrats which gave an error message when producing bipartite indices
# also exclude TW3, as even though it didn't produce an error message, it's full of NAs which stop this running
indicesTiw <- subset(dat1, Site == "Tiwoho" & network_size_sufficient_for_analysis == "YES" & Quadrat != "TW3",
                     select=c(weighted_connectance, weighted_NODF,
                              generality.HL, robustness.HL, 
                              interaction_evenness, vulnerability.LL,
                              H2, modularity_index))

xyTiw <- as.matrix(xyTiw)

## define whether we want the MEM variables modelling
## positively autocorrelated patterns ("positive"), 
## negatively autocorrelated patterns ("negative"), or all n-1
## MEM variables ("all")

MEM_model <- "all"

## build the set of spatial eigenvectors (stored in object MEM)
style <- "B"
nb <- graph2nb(gabrielneigh(as.matrix(xyTiw), nnmult = 5), sym = TRUE)
listw <- nb2listw(nb, style = style)
MEM_Tiw <- scores.listw(listw, MEM.autocor = MEM_model)


## forward selection (FWD) of Blanchet et al. (2008)
# raw variables

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(indicesTiw, MEM_Tiw), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(indicesTiw, MEM_Tiw))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(indicesTiw, MEM_Tiw, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM_Tiw)[, c(sorted_sel)]
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
      mem_Tiw <- MEM_Tiw[, which(attributes(MEM_Tiw)$values > 0)]
    } else { # Negative MEM
      mem_Tiw <- MEM_Tiw[, which(attributes(MEM_Tiw)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(indicesTiw, mem_Tiw), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(indicesTiw, mem_Tiw))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(indicesTiw, mem_Tiw, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem_Tiw)[, c(sorted_sel)]
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


### do again but re-scale all variables so mean = 0 and SD = 1

scaled.indicesTiw <- scale(indicesTiw)

if (MEM_model != "all") { # We consider only positively or negatively autocorrelated MEM
  if (anova.cca(rda(scaled.indicesTiw, MEM_Tiw), permutations = 9999)$Pr[1] <= 0.05) {
    # Global adjusted R-squared of the model
    R2adj <- RsquareAdj(rda(scaled.indicesTiw, MEM_Tiw))$adj.r.squared
    # FWD with two stopping criteria
    fsel <- forward.sel(scaled.indicesTiw, MEM_Tiw, adjR2thresh = R2adj, nperm = 999)
    # We order the selected MEM by decreasing eigenvalue
    sorted_sel <- sort(fsel$order)
    # Object containing the selected MEM
    MEM.select <- as.data.frame(MEM_Tiw)[, c(sorted_sel)]
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
      mem_Tiw <- MEM_Tiw[, which(attributes(MEM_Tiw)$values > 0)]
    } else { # Negative MEM
      mem_Tiw <- MEM_Tiw[, which(attributes(MEM_Tiw)$values < 0)]
    }
    #    Global test of significance with the Sidak correction for multiple tests
    if (anova.cca(rda(scaled.indicesTiw, mem_Tiw), permutations = 9999)$Pr[1] <= (1-(1-0.05)^0.5)) {
      # Global adjusted R-squared of the model
      R2adj <- RsquareAdj(rda(scaled.indicesTiw, mem_Tiw))$adj.r.squared
      # FWD with two stopping criteria
      fsel <- forward.sel(scaled.indicesTiw, mem_Tiw, adjR2thresh = R2adj, nperm = 999)
      # We order the selected MEM by decreasing eigenvalue
      sorted_sel <- sort(fsel$order)
      # We save the selection of MEM
      mem.sign[[i]] <- as.data.frame(mem_Tiw)[, c(sorted_sel)]
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
