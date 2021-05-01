##################################################################################
#### INTIAL WORK WITH INTERACTION DATA
#### GETTING FROM AN EDGE LIST OF SPECIES INTERATIONS TO A SPECIES INTERACTION MATRIX
#### GENERATING BIPARTITE PLOTS
#### GENERATING NETWORK LEVEL AND SPECIES LEVEL INDICIES
#### ASSESSEMTN OF BETADIVERSITY AND INTERACTION TURNOVER



# Housekeeping
graphics.off() 
rm(list=ls())

#setwd("")

# Read in the data
dat1<-read.csv(file.choose()) #Interaction_data.csv


# Packages
#install.packages("bipartite")
#install.packages("betalink")

## FOR BIPARTITE PLOTTING AND GENERATING INDICIES
library(bipartite)

## INTERACTION TURNOVER AND BETADIVERSITY
library(betalink)


#################################################################################
####### BIPARTITE PLOTTING AND GENERATION OF INDICES ############################
#################################################################################


### SPLITTING OUT EACH TREATMENT ###

#########################################
### Likupang Monoculture Reforestation ####
#########################################

dfLikMono <- subset(dat1, Site == "Likupang" & Treatment == "Monoculture Reforestation",
                  select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LikMonom1 <- table( dfLikMono )
(LikMonomat1 <- unclass(LikMonom1))
class(LikMonomat1)

# export matrix as csv if desired
#write.csv(LikMonomat1, file = "LikMono_interaction_matrix.csv")

### PLOTTING Likupang Monoculture Reforestation  ###

# basic bipartite plot for Likupang Monoculture Reforestation 

par(xpd=T)
plotweb(sortweb(LikMonomat1, sort.order="dec"), method = "normal", labsize = 1.2, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.04, low.spacing = 0.21,
        col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        col.high = c("red", "red", "red", "blue", "red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "blue", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))


# extract bipartite plot if required
pdf(file = "Likupang_Monoculture_Reforestation_bipartite_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(xpd=T)
plotweb(sortweb(LikMonomat1, sort.order="dec"), method = "normal", labsize = 1.2, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.04, low.spacing = 0.21,
        col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        col.high = c("red", "red", "red", "blue", "red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "blue", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

dev.off()


### INDICES Likupang Monoculture Reforestation  ###

# Network and group indices Likupang Monoculture Reforestation 
networklevel(LikMonomat1)
# export indices as a csv
write.csv(networklevel(LikMonomat1), file = "LikMono_network_level_indices.csv")

# Species level indices Likupang Monoculture Reforestation 
specieslevel(LikMonomat1, level="lower")

# extract for lower level (plants/sediment) 
write.csv(specieslevel(LikMonomat1, level="lower"), file = "LikMono_species_level_indices-lower.csv")

######################################################
### Likupang Mixed Species Regeneration ####
######################################################

dfLikMixed <- subset(dat1, Site == "Likupang" & Treatment == "Mixed Species Regeneration",
                    select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LikMixedm1 <- table( dfLikMixed )
(LikMixedmat1 <- unclass(LikMixedm1))
class(LikMixedmat1)

# export matrix as csv if desired
#write.csv(LikMixedmat1, file = "LikMixed_interaction_matrix.csv")

### PLOTTING Likupang Mixed Species Regeneration  ###


# basic bipartite plot for Likupang Mixed Species Regeneration

par(xpd=T)
plotweb(sortweb(LikMixedmat1, sort.order="dec"), method = "normal", labsize = 1.5, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.05, low.spacing = 0.24,
        col.low = c("black", "green4", "green4","green4", "green4", "green4",
                    "green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "green4",
                    "green4", "green4"),
        col.high = c("red", "red", "red", "red", "blue", "red", "red",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red", "blue", "red", "red",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue"))
        

# extract bipartite plot if required
pdf(file = "Likupang_Mixed_Species_Regeneration_Bipartite_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(xpd=T)
plotweb(sortweb(LikMixedmat1, sort.order="dec"), method = "normal", labsize = 1.2, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.05, low.spacing = 0.24,
        col.low = c("black", "green4", "green4","green4", "green4", "green4",
                    "green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "green4",
                        "green4", "green4"),
        col.high = c("red", "red", "red", "red", "blue", "red", "red",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red", "blue", "red", "red",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue"))
dev.off()


### INDICES Likupang Mixed Species Regeneration  ###

# Network and group indices Likupang Mixed Species Regeneration
networklevel(LikMixedmat1)
# export indices as a csv
write.csv(networklevel(LikMixedmat1), file = "LikMixed_network_level_indices.csv")

# Species level indices Likupang Mixed Species Regeneration
specieslevel(LikMixedmat1, level="lower")

# extract for lower level (plants/sediment) 
write.csv(specieslevel(LikMixedmat1, level="lower"), file = "LikMixed_species_level_indices-lower.csv")


######################################################
### Likupang Reference Forest ####
######################################################

dfLikReference_Forest <- subset(dat1, Site == "Likupang" & Treatment == "Reference Forest",
                       select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LikReference_Forestm1 <- table( dfLikReference_Forest )
(LikReference_Forestmat1 <- unclass(LikReference_Forestm1))
class(LikReference_Forestmat1)

# export matrix as csv if desired
#write.csv(LikReference_Forestmat1, file = "LikReference_Forest_interaction_matrix.csv")

### PLOTTING Likupang Reference Forest  ###

# basic bipartite plot for Likupang Reference Forest
par(xpd=T)
plotweb(sortweb(LikReference_Forestmat1, sort.order="dec"), method = "normal", labsize = 1.1, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.05, low.spacing = 0.2,
        col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                    "brown","green4",  "green4", "green4", "green4"),
        bor.col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                        "brown","green4", "green4", "green4", "green4"),
        col.high = c("red", "red", "red", "red","red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red","red", "red", "blue",  
                        "blue", "blue", "blue", "blue", "blue","blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue", "blue", "blue", "blue", "blue"))


# extract bipartite plot if required
pdf(file = "LikReference_Forest_bipartite_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(xpd=T)
plotweb(sortweb(LikReference_Forestmat1, sort.order="dec"), method = "normal", labsize = 1.1, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.05, low.spacing = 0.2,
        col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                    "brown","green4",  "green4", "green4", "green4"),
        bor.col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                        "brown","green4", "green4", "green4", "green4"),
        col.high = c("red", "red", "red", "red","red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red","red", "red", "blue",  
                        "blue", "blue", "blue", "blue", "blue","blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue", "blue", "blue", "blue", "blue"))
dev.off()


### INDICES Likupang Reference Forest  ###

# Network and group indices Likupang Reference Forest
networklevel(LikReference_Forestmat1)
# export indices as a csv
write.csv(networklevel(LikReference_Forestmat1), file = "LikReference_Forest_network_level_indices.csv")

# Species level indices Likupang Reference Forest
specieslevel(LikReference_Forestmat1, level="lower")

# extract for lower level (plants/sediment) 
write.csv(specieslevel(LikReference_Forestmat1, level="lower"), file = "LikReference_Forest_species_level_indices-lower.csv")


######################################################
### Tiwoho Monoculture Reforestation ####
######################################################

dfTiwMono <- subset(dat1, Site == "Tiwoho" & Treatment == "Monoculture Reforestation",
                    select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TiwMonom1 <- table( dfTiwMono )
(TiwMonomat1 <- unclass(TiwMonom1))
class(TiwMonomat1)

# export matrix as csv if desired
#write.csv(TiwMonomat1, file = "TiwMono_interaction_matrix.csv")

### PLOTTING Tiwoho Monoculture Reforestation  ###

# basic bipartite plot for Tiwoho Monoculture Reforestation
par(xpd=T)
plotweb(sortweb(TiwMonomat1, sort.order="dec"), method = "normal", labsize = 1.2, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.05, low.spacing = 0.58,
        col.low = c("black", "green4","green4", "green4"),
        bor.col.low = c("black", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue",  "blue", "blue"),
        bor.col.high= c("red", "red", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue",  "blue", "blue"))


# extract bipartite plot if required
pdf(file = "Tiw_Monoculture_Reforestation_bipartite_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(xpd=T)
plotweb(sortweb(TiwMonomat1, sort.order="dec"), method = "normal", labsize = 1.2, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.05, low.spacing = 0.58,
        col.low = c("black", "green4","green4", "green4"),
        bor.col.low = c("black", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue",  "blue", "blue"),
        bor.col.high= c("red", "red", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue",  "blue", "blue"))
dev.off()

### INDICES Tiwoho Monoculture Reforestation  ###

# Network and group indices Tiwoho Monoculture Reforestation
networklevel(TiwMonomat1)
# export indices as a csv
write.csv(networklevel(TiwMonomat1), file = "TiwMono_network_level_indices.csv")

# Species level indices Tiwoho Monoculture Reforestation
specieslevel(TiwMonomat1, level="lower")

# extract for both lower level (plants/sediment) 
write.csv(specieslevel(TiwMonomat1, level="lower"), file = "TiwMono_species_level_indices-lower.csv")


######################################################
### Tiwoho Mixed Species Ecological Restoration ####
######################################################


dfTiwMixed <- subset(dat1, Site == "Tiwoho" & Treatment == "Mixed Species Regeneration",
                     select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TiwMixedm1 <- table( dfTiwMixed )
(TiwMixedmat1 <- unclass(TiwMixedm1))
class(TiwMixedmat1)

# export matrix as csv if desired
#write.csv(TiwMixedmat1, file = "TiwMixed_interaction_matrix.csv")

### PLOTTING Tiwoho Mixed Species Ecological Restoration  ###

# basic bipartite plot for Tiwoho Mixed Species Ecological Restoration
par(xpd=T)
plotweb(sortweb(TiwMixedmat1, sort.order="dec"), method = "normal", labsize = 1.1, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.04, low.spacing = 0.28,
        col.low = c("black", "green4", "green4","green4", "green4", "brown",
                    "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "brown",
                        "green4")
,
        col.high = c("red", "red", "blue", "red", "red","blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue","blue")
,
        bor.col.high= c("red", "red", "blue", "red", "red", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue","blue"))

# extract bipartite plot if required
pdf(file = "TiwMixed_Species_Regeneration_Bipartite_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(xpd=T)
plotweb(sortweb(TiwMixedmat1, sort.order="dec"), method = "normal", labsize = 1.1, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.04, low.spacing = 0.28,
        col.low = c("black", "green4", "green4","green4", "green4", "brown",
                    "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "brown",
                        "green4")
        ,
        col.high = c("red", "red", "blue", "red", "red","blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue","blue")
        ,
        bor.col.high= c("red", "red", "blue", "red", "red", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue","blue"))
dev.off()


### INDICES Tiwoho Mixed Species Ecological Restoration  ###

# Network and group indices Tiwoho Mixed Species Ecological Restoration
networklevel(TiwMixedmat1)
# export indices as a csv
write.csv(networklevel(TiwMixedmat1), file = "TiwMixed_network_level_indices.csv")

# Species level indices Tiwoho Mixed Species Ecological Restoration
specieslevel(TiwMixedmat1, level="lower")

# extract for both lower level (plants/sediment) 
write.csv(specieslevel(TiwMixedmat1, level="lower"), file = "TiwMixed_species_level_indices-lower.csv")


######################################################
### Tiwoho Reference Forest ####
######################################################

dfTiwReference_Forest <- subset(dat1, Site == "Tiwoho" & Treatment == "Reference Forest",
                       select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TiwReference_Forestm1 <- table( dfTiwReference_Forest )
(TiwReference_Forestmat1 <- unclass(TiwReference_Forestm1))
class(TiwReference_Forestmat1)

# export matrix as csv if desired
#write.csv(TiwReference_Forestmat1, file = "TiwReference_Forest_interaction_matrix.csv")

### PLOTTING Tiwoho Reference Forest  ###


# basic bipartite plot for Tiwoho Reference Forest

par(xpd=T)
plotweb(sortweb(TiwReference_Forestmat1, sort.order="dec"), method = "normal", labsize = 1.1, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.03, low.spacing = 0.27,
        col.low = c("black", "green4", "green4","green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "purple", "red", "red", "red", "red", "red",
                     "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue",  "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue")
        ,
        bor.col.high= c("red", "red", "purple", "red", "red", "red", "red", "red",
                        "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue",  "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue"))

# extract bipartite plot if required
pdf(file = "TiwReference_Forest_bipartite_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(xpd=T)
plotweb(sortweb(TiwReference_Forestmat1, sort.order="dec"), method = "normal", labsize = 1.1, text.rot=90, 
        adj.high =0, adj.low = 1, ybig= 1.2, high.spacing= 0.03, low.spacing = 0.27,
        col.low = c("black", "green4", "green4","green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "purple", "red", "red", "red", "red", "red",
                     "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue",  "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue")
        ,
        bor.col.high= c("red", "red", "purple", "red", "red", "red", "red", "red",
                        "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue",  "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue"))
dev.off()


### INDICES Tiwoho Reference Forest  ###

# Network and group indices Tiwoho Reference Forest
networklevel(TiwReference_Forestmat1)
# export indices as a csv
write.csv(networklevel(TiwReference_Forestmat1), file = "TiwReference_Forest_network_level_indices.csv")

# Species level indices Tiwoho Reference Forest
specieslevel(TiwReference_Forestmat1, level="lower")

# extract for both lower level (plants/sediment) 
write.csv(specieslevel(TiwReference_Forestmat1, level="lower"), file = "TiwReference_Forest_species_level_indices-lower.csv")


#######################################################
##### Plotting all bipartite plots together ###########
#######################################################
dev.off()

par(mfrow=c(3,2),oma = c(0, 2, 2, 0))
par(xpd=T)
plotweb(sortweb(LikMonomat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0,
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.04, low.spacing = 0.21,
        col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        col.high = c("red", "red", "red", "blue", "red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "blue", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))
plotweb(sortweb(TiwMonomat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.58,
        col.low = c("black", "green4","green4", "green4"),
        bor.col.low = c("black", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue",  "blue", "blue"),
        bor.col.high= c("red", "red", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue",  "blue", "blue"))
plotweb(sortweb(LikMixedmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0,
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.24,
        col.low = c("black", "green4", "green4","green4", "green4", "green4",
                    "green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "green4",
                        "green4", "green4"),
        col.high = c("red", "red", "red", "red", "blue", "red", "red",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red", "blue", "red", "red",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue"))
plotweb(sortweb(TiwMixedmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.04, low.spacing = 0.28,
        col.low = c("black", "green4", "green4","green4", "green4", "brown",
                    "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "brown",
                        "green4")
        ,
        col.high = c("red", "red", "blue", "red", "red","blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue","blue")
        ,
        bor.col.high= c("red", "red", "blue", "red", "red", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue","blue"))
plotweb(sortweb(LikReference_Forestmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.2,
        col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                    "brown","green4",  "green4", "green4", "green4"),
        bor.col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                        "brown","green4", "green4", "green4", "green4"),
        col.high = c("red", "red", "red", "red","red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red","red", "red", "blue",  
                        "blue", "blue", "blue", "blue", "blue","blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue", "blue", "blue", "blue", "blue"))
plotweb(sortweb(TiwReference_Forestmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.03, low.spacing = 0.27,
        col.low = c("black", "green4", "green4","green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "purple", "red", "red", "red", "red", "red",
                     "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue",  "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue")
        ,
        bor.col.high= c("red", "red", "purple", "red", "red", "red", "red", "red",
                        "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue",  "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue"))

mtext(text = "Likupang                                                Tiwoho",side=3,line=-0.5, outer = TRUE, cex = 1.5, las=0)
mtext("    Reference             Mixed Res           Mono Res",side=2,line=-3,outer=TRUE,cex=1.5,las=0)


########################################################
######  export bipartite panel plot###########
####################################################

pdf(file = "Bipartite_panel_plot.pdf", width = 8, height = 6, family = "Helvetica")
par(mfrow=c(3,2),oma = c(0, 2, 2, 0))
par(xpd=T)
plotweb(sortweb(LikMonomat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0,
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.04, low.spacing = 0.21,
        col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        col.high = c("red", "red", "red", "blue", "red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "blue", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))
plotweb(sortweb(TiwMonomat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.58,
        col.low = c("black", "green4","green4", "green4"),
        bor.col.low = c("black", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue",  "blue", "blue"),
        bor.col.high= c("red", "red", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue",  "blue", "blue"))
plotweb(sortweb(LikMixedmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0,
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.24,
        col.low = c("black", "green4", "green4","green4", "green4", "green4",
                    "green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "green4",
                        "green4", "green4"),
        col.high = c("red", "red", "red", "red", "blue", "red", "red",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red", "blue", "red", "red",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue"))
plotweb(sortweb(TiwMixedmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.04, low.spacing = 0.28,
        col.low = c("black", "green4", "green4","green4", "green4", "brown",
                    "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "brown",
                        "green4")
        ,
        col.high = c("red", "red", "blue", "red", "red","blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue","blue")
        ,
        bor.col.high= c("red", "red", "blue", "red", "red", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue","blue"))
plotweb(sortweb(LikReference_Forestmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.2,
        col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                    "brown","green4",  "green4", "green4", "green4"),
        bor.col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                        "brown","green4", "green4", "green4", "green4"),
        col.high = c("red", "red", "red", "red","red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red","red", "red", "blue",  
                        "blue", "blue", "blue", "blue", "blue","blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue", "blue", "blue", "blue", "blue"))
plotweb(sortweb(TiwReference_Forestmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.03, low.spacing = 0.27,
        col.low = c("black", "green4", "green4","green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "purple", "red", "red", "red", "red", "red",
                     "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue",  "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue")
        ,
        bor.col.high= c("red", "red", "purple", "red", "red", "red", "red", "red",
                        "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue",  "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue"))

mtext(text = "Likupang                                                Tiwoho",side=3,line=-0.5, outer = TRUE, cex = 1.5, las=0)
mtext("      Reference            Mixed Reg           Mono Res",side=2,line=-3,outer=TRUE,cex=1.5,las=0)

dev.off()



# TIFF
tiff("Bipartite_panel_plot.tiff", height = 15, width = 17, units = 'cm', res = 300)
par(mfrow=c(3,2),oma = c(0, 2, 2, 0))
par(xpd=T)
plotweb(sortweb(LikMonomat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0,
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.04, low.spacing = 0.21,
        col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "brown"),
        col.high = c("red", "red", "red", "blue", "red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "blue", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))
plotweb(sortweb(TiwMonomat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.58,
        col.low = c("black", "green4","green4", "green4"),
        bor.col.low = c("black", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue",  "blue", "blue"),
        bor.col.high= c("red", "red", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue",  "blue", "blue"))
plotweb(sortweb(LikMixedmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0,
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.24,
        col.low = c("black", "green4", "green4","green4", "green4", "green4",
                    "green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4", "green4",
                        "green4", "green4"),
        col.high = c("red", "red", "red", "red", "blue", "red", "red",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red", "blue", "red", "red",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue"))
plotweb(sortweb(TiwMixedmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.04, low.spacing = 0.28,
        col.low = c("black", "green4", "green4","green4", "green4", "brown",
                    "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "brown",
                        "green4")
        ,
        col.high = c("red", "red", "blue", "red", "red","blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue","blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue","blue")
        ,
        bor.col.high= c("red", "red", "blue", "red", "red", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue","blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue","blue"))
plotweb(sortweb(LikReference_Forestmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.05, low.spacing = 0.2,
        col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                    "brown","green4",  "green4", "green4", "green4"),
        bor.col.low = c("green4","black", "green4", "green4","green4", "green4", "green4",
                        "brown","green4", "green4", "green4", "green4"),
        col.high = c("red", "red", "red", "red","red", "red", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", 
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue", "blue", "blue", "blue", "blue"),
        bor.col.high= c("red", "red", "red", "red","red", "red", "blue",  
                        "blue", "blue", "blue", "blue", "blue","blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", 
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue", "blue", "blue", "blue", "blue"))
plotweb(sortweb(TiwReference_Forestmat1, sort.order="dec"), method = "normal", high.lablength =0, low.lablength = 0, 
        adj.high =0, adj.low = 1, ybig= 0.7, high.spacing= 0.03, low.spacing = 0.27,
        col.low = c("black", "green4", "green4","green4", "green4"),
        bor.col.low = c("black", "green4", "green4","green4", "green4")
        ,
        col.high = c("red", "red", "purple", "red", "red", "red", "red", "red",
                     "blue", "blue", "blue", "blue", "blue",
                     "blue", "blue", "blue", "blue",  "blue", "blue",
                     "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                     , "blue")
        ,
        bor.col.high= c("red", "red", "purple", "red", "red", "red", "red", "red",
                        "blue", "blue", "blue", "blue", "blue",
                        "blue", "blue", "blue", "blue",  "blue", "blue",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue"
                        , "blue"))

mtext(text = "Likupang                                    Tiwoho",side=3,line=-0.5, outer = TRUE, cex = 1.5, las=0)
mtext("       Reference           Mixed Reg          Mono Res",side=2,line=-3,outer=TRUE,cex=1.5,las=0)

dev.off()

#################################################################################
########## INTERACTION TURNOVER AND BETADIVERSITY WITHIN SiteS ###################
#################################################################################

# Compute network dissimilarities between Mono, Mixed and Reference Forest treatments
library(betalink)

### Likupang Site, between treatment comparisons
multilayer1<-list(LikMonomat1,LikMixedmat1,LikReference_Forestmat1)
networks1<-prepare_networks(multilayer1, directed = TRUE)

betalink(networks1[[1]],networks1[[2]], bf = B10) #B10 is the Jaccard beta-diversity index
betalink(networks1[[2]],networks1[[3]], bf = B10)
betalink(networks1[[3]],networks1[[1]], bf = B10)

## plots showing items unique to network 1 (green), unique to network 2 (blue)
## and shared between the two networks (grey)
network_betaplot(networks1[[1]],networks1[[2]], na = "#2ca02c", nb = "#1f77b4", ns = "grey")
network_betaplot(networks1[[2]],networks1[[3]], na = "#2ca02c", nb = "#1f77b4", ns = "grey")
network_betaplot(networks1[[3]],networks1[[1]], na = "#2ca02c", nb = "#1f77b4", ns = "grey")


### Tiwoho Site, between treatment comparisons
multilayer2<-list(TiwMonomat1,TiwMixedmat1,TiwReference_Forestmat1)
networks2<-prepare_networks(multilayer2, directed = TRUE)

betalink(networks2[[1]],networks2[[2]], bf = B10) #B10 is the Jaccard beta-diversity index
betalink(networks2[[2]],networks2[[3]], bf = B10)
betalink(networks2[[3]],networks2[[1]], bf = B10)


#################################################################################
########## INTERACTION TURNOVER AND BETADIVERSITY BETWEEN SiteS #################
#################################################################################

########
### EXTRACT AN INTERATION MATRIX FOR BOTH TIWOHO AND LIKUPANG AS A WHOLE SITE, NOT SPLIT BY TREATMENT

dfLik <- subset(dat1, Site == "Likupang",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
Likm1 <- table( dfLik )
(Likmat1 <- unclass(Likm1))
class(Likmat1)

dfTiw <- subset(dat1, Site== "Tiwoho",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
Tiwm1 <- table( dfTiw )
(Tiwmat1 <- unclass(Tiwm1))
class(Tiwmat1)

### interaction turnover and betadiversity comparison between Tiwoho and Likupang
multilayer3<-list(Tiwmat1,Likmat1)
networks3<-prepare_networks(multilayer3, directed = TRUE)
betalink(networks3[[1]],networks3[[2]], bf = B10) #B10 is the Jaccard beta-diversity index



# Housekeeping
graphics.off() 
rm(list=ls())