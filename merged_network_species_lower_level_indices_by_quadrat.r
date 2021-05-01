##############################################################
#### CALCULATING SPECIES LEVEL INDICIES FOR
#### MERGED NETWORK LOWER LEVEL FOR EACH QUADRAT


# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Interaction_data.csv

# Packages
#install.packages("bipartite")


library(bipartite)

### SPLITTING OUT EACH Quadrat ###

#########################################
### Likupang Monoculture Reforestation ####
#########################################

###  LKA ###

dfLKA <- subset(dat1, Quadrat == "LKA",
                    select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKAm1 <- table( dfLKA )
(LKAmat1 <- unclass(LKAm1))
class(LKAmat1)

### INDICES Likupang LKA  ###

# Network and group indices LKA 
specieslevel(LKAmat1, level="lower")

# export indices as a csv
write.csv(specieslevel(LKAmat1, level="lower"), file = "LKA_merged_network_species_level_indices-lower.csv")

###  LKB ###

dfLKB <- subset(dat1, Quadrat == "LKB",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKBm1 <- table( dfLKB )
(LKBmat1 <- unclass(LKBm1))
class(LKBmat1)

### INDICES Likupang LKB  ###

# Network and group indices LKB 
specieslevel(LKBmat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LKBmat1, level="lower"), file = "LKB_merged_network_species_level_indices-lower.csv")

###  LKC ###

dfLKC <- subset(dat1, Quadrat == "LKC",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKCm1 <- table( dfLKC )
(LKCmat1 <- unclass(LKCm1))
class(LKCmat1)

### INDICES Likupang LKC  ###


# Network and group indices LKC 
specieslevel(LKCmat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LKCmat1, level="lower"), file = "LKC_merged_network_species_level_indices-lower.csv")

###  LK4 ###


dfLK4 <- subset(dat1, Quadrat == "LK4",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK4m1 <- table( dfLK4 )
(LK4mat1 <- unclass(LK4m1))
class(LK4mat1)

### INDICES Likupang LK4  ###

# Network and group indices LK4 
specieslevel(LK4mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK4mat1, level="lower"), file = "LK4_merged_network_species_level_indices-lower.csv")

###  LK5 ###


dfLK5 <- subset(dat1, Quadrat == "LK5",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK5m1 <- table( dfLK5 )
(LK5mat1 <- unclass(LK5m1))
class(LK5mat1)

### INDICES Likupang LK5  ###

# Network and group indices LK5 
specieslevel(LK5mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK5mat1, level="lower"), file = "LK5_merged_network_species_level_indices-lower.csv")

###  LK6 ###


dfLK6 <- subset(dat1, Quadrat == "LK6",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK6m1 <- table( dfLK6 )
(LK6mat1 <- unclass(LK6m1))
class(LK6mat1)

### INDICES Likupang LK6  ###

# Network and group indices LK6 
specieslevel(LK6mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK6mat1, level="lower"), file = "LK6_merged_network_species_level_indices-lower.csv")

#########################################
### Likupang Mixed Species Regeneration ##########
#########################################

###  LK7 ###


dfLK7 <- subset(dat1, Quadrat == "LK7",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK7m1 <- table( dfLK7 )
(LK7mat1 <- unclass(LK7m1))
class(LK7mat1)

### INDICES Likupang LK7  ###

# Network and group indices LK7 
specieslevel(LK7mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK7mat1, level="lower"), file = "LK7_merged_network_species_level_indices-lower.csv")

###  LK8 ###


dfLK8 <- subset(dat1, Quadrat == "LK8",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK8m1 <- table( dfLK8 )
(LK8mat1 <- unclass(LK8m1))
class(LK8mat1)

### INDICES Likupang LK8  ###

# Network and group indices LK8 
specieslevel(LK8mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK8mat1, level="lower"), file = "LK8_merged_network_species_level_indices-lower.csv")

###  LK9 ###


dfLK9 <- subset(dat1, Quadrat == "LK9",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK9m1 <- table( dfLK9 )
(LK9mat1 <- unclass(LK9m1))
class(LK9mat1)

### INDICES Likupang LK9  ###

# Network and group indices LK9 
specieslevel(LK9mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK9mat1, level="lower"), file = "LK9_merged_network_species_level_indices-lower.csv")

###  LK10 ###


dfLK10 <- subset(dat1, Quadrat == "LK10",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK10m1 <- table( dfLK10 )
(LK10mat1 <- unclass(LK10m1))
class(LK10mat1)

### INDICES Likupang LK10  ###

# Network and group indices LK10 
specieslevel(LK10mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK10mat1, level="lower"), file = "LK10_merged_network_species_level_indices-lower.csv")

###  LK11 ###


dfLK11 <- subset(dat1, Quadrat == "LK11",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK11m1 <- table( dfLK11 )
(LK11mat1 <- unclass(LK11m1))
class(LK11mat1)

### INDICES Likupang LK11  ###

# Network and group indices LK11 
specieslevel(LK11mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK11mat1, level="lower"), file = "LK11_merged_network_species_level_indices-lower.csv")

###  LK12 ###


dfLK12 <- subset(dat1, Quadrat == "LK12",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK12m1 <- table( dfLK12 )
(LK12mat1 <- unclass(LK12m1))
class(LK12mat1)

### INDICES Likupang LK12  ###

# Network and group indices LK12 
specieslevel(LK12mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK12mat1, level="lower"), file = "LK12_merged_network_species_level_indices-lower.csv")

#########################################
### Likupang Reference Forest ####################
#########################################

###  LK13 ###


dfLK13 <- subset(dat1, Quadrat == "LK13",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK13m1 <- table( dfLK13 )
(LK13mat1 <- unclass(LK13m1))
class(LK13mat1)

### INDICES Likupang LK13  ###

# Network and group indices LK13 
specieslevel(LK13mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK13mat1, level="lower"), file = "LK13_merged_network_species_level_indices-lower.csv")

###  LK14 ###


dfLK14 <- subset(dat1, Quadrat == "LK14",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK14m1 <- table( dfLK14 )
(LK14mat1 <- unclass(LK14m1))
class(LK14mat1)

### INDICES Likupang LK14  ###

# Network and group indices LK14 
specieslevel(LK14mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK14mat1, level="lower"), file = "LK14_merged_network_species_level_indices-lower.csv")

###  LK15 ###


dfLK15 <- subset(dat1, Quadrat == "LK15",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK15m1 <- table( dfLK15 )
(LK15mat1 <- unclass(LK15m1))
class(LK15mat1)

### INDICES Likupang LK15  ###

# Network and group indices LK15 
specieslevel(LK15mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK15mat1, level="lower"), file = "LK15_merged_network_species_level_indices-lower.csv")

###  LK16 ###


dfLK16 <- subset(dat1, Quadrat == "LK16",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK16m1 <- table( dfLK16 )
(LK16mat1 <- unclass(LK16m1))
class(LK16mat1)

### INDICES Likupang LK16  ###

# Network and group indices LK16 
specieslevel(LK16mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK16mat1, level="lower"), file = "LK16_merged_network_species_level_indices-lower.csv")

###  LK17 ###


dfLK17 <- subset(dat1, Quadrat == "LK17",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK17m1 <- table( dfLK17 )
(LK17mat1 <- unclass(LK17m1))
class(LK17mat1)

### INDICES Likupang LK17  ###

# Network and group indices LK17 
specieslevel(LK17mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(LK17mat1, level="lower"), file = "LK17_merged_network_species_level_indices-lower.csv")



#########################################
### Tiwoho Monoculture Reforestation ####
#########################################


###  TW1 ###


dfTW1 <- subset(dat1, Quadrat == "TW1",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW1m1 <- table( dfTW1 )
(TW1mat1 <- unclass(TW1m1))
class(TW1mat1)

### INDICES Tiwoho TW1  ###

# Network and group indices TW1 
specieslevel(TW1mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW1mat1, level="lower"), file = "TW1_merged_network_species_level_indices-lower.csv")

###  TW2 ###


dfTW2 <- subset(dat1, Quadrat == "TW2",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW2m1 <- table( dfTW2 )
(TW2mat1 <- unclass(TW2m1))
class(TW2mat1)

### INDICES Tiwoho TW2  ###

# Network and group indices TW2 
specieslevel(TW2mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW2mat1, level="lower"), file = "TW2_merged_network_species_level_indices-lower.csv")

###  TW3 ###


dfTW3 <- subset(dat1, Quadrat == "TW3",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW3m1 <- table( dfTW3 )
(TW3mat1 <- unclass(TW3m1))
class(TW3mat1)

### INDICES Tiwoho TW3  ###

# Network and group indices TW3 
specieslevel(TW3mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW3mat1, level="lower"), file = "TW3_merged_network_species_level_indices-lower.csv")

###  TW4 ###


dfTW4 <- subset(dat1, Quadrat == "TW4",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW4m1 <- table( dfTW4 )
(TW4mat1 <- unclass(TW4m1))
class(TW4mat1)

### INDICES Tiwoho TW4  ###

# Network and group indices TW4 
specieslevel(TW4mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW4mat1, level="lower"), file = "TW4_merged_network_species_level_indices-lower.csv")

###  TW5 ###


dfTW5 <- subset(dat1, Quadrat == "TW5",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW5m1 <- table( dfTW5 )
(TW5mat1 <- unclass(TW5m1))
class(TW5mat1)

### INDICES Tiwoho TW5  ###

# Network and group indices TW5 
specieslevel(TW5mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW5mat1, level="lower"), file = "TW5_merged_network_species_level_indices-lower.csv")

###  TW6 ###


dfTW6 <- subset(dat1, Quadrat == "TW6",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW6m1 <- table( dfTW6 )
(TW6mat1 <- unclass(TW6m1))
class(TW6mat1)

### INDICES Tiwoho TW6  ###

# Network and group indices TW6 
specieslevel(TW6mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW6mat1, level="lower"), file = "TW6_merged_network_species_level_indices-lower.csv")

#########################################
### Tiwoho Mixed Species Regeneration ############
#########################################

###  TW7 ###


dfTW7 <- subset(dat1, Quadrat == "TW7",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW7m1 <- table( dfTW7 )
(TW7mat1 <- unclass(TW7m1))
class(TW7mat1)

### INDICES Tiwoho TW7  ###

# Network and group indices TW7 
specieslevel(TW7mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW7mat1, level="lower"), file = "TW7_merged_network_species_level_indices-lower.csv")

###  TW8 ###


dfTW8 <- subset(dat1, Quadrat == "TW8",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW8m1 <- table( dfTW8 )
(TW8mat1 <- unclass(TW8m1))
class(TW8mat1)

### INDICES Tiwoho TW8  ###

# Network and group indices TW8 
specieslevel(TW8mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW8mat1, level="lower"), file = "TW8_merged_network_species_level_indices-lower.csv")

###  TW9 ###


dfTW9 <- subset(dat1, Quadrat == "TW9",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW9m1 <- table( dfTW9 )
(TW9mat1 <- unclass(TW9m1))
class(TW9mat1)

### INDICES Tiwoho TW9  ###

# Network and group indices TW9 
specieslevel(TW9mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW9mat1, level="lower"), file = "TW9_merged_network_species_level_indices-lower.csv")

###  TW10 ###


dfTW10 <- subset(dat1, Quadrat == "TW10",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW10m1 <- table( dfTW10 )
(TW10mat1 <- unclass(TW10m1))
class(TW10mat1)

### INDICES Tiwoho TW10  ###

# Network and group indices TW10 
specieslevel(TW10mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW10mat1, level="lower"), file = "TW10_merged_network_species_level_indices-lower.csv")


###  TW11 ###


dfTW11 <- subset(dat1, Quadrat == "TW11",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW11m1 <- table( dfTW11 )
(TW11mat1 <- unclass(TW11m1))
class(TW11mat1)

### INDICES Tiwoho TW11  ###

# Network and group indices TW11 
specieslevel(TW11mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW11mat1, level="lower"), file = "TW11_merged_network_species_level_indices-lower.csv")

###  TW12 ###


dfTW12 <- subset(dat1, Quadrat == "TW12",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW12m1 <- table( dfTW12 )
(TW12mat1 <- unclass(TW12m1))
class(TW12mat1)

### INDICES Tiwoho TW12  ###

# Network and group indices TW12 
specieslevel(TW12mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW12mat1, level="lower"), file = "TW12_merged_network_species_level_indices-lower.csv")

#########################################
####### Tiwoho Reference Forest ##################
#########################################


###  TW13 ###


dfTW13 <- subset(dat1, Quadrat == "TW13",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW13m1 <- table( dfTW13 )
(TW13mat1 <- unclass(TW13m1))
class(TW13mat1)

### INDICES Tiwoho TW13  ###

# Network and group indices TW13 
specieslevel(TW13mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW13mat1, level="lower"), file = "TW13_merged_network_species_level_indices-lower.csv")
###  TW14 ###


dfTW14 <- subset(dat1, Quadrat == "TW14",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW14m1 <- table( dfTW14 )
(TW14mat1 <- unclass(TW14m1))
class(TW14mat1)

### INDICES Tiwoho TW14  ###

# Network and group indices TW14 
specieslevel(TW14mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW14mat1, level="lower"), file = "TW14_merged_network_species_level_indices-lower.csv")

###  TW15 ###


dfTW15 <- subset(dat1, Quadrat == "TW15",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW15m1 <- table( dfTW15 )
(TW15mat1 <- unclass(TW15m1))
class(TW15mat1)

### INDICES Tiwoho TW15  ###

# Network and group indices TW15 
specieslevel(TW15mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW15mat1, level="lower"), file = "TW15_merged_network_species_level_indices-lower.csv")

###  TW16 ###


dfTW16 <- subset(dat1, Quadrat == "TW16",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW16m1 <- table( dfTW16 )
(TW16mat1 <- unclass(TW16m1))
class(TW16mat1)

### INDICES Tiwoho TW16  ###

# Network and group indices TW16 
specieslevel(TW16mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW16mat1, level="lower"), file = "TW16_merged_network_species_level_indices-lower.csv")

###  TW17 ###


dfTW17 <- subset(dat1, Quadrat == "TW17",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW17m1 <- table( dfTW17 )
(TW17mat1 <- unclass(TW17m1))
class(TW17mat1)

### INDICES Tiwoho TW17  ###

# Network and group indices TW17 
specieslevel(TW17mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW17mat1, level="lower"), file = "TW17_merged_network_species_level_indices-lower.csv")

###  TW18 ###


dfTW18 <- subset(dat1, Quadrat == "TW18",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW18m1 <- table( dfTW18 )
(TW18mat1 <- unclass(TW18m1))
class(TW18mat1)

### INDICES Tiwoho TW18  ###

# Network and group indices TW18 
specieslevel(TW18mat1, level="lower")
# export indices as a csv
write.csv(specieslevel(TW18mat1, level="lower"), file = "TW18_merged_network_species_level_indices-lower.csv")

# Housekeeping
graphics.off() 
rm(list=ls())
