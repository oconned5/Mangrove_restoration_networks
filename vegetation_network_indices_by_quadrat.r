##############################################################
#### CALCULATING VEGETATION NETWORK INDICES FOR EACH QUADRAT


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

dfLKA <- subset(dat1, Quadrat == "LKA"
               & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                    select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKAm1 <- table( dfLKA )
(LKAmat1 <- unclass(LKAm1))
class(LKAmat1)

### INDICES Likupang LKA  ###

# Network and group indices LKA 
networklevel(LKAmat1)
# export indices as a csv
write.csv(networklevel(LKAmat1), file = "LKA_vegetation_network_level_indices.csv")


###  LKB ###

dfLKB <- subset(dat1, Quadrat == "LKB"
               & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKBm1 <- table( dfLKB )
(LKBmat1 <- unclass(LKBm1))
class(LKBmat1)

### INDICES Likupang LKB  ###

# Network and group indices LKB 
networklevel(LKBmat1)
# export indices as a csv
write.csv(networklevel(LKBmat1), file = "LKB_vegetation_network_level_indices.csv")


###  LKC ###

dfLKC <- subset(dat1, Quadrat == "LKC"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKCm1 <- table( dfLKC )
(LKCmat1 <- unclass(LKCm1))
class(LKCmat1)

### INDICES Likupang LKC  ###


# Network and group indices LKC 
networklevel(LKCmat1)
# export indices as a csv
write.csv(networklevel(LKCmat1), file = "LKC_vegetation_network_level_indices.csv")


###  LK4 ###


dfLK4 <- subset(dat1, Quadrat == "LK4"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK4m1 <- table( dfLK4 )
(LK4mat1 <- unclass(LK4m1))
class(LK4mat1)

### INDICES Likupang LK4  ###

# Network and group indices LK4 
networklevel(LK4mat1)
# export indices as a csv
write.csv(networklevel(LK4mat1), file = "LK4_vegetation_network_level_indices.csv")


###  LK5 ###


dfLK5 <- subset(dat1, Quadrat == "LK5"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK5m1 <- table( dfLK5 )
(LK5mat1 <- unclass(LK5m1))
class(LK5mat1)

### INDICES Likupang LK5  ###

# Network and group indices LK5 
networklevel(LK5mat1)
# export indices as a csv
write.csv(networklevel(LK5mat1), file = "LK5_vegetation_network_level_indices.csv")


###  LK6 ###


dfLK6 <- subset(dat1, Quadrat == "LK6"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK6m1 <- table( dfLK6 )
(LK6mat1 <- unclass(LK6m1))
class(LK6mat1)

### INDICES Likupang LK6  ###

# Network and group indices LK6 
networklevel(LK6mat1)
# export indices as a csv
write.csv(networklevel(LK6mat1), file = "LK6_vegetation_network_level_indices.csv")


#########################################
### Likupang Mixed Species Regeneration ##########
#########################################

###  LK7 ###


dfLK7 <- subset(dat1, Quadrat == "LK7"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK7m1 <- table( dfLK7 )
(LK7mat1 <- unclass(LK7m1))
class(LK7mat1)

### INDICES Likupang LK7  ###

# Network and group indices LK7 
networklevel(LK7mat1)
# export indices as a csv
write.csv(networklevel(LK7mat1), file = "LK7_vegetation_network_level_indices.csv")


###  LK8 ###


dfLK8 <- subset(dat1, Quadrat == "LK8"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK8m1 <- table( dfLK8 )
(LK8mat1 <- unclass(LK8m1))
class(LK8mat1)

### INDICES Likupang LK8  ###

# Network and group indices LK8 
networklevel(LK8mat1)
# export indices as a csv
write.csv(networklevel(LK8mat1), file = "LK8_vegetation_network_level_indices.csv")


###  LK9 ###


dfLK9 <- subset(dat1, Quadrat == "LK9"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK9m1 <- table( dfLK9 )
(LK9mat1 <- unclass(LK9m1))
class(LK9mat1)

### INDICES Likupang LK9  ###

# Network and group indices LK9 
networklevel(LK9mat1)
# export indices as a csv
write.csv(networklevel(LK9mat1), file = "LK9_vegetation_network_level_indices.csv")


###  LK10 ###


dfLK10 <- subset(dat1, Quadrat == "LK10"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK10m1 <- table( dfLK10 )
(LK10mat1 <- unclass(LK10m1))
class(LK10mat1)

### INDICES Likupang LK10  ###

# Network and group indices LK10 
networklevel(LK10mat1)
# export indices as a csv
write.csv(networklevel(LK10mat1), file = "LK10_vegetation_network_level_indices.csv")


###  LK11 ###


dfLK11 <- subset(dat1, Quadrat == "LK11"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK11m1 <- table( dfLK11 )
(LK11mat1 <- unclass(LK11m1))
class(LK11mat1)

### INDICES Likupang LK11  ###

# Network and group indices LK11 
networklevel(LK11mat1)
# export indices as a csv
write.csv(networklevel(LK11mat1), file = "LK11_vegetation_network_level_indices.csv")


###  LK12 ###


dfLK12 <- subset(dat1, Quadrat == "LK12"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK12m1 <- table( dfLK12 )
(LK12mat1 <- unclass(LK12m1))
class(LK12mat1)

### INDICES Likupang LK12  ###

# Network and group indices LK12 
networklevel(LK12mat1)
# export indices as a csv
write.csv(networklevel(LK12mat1), file = "LK12_vegetation_network_level_indices.csv")


#########################################
### Likupang Reference Forest ####################
#########################################

###  LK13 ###


dfLK13 <- subset(dat1, Quadrat == "LK13"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK13m1 <- table( dfLK13 )
(LK13mat1 <- unclass(LK13m1))
class(LK13mat1)

### INDICES Likupang LK13  ###

# Network and group indices LK13 
networklevel(LK13mat1)
# export indices as a csv
write.csv(networklevel(LK13mat1), file = "LK13_vegetation_network_level_indices.csv")


###  LK14 ###


dfLK14 <- subset(dat1, Quadrat == "LK14"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK14m1 <- table( dfLK14 )
(LK14mat1 <- unclass(LK14m1))
class(LK14mat1)

### INDICES Likupang LK14  ###

# Network and group indices LK14 
networklevel(LK14mat1)
# export indices as a csv
write.csv(networklevel(LK14mat1), file = "LK14_vegetation_network_level_indices.csv")

###  LK15 ###


dfLK15 <- subset(dat1, Quadrat == "LK15"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK15m1 <- table( dfLK15 )
(LK15mat1 <- unclass(LK15m1))
class(LK15mat1)

### INDICES Likupang LK15  ###

# Network and group indices LK15 
networklevel(LK15mat1)
# export indices as a csv
write.csv(networklevel(LK15mat1), file = "LK15_vegetation_network_level_indices.csv")


###  LK16 ###


dfLK16 <- subset(dat1, Quadrat == "LK16"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK16m1 <- table( dfLK16 )
(LK16mat1 <- unclass(LK16m1))
class(LK16mat1)

### INDICES Likupang LK16  ###

# Network and group indices LK16 
networklevel(LK16mat1)
# export indices as a csv
write.csv(networklevel(LK16mat1), file = "LK16_vegetation_network_level_indices.csv")


###  LK17 ###


dfLK17 <- subset(dat1, Quadrat == "LK17"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK17m1 <- table( dfLK17 )
(LK17mat1 <- unclass(LK17m1))
class(LK17mat1)

### INDICES Likupang LK17  ###

# Network and group indices LK17 
networklevel(LK17mat1)
# export indices as a csv
write.csv(networklevel(LK17mat1), file = "LK17_vegetation_network_level_indices.csv")


###  LK18 ###

dfLK18 <- subset(dat1, Quadrat == "LK18"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK18m1 <- table( dfLK18 )
(LK18mat1 <- unclass(LK18m1))
class(LK18mat1)

### INDICES Likupang LK18  ###

# Network and group indices LK18 
networklevel(LK18mat1)
# export indices as a csv
write.csv(networklevel(LK18mat1), file = "LK18_vegetation_network_level_indices_no-video.csv")


#########################################
### Tiwoho Monoculture Reforestation ####
#########################################


###  TW1 ###


dfTW1 <- subset(dat1, Quadrat == "TW1"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW1m1 <- table( dfTW1 )
(TW1mat1 <- unclass(TW1m1))
class(TW1mat1)

### INDICES Tiwoho TW1  ###

# Network and group indices TW1 
networklevel(TW1mat1)
# export indices as a csv
write.csv(networklevel(TW1mat1), file = "TW1_vegetation_network_level_indices.csv")


###  TW2 ###


dfTW2 <- subset(dat1, Quadrat == "TW2"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW2m1 <- table( dfTW2 )
(TW2mat1 <- unclass(TW2m1))
class(TW2mat1)

### INDICES Tiwoho TW2  ###

# Network and group indices TW2 
networklevel(TW2mat1)
# export indices as a csv
write.csv(networklevel(TW2mat1), file = "TW2_vegetation_network_level_indices.csv")


###  TW3 ###


dfTW3 <- subset(dat1, Quadrat == "TW3"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW3m1 <- table( dfTW3 )
(TW3mat1 <- unclass(TW3m1))
class(TW3mat1)

### INDICES Tiwoho TW3  ###

# Network and group indices TW3 
networklevel(TW3mat1)
# export indices as a csv
write.csv(networklevel(TW3mat1), file = "TW3_vegetation_network_level_indices.csv")


###  TW4 ###


dfTW4 <- subset(dat1, Quadrat == "TW4"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW4m1 <- table( dfTW4 )
(TW4mat1 <- unclass(TW4m1))
class(TW4mat1)

### INDICES Tiwoho TW4  ###

# Network and group indices TW4 
networklevel(TW4mat1)
# export indices as a csv
write.csv(networklevel(TW4mat1), file = "TW4_vegetation_network_level_indices.csv")


###  TW5 ###


dfTW5 <- subset(dat1, Quadrat == "TW5"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW5m1 <- table( dfTW5 )
(TW5mat1 <- unclass(TW5m1))
class(TW5mat1)

### INDICES Tiwoho TW5  ###

# Network and group indices TW5 
networklevel(TW5mat1)
# export indices as a csv
write.csv(networklevel(TW5mat1), file = "TW5_vegetation_network_level_indices.csv")


###  TW6 ###


dfTW6 <- subset(dat1, Quadrat == "TW6"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW6m1 <- table( dfTW6 )
(TW6mat1 <- unclass(TW6m1))
class(TW6mat1)

### INDICES Tiwoho TW6  ###

# Network and group indices TW6 
networklevel(TW6mat1)
# export indices as a csv
write.csv(networklevel(TW6mat1), file = "TW6_vegetation_network_level_indices.csv")


#########################################
### Tiwoho Mixed Species Regeneration ############
#########################################

###  TW7 ###


dfTW7 <- subset(dat1, Quadrat == "TW7"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW7m1 <- table( dfTW7 )
(TW7mat1 <- unclass(TW7m1))
class(TW7mat1)

### INDICES Tiwoho TW7  ###

# Network and group indices TW7 
networklevel(TW7mat1)
# export indices as a csv
write.csv(networklevel(TW7mat1), file = "TW7_vegetation_network_level_indices.csv")


###  TW8 ###


dfTW8 <- subset(dat1, Quadrat == "TW8"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW8m1 <- table( dfTW8 )
(TW8mat1 <- unclass(TW8m1))
class(TW8mat1)

### INDICES Tiwoho TW8  ###

# Network and group indices TW8 
networklevel(TW8mat1)
# export indices as a csv
write.csv(networklevel(TW8mat1), file = "TW8_vegetation_network_level_indices.csv")


###  TW9 ###


dfTW9 <- subset(dat1, Quadrat == "TW9"
                & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW9m1 <- table( dfTW9 )
(TW9mat1 <- unclass(TW9m1))
class(TW9mat1)

### INDICES Tiwoho TW9  ###

# Network and group indices TW9 
networklevel(TW9mat1)
# export indices as a csv
write.csv(networklevel(TW9mat1), file = "TW9_vegetation_network_level_indices.csv")


###  TW10 ###


dfTW10 <- subset(dat1, Quadrat == "TW10"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW10m1 <- table( dfTW10 )
(TW10mat1 <- unclass(TW10m1))
class(TW10mat1)

### INDICES Tiwoho TW10  ###

# Network and group indices TW10 
networklevel(TW10mat1)
# export indices as a csv
write.csv(networklevel(TW10mat1), file = "TW10_vegetation_network_level_indices.csv")

###  TW11 ###


dfTW11 <- subset(dat1, Quadrat == "TW11"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW11m1 <- table( dfTW11 )
(TW11mat1 <- unclass(TW11m1))
class(TW11mat1)

### INDICES Tiwoho TW11  ###

# Network and group indices TW11 
networklevel(TW11mat1)
# export indices as a csv
write.csv(networklevel(TW11mat1), file = "TW11_vegetation_network_level_indices.csv")


###  TW12 ###


dfTW12 <- subset(dat1, Quadrat == "TW12"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW12m1 <- table( dfTW12 )
(TW12mat1 <- unclass(TW12m1))
class(TW12mat1)

### INDICES Tiwoho TW12  ###

# Network and group indices TW12 
networklevel(TW12mat1)
# export indices as a csv
write.csv(networklevel(TW12mat1), file = "TW12_vegetation_network_level_indices.csv")


#########################################
####### Tiwoho Reference Forest ##################
#########################################


###  TW13 ###


dfTW13 <- subset(dat1, Quadrat == "TW13"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW13m1 <- table( dfTW13 )
(TW13mat1 <- unclass(TW13m1))
class(TW13mat1)

### INDICES Tiwoho TW13  ###

# Network and group indices TW13 
networklevel(TW13mat1)
# export indices as a csv
write.csv(networklevel(TW13mat1), file = "TW13_vegetation_network_level_indices.csv")


###  TW14 ###


dfTW14 <- subset(dat1, Quadrat == "TW14"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW14m1 <- table( dfTW14 )
(TW14mat1 <- unclass(TW14m1))
class(TW14mat1)

### INDICES Tiwoho TW14  ###

# Network and group indices TW14 
networklevel(TW14mat1)
# export indices as a csv
write.csv(networklevel(TW14mat1), file = "TW14_vegetation_network_level_indices.csv")


###  TW15 ###


dfTW15 <- subset(dat1, Quadrat == "TW15"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW15m1 <- table( dfTW15 )
(TW15mat1 <- unclass(TW15m1))
class(TW15mat1)

### INDICES Tiwoho TW15  ###

# Network and group indices TW15 
networklevel(TW15mat1)
# export indices as a csv
write.csv(networklevel(TW15mat1), file = "TW15_vegetation_network_level_indices.csv")


###  TW16 ###


dfTW16 <- subset(dat1, Quadrat == "TW16"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW16m1 <- table( dfTW16 )
(TW16mat1 <- unclass(TW16m1))
class(TW16mat1)

### INDICES Tiwoho TW16  ###

# Network and group indices TW16 
networklevel(TW16mat1)
# export indices as a csv
write.csv(networklevel(TW16mat1), file = "TW16_vegetation_network_level_indices.csv")


###  TW17 ###


dfTW17 <- subset(dat1, Quadrat == "TW17"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW17m1 <- table( dfTW17 )
(TW17mat1 <- unclass(TW17m1))
class(TW17mat1)

### INDICES Tiwoho TW17  ###

# Network and group indices TW17 
networklevel(TW17mat1)
# export indices as a csv
write.csv(networklevel(TW17mat1), file = "TW17_vegetation_network_level_indices.csv")


###  TW18 ###


dfTW18 <- subset(dat1, Quadrat == "TW18"
                 & Network_lower_level != "sediment" & Network_lower_level_shorthand != "DW" & Method != "Video",
                 select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW18m1 <- table( dfTW18 )
(TW18mat1 <- unclass(TW18m1))
class(TW18mat1)

### INDICES Tiwoho TW18  ###

# Network and group indices TW18 
networklevel(TW18mat1)
# export indices as a csv
write.csv(networklevel(TW18mat1), file = "TW18_vegetation_network_level_indices.csv")


# Housekeeping
graphics.off() 
rm(list=ls())