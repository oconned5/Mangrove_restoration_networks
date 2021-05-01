##############################################
### Calculating Modularity Index for each Quadrat, Merged Network

# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Interaction_data.csv



###################################################################################
######### Modularity ##################################################
################################################################################

# using compute modules function in Bipartite package

library(bipartite)


################################################################################
##########  SPLITING OUT BY Quadrat ###########################################
################################################################################

#### LKA ### 

df__LKA <- subset(dat1, Site == "Likupang" & Quadrat == "LKA",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKAm1_ <- table( df__LKA )
(LKAmat1_ <- unclass(LKAm1_))
class(LKAmat1_)

# export matrix as csv
write.csv(LKAmat1_, file = "LKA_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LKA__dataset <- read.csv("LKA_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LKA__matrix <- LKA__dataset[,-1]

### Modularity Q
mod_LKA__matrix <- computeModules(web=LKA__matrix, steps=1E8)

mod_LKA__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LKA__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LKA__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LKB ### 

df__LKB <- subset(dat1, Site == "Likupang" & Quadrat == "LKB",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKBm1_ <- table( df__LKB )
(LKBmat1_ <- unclass(LKBm1_))
class(LKBmat1_)

# export matrix as csv
write.csv(LKBmat1_, file = "LKB_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LKB__dataset <- read.csv("LKB_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LKB__matrix <- LKB__dataset[,-1]


### Modularity Q
mod_LKB__matrix <- computeModules(web=LKB__matrix, steps=1E8)

mod_LKB__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LKB__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LKB__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LKC ### 

df__LKC <- subset(dat1, Site == "Likupang" & Quadrat == "LKC",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKCm1_ <- table( df__LKC )
(LKCmat1_ <- unclass(LKCm1_))
class(LKCmat1_)

# export matrix as csv
write.csv(LKCmat1_, file = "LKC_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LKC__dataset <- read.csv("LKC_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LKC__matrix <- LKC__dataset[,-1]

### Modularity Q
mod_LKC__matrix <- computeModules(web=LKC__matrix, steps=1E8)

mod_LKC__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LKC__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LKC__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK4 

df__LK4 <- subset(dat1, Site == "Likupang" & Quadrat == "LK4",
                  select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK4m1_ <- table( df__LK4 )
(LK4mat1_ <- unclass(LK4m1_))
class(LK4mat1_)

# export matrix as csv
write.csv(LK4mat1_, file = "LK4_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK4__dataset <- read.csv("LK4_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK4__matrix <- LK4__dataset[,-1]

### Modularity Q
mod_LK4__matrix <- computeModules(web=LK4__matrix, steps=1E8)

mod_LK4__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK4__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK4__matrix@likelihood - mean(like.nulls))/sd(like.nulls))




#### LK5 

df__LK5 <- subset(dat1, Site == "Likupang" & Quadrat == "LK5",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK5m1_ <- table( df__LK5 )
(LK5mat1_ <- unclass(LK5m1_))
class(LK5mat1_)

# export matrix as csv
write.csv(LK5mat1_, file = "LK5_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK5__dataset <- read.csv("LK5_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK5__matrix <- LK5__dataset[,-1]

### Modularity Q
mod_LK5__matrix <- computeModules(web=LK5__matrix, steps=1E8)

mod_LK5__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK5__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK5__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK6 

df__LK6 <- subset(dat1, Site == "Likupang" & Quadrat == "LK6",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK6m1_ <- table( df__LK6 )
(LK6mat1_ <- unclass(LK6m1_))
class(LK6mat1_)

# export matrix as csv
write.csv(LK6mat1_, file = "LK6_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK6__dataset <- read.csv("LK6_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK6__matrix <- LK6__dataset[,-1]

### Modularity Q
mod_LK6__matrix <- computeModules(web=LK6__matrix, steps=1E8)

mod_LK6__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK6__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK6__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK7 

df__LK7 <- subset(dat1, Site == "Likupang" & Quadrat == "LK7",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK7m1_ <- table( df__LK7 )
(LK7mat1_ <- unclass(LK7m1_))
class(LK7mat1_)

# export matrix as csv
write.csv(LK7mat1_, file = "LK7_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK7__dataset <- read.csv("LK7_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK7__matrix <- LK7__dataset[,-1]

### Modularity Q
mod_LK7__matrix <- computeModules(web=LK7__matrix, steps=1E8)

mod_LK7__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK7__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK7__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK8 

df__LK8 <- subset(dat1, Site == "Likupang" & Quadrat == "LK8",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK8m1_ <- table( df__LK8 )
(LK8mat1_ <- unclass(LK8m1_))
class(LK8mat1_)

# export matrix as csv
write.csv(LK8mat1_, file = "LK8_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK8__dataset <- read.csv("LK8_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK8__matrix <- LK8__dataset[,-1]

### Modularity Q
mod_LK8__matrix <- computeModules(web=LK8__matrix, steps=1E8)

mod_LK8__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK8__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK8__matrix@likelihood - mean(like.nulls))/sd(like.nulls))




#### LK9 

df__LK9 <- subset(dat1, Site == "Likupang" & Quadrat == "LK9",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK9m1_ <- table( df__LK9 )
(LK9mat1_ <- unclass(LK9m1_))
class(LK9mat1_)

# export matrix as csv
write.csv(LK9mat1_, file = "LK9_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK9__dataset <- read.csv("LK9_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK9__matrix <- LK9__dataset[,-1]

### Modularity Q
mod_LK9__matrix <- computeModules(web=LK9__matrix, steps=1E8)

mod_LK9__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK9__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK9__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK10 

df__LK10 <- subset(dat1, Site == "Likupang" & Quadrat == "LK10",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK10m1_ <- table( df__LK10 )
(LK10mat1_ <- unclass(LK10m1_))
class(LK10mat1_)

# export matrix as csv
write.csv(LK10mat1_, file = "LK10_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK10__dataset <- read.csv("LK10_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK10__matrix <- LK10__dataset[,-1]

### Modularity Q
mod_LK10__matrix <- computeModules(web=LK10__matrix, steps=1E8)

mod_LK10__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK10__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK10__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK11 

df__LK11 <- subset(dat1, Site == "Likupang" & Quadrat == "LK11",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK11m1_ <- table( df__LK11 )
(LK11mat1_ <- unclass(LK11m1_))
class(LK11mat1_)

# export matrix as csv
write.csv(LK11mat1_, file = "LK11_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK11__dataset <- read.csv("LK11_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK11__matrix <- LK11__dataset[,-1]

# remove zeros
#LK11_new_mtx <- LK11__matrix [which(rowSums(LK11__matrix) > 0), ]



### Modularity Q
mod_LK11__matrix <- computeModules(web=LK11__matrix, steps=1E8)

mod_LK11__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK11__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK11__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK12 

df__LK12 <- subset(dat1, Site == "Likupang" & Quadrat == "LK12",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK12m1_ <- table( df__LK12 )
(LK12mat1_ <- unclass(LK12m1_))
class(LK12mat1_)

# export matrix as csv
write.csv(LK12mat1_, file = "LK12_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK12__dataset <- read.csv("LK12_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK12__matrix <- LK12__dataset[,-1]


### Modularity Q
mod_LK12__matrix <- computeModules(web=LK12__matrix, steps=1E8)

mod_LK12__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK12__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK12__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK13 

df__LK13 <- subset(dat1, Site == "Likupang" & Quadrat == "LK13",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK13m1_ <- table( df__LK13 )
(LK13mat1_ <- unclass(LK13m1_))
class(LK13mat1_)

# export matrix as csv
write.csv(LK13mat1_, file = "LK13_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK13__dataset <- read.csv("LK13_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK13__matrix <- LK13__dataset[,-1]


### Modularity Q
mod_LK13__matrix <- computeModules(web=LK13__matrix, steps=1E8)

mod_LK13__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK13__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK13__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK14 

df__LK14 <- subset(dat1, Site == "Likupang" & Quadrat == "LK14",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK14m1_ <- table( df__LK14 )
(LK14mat1_ <- unclass(LK14m1_))
class(LK14mat1_)

# export matrix as csv
write.csv(LK14mat1_, file = "LK14_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK14__dataset <- read.csv("LK14_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK14__matrix <- LK14__dataset[,-1]

### Modularity Q
mod_LK14__matrix <- computeModules(web=LK14__matrix, steps=1E8)

mod_LK14__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK14__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK14__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK15 

df__LK15 <- subset(dat1, Site == "Likupang" & Quadrat == "LK15",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK15m1_ <- table( df__LK15 )
(LK15mat1_ <- unclass(LK15m1_))
class(LK15mat1_)

# export matrix as csv
write.csv(LK15mat1_, file = "LK15_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK15__dataset <- read.csv("LK15_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK15__matrix <- LK15__dataset[,-1]

### Modularity Q
mod_LK15__matrix <- computeModules(web=LK15__matrix, steps=1E8)

mod_LK15__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK15__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK15__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK16 

df__LK16 <- subset(dat1, Site == "Likupang" & Quadrat == "LK16",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK16m1_ <- table( df__LK16 )
(LK16mat1_ <- unclass(LK16m1_))
class(LK16mat1_)

# export matrix as csv
write.csv(LK16mat1_, file = "LK16_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK16__dataset <- read.csv("LK16_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK16__matrix <- LK16__dataset[,-1]

### Modularity Q
mod_LK16__matrix <- computeModules(web=LK16__matrix, steps=1E8)

mod_LK16__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK16__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK16__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK17 

df__LK17 <- subset(dat1, Site == "Likupang" & Quadrat == "LK17",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK17m1_ <- table( df__LK17 )
(LK17mat1_ <- unclass(LK17m1_))
class(LK17mat1_)

# export matrix as csv
write.csv(LK17mat1_, file = "LK17_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK17__dataset <- read.csv("LK17_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK17__matrix <- LK17__dataset[,-1]

### Modularity Q
mod_LK17__matrix <- computeModules(web=LK17__matrix, steps=1E8)

mod_LK17__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK17__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK17__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#################################################
########## BY Quadrat ##############################
################## TIWOHO #######################


#### TW1 

df__TW1 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW1",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW1m1_ <- table( df__TW1 )
(TW1mat1_ <- unclass(TW1m1_))
class(TW1mat1_)

# export matrix as csv
write.csv(TW1mat1_, file = "TW1_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW1__dataset <- read.csv("TW1_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW1__matrix <- TW1__dataset[,-1]

### Modularity Q
mod_TW1__matrix <- computeModules(web=TW1__matrix, steps=1E8)

mod_TW1__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(TW1__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW1__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW2 

df__TW2 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW2",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW2m1_ <- table( df__TW2 )
(TW2mat1_ <- unclass(TW2m1_))
class(TW2mat1_)

# export matrix as csv
write.csv(TW2mat1_, file = "TW2_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW2__dataset <- read.csv("TW2_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW2__matrix <- TW2__dataset[,-1]

### Modularity Q
mod_TW2__matrix <- computeModules(web=TW2__matrix, steps=1E8)

mod_TW2__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(TW2__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW2__matrix@likelihood - mean(like.nulls))/sd(like.nulls))

#### TW3 

df__TW3 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW3",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW3m1_ <- table( df__TW3 )
(TW3mat1_ <- unclass(TW3m1_))
class(TW3mat1_)

# export matrix as csv
write.csv(TW3mat1_, file = "TW3_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW3__dataset <- read.csv("TW3_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW3__matrix <- TW3__dataset[,-1]

### Modularity Q
mod_TW3__matrix <- computeModules(web=TW3__matrix, steps=1E8)

mod_TW3__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW3__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW3__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW4 

df__TW4 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW4",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW4m1_ <- table( df__TW4 )
(TW4mat1_ <- unclass(TW4m1_))
class(TW4mat1_)

# export matrix as csv
write.csv(TW4mat1_, file = "TW4_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW4__dataset <- read.csv("TW4_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW4__matrix <- TW4__dataset[,-1]

### Modularity Q
mod_TW4__matrix <- computeModules(web=TW4__matrix, steps=1E8)

mod_TW4__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW4__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW4__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### TW5 

df__TW5 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW5",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW5m1_ <- table( df__TW5 )
(TW5mat1_ <- unclass(TW5m1_))
class(TW5mat1_)

# export matrix as csv
write.csv(TW5mat1_, file = "TW5_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW5__dataset <- read.csv("TW5_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW5__matrix <- TW5__dataset[,-1]


### Modularity Q
mod_TW5__matrix <- computeModules(web=TW5__matrix, steps=1E8)

mod_TW5__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW5__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW5__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW6 

df__TW6 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW6",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW6m1_ <- table( df__TW6 )
(TW6mat1_ <- unclass(TW6m1_))
class(TW6mat1_)

# export matrix as csv
write.csv(TW6mat1_, file = "TW6_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW6__dataset <- read.csv("TW6_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW6__matrix <- TW6__dataset[,-1]

### Modularity Q
mod_TW6__matrix <- computeModules(web=TW6__matrix, steps=1E8)

mod_TW6__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW6__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW6__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW7 

df__TW7 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW7",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW7m1_ <- table( df__TW7 )
(TW7mat1_ <- unclass(TW7m1_))
class(TW7mat1_)

# export matrix as csv
write.csv(TW7mat1_, file = "TW7_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW7__dataset <- read.csv("TW7_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW7__matrix <- TW7__dataset[,-1]

### Modularity Q
mod_TW7__matrix <- computeModules(web=TW7__matrix, steps=1E8)

mod_TW7__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW7__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW7__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW8 

df__TW8 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW8",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW8m1_ <- table( df__TW8 )
(TW8mat1_ <- unclass(TW8m1_))
class(TW8mat1_)

# export matrix as csv
write.csv(TW8mat1_, file = "TW8_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW8__dataset <- read.csv("TW8_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW8__matrix <- TW8__dataset[,-1]

### Modularity Q
mod_TW8__matrix <- computeModules(web=TW8__matrix, steps=1E8)

mod_TW8__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW8__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW8__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW9 

df__TW9 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW9",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW9m1_ <- table( df__TW9 )
(TW9mat1_ <- unclass(TW9m1_))
class(TW9mat1_)

# export matrix as csv
write.csv(TW9mat1_, file = "TW9_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW9__dataset <- read.csv("TW9_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW9__matrix <- TW9__dataset[,-1]

### Modularity Q
mod_TW9__matrix <- computeModules(web=TW9__matrix, steps=1E8)

mod_TW9__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW9__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW9__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### TW10 

df__TW10 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW10",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW10m1_ <- table( df__TW10 )
(TW10mat1_ <- unclass(TW10m1_))
class(TW10mat1_)

# export matrix as csv
write.csv(TW10mat1_, file = "TW10_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW10__dataset <- read.csv("TW10_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW10__matrix <- TW10__dataset[,-1]


### Modularity Q
mod_TW10__matrix <- computeModules(web=TW10__matrix, steps=1E8)

mod_TW10__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW10__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW10__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### TW11 

df__TW11 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW11",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW11m1_ <- table( df__TW11 )
(TW11mat1_ <- unclass(TW11m1_))
class(TW11mat1_)

# export matrix as csv
write.csv(TW11mat1_, file = "TW11_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW11__dataset <- read.csv("TW11_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW11__matrix <- TW11__dataset[,-1]

### Modularity Q
mod_TW11__matrix <- computeModules(web=TW11__matrix, steps=1E8)

mod_TW11__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW11__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW11__matrix@likelihood - mean(like.nulls))/sd(like.nulls))

#### TW12 

df__TW12 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW12",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW12m1_ <- table( df__TW12 )
(TW12mat1_ <- unclass(TW12m1_))
class(TW12mat1_)

# export matrix as csv
write.csv(TW12mat1_, file = "TW12_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW12__dataset <- read.csv("TW12_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW12__matrix <- TW12__dataset[,-1]


### Modularity Q
mod_TW12__matrix <- computeModules(web=TW12__matrix, steps=1E8)

mod_TW12__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW12__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW12__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### TW13 

df__TW13 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW13",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW13m1_ <- table( df__TW13 )
(TW13mat1_ <- unclass(TW13m1_))
class(TW13mat1_)

# export matrix as csv
write.csv(TW13mat1_, file = "TW13_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW13__dataset <- read.csv("TW13_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW13__matrix <- TW13__dataset[,-1]

### Modularity Q
mod_TW13__matrix <- computeModules(web=TW13__matrix, steps=1E8)

mod_TW13__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW13__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW13__matrix@likelihood - mean(like.nulls))/sd(like.nulls))




#### TW14 

df__TW14 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW14",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW14m1_ <- table( df__TW14 )
(TW14mat1_ <- unclass(TW14m1_))
class(TW14mat1_)

# export matrix as csv
write.csv(TW14mat1_, file = "TW14_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW14__dataset <- read.csv("TW14_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW14__matrix <- TW14__dataset[,-1]


### Modularity Q
mod_TW14__matrix <- computeModules(web=TW14__matrix, steps=1E8)

mod_TW14__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW14__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW14__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW15 

df__TW15 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW15",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW15m1_ <- table( df__TW15 )
(TW15mat1_ <- unclass(TW15m1_))
class(TW15mat1_)

# export matrix as csv
write.csv(TW15mat1_, file = "TW15_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW15__dataset <- read.csv("TW15_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW15__matrix <- TW15__dataset[,-1]



### Modularity Q
mod_TW15__matrix <- computeModules(web=TW15__matrix, steps=1E8)

mod_TW15__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW15__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW15__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### TW16 

df__TW16 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW16",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW16m1_ <- table( df__TW16 )
(TW16mat1_ <- unclass(TW16m1_))
class(TW16mat1_)

# export matrix as csv
write.csv(TW16mat1_, file = "TW16_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW16__dataset <- read.csv("TW16_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW16__matrix <- TW16__dataset[,-1]

### Modularity Q
mod_TW16__matrix <- computeModules(web=TW16__matrix, steps=1E8)

mod_TW16__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW16__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW16__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW17 

df__TW17 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW17",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW17m1_ <- table( df__TW17 )
(TW17mat1_ <- unclass(TW17m1_))
class(TW17mat1_)

# export matrix as csv
write.csv(TW17mat1_, file = "TW17_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW17__dataset <- read.csv("TW17_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW17__matrix <- TW17__dataset[,-1]

### Modularity Q
mod_TW17__matrix <- computeModules(web=TW17__matrix, steps=1E8)

mod_TW17__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW17__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW17__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### TW18 

df__TW18 <- subset(dat1, Site == "Tiwoho" & Quadrat == "TW18",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
TW18m1_ <- table( df__TW18 )
(TW18mat1_ <- unclass(TW18m1_))
class(TW18mat1_)

# export matrix as csv
write.csv(TW18mat1_, file = "TW18_merged_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
TW18__dataset <- read.csv("TW18_merged_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

TW18__matrix <- TW18__dataset[,-1]


### Modularity Q
mod_TW18__matrix <- computeModules(web=TW18__matrix, steps=1E8)

mod_TW18__matrix@likelihood

### Creating an index from Q
nulls <- nullmodel(TW18__matrix, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_TW18__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



# Housekeeping
graphics.off() 
rm(list=ls())