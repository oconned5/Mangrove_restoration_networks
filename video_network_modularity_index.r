##############################################
### Calculating Modularity Index for each Quadrat, Vegetation Network

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

### Likupang ###

#### LKA ### 

df__LKA <- subset(dat1, Site == "Likupang" & Quadrat == "LKA"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKAm1_ <- table( df__LKA )
(LKAmat1_ <- unclass(LKAm1_))
class(LKAmat1_)

# export matrix as csv
write.csv(LKAmat1_, file = "LKA_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LKA__dataset <- read.csv("LKA_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LKA__matrix <- LKA__dataset[,-1]

### Modularity Q
mod_LKA__matrix <- computeModules(web=LKA__matrix, steps=1E8)

mod_LKA__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LKA__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LKA__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LKB ### 

df__LKB <- subset(dat1, Site == "Likupang" & Quadrat == "LKB"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKBm1_ <- table( df__LKB )
(LKBmat1_ <- unclass(LKBm1_))
class(LKBmat1_)

# export matrix as csv
write.csv(LKBmat1_, file = "LKB_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LKB__dataset <- read.csv("LKB_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LKB__matrix <- LKB__dataset[,-1]


### Modularity Q
mod_LKB__matrix <- computeModules(web=LKB__matrix, steps=1E8)

mod_LKB__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LKB__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LKB__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LKC ### 

df__LKC <- subset(dat1, Site == "Likupang" & Quadrat == "LKC"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LKCm1_ <- table( df__LKC )
(LKCmat1_ <- unclass(LKCm1_))
class(LKCmat1_)

# export matrix as csv
write.csv(LKCmat1_, file = "LKC_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LKC__dataset <- read.csv("LKC_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LKC__matrix <- LKC__dataset[,-1]

### Modularity Q
mod_LKC__matrix <- computeModules(web=LKC__matrix, steps=1E8)

mod_LKC__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LKC__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LKC__matrix@likelihood - mean(like.nulls))/sd(like.nulls))

#### LK4 

df__LK4 <- subset(dat1, Site == "Likupang" & Quadrat == "LK4"
                  & Method == "Video",
                  select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK4m1_ <- table( df__LK4 )
(LK4mat1_ <- unclass(LK4m1_))
class(LK4mat1_)

# export matrix as csv
write.csv(LK4mat1_, file = "LK4_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK4__dataset <- read.csv("LK4_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK4__matrix <- LK4__dataset[,-1]

### Modularity Q
mod_LK4__matrix <- computeModules(web=LK4__matrix, steps=1E8)

mod_LK4__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK4__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK4__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK5 

df__LK5 <- subset(dat1, Site == "Likupang" & Quadrat == "LK5"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK5m1_ <- table( df__LK5 )
(LK5mat1_ <- unclass(LK5m1_))
class(LK5mat1_)

# export matrix as csv
write.csv(LK5mat1_, file = "LK5_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK5__dataset <- read.csv("LK5_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK5__matrix <- LK5__dataset[,-1]

### Modularity Q
mod_LK5__matrix <- computeModules(web=LK5__matrix, steps=1E8)

mod_LK5__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK5__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK5__matrix@likelihood - mean(like.nulls))/sd(like.nulls))




#### LK6 

df__LK6 <- subset(dat1, Site == "Likupang" & Quadrat == "LK6"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK6m1_ <- table( df__LK6 )
(LK6mat1_ <- unclass(LK6m1_))
class(LK6mat1_)

# export matrix as csv
write.csv(LK6mat1_, file = "LK6_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK6__dataset <- read.csv("LK6_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK6__matrix <- LK6__dataset[,-1]

### Modularity Q
mod_LK6__matrix <- computeModules(web=LK6__matrix, steps=1E8)

mod_LK6__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK6__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK6__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK7 

df__LK7 <- subset(dat1, Site == "Likupang" & Quadrat == "LK7"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK7m1_ <- table( df__LK7 )
(LK7mat1_ <- unclass(LK7m1_))
class(LK7mat1_)

# export matrix as csv
write.csv(LK7mat1_, file = "LK7_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK7__dataset <- read.csv("LK7_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK7__matrix <- LK7__dataset[,-1]

### Modularity Q
mod_LK7__matrix <- computeModules(web=LK7__matrix, steps=1E8)

mod_LK7__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK7__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK7__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK8 

df__LK8 <- subset(dat1, Site == "Likupang" & Quadrat == "LK8"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK8m1_ <- table( df__LK8 )
(LK8mat1_ <- unclass(LK8m1_))
class(LK8mat1_)

# export matrix as csv
write.csv(LK8mat1_, file = "LK8_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK8__dataset <- read.csv("LK8_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK8__matrix <- LK8__dataset[,-1]

### Modularity Q
mod_LK8__matrix <- computeModules(web=LK8__matrix, steps=1E8)

mod_LK8__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK8__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK8__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK9 

df__LK9 <- subset(dat1, Site == "Likupang" & Quadrat == "LK9"
                  & Method == "Video",
                         select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK9m1_ <- table( df__LK9 )
(LK9mat1_ <- unclass(LK9m1_))
class(LK9mat1_)

# export matrix as csv
write.csv(LK9mat1_, file = "LK9_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK9__dataset <- read.csv("LK9_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK9__matrix <- LK9__dataset[,-1]

### Modularity Q
mod_LK9__matrix <- computeModules(web=LK9__matrix, steps=1E8)

mod_LK9__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK9__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK9__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK10 

df__LK10 <- subset(dat1, Site == "Likupang" & Quadrat == "LK10"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK10m1_ <- table( df__LK10 )
(LK10mat1_ <- unclass(LK10m1_))
class(LK10mat1_)

# export matrix as csv
write.csv(LK10mat1_, file = "LK10_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK10__dataset <- read.csv("LK10_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK10__matrix <- LK10__dataset[,-1]

### Modularity Q
mod_LK10__matrix <- computeModules(web=LK10__matrix, steps=1E8)

mod_LK10__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK10__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK10__matrix@likelihood - mean(like.nulls))/sd(like.nulls))



#### LK11 

df__LK11 <- subset(dat1, Site == "Likupang" & Quadrat == "LK11"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK11m1_ <- table( df__LK11 )
(LK11mat1_ <- unclass(LK11m1_))
class(LK11mat1_)

# export matrix as csv
write.csv(LK11mat1_, file = "LK11_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK11__dataset <- read.csv("LK11_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK11__matrix <- LK11__dataset[,-1]

# remove zeros
#LK11_new_mtx <- LK11__matrix [which(rowSums(LK11__matrix) > 0), ]



### Modularity Q
mod_LK11__matrix <- computeModules(web=LK11__matrix, steps=1E8)

mod_LK11__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK11__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK11__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK12 

df__LK12 <- subset(dat1, Site == "Likupang" & Quadrat == "LK12"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK12m1_ <- table( df__LK12 )
(LK12mat1_ <- unclass(LK12m1_))
class(LK12mat1_)

# export matrix as csv
write.csv(LK12mat1_, file = "LK12_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK12__dataset <- read.csv("LK12_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK12__matrix <- LK12__dataset[,-1]


### Modularity Q
mod_LK12__matrix <- computeModules(web=LK12__matrix, steps=1E8)

mod_LK12__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK12__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK12__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK13 

df__LK13 <- subset(dat1, Site == "Likupang" & Quadrat == "LK13"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK13m1_ <- table( df__LK13 )
(LK13mat1_ <- unclass(LK13m1_))
class(LK13mat1_)

# export matrix as csv
write.csv(LK13mat1_, file = "LK13_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK13__dataset <- read.csv("LK13_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK13__matrix <- LK13__dataset[,-1]


### Modularity Q
mod_LK13__matrix <- computeModules(web=LK13__matrix, steps=1E8)

mod_LK13__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK13__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK13__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK14 

df__LK14 <- subset(dat1, Site == "Likupang" & Quadrat == "LK14"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK14m1_ <- table( df__LK14 )
(LK14mat1_ <- unclass(LK14m1_))
class(LK14mat1_)

# export matrix as csv
write.csv(LK14mat1_, file = "LK14_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK14__dataset <- read.csv("LK14_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK14__matrix <- LK14__dataset[,-1]

### Modularity Q
mod_LK14__matrix <- computeModules(web=LK14__matrix, steps=1E8)

mod_LK14__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK14__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK14__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK15 

df__LK15 <- subset(dat1, Site == "Likupang" & Quadrat == "LK15"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK15m1_ <- table( df__LK15 )
(LK15mat1_ <- unclass(LK15m1_))
class(LK15mat1_)

# export matrix as csv
write.csv(LK15mat1_, file = "LK15_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK15__dataset <- read.csv("LK15_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK15__matrix <- LK15__dataset[,-1]

### Modularity Q
mod_LK15__matrix <- computeModules(web=LK15__matrix, steps=1E8)

mod_LK15__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK15__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK15__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK16 

df__LK16 <- subset(dat1, Site == "Likupang" & Quadrat == "LK16"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK16m1_ <- table( df__LK16 )
(LK16mat1_ <- unclass(LK16m1_))
class(LK16mat1_)

# export matrix as csv
write.csv(LK16mat1_, file = "LK16_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK16__dataset <- read.csv("LK16_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK16__matrix <- LK16__dataset[,-1]

### Modularity Q
mod_LK16__matrix <- computeModules(web=LK16__matrix, steps=1E8)

mod_LK16__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK16__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK16__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


#### LK17 

df__LK17 <- subset(dat1, Site == "Likupang" & Quadrat == "LK17"
                   & Method == "Video",
                          select=c(Network_lower_level_shorthand, Network_upper_level_shorthand))

# convert edge list to matrix
LK17m1_ <- table( df__LK17 )
(LK17mat1_ <- unclass(LK17m1_))
class(LK17mat1_)

# export matrix as csv
write.csv(LK17mat1_, file = "LK17_sampling_modularity.csv")

## ----read_data----- previously exported matrix csv
LK17__dataset <- read.csv("LK17_sampling_modularity.csv", header=T)


### delete first row to ensure all numeric

LK17__matrix <- LK17__dataset[,-1]

### Modularity Q
mod_LK17__matrix <- computeModules(web=LK17__matrix, steps=1E8)

mod_LK17__matrix@likelihood

### Creating an index from Q accounting for network size
nulls <- nullmodel(LK17__matrix, N=100, Method="r2d")
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod_LK17__matrix@likelihood - mean(like.nulls))/sd(like.nulls))


# Housekeeping
graphics.off() 
rm(list=ls())