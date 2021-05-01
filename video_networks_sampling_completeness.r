#####################################################
### Calculating sampling completeness for video data networks


# Housekeeping
graphics.off() 
rm(list=ls())


# Read in the data
dat1<-read.csv(file.choose()) # Interaction_data.csv


###################################################################################
######### SAMPLING COMPLETENESS ##################################################
################################################################################
# code modified from Macgregor 2017 Estimating sampling completeness of interactions in quantitative 
# bipartite ecological networks: incorporating variation in species' specialisation

## ----load_function from R file stored in the same location---- ##
f <- c("Macgregor_2017_functions.R")
lapply(f, source)


################################################################################
##########  SPLITING OUT BY Quadrat ###########################################
################################################################################

#### LKA ### 

df_flipped_LKA <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LKA",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LKAm1_flipped <- table( df_flipped_LKA )
(LKAmat1_flipped <- unclass(LKAm1_flipped))
class(LKAmat1_flipped)

# export matrix as csv
write.csv(LKAmat1_flipped, file = "LKA_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LKA__dataset <- read.csv("LKA_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LKA__matrix <- LKA__dataset[,-1]

# remove zeros
#LKA_new_mtx <- LKA__matrix [which(rowSums(LKA__matrix) > 0), ]


LKA__comp <- SCw1(LKA__matrix)
LKA__comp

## ----estimator_flag1------ ##
LKA__comp1 <- SCw1(LKA__matrix, estimator="ACE")
LKA__comp1

LKA__comp2 <- SCw1(LKA__matrix, estimator=c("ACE","Chao1"))
LKA__comp2

#### LKB ### 

df_flipped_LKB <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LKB",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LKBm1_flipped <- table( df_flipped_LKB )
(LKBmat1_flipped <- unclass(LKBm1_flipped))
class(LKBmat1_flipped)

# export matrix as csv
write.csv(LKBmat1_flipped, file = "LKB_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LKB__dataset <- read.csv("LKB_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LKB__matrix <- LKB__dataset[,-1]

# remove zeros
#LKB_new_mtx <- LKB__matrix [which(rowSums(LKB__matrix) > 0), ]


LKB__comp <- SCw1(LKB__matrix)
LKB__comp

## ----estimator_flag1------ ##
LKB__comp1 <- SCw1(LKB__matrix, estimator="ACE")
LKB__comp1

LKB__comp2 <- SCw1(LKB__matrix, estimator=c("ACE","Chao1"))
LKB__comp2

#### LKC ### 

df_flipped_LKC <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LKC",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LKCm1_flipped <- table( df_flipped_LKC )
(LKCmat1_flipped <- unclass(LKCm1_flipped))
class(LKCmat1_flipped)

# export matrix as csv
write.csv(LKCmat1_flipped, file = "LKC_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LKC__dataset <- read.csv("LKC_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LKC__matrix <- LKC__dataset[,-1]

# remove zeros
#LKC_new_mtx <- LKC__matrix [which(rowSums(LKC__matrix) > 0), ]


LKC__comp <- SCw1(LKC__matrix)
LKC__comp

## ----estimator_flag1------ ##
LKC__comp1 <- SCw1(LKC__matrix, estimator="ACE")
LKC__comp1

LKC__comp2 <- SCw1(LKC__matrix, estimator=c("ACE","Chao1"))
LKC__comp2


#### LK4 with just vegetation

df_flipped_LK4 <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LK4",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK4m1_flipped <- table( df_flipped_LK4 )
(LK4mat1_flipped <- unclass(LK4m1_flipped))
class(LK4mat1_flipped)

# export matrix as csv
write.csv(LK4mat1_flipped, file = "LK4_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK4_video_dataset <- read.csv("LK4_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK4_video_matrix <- LK4_video_dataset[,-1]

# remove zeros
#LK4_new_mtx <- LK4_video_matrix [which(rowSums(LK4_video_matrix) > 0), ]


LK4_video_comp <- SCw1(LK4_video_matrix)
LK4_video_comp

## ----estimator_flag1------ ##
LK4_video_comp1 <- SCw1(LK4_video_matrix, estimator="ACE")
LK4_video_comp1

LK4_video_comp2 <- SCw1(LK4_video_matrix, estimator=c("ACE","Chao1"))
LK4_video_comp2


#### LK5 with just vegetation

df_flipped_LK5 <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LK5",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK5m1_flipped <- table( df_flipped_LK5 )
(LK5mat1_flipped <- unclass(LK5m1_flipped))
class(LK5mat1_flipped)

# export matrix as csv
write.csv(LK5mat1_flipped, file = "LK5_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK5_video_dataset <- read.csv("LK5_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK5_video_matrix <- LK5_video_dataset[,-1]

# remove zeros
#LK5_new_mtx <- LK5_video_matrix [which(rowSums(LK5_video_matrix) > 0), ]


LK5_video_comp <- SCw1(LK5_video_matrix)
LK5_video_comp

## ----estimator_flag1------ ##
LK5_video_comp1 <- SCw1(LK5_video_matrix, estimator="ACE")
LK5_video_comp1

LK5_video_comp2 <- SCw1(LK5_video_matrix, estimator=c("ACE","Chao1"))
LK5_video_comp2


#### LK6 with just vegetation

df_flipped_LK6 <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LK6",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK6m1_flipped <- table( df_flipped_LK6 )
(LK6mat1_flipped <- unclass(LK6m1_flipped))
class(LK6mat1_flipped)

# export matrix as csv
write.csv(LK6mat1_flipped, file = "LK6_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK6_video_dataset <- read.csv("LK6_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK6_video_matrix <- LK6_video_dataset[,-1]

# remove zeros
#LK6_new_mtx <- LK6_video_matrix [which(rowSums(LK6_video_matrix) > 0), ]


LK6_video_comp <- SCw1(LK6_video_matrix)
LK6_video_comp

## ----estimator_flag1------ ##
LK6_video_comp1 <- SCw1(LK6_video_matrix, estimator="ACE")
LK6_video_comp1

LK6_video_comp2 <- SCw1(LK6_video_matrix, estimator=c("ACE","Chao1"))
LK6_video_comp2


#### LK7 with just vegetation

df_flipped_LK7 <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LK7",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK7m1_flipped <- table( df_flipped_LK7 )
(LK7mat1_flipped <- unclass(LK7m1_flipped))
class(LK7mat1_flipped)

# export matrix as csv
write.csv(LK7mat1_flipped, file = "LK7_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK7_video_dataset <- read.csv("LK7_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK7_video_matrix <- LK7_video_dataset[,-1]

# remove zeros
#LK7_new_mtx <- LK7_video_matrix [which(rowSums(LK7_video_matrix) > 0), ]


LK7_video_comp <- SCw1(LK7_video_matrix)
LK7_video_comp

## ----estimator_flag1------ ##
LK7_video_comp1 <- SCw1(LK7_video_matrix, estimator="ACE")
LK7_video_comp1

LK7_video_comp2 <- SCw1(LK7_video_matrix, estimator=c("ACE","Chao1"))
LK7_video_comp2



#### LK8 with just vegetation

df_flipped_LK8 <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LK8",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK8m1_flipped <- table( df_flipped_LK8 )
(LK8mat1_flipped <- unclass(LK8m1_flipped))
class(LK8mat1_flipped)

# export matrix as csv
write.csv(LK8mat1_flipped, file = "LK8_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK8_video_dataset <- read.csv("LK8_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK8_video_matrix <- LK8_video_dataset[,-1]

# remove zeros
#LK8_new_mtx <- LK8_video_matrix [which(rowSums(LK8_video_matrix) > 0), ]


LK8_video_comp <- SCw1(LK8_video_matrix)
LK8_video_comp

## ----estimator_flag1------ ##
LK8_video_comp1 <- SCw1(LK8_video_matrix, estimator="ACE")
LK8_video_comp1

LK8_video_comp2 <- SCw1(LK8_video_matrix, estimator=c("ACE","Chao1"))
LK8_video_comp2



#### LK9 with just vegetation

df_flipped_LK9 <- subset(dat1, Method == "Video" &
                           Site == "Likupang" & Quadrat == "LK9",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK9m1_flipped <- table( df_flipped_LK9 )
(LK9mat1_flipped <- unclass(LK9m1_flipped))
class(LK9mat1_flipped)

# export matrix as csv
write.csv(LK9mat1_flipped, file = "LK9_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK9_video_dataset <- read.csv("LK9_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK9_video_matrix <- LK9_video_dataset[,-1]

# remove zeros
#LK9_new_mtx <- LK9_video_matrix [which(rowSums(LK9_video_matrix) > 0), ]


LK9_video_comp <- SCw1(LK9_video_matrix)
LK9_video_comp

## ----estimator_flag1------ ##
LK9_video_comp1 <- SCw1(LK9_video_matrix, estimator="ACE")
LK9_video_comp1

LK9_video_comp2 <- SCw1(LK9_video_matrix, estimator=c("ACE","Chao1"))
LK9_video_comp2



#### LK10 with just vegetation

df_flipped_LK10 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK10",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK10m1_flipped <- table( df_flipped_LK10 )
(LK10mat1_flipped <- unclass(LK10m1_flipped))
class(LK10mat1_flipped)

# export matrix as csv
write.csv(LK10mat1_flipped, file = "LK10_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK10_video_dataset <- read.csv("LK10_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK10_video_matrix <- LK10_video_dataset[,-1]

# remove zeros
#LK10_new_mtx <- LK10_video_matrix [which(rowSums(LK10_video_matrix) > 0), ]


LK10_video_comp <- SCw1(LK10_video_matrix)
LK10_video_comp

## ----estimator_flag1------ ##
LK10_video_comp1 <- SCw1(LK10_video_matrix, estimator="ACE")
LK10_video_comp1

LK10_video_comp2 <- SCw1(LK10_video_matrix, estimator=c("ACE","Chao1"))
LK10_video_comp2


#### LK11 with just vegetation

df_flipped_LK11 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK11",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK11m1_flipped <- table( df_flipped_LK11 )
(LK11mat1_flipped <- unclass(LK11m1_flipped))
class(LK11mat1_flipped)

# export matrix as csv
write.csv(LK11mat1_flipped, file = "LK11_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK11_video_dataset <- read.csv("LK11_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK11_video_matrix <- LK11_video_dataset[,-1]

# remove zeros
#LK11_new_mtx <- LK11_video_matrix [which(rowSums(LK11_video_matrix) > 0), ]


LK11_video_comp <- SCw1(LK11_video_matrix)
LK11_video_comp

## ----estimator_flag1------ ##
LK11_video_comp1 <- SCw1(LK11_video_matrix, estimator="ACE")
LK11_video_comp1

LK11_video_comp2 <- SCw1(LK11_video_matrix, estimator=c("ACE","Chao1"))
LK11_video_comp2


#### LK12 with just vegetation

df_flipped_LK12 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK12",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK12m1_flipped <- table( df_flipped_LK12 )
(LK12mat1_flipped <- unclass(LK12m1_flipped))
class(LK12mat1_flipped)

# export matrix as csv
write.csv(LK12mat1_flipped, file = "LK12_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK12_video_dataset <- read.csv("LK12_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK12_video_matrix <- LK12_video_dataset[,-1]

# remove zeros
#LK12_new_mtx <- LK12_video_matrix [which(rowSums(LK12_video_matrix) > 0), ]


LK12_video_comp <- SCw1(LK12_video_matrix)
LK12_video_comp

## ----estimator_flag1------ ##
LK12_video_comp1 <- SCw1(LK12_video_matrix, estimator="ACE")
LK12_video_comp1

LK12_video_comp2 <- SCw1(LK12_video_matrix, estimator=c("ACE","Chao1"))
LK12_video_comp2


#### LK13 with just vegetation

df_flipped_LK13 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK13",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK13m1_flipped <- table( df_flipped_LK13 )
(LK13mat1_flipped <- unclass(LK13m1_flipped))
class(LK13mat1_flipped)

# export matrix as csv
write.csv(LK13mat1_flipped, file = "LK13_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK13_video_dataset <- read.csv("LK13_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK13_video_matrix <- LK13_video_dataset[,-1]

# remove zeros
#LK13_new_mtx <- LK13_video_matrix [which(rowSums(LK13_video_matrix) > 0), ]


LK13_video_comp <- SCw1(LK13_video_matrix)
LK13_video_comp

## ----estimator_flag1------ ##
LK13_video_comp1 <- SCw1(LK13_video_matrix, estimator="ACE")
LK13_video_comp1

LK13_video_comp2 <- SCw1(LK13_video_matrix, estimator=c("ACE","Chao1"))
LK13_video_comp2


#### LK14 with just vegetation

df_flipped_LK14 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK14",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK14m1_flipped <- table( df_flipped_LK14 )
(LK14mat1_flipped <- unclass(LK14m1_flipped))
class(LK14mat1_flipped)

# export matrix as csv
write.csv(LK14mat1_flipped, file = "LK14_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK14_video_dataset <- read.csv("LK14_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK14_video_matrix <- LK14_video_dataset[,-1]

# remove zeros
#LK14_new_mtx <- LK14_video_matrix [which(rowSums(LK14_video_matrix) > 0), ]


LK14_video_comp <- SCw1(LK14_video_matrix)
LK14_video_comp

## ----estimator_flag1------ ##
LK14_video_comp1 <- SCw1(LK14_video_matrix, estimator="ACE")
LK14_video_comp1

LK14_video_comp2 <- SCw1(LK14_video_matrix, estimator=c("ACE","Chao1"))
LK14_video_comp2



#### LK15 with just vegetation

df_flipped_LK15 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK15",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK15m1_flipped <- table( df_flipped_LK15 )
(LK15mat1_flipped <- unclass(LK15m1_flipped))
class(LK15mat1_flipped)

# export matrix as csv
write.csv(LK15mat1_flipped, file = "LK15_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK15_video_dataset <- read.csv("LK15_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK15_video_matrix <- LK15_video_dataset[,-1]

# remove zeros
#LK15_new_mtx <- LK15_video_matrix [which(rowSums(LK15_video_matrix) > 0), ]


LK15_video_comp <- SCw1(LK15_video_matrix)
LK15_video_comp

## ----estimator_flag1------ ##
LK15_video_comp1 <- SCw1(LK15_video_matrix, estimator="ACE")
LK15_video_comp1

LK15_video_comp2 <- SCw1(LK15_video_matrix, estimator=c("ACE","Chao1"))
LK15_video_comp2



#### LK16 with just vegetation

df_flipped_LK16 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK16",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK16m1_flipped <- table( df_flipped_LK16 )
(LK16mat1_flipped <- unclass(LK16m1_flipped))
class(LK16mat1_flipped)

# export matrix as csv
write.csv(LK16mat1_flipped, file = "LK16_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK16_video_dataset <- read.csv("LK16_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK16_video_matrix <- LK16_video_dataset[,-1]

# remove zeros
#LK16_new_mtx <- LK16_video_matrix [which(rowSums(LK16_video_matrix) > 0), ]


LK16_video_comp <- SCw1(LK16_video_matrix)
LK16_video_comp

## ----estimator_flag1------ ##
LK16_video_comp1 <- SCw1(LK16_video_matrix, estimator="ACE")
LK16_video_comp1

LK16_video_comp2 <- SCw1(LK16_video_matrix, estimator=c("ACE","Chao1"))
LK16_video_comp2



#### LK17 with just vegetation

df_flipped_LK17 <- subset(dat1, Method == "Video" &
                            Site == "Likupang" & Quadrat == "LK17",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
LK17m1_flipped <- table( df_flipped_LK17 )
(LK17mat1_flipped <- unclass(LK17m1_flipped))
class(LK17mat1_flipped)

# export matrix as csv
write.csv(LK17mat1_flipped, file = "LK17_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
LK17_video_dataset <- read.csv("LK17_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

LK17_video_matrix <- LK17_video_dataset[,-1]

# remove zeros
#LK17_new_mtx <- LK17_video_matrix [which(rowSums(LK17_video_matrix) > 0), ]


LK17_video_comp <- SCw1(LK17_video_matrix)
LK17_video_comp

## ----estimator_flag1------ ##
LK17_video_comp1 <- SCw1(LK17_video_matrix, estimator="ACE")
LK17_video_comp1

LK17_video_comp2 <- SCw1(LK17_video_matrix, estimator=c("ACE","Chao1"))
LK17_video_comp2


#################################################
########## BY Quadrat ##############################
################## TIWOHO #######################


#### TW1 with just vegetation

df_flipped_TW1 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW1",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW1m1_flipped <- table( df_flipped_TW1 )
(TW1mat1_flipped <- unclass(TW1m1_flipped))
class(TW1mat1_flipped)

# export matrix as csv
write.csv(TW1mat1_flipped, file = "TW1_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW1_video_dataset <- read.csv("TW1_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW1_video_matrix <- TW1_video_dataset[,-1]

# remove zeros
#TW1_new_mtx <- TW1_video_matrix [which(rowSums(TW1_video_matrix) > 0), ]


TW1_video_comp <- SCw1(TW1_video_matrix)
TW1_video_comp

## ----estimator_flag1------ ##
TW1_video_comp1 <- SCw1(TW1_video_matrix, estimator="ACE")
TW1_video_comp1

TW1_video_comp2 <- SCw1(TW1_video_matrix, estimator=c("ACE","Chao1"))
TW1_video_comp2



#### TW2 with just vegetation

df_flipped_TW2 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW2",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW2m1_flipped <- table( df_flipped_TW2 )
(TW2mat1_flipped <- unclass(TW2m1_flipped))
class(TW2mat1_flipped)

# export matrix as csv
write.csv(TW2mat1_flipped, file = "TW2_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW2_video_dataset <- read.csv("TW2_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW2_video_matrix <- TW2_video_dataset[,-1]

# remove zeros
#TW2_new_mtx <- TW2_video_matrix [which(rowSums(TW2_video_matrix) > 0), ]


TW2_video_comp <- SCw1(TW2_video_matrix)
TW2_video_comp

## ----estimator_flag1------ ##
TW2_video_comp1 <- SCw1(TW2_video_matrix, estimator="ACE")
TW2_video_comp1

TW2_video_comp2 <- SCw1(TW2_video_matrix, estimator=c("ACE","Chao1"))
TW2_video_comp2



#### TW3 with just vegetation

df_flipped_TW3 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW3",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW3m1_flipped <- table( df_flipped_TW3 )
(TW3mat1_flipped <- unclass(TW3m1_flipped))
class(TW3mat1_flipped)

# export matrix as csv
write.csv(TW3mat1_flipped, file = "TW3_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW3_video_dataset <- read.csv("TW3_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW3_video_matrix <- TW3_video_dataset[,-1]

# remove zeros
#TW3_new_mtx <- TW3_video_matrix [which(rowSums(TW3_video_matrix) > 0), ]


TW3_video_comp <- SCw1(TW3_video_matrix)
TW3_video_comp

## ----estimator_flag1------ ##
TW3_video_comp1 <- SCw1(TW3_video_matrix, estimator="ACE")
TW3_video_comp1

TW3_video_comp2 <- SCw1(TW3_video_matrix, estimator=c("ACE","Chao1"))
TW3_video_comp2




#### TW4 with just vegetation

df_flipped_TW4 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW4",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW4m1_flipped <- table( df_flipped_TW4 )
(TW4mat1_flipped <- unclass(TW4m1_flipped))
class(TW4mat1_flipped)

# export matrix as csv
write.csv(TW4mat1_flipped, file = "TW4_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW4_video_dataset <- read.csv("TW4_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW4_video_matrix <- TW4_video_dataset[,-1]

# remove zeros
#TW4_new_mtx <- TW4_video_matrix [which(rowSums(TW4_video_matrix) > 0), ]


TW4_video_comp <- SCw1(TW4_video_matrix)
TW4_video_comp

## ----estimator_flag1------ ##
TW4_video_comp1 <- SCw1(TW4_video_matrix, estimator="ACE")
TW4_video_comp1

TW4_video_comp2 <- SCw1(TW4_video_matrix, estimator=c("ACE","Chao1"))
TW4_video_comp2




#### TW5 with just vegetation

df_flipped_TW5 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW5",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW5m1_flipped <- table( df_flipped_TW5 )
(TW5mat1_flipped <- unclass(TW5m1_flipped))
class(TW5mat1_flipped)

# export matrix as csv
write.csv(TW5mat1_flipped, file = "TW5_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW5_video_dataset <- read.csv("TW5_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW5_video_matrix <- TW5_video_dataset[,-1]

# remove zeros
#TW5_new_mtx <- TW5_video_matrix [which(rowSums(TW5_video_matrix) > 0), ]


TW5_video_comp <- SCw1(TW5_video_matrix)
TW5_video_comp

## ----estimator_flag1------ ##
TW5_video_comp1 <- SCw1(TW5_video_matrix, estimator="ACE")
TW5_video_comp1

TW5_video_comp2 <- SCw1(TW5_video_matrix, estimator=c("ACE","Chao1"))
TW5_video_comp2




#### TW6 with just vegetation

df_flipped_TW6 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW6",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW6m1_flipped <- table( df_flipped_TW6 )
(TW6mat1_flipped <- unclass(TW6m1_flipped))
class(TW6mat1_flipped)

# export matrix as csv
write.csv(TW6mat1_flipped, file = "TW6_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW6_video_dataset <- read.csv("TW6_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW6_video_matrix <- TW6_video_dataset[,-1]

# remove zeros
#TW6_new_mtx <- TW6_video_matrix [which(rowSums(TW6_video_matrix) > 0), ]


TW6_video_comp <- SCw1(TW6_video_matrix)
TW6_video_comp

## ----estimator_flag1------ ##
TW6_video_comp1 <- SCw1(TW6_video_matrix, estimator="ACE")
TW6_video_comp1

TW6_video_comp2 <- SCw1(TW6_video_matrix, estimator=c("ACE","Chao1"))
TW6_video_comp2




#### TW7 with just vegetation

df_flipped_TW7 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW7",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW7m1_flipped <- table( df_flipped_TW7 )
(TW7mat1_flipped <- unclass(TW7m1_flipped))
class(TW7mat1_flipped)

# export matrix as csv
write.csv(TW7mat1_flipped, file = "TW7_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW7_video_dataset <- read.csv("TW7_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW7_video_matrix <- TW7_video_dataset[,-1]

# remove zeros
#TW7_new_mtx <- TW7_video_matrix [which(rowSums(TW7_video_matrix) > 0), ]


TW7_video_comp <- SCw1(TW7_video_matrix)
TW7_video_comp

## ----estimator_flag1------ ##
TW7_video_comp1 <- SCw1(TW7_video_matrix, estimator="ACE")
TW7_video_comp1

TW7_video_comp2 <- SCw1(TW7_video_matrix, estimator=c("ACE","Chao1"))
TW7_video_comp2




#### TW8 with just vegetation

df_flipped_TW8 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW8",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW8m1_flipped <- table( df_flipped_TW8 )
(TW8mat1_flipped <- unclass(TW8m1_flipped))
class(TW8mat1_flipped)

# export matrix as csv
write.csv(TW8mat1_flipped, file = "TW8_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW8_video_dataset <- read.csv("TW8_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW8_video_matrix <- TW8_video_dataset[,-1]

# remove zeros
#TW8_new_mtx <- TW8_video_matrix [which(rowSums(TW8_video_matrix) > 0), ]


TW8_video_comp <- SCw1(TW8_video_matrix)
TW8_video_comp

## ----estimator_flag1------ ##
TW8_video_comp1 <- SCw1(TW8_video_matrix, estimator="ACE")
TW8_video_comp1

TW8_video_comp2 <- SCw1(TW8_video_matrix, estimator=c("ACE","Chao1"))
TW8_video_comp2





#### TW9 with just vegetation

df_flipped_TW9 <- subset(dat1, Method == "Video" &
                           Site == "Tiwoho" & Quadrat == "TW9",
                         select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW9m1_flipped <- table( df_flipped_TW9 )
(TW9mat1_flipped <- unclass(TW9m1_flipped))
class(TW9mat1_flipped)

# export matrix as csv
write.csv(TW9mat1_flipped, file = "TW9_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW9_video_dataset <- read.csv("TW9_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW9_video_matrix <- TW9_video_dataset[,-1]

# remove zeros
#TW9_new_mtx <- TW9_video_matrix [which(rowSums(TW9_video_matrix) > 0), ]


TW9_video_comp <- SCw1(TW9_video_matrix)
TW9_video_comp

## ----estimator_flag1------ ##
TW9_video_comp1 <- SCw1(TW9_video_matrix, estimator="ACE")
TW9_video_comp1

TW9_video_comp2 <- SCw1(TW9_video_matrix, estimator=c("ACE","Chao1"))
TW9_video_comp2





#### TW10 with just vegetation

df_flipped_TW10 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW10",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW10m1_flipped <- table( df_flipped_TW10 )
(TW10mat1_flipped <- unclass(TW10m1_flipped))
class(TW10mat1_flipped)

# export matrix as csv
write.csv(TW10mat1_flipped, file = "TW10_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW10_video_dataset <- read.csv("TW10_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW10_video_matrix <- TW10_video_dataset[,-1]

# remove zeros
#TW10_new_mtx <- TW10_video_matrix [which(rowSums(TW10_video_matrix) > 0), ]


TW10_video_comp <- SCw1(TW10_video_matrix)
TW10_video_comp

## ----estimator_flag1------ ##
TW10_video_comp1 <- SCw1(TW10_video_matrix, estimator="ACE")
TW10_video_comp1

TW10_video_comp2 <- SCw1(TW10_video_matrix, estimator=c("ACE","Chao1"))
TW10_video_comp2



#### TW11 with just vegetation

df_flipped_TW11 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW11",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW11m1_flipped <- table( df_flipped_TW11 )
(TW11mat1_flipped <- unclass(TW11m1_flipped))
class(TW11mat1_flipped)

# export matrix as csv
write.csv(TW11mat1_flipped, file = "TW11_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW11_video_dataset <- read.csv("TW11_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW11_video_matrix <- TW11_video_dataset[,-1]

# remove zeros
#TW11_new_mtx <- TW11_video_matrix [which(rowSums(TW11_video_matrix) > 0), ]


TW11_video_comp <- SCw1(TW11_video_matrix)
TW11_video_comp

## ----estimator_flag1------ ##
TW11_video_comp1 <- SCw1(TW11_video_matrix, estimator="ACE")
TW11_video_comp1

TW11_video_comp2 <- SCw1(TW11_video_matrix, estimator=c("ACE","Chao1"))
TW11_video_comp2




#### TW12 with just vegetation

df_flipped_TW12 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW12",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW12m1_flipped <- table( df_flipped_TW12 )
(TW12mat1_flipped <- unclass(TW12m1_flipped))
class(TW12mat1_flipped)

# export matrix as csv
write.csv(TW12mat1_flipped, file = "TW12_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW12_video_dataset <- read.csv("TW12_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW12_video_matrix <- TW12_video_dataset[,-1]

# remove zeros
#TW12_new_mtx <- TW12_video_matrix [which(rowSums(TW12_video_matrix) > 0), ]


TW12_video_comp <- SCw1(TW12_video_matrix)
TW12_video_comp

## ----estimator_flag1------ ##
TW12_video_comp1 <- SCw1(TW12_video_matrix, estimator="ACE")
TW12_video_comp1

TW12_video_comp2 <- SCw1(TW12_video_matrix, estimator=c("ACE","Chao1"))
TW12_video_comp2




#### TW13 with just vegetation

df_flipped_TW13 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW13",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW13m1_flipped <- table( df_flipped_TW13 )
(TW13mat1_flipped <- unclass(TW13m1_flipped))
class(TW13mat1_flipped)

# export matrix as csv
write.csv(TW13mat1_flipped, file = "TW13_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW13_video_dataset <- read.csv("TW13_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW13_video_matrix <- TW13_video_dataset[,-1]

# remove zeros
#TW13_new_mtx <- TW13_video_matrix [which(rowSums(TW13_video_matrix) > 0), ]


TW13_video_comp <- SCw1(TW13_video_matrix)
TW13_video_comp

## ----estimator_flag1------ ##
TW13_video_comp1 <- SCw1(TW13_video_matrix, estimator="ACE")
TW13_video_comp1

TW13_video_comp2 <- SCw1(TW13_video_matrix, estimator=c("ACE","Chao1"))
TW13_video_comp2



#### TW14 with just vegetation

df_flipped_TW14 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW14",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW14m1_flipped <- table( df_flipped_TW14 )
(TW14mat1_flipped <- unclass(TW14m1_flipped))
class(TW14mat1_flipped)

# export matrix as csv
write.csv(TW14mat1_flipped, file = "TW14_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW14_video_dataset <- read.csv("TW14_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW14_video_matrix <- TW14_video_dataset[,-1]

# remove zeros
#TW14_new_mtx <- TW14_video_matrix [which(rowSums(TW14_video_matrix) > 0), ]


TW14_video_comp <- SCw1(TW14_video_matrix)
TW14_video_comp

## ----estimator_flag1------ ##
TW14_video_comp1 <- SCw1(TW14_video_matrix, estimator="ACE")
TW14_video_comp1

TW14_video_comp2 <- SCw1(TW14_video_matrix, estimator=c("ACE","Chao1"))
TW14_video_comp2


#### TW15 with just vegetation

df_flipped_TW15 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW15",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW15m1_flipped <- table( df_flipped_TW15 )
(TW15mat1_flipped <- unclass(TW15m1_flipped))
class(TW15mat1_flipped)

# export matrix as csv
write.csv(TW15mat1_flipped, file = "TW15_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW15_video_dataset <- read.csv("TW15_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW15_video_matrix <- TW15_video_dataset[,-1]

# remove zeros
#TW15_new_mtx <- TW15_video_matrix [which(rowSums(TW15_video_matrix) > 0), ]


TW15_video_comp <- SCw1(TW15_video_matrix)
TW15_video_comp

## ----estimator_flag1------ ##
TW15_video_comp1 <- SCw1(TW15_video_matrix, estimator="ACE")
TW15_video_comp1

TW15_video_comp2 <- SCw1(TW15_video_matrix, estimator=c("ACE","Chao1"))
TW15_video_comp2




#### TW16 with just vegetation

df_flipped_TW16 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW16",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW16m1_flipped <- table( df_flipped_TW16 )
(TW16mat1_flipped <- unclass(TW16m1_flipped))
class(TW16mat1_flipped)

# export matrix as csv
write.csv(TW16mat1_flipped, file = "TW16_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW16_video_dataset <- read.csv("TW16_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW16_video_matrix <- TW16_video_dataset[,-1]

# remove zeros
#TW16_new_mtx <- TW16_video_matrix [which(rowSums(TW16_video_matrix) > 0), ]


TW16_video_comp <- SCw1(TW16_video_matrix)
TW16_video_comp

## ----estimator_flag1------ ##
TW16_video_comp1 <- SCw1(TW16_video_matrix, estimator="ACE")
TW16_video_comp1

TW16_video_comp2 <- SCw1(TW16_video_matrix, estimator=c("ACE","Chao1"))
TW16_video_comp2



#### TW17 with just vegetation

df_flipped_TW17 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW17",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW17m1_flipped <- table( df_flipped_TW17 )
(TW17mat1_flipped <- unclass(TW17m1_flipped))
class(TW17mat1_flipped)

# export matrix as csv
write.csv(TW17mat1_flipped, file = "TW17_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW17_video_dataset <- read.csv("TW17_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW17_video_matrix <- TW17_video_dataset[,-1]

# remove zeros
#TW17_new_mtx <- TW17_video_matrix [which(rowSums(TW17_video_matrix) > 0), ]


TW17_video_comp <- SCw1(TW17_video_matrix)
TW17_video_comp

## ----estimator_flag1------ ##
TW17_video_comp1 <- SCw1(TW17_video_matrix, estimator="ACE")
TW17_video_comp1

TW17_video_comp2 <- SCw1(TW17_video_matrix, estimator=c("ACE","Chao1"))
TW17_video_comp2


#### TW18 with just vegetation

df_flipped_TW18 <- subset(dat1, Method == "Video" &
                            Site == "Tiwoho" & Quadrat == "TW18",
                          select=c(Network_upper_level_shorthand, Network_lower_level_shorthand))

# convert edge list to matrix
TW18m1_flipped <- table( df_flipped_TW18 )
(TW18mat1_flipped <- unclass(TW18m1_flipped))
class(TW18mat1_flipped)

# export matrix as csv
write.csv(TW18mat1_flipped, file = "TW18_video_data_sampling_completeness.csv")

## ----read_data----- previously exported matrix csv
TW18_video_dataset <- read.csv("TW18_video_data_sampling_completeness.csv", header=T)


### delete first row to ensure all numeric

TW18_video_matrix <- TW18_video_dataset[,-1]

# remove zeros
#TW18_new_mtx <- TW18_video_matrix [which(rowSums(TW18_video_matrix) > 0), ]


TW18_video_comp <- SCw1(TW18_video_matrix)
TW18_video_comp

## ----estimator_flag1------ ##
TW18_video_comp1 <- SCw1(TW18_video_matrix, estimator="ACE")
TW18_video_comp1

TW18_video_comp2 <- SCw1(TW18_video_matrix, estimator=c("ACE","Chao1"))
TW18_video_comp2


# Housekeeping
graphics.off() 
rm(list=ls())