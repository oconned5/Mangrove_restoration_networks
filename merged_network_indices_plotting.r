###########################################################
### FIGURES FOR ANALYSES OF NETWORK INDICIES



# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Merged_Network_indices.csv


#################################
##### PLOTTING ##################
#################################

#install.packages("ggplot2")

library(ggplot2)


#################################
### Weighted Connectance ########
################################

j1 <- ggplot(data=dat1, aes(x=Site, y=weighted_connectance)) + geom_boxplot(aes(fill=Treatment))
j2 <- j1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
j3 <- j2 + ylab("Weighted Connectance")
j4 <- j3 + xlab("Site")
j5 <- j4 + theme(axis.line = element_line(colour = "black"))
j6 <- j5 + scale_color_manual(values=c("black", "green"))
j7 <- j6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
j8 <- j7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
j9 <- j8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
j9



### exporting high res image
# PDF
pdf(file = "Weighted Connectance.pdf", width = 8, height = 6, family = "Helvetica")
j9
dev.off()

# TIFF
tiff("Weighted Connectance.tiff", height = 12, width = 17, units = 'cm', res = 300)
j9
dev.off()


####################
### H2  ############
####################

o1 <- ggplot(data=dat1, aes(x=Site, y=H2)) + geom_boxplot(aes(fill=Treatment))
o2 <- o1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
o3 <- o2 + ylab("H2")
o4 <- o3 + xlab("Site")
o5 <- o4 + theme(axis.line = element_line(colour = "black"))
o6 <- o5 + scale_color_manual(values=c("black", "green"))
o7 <- o6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
o8 <- o7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
o9 <- o8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
o9


### exporting high res image
# PDF
pdf(file = "H2.pdf", width = 8, height = 6, family = "Helvetica")
o9
dev.off()

# TIFF
tiff("H2.tiff", height = 12, width = 17, units = 'cm', res = 300)
o9
dev.off()


###############################
### no._interactions  #########
###############################

au1 <- ggplot(data=dat1, aes(x=Site, y=no._interactions)) + geom_boxplot(aes(fill=Treatment))
au2 <- au1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
au3 <- au2 + ylab("Number of interactions")
au4 <- au3 + xlab("Site")
au5 <- au4 + theme(axis.line = element_line(colour = "black"))
au6 <- au5 + scale_color_manual(values=c("black", "green"))
au7 <- au6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
au8 <- au7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
au9 <- au8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
au9


### exporting high res image
# PDF
pdf(file = "Number of interactions.pdf", width = 8, height = 6, family = "Helvetica")
au9
dev.off()

# TIFF
tiff("Number of interactions.tiff", height = 12, width = 17, units = 'cm', res = 300)
au9
dev.off()

###################################
### no._interactions_video  #######
###################################

av1 <- ggplot(data=dat1, aes(x=Site, y=no._interactions_video)) + geom_boxplot(aes(fill=Treatment))
av2 <- av1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
av3 <- av2 + ylab("Number of video interactions")
av4 <- av3 + xlab("Site")
av5 <- av4 + theme(axis.line = element_line(colour = "black"))
av6 <- av5 + scale_color_manual(values=c("black", "green"))
av7 <- av6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
av8 <- av7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
av9 <- av8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
av9


### exporting high res image
# PDF
pdf(file = "Number of video interactions.pdf", width = 8, height = 6, family = "Helvetica")
av9
dev.off()

# TIFF
tiff("Number of video interactions.tiff", height = 12, width = 17, units = 'cm', res = 300)
av9
dev.off()

#########################################
### no._interactions_all_vegetation #####
#########################################

aq1 <- ggplot(data=dat1, aes(x=Site, y=no._interactions_all_veg)) + geom_boxplot(aes(fill=Treatment))
aq2 <- aq1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
aq3 <- aq2 + ylab("Number of interactions - all veg")
aq4 <- aq3 + xlab("Site")
aq5 <- aq4 + theme(axis.line = element_line(colour = "black"))
aq6 <- aq5 + scale_color_manual(values=c("black", "green"))
aq7 <- aq6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
aq8 <- aq7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
aq9 <- aq8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
aq9


### exporting high res image
# PDF
pdf(file = "Number of interactions - all veg.pdf", width = 8, height = 6, family = "Helvetica")
aq9
dev.off()

# TIFF
tiff("Number of interactions - all veg.tiff", height = 12, width = 17, units = 'cm', res = 300)
aq9
dev.off()

## scattterplot Number of interactions - all veg vs CaCo Vegetation Index

dfLik <- subset(dat1, Site == "Likupang")
reg_line <- lm(no._interactions_all_veg ~ CaCo_index, data = dfLik)


xy1 <- ggplot(data=dfLik, aes(x = CaCo_index, y = no._interactions_all_veg, color = Treatment)) +
  geom_point() 
xy2 <- xy1 + labs(x = "CaCo Vegetation Index", y = "Number of interactions - all veg") 
xy3 <- xy2 + theme_classic()
xy4 <- xy3 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
xy5 <- xy4 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
xy6 <- xy5 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
xy7 <- xy6 + theme(legend.key = element_rect(fill = NA),
                   legend.title = element_text(color = "black",
                                               size = 14, face = 2))
xy8 <- xy7 + scale_color_discrete("Treatment:") +
  guides(color = guide_legend(override.aes = list(size = 4)))
xy9 <- xy8 + geom_point(size = 3)
xy10 <- xy9 + ggtitle("Likupang")
xy11 <- xy10 + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
xy12 <- xy11 + xlim(37, 90) + ylim(8, 50)
xy12

xy13 <- xy12 + geom_abline(intercept = coefficients(reg_line)[1],
                           slope = coefficients(reg_line)[2],
                          color = "grey", size = 1)
xy13

### exporting high res image
# PDF
pdf(file = "Number of interactions - all veg vs CaCo scatterplot.pdf", width = 8, height = 6, family = "Helvetica")
xy12
dev.off()

# TIFF
tiff("Number of interactions - all veg vs CaCo scatterplot.tiff", height = 12, width = 17, units = 'cm', res = 300)
xy12
dev.off()

### exporting high res image with trend line
# PDF
pdf(file = "Number of interactions - all veg vs CaCo scatterplot with trend line.pdf", width = 8, height = 6, family = "Helvetica")
xy13
dev.off()

# TIFF
tiff("Number of interactions - all veg vs CaCo scatterplot with trend line.tiff", height = 12, width = 17, units = 'cm', res = 300)
xy13
dev.off()




#########################################
### number.of.species.HL ################
#########################################

aqz1 <- ggplot(data=dat1, aes(x=Site, y=number.of.species.HL)) + geom_boxplot(aes(fill=Treatment))
aqz2 <- aqz1 + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
aqz3 <- aqz2 + ylab("Faunal taxa richness")
aqz4 <- aqz3 + xlab("Site")
aqz5 <- aqz4 + theme(axis.line = element_line(colour = "black"))
aqz6 <- aqz5 + scale_color_manual(values=c("black", "green"))
aqz7 <- aqz6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
aqz8 <- aqz7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
aqz9 <- aqz8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
aqz9


### exporting high res image
# PDF
pdf(file = "Faunal taxa richness.pdf", width = 8, height = 6, family = "Helvetica")
aqz9
dev.off()

# TIFF
tiff("Faunal taxa richness.tiff", height = 12, width = 17, units = 'cm', res = 300)
aqz9
dev.off()


# Housekeeping
graphics.off() 
rm(list=ls())
