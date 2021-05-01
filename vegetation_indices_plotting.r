##################################################
### PLOTTING VEGETATION STRUCTURE INDICIES TO CHOSE THE APPROPTIATE INDEX FOR MODELLING


# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
dat1<-read.csv(file.choose()) # Merged_Network_indices.csv

dfLik <- subset(dat1, Site == "Likupang")

dfTiw <- subset(dat1, Site == "Tiwoho")



#################################
#### which vegetation indices? ##
#################################

### Likupang

cor.test(dfLik$CaCo_index, dfLik$Foliage_projective_cover)

### Tiwoho


cor.test(dfTiw$CaCo_index, dfTiw$Foliage_projective_cover )


#############################################################
#### Plotting Likupang ######################################
#############################################################

library(ggplot2)



## scattterplot Foliage Projective Cover vs CaCo Vegetation Index

reg_line_xz <- lm(Foliage_projective_cover  ~ CaCo_index, data = dfLik)


xz1 <- ggplot(data=dfLik, aes(x = CaCo_index, y = Foliage_projective_cover , color = Treatment)) +
  geom_point() 
xz2 <- xz1 + labs(x = "CaCo Vegetation Index", y = "Foliage Projective Cover") 
xz3 <- xz2 + theme_classic()
xz4 <- xz3 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
xz5 <- xz4 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
xz6 <- xz5 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
xz7 <- xz6 + theme(legend.key = element_rect(fill = NA),
                   legend.title = element_text(color = "black",
                                               size = 14, face = 2))
xz8 <- xz7 + scale_color_discrete("Treatment:") +
  guides(color = guide_legend(override.aes = list(size = 4)))
xz9 <- xz8 + geom_point(size = 3)
xz10 <- xz9 + ggtitle("Likupang")
xz11 <- xz10 + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
xz12 <- xz11 + xlim(40, 90) + ylim(35, 100)
xz12

xz13 <- xz12 + geom_abline(intercept = coefficients(reg_line_xz)[1],
                           slope = coefficients(reg_line_xz)[2],
                           color = "grey", size = 1)
xz13

### exporting high res image
# PDF
pdf(file = "Foliage Projective Cover vs CaCo scatterplot Likupang.pdf", width = 8, height = 6, family = "Helvetica")
xz12
dev.off()

# TIFF
tiff("Foliage Projective Cover vs CaCo scatterplot Likupang.tiff", height = 12, width = 17, units = 'cm', res = 300)
xz12
dev.off()

### exporting high res image with trend line
# PDF
pdf(file = "Foliage Projective Cover vs CaCo scatterplot Likupang with trend line.pdf", width = 8, height = 6, family = "Helvetica")
xz13
dev.off()

# TIFF
tiff("Foliage Projective Cover vs CaCo scatterplot Likupang with trend line.tiff", height = 12, width = 17, units = 'cm', res = 300)
xz13
dev.off()


#############################################################
#### Plotting Tiwoho   ######################################
#############################################################


## scatterplot Foliage Projective Cover vs CaCo Vegetation Index

reg_line_xv <- lm(Foliage_projective_cover  ~ CaCo_index, data = dfTiw)


xv1 <- ggplot(data=dfTiw, aes(x = CaCo_index, y = Foliage_projective_cover , color = Treatment)) +
  geom_point() 
xv2 <- xv1 + labs(x = "CaCo Vegetation Index", y = "Foliage Projective Cover") 
xv3 <- xv2 + theme_classic()
xv4 <- xv3 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
xv5 <- xv4 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
xv6 <- xv5 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
xv7 <- xv6 + theme(legend.key = element_rect(fill = NA),
                   legend.title = element_text(color = "black",
                                               size = 14, face = 2))
xv8 <- xv7 + scale_color_discrete("Treatment:") +
  guides(color = guide_legend(override.aes = list(size = 4)))
xv9 <- xv8 + geom_point(size = 3)
xv10 <- xv9 + ggtitle("Tiwoho")
xv11 <- xv10 + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
xv12 <- xv11 + xlim(0, 85) + ylim(0, 100)
xv12

xv13 <- xv12 + geom_abline(intercept = coefficients(reg_line_xv)[1],
                           slope = coefficients(reg_line_xv)[2],
                           color = "grey", size = 1)
xv13

### exporting high res image
# PDF
pdf(file = "Foliage Projective Cover vs CaCo scatterplot Tiwoho.pdf", width = 8, height = 6, family = "Helvetica")
xv12
dev.off()

# TIFF
tiff("Foliage Projective Cover vs CaCo scatterplot Tiwoho.tiff", height = 12, width = 17, units = 'cm', res = 300)
xv12
dev.off()

### exporting high res image with trend line
# PDF
pdf(file = "Foliage Projective Cover vs CaCo scatterplot Tiwoho with trend line.pdf", width = 8, height = 6, family = "Helvetica")
xv13
dev.off()

# TIFF
tiff("Foliage Projective Cover vs CaCo scatterplot Tiwoho with trend line.tiff", height = 12, width = 17, units = 'cm', res = 300)
xv13
dev.off()



# Housekeeping
graphics.off() 
rm(list=ls())