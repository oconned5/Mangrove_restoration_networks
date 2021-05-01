###########################################
### Species level indicies plotting


# Housekeeping
graphics.off() 
rm(list=ls())

# Read in the data
df1<-read.csv(file.choose()) #species_level_indicies_plants.csv

## Normalised degree modified for beta glm
ND <- df1$normalised.degree - 0.00001 # put normalised degree between 0-1 for beta glm


dat1 <- cbind(df1, ND)


dfLik <- subset(dat1, Site == "Likupang")

dfTiw <- subset(dat1, Site == "Tiwoho")

# Packages
#install.packages("ggplot2")
#install.packages("ggpubr")

library(ggpubr)
library(ggplot2)


################################################
####### Tiwoho ###############################
################################################

###########################
## Ceriops tagal ##
###########################

dfTiwCer <- subset(dfTiw, network_lower_level_shorthand == "P05" & Treatment != "Mixed Species Regeneration") 


##############################
## Normalised Degree #########
##############################


#dfTiwCer <- within(dfTiwCer, Treatment <- relevel(Treatment, ref = "Mixed Species Regeneration"))

#factor(dfTiwCer$Treatment, levels = c("Mixed Species Regeneration", "Monoculture Reforestation", "Reference Forest"))

## boxplot Normalised Degree vs treatments

pw11 <- ggplot(data=dfTiwCer, aes(x=Treatment, y=ND)) + geom_boxplot(aes(fill=Treatment))
pw12 <- pw11 + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
pw13 <- pw12 + ylab("Normalised Degree")
pw14 <- pw13 + xlab("Tiwoho")
pw15 <- pw14 + theme(axis.line = element_line(colour = "black"))
pw16 <- pw15 + scale_color_manual(values=c("black", "green"))
pw17 <- pw16 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
pw18 <- pw17 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
pw19 <- pw18 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
pw19a <- pw19 + ggtitle("Ceriops tagal")
pw19b <- pw19a + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))
pw19c <- pw19b + theme(legend.position = "none")
pw20 <- pw19c + scale_fill_manual(breaks = c("Reference Forest", "Monoculture Reforestation"), 
                                  values=c("steelblue3", "green4"))
pw20

### exporting high res image
# PDF
pdf(file = "Ceriops tagal Tiwoho Normalised Degree by treatment.pdf", width = 8, height = 6, family = "Helvetica")
pw20
dev.off()

# TIFF
tiff("Ceriops tagal Tiwoho Normalised Degree by treatment.tiff", height = 12, width = 17, units = 'cm', res = 300)
pw20
dev.off()



################################################
####### Likupang ###############################
################################################

###########################
## Bruguiera gymnorrhiza ##
###########################

dfLikBru <- subset(dfLik, network_lower_level_shorthand == "P02") 

##############################
## Normalised Degree #########
##############################


#dfLikBru <- within(dfLikBru, Treatment <- relevel(Treatment, ref = "Mixed Species Regeneration"))

## boxplot Normalised Degree vs treatment

p1 <- ggplot(data=dfLikBru, aes(x=Treatment, y=ND)) + geom_boxplot(aes(fill=Treatment))
p2 <- p1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
p3 <- p2 + ylab("Normalised Degree")
p4 <- p3 + xlab("Likupang")
p5 <- p4 + theme(axis.line = element_line(colour = "black"))
p6 <- p5 + scale_color_manual(values=c("black", "green"))
p7 <- p6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
p8 <- p7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
p9 <- p8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
p9a <- p9 + ggtitle("Bruguiera gymnorrhiza")
p9b <- p9a + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))
p9c <- p9b + theme(legend.position = "none")
p10 <- p9c + scale_fill_manual(breaks = c("Reference Forest", "Mixed Species Regeneration"), 
                               values=c("steelblue3", "indianred2"))
p10

### exporting high res image
# PDF
pdf(file = "Bruguiera gymnorrhiza Normalised Degree by Site.pdf", width = 8, height = 6, family = "Helvetica")
p10
dev.off()

# TIFF
tiff("Bruguiera gymnorrhiza Normalised Degree  by Site.tiff", height = 12, width = 17, units = 'cm', res = 300)
p10
dev.off()


###########################
## Rhizophora apiculata  ##
###########################

dfLikRhAp <- subset(dfLik, network_lower_level_shorthand == "P06") 

##############################
## Normalised Degree #########
##############################



#dfLikRhAp <- within(dfLikRhAp, Treatment <- relevel(Treatment, ref = "Mixed Species Regeneration"))

#factor(dfLikRhAp$Treatment, levels = c("Mixed Species Regeneration", "Monoculture Reforestation", "Reference Forest"))

## boxplot Normalised Degree vs treatments

pz1 <- ggplot(data=dfLikRhAp, aes(x=Treatment, y=ND)) + geom_boxplot(aes(fill=Treatment))
pz2 <- pz1 + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
pz3 <- pz2 + ylab("Normalised Degree")
pz4 <- pz3 + xlab("Likupang")
pz5 <- pz4 + theme(axis.line = element_line(colour = "black"))
pz6 <- pz5 + scale_color_manual(values=c("black", "green"))
pz7 <- pz6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
pz8 <- pz7 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
pz9 <- pz8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
pz9a <- pz9 + ggtitle("Rhizophora apiculata")
pz9b <- pz9a + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))
pz9c <- pz9b + theme(legend.position = "none")
pz10 <- pz9c + scale_fill_manual(breaks = c("Mixed Species Regeneration", "Monoculture Reforestation", "Reference Forest"), 
                                 values=c("indianred2", "green4", "steelblue3"))
pz10



### exporting high res image
# PDF
pdf(file = "Rhizophora apiculata Likupang Normalised Degree by Site.pdf", width = 8, height = 6, family = "Helvetica")
pz10
dev.off()

# TIFF
tiff("Rhizophora apiculata Likupang Normalised Degree by Site.tiff", height = 12, width = 18, units = 'cm', res = 300)
pz10
dev.off()


##################################################################
##### Rhizorphora mucronata ######################################
##################################################################

dfLikRhMu <- subset(dfLik, network_lower_level_shorthand == "P07") 

##############################
## Normalised Degree #########
##############################

#factor(dfLikRhMu$Treatment, levels = c("Mixed Species Regeneration", "Monoculture Reforestation", "Reference Forest"))

## boxplot Normalised Degree vs treatments

pz11 <- ggplot(data=dfLikRhMu, aes(x=Treatment, y=ND)) + geom_boxplot(aes(fill=Treatment))
pz12 <- pz11 + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
pz13 <- pz12 + ylab("Normalised Degree")
pz14 <- pz13 + xlab("Likupang")
pz15 <- pz14 + theme(axis.line = element_line(colour = "black"))
pz16 <- pz15 + scale_color_manual(values=c("black", "green"))
pz17 <- pz16 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
pz18 <- pz17 +  theme(axis.text.x=element_text(face="bold", size=10, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=10, colour = "black"))
pz19 <- pz18 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
pz19a <- pz19 + ggtitle("Rhizorphora mucronata")
pz19b <- pz19a + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))
pz19c <- pz19b + theme(legend.position = "none")
pz20 <- pz19c + scale_fill_manual(breaks = c("Mixed Species Regeneration", "Monoculture Reforestation", "Reference Forest"), 
                                  values=c("indianred2", "green4", "steelblue3"))
pz20


### exporting high res image
# PDF
pdf(file = "Rhizophora mucronata Normalised Degree by Site.pdf", width = 8, height = 6, family = "Helvetica")
pz20
dev.off()

# TIFF
tiff("Rhizophora mucronata Normalised Degree by Site.tiff", height = 12, width = 18, units = 'cm', res = 300)
pz20
dev.off()


######################################################################################
########## PANEL PLOT ################################################################
######################################################################################


panel_plot <- ggarrange(p10, pz10, pz20, pw20, 
                        labels = c("A", "B", "C", "D"),
                        font.label = list(size = 25, color = "red"), 
                        ncol = 2, nrow = 2)
panel_plot
### exporting high res image
# PDF
pdf(file = "Panel Plot Normalised Degree by site.pdf", width = 15, height = 9, family = "Helvetica")
panel_plot
dev.off()

# TIFF
tiff("Panel Plot Normalised Degree by site.tiff", height = 27, width = 40, units = 'cm', res = 300)
panel_plot
dev.off()




# Housekeeping
graphics.off() 
rm(list=ls())
