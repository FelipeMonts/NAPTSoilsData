##############################################################################################################
# 
# 
# Program to explore plotting cumulative particle size distribution from the laser diffraction analysis
#  and the Malvern Mastersizer 3000
# 
# Felipe Montes 2018 06/30
# 
# Uses the  package 'soiltexture' to plot the texture
# Also uses the package 'XLConnect' to read and write information from excel spreadsheets with soil samples information
# 
# 
############################################################################################################### 



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;



###### Introduction to Web Scraping #####

# Preliminaries
rm(list = ls())
# Set your working directory to some place you can find

setwd("C:/Felipe/LaserDifractionSoilTextureAnalysis/NAPTSoilsData") ;

load('NAPTTexturePlot.RData') ;


#install.packages("soiltexture")
library('soiltexture')


########################################################################################################
# 
#                      Get the data from the lasser diffraction instrument for plotting
#
#########################################################################################################


library(XLConnect) ;

############################################## REad the names of the samples from the spreadsheet ######################################

LassDiff.name<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=2, endRow=2, header=F) ; 


############################################# Compare seleted samples with the origonal data from NAPT and ALP ##########################


# Use the LassDiff.name to get the name of the samples selected in the apropriate format to compare with other sample selection

LDRunSamples.all<-sapply(strsplit(sapply(strsplit(unlist(LassDiff.name[1,1:51]), " "), "[",2), "-5"), '[',1) ;

LDRunSamples.Not_NA<-LDRunSamples.all[!is.na(LDRunSamples.all)];


# LDRunSamples<-data.frame(LDRunSamples.Not_NA,c("Pipette"),c("Hydrometer"))  ;
# 
# match(LDRunSamples[,1:3],NAPT.all[,c("SAMPLE", "ANALYSIS")])




############################################# Read the LD data from the spreadsheet  ########################################################



LassDiff.1<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=5, endRow=104, header=F) ; 


names(LassDiff.1)<-c('Row','Size', 'nothing' ,sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1))

head(LassDiff.1)
tail(LassDiff.1)


################ getting the total sample mass and the mass of Sand #################


MassANDSand<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=111, endRow=112, startCol=4, header=F) ;




head(MassANDSand)

barplot(LassDiff.1[,4],names.arg =LassDiff.1[,2], horiz = F)




#####################################
# clay is less than 6 microns
# Silt is bethween 6 and 100 microns
#####################################


###### Trying cumulative distribution

cumsum(LassDiff.1[1:74,4])


barplot(cumsum(LassDiff.1[1:74,4]),names.arg =LassDiff.1[1:74,2], horiz = T, xlim=c(0,100))



###### Trying size distribution

head(LassDiff.1)


LassDiff.1[1:51,c(2,4)]

LassDiff.1[52:74,c(2,4)]

max(LassDiff.1[52:74,c(2,5)])

barplot(height=LassDiff.1[1:74,4],width=rep(1,74), names.arg=LassDiff.1[1:74,2], col=NA, border=gray(0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,5))
barplot(height=LassDiff.1[1:74,4],width=rep(1,74), names.arg=LassDiff.1[1:74,2],beside=T, col=gray(0.3,alpha=0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,max(LassDiff.1[52:74,c(2,3)])),add=T)
barplot(as.matrix(LassDiff.1[1:74,5]), beside=T,add=T, col=rgb(1,0,1, alpha=0.5) , horiz = T)



# barplot(height=LassDiff.1[1:74,4],width=diff(LassDiff.1[1:74,2],differences = 1), names.arg=LassDiff.1[1:74,2],beside=T, col=rgb(0,0,1, alpha=0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,max(LassDiff.1[52:74,c(2,4)])))
# barplot(height=LassDiff.1[1:74,4],width=diff(LassDiff.1[1:74,2],differences = 1), names.arg=LassDiff.1[1:74,2],beside=T, col=rgb(1,0,0, alpha=0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,max(LassDiff.1[52:74,c(2,3)])),add=T)
# barplot(as.matrix(LassDiff.1[1:74,5]),beside=T,add=T, col=rgb(1,0,1, alpha=0.5) , horiz = T)
# 
# diff(LassDiff.1[1:74,2],differences = 1)



###############################################################################################################################
# # Scaling the laser diffraction data with the total mass sample and the mass of sand extracted with the 53 micron sieve 
# 
# 
#   T =  total mass of sample (g)
#   msand = mass of Sand in the sample extracted with the 53 micron sieve (g)
#   Sand = fraction of sand in the sample = msand/T
#   CLAY%LD = Volume of clay particles as percent of the total volume analzed in the LD method
#   CLAYLD = fraction of clay particles as percent of the total volume analzed in the LD method = CLAY% x 100
#   CLAY= fraction of clay in the sample = [(T - msand) / T] x CLAY% x 100
#                                        = [(T - msand) / T] x CLAYLD
#                                        = [(T/T) - (msand/T)] x CLAYLD
#                                        = [1- Sand] x CLAYLD
#   
#   SILT%LD = Volume of silt particles as percent of the total volume analzed in the LD method
#   SILTLD = fraction of silt particles as percent of the total volume analzed in the LD method = SILT% x 100
#   SILT= fraction of silt in the sample = [(T - msand) / T] x SILT% x 100
#                                        = [(T - msand) / T] x SILTLD
#                                        = [(T/T) - (msand/T)] x SILTLD
#                                        = [1- Sand] x SILTLD
# 
#
# ############################################################################################################################

length(MassANDSand)
head(MassANDSand)





ScalingFactor<-(1-(MassANDSand[2,1:length(MassANDSand)]/MassANDSand[1,1:length(MassANDSand)]))  ;

#  some of the ScalingFactor calculation below produces NA as a result that theres in A 'NA' value in the cell, therefore the result need to be filtered for 'it'NA values.
 
is.na(ScalingFactor)


ScalingFactor.Not_NA<-ScalingFactor[!is.na(ScalingFactor)]

ScaledLD.data<-data.frame(LassDiff.1[1:74,2],as.matrix(LassDiff.1[1:74,seq(4,length(MassANDSand)-1)]) %*% diag(ScalingFactor.Not_NA/100));



names(ScaledLD.data)<-c('Size',LassDiff.name[1:51])

head(ScaledLD.data)


names(ScaledLD.data)

#Try plot the clay and silt in separate colors

# create a data series only with clay

# ClayScaled.data<-ScaledLD.data ;
# #ClayScaled.data[55:74,-1]<-0 ;
# 
# head(ClayScaled.data)
# names(ClayScaled.data)
# # create a data series only with silt
# 
# SiltScaled.data<-ScaledLD.data ;
# #SiltScaled.data[1:54,-1]<-0 ;


################ Ploting the scaled fraction with sand ##########  

######## changing the margin parameter prr mar
#par(mar=c(5.1, 10, 4.1, 2.1))

# creating an adequate size labels numbering for the bar plot
SizeLabels<-c(as.character(signif(ScaledLD.data[1:18,1],2)), as.character(signif(ScaledLD.data[19:38,1],2)), as.character(signif(ScaledLD.data[39:55,1],2)), as.character(signif(ScaledLD.data[56:74,1],2)))  ;


i=2


# plotting the figures as a high resolution tiff

for (i in seq(2,length(MassANDSand))) {
  
  tiff(filename=paste0("../Manuscript/Figures/LDPArticleSizeDist_", i,".tiff"), width=3840 , height=3840, pointsize = 80  )
  
  #initilaizing the horizontal bar plot with the first scaled LD results
  
  barplot(height=ScaledLD.data[,i], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(1,0,0,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))
  
  abline(h=85,lty=2, col=rgb(1,0,0,1), lwd=10)
  
  text(0.01,40, "CLAY", col="black")
  
  
  dev.off()

}

# # ############################################################################################################################
# 
# 
# 
#                   Plotting sample comparisons for the LDM
# 
# 
# # ############################################################################################################################


# Original c('2011-118' ,'2011-119' , '2017-113' , '2012-103' , '2013-119' , 'SRS1709' , ' SRSSRS1508')

#SamplesToCompare<-c('2011-106' ,'2011-109' , '2016-111' , '2012-101' , '2013-109' , 'SRS1709' , ' SRSSRS1508') ;

############################################# PLot 1  2011-118 and 2017-113 #################################################################

tiff(filename=paste0("../Manuscript/Figures/Comparison", 1,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,40], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,1,1), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))

barplot(height=ScaledLD.data[,21], width=rep(1.4,74), space=0.2, col=rgb(1,1,1,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('2017-113', '2011-118'), pch=c( 22, 22), pt.bg = c(rgb(0,0,1,1) , rgb(1,1,1,0.5) ), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()

############################################# PLot 2  2011-119  and 2012-103 ####################################################################

tiff(filename=paste0("../Manuscript/Figures/Comparison", 2,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,2], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,1,0,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))

barplot(height=ScaledLD.data[,47], width=rep(1.4,74), space=0.2, col=rgb(1,1,0,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('2011-119','2012-103'), pch=c( 22, 22), pt.bg = c(rgb(0,1,0,0.5), rgb(1,1,0,0.5)), pt.lwd=3)

text(2.2,113, "SILT", col="black")

text(2.2,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()




############################################# PLot 3  2013-119, SRS-1709 , SRS-1508  ########################################################


tiff(filename=paste0("../Manuscript/Figures/Comparison", 3,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,19], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,0,1), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")),xlim =c(0,2.5))

barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(1,0,1,0.5), horiz = T,las=1,add=T)

barplot(height=ScaledLD.data[,20], width=rep(1.4,74), space=0.2, col=rgb(1,0,0,0.5), horiz = T,las=1,add=T)



legend("bottomright", legend = c('SRS-1709','2013-119', 'SRS-1508'), pch=c( 22, 22 , 22), pt.bg = c(rgb(0,0,0,1), rgb(1,0,1,0.5),rgb(1,0,0,0.5)), pt.lwd=3)

text(2.4,112, "SILT", col="black")

text(2.4,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()


############################################# PLot 4  SRS-1508  and SRS-1604 ####################################################################

tiff(filename=paste0("../Manuscript/Figures/Comparison", 2,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ClayScaled.data[,24], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,1,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))

barplot(height=SiltScaled.data[,24], width=rep(1.4,74), space=0.2, col=rgb(0,0,1,0.5), horiz = T,las=1,add=T)

barplot(height=ClayScaled.data[,20], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.5), horiz = T,las=1,add=T)

barplot(height=SiltScaled.data[,20], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('2013-111','2016-111'), pch=c( 22, 22), pt.bg = c(rgb(0,0,1,0.5), rgb(0,1,0,0.5)), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()

########################################## PLot 5  SRS-1709, 2013-119 and  2011-118  ######################################################


tiff(filename=paste0("../Manuscript/Figures/Comparison", 5,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,19], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,0,0.8), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")), xlim=c(0,3.0))

barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.8), horiz = T,las=1,add=T)

barplot(height=ScaledLD.data[,21], width=rep(1.4,74), space=0.2, col=rgb(0,0,1,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('SRS-1709', '2013-119', '2011-118' ), pch=c( 22, 22, 22), pt.bg = c(rgb(0,0,0,0.8) , rgb(0,1,0,0.8), rgb(0,0,1,0.5) ), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()


########################################## PLot 6  SRS-1709, 2013-119 and  2011-118  ######################################################


tiff(filename=paste0("../Manuscript/Figures/Comparison", 6,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,19], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(1,0,0,0.8), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")), xlim=c(0,3.0))

barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(1,1,0,0.8), horiz = T,las=1,add=T)

barplot(height=ScaledLD.data[,21], width=rep(1.4,74), space=0.2, col=rgb(0,0,0,1), horiz = T,las=1,add=T)

legend("bottomright", legend = c('SRS-1709', '2013-119', '2011-118' ), pch=c( 22, 22, 22), pt.bg = c(rgb(1,0,0,0.8) , rgb(1,1,0,0.8), rgb(0,0,0,1) ), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()



########################################## PLot 7  2011-119 2012-103 SRS-1709 ######################################################


tiff(filename=paste0("../Manuscript/Figures/Comparison", 7,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,19], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,0,0.8), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")), xlim=c(0,3.0))

barplot(height=ScaledLD.data[,3], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.8), horiz = T,las=1,add=T)

barplot(height=ScaledLD.data[,47], width=rep(1.4,74), space=0.2, col=rgb(0,0,1,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('SRS-1709', '2011-119', '2012-103' ), pch=c( 22, 22, 22), pt.bg = c(rgb(0,0,0,0.8) , rgb(0,1,0,0.8), rgb(0,0,1,0.5) ), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()



########################################## PLot 8  2011-119 2012-103 SRS-1709 ######################################################


tiff(filename=paste0("../Manuscript/Figures/Comparison", 8,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ScaledLD.data[,19], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,0,0.8), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")), xlim=c(0,3.0))

barplot(height=ScaledLD.data[,3], width=rep(1.4,74), space=0.2, col=rgb(1,0,0,0.8), horiz = T,las=1,add=T)

barplot(height=ScaledLD.data[,47], width=rep(1.4,74), space=0.2, col=rgb(1,1,0,0.8), horiz = T,las=1,add=T)

legend("bottomright", legend = c('SRS-1709', '2011-119', '2012-103' ), pch=c( 22, 22, 22), pt.bg = c(rgb(0,0,0,0.8) , rgb(1,0,0,0.8), rgb(1,1,0,0.8) ), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()


########################################## Final Manuscript plot Figure 2. SRS-1709, 2013-119 and 2011-118 ###################################



tiff(filename=paste0("../Manuscript/Figures/Figure.2",".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

par(fig = c(0,1,0,1))
par(plt=c(0.1,0.9,0.1,0.9))

barplot(height=ScaledLD.data[,19], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,0,0.8), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=0.5,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")), xlim=c(0,3.0))

barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.6), horiz = T,las=1,add=T)

barplot(height=ScaledLD.data[,21], width=rep(1.4,74), space=0.2, col=rgb(0,0,1,0.7), horiz = T,las=1,add=T)

#legend("bottomright", legend = c('SRS-1709', '2013-119', '2011-118' ), pch=c( 22, 22, 22), pt.bg = c(rgb(0,0,0,0.8) , rgb(0,1,0,0.8), rgb(0,0,1,1) ), pt.lwd=3)

text(2.5,96, "SILT", col="black")

text(2.5,87, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


par(fig = c(0.6,0.95, 0.1, 0.6 ), new=T)


# dev.off()


################################## Trial of insert Texture picture ######################
#par(fig = c(0,1,0,1))
par(fig = c(0.6,0.95, 0.1, 0.6 ), new=T)


# barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.6), horiz = T,las=1,add=T)
TT.plot(
  class.sys          ="USDA-NCSS.TT",
  main               =NA,
  tri.data           = Comparing.Samples[7,],
  css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  frame.bg.col       ="gray75",
  pch                =16,
  col                 ="Black",
  cex                = 1.5,
  lwd                = 1,
  cex.axis           = 0.8,
  lwd.axis           = 0.8,
  lwd.lab            = 0.8,
  cex.lab            =0.8
)


TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[5,],
  css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  pch                = 21, 
  bg                 ="GREEN",
  cex                = 1.5,
  lwd                = 0.5
)



TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[1,],
  css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  pch                = 21, 
  bg                 ="BLUE",
  cex                = 1.5,
  lwd                = 0.5
)


TT.text(
  tri.data           = Comparing.Samples[c(7,5,1),],
  geo                = geo.ALLP,
  css.names          = c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  labels             = Comparing.Samples[c(7,5,1),"SAMPLE"],
  pos                = 3,
  cex                = 1,
  offset             = 1
)
 
dev.off()   






    
################################### Final Manuscript plot 2 pannels, vertical bars. SRS-1709, 2013-119 and 2011-118 ###################################

geo.ALLP<-TT.plot(
  class.sys          ="USDA-NCSS.TT",
  frame.bg.col       ="gray75",
  bg                 ="white"
  
)

tiff(filename=paste0("../Manuscript/Figures/HorizontalDist",".tiff"), width=3840 , height=3840, pointsize = 80)

par(mfrow=c(2,1))

#initilaizing the horizontal bar plot with the first scaled LD results

# par(fig = c(0,1,0,1))
# par(plt=c(0.1,0.9,0.2,0.9))

# par(mar= c(5.1 4.1 4.1 2.1))

par(mar= c(1, 4.1 ,2.1, 2.1))


barplot( height=t(ScaledLD.data[,c(27,15,25)]), beside=T, col= c('GREEN','BLACK','BLUE'), names.arg=SizeLabels[seq(1,74)], axisnames= F, ylim=c(0,0.04), cex.names =0.5, cex.axis=0.5, cex.lab= 0.7, ylab="Particle Size Fraction", xlab=NA, space=c(0,0.1),las=2)


text(182,0.038, "SILT", col="BLACK", cex=0.8)

text(120,0.038, "CLAY", col="BLACK", cex=0.8)

abline(v=157,lty=2, col="RED", lwd=5)

par(mar= c(4.1, 4.1, 0, 2.1))

barplot( height=t(ScaledLD.data[,c(19,8,13)]), beside=T, col= c('BLACK','BLUE','GREEN'), names.arg=SizeLabels[seq(1,74)], ylim=c(0,0.04), cex.names =0.5, cex.axis=0.5, cex.lab= 0.7, ylab="Particle Size Fraction", xlab=expression(paste("Equivalent particle size ( ", mu, "m)")), space=c(0,0.1),las=2)


text(182,0.038, "SILT", col="BLACK", cex=0.8)

text(120,0.038, "CLAY", col="BLACK", cex=0.8)

abline(v=157,lty=2, col="RED", lwd=5)




##################################  insert Texture picture in first panel ######################

par(fig = c(0.12, 0.35, 0.62, 0.98 ), new=T)

par(mar= c(5.1, 4.1, 4.1, 2.1))

# barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.6), horiz = T,las=1,add=T)
TT.plot(
  class.sys          ="USDA-NCSS.TT",
  main               =NA,
  tri.data           = Comparing.Samples[10,],
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  frame.bg.col       ="gray75",
  pch                =16,
  col                 ="Black",
  cex                = 0.5,
  lwd                = 0.3,
  cex.axis           = 0.3,
  lwd.axis           = 0.3,
  lwd.lab            = 0.4,
  cex.lab            =0.4,
  class.lab.show     ='none'
)


TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[9,],
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  pch                = 21,
  bg                 ="GREEN",
  cex                = 0.5,
  lwd                = 0.5
)



TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[14,],
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  pch                = 21,
  bg                 ="BLUE",
  cex                = 0.5,
  lwd                = 0.5
)

Comparing.Samples[14,"SAMPLE"]<-c('SRS1604') ;

TT.text(
  tri.data           = Comparing.Samples[c(10,9,14),],
  geo                = geo.ALLP,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  labels             = Comparing.Samples[c(10,9,14),"SAMPLE"],
  pos                = 3,
  cex                = 0.4,
  offset             = 0.2,
  font               =2
)



##################################  insert Texture picture in second panel ######################

par(fig = c(0.12, 0.35, 0.18, 0.54 ), new=T)

par(mar= c(5.1, 4.1, 4.1, 2.1))

# barplot(height=ScaledLD.data[,18], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.6), horiz = T,las=1,add=T)
TT.plot(
  class.sys          ="USDA-NCSS.TT",
  main               =NA,
  tri.data           = Comparing.Samples[15,],
  #css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, 
  frame.bg.col       ="gray75",
  pch                =16,
  col                 ="BLACK",
  cex                = 0.5,
  lwd                = 0.3,
  cex.axis           = 0.3,
  lwd.axis           = 0.3,
  lwd.lab            = 0.4,
  cex.lab            =0.4,
  class.lab.show     ='none'
)


TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[6,],
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, 
  pch                = 21,
  bg                 ="GREEN",
  cex                = 0.5,
  lwd                = 0.5
)



TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[11,],
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, 
  pch                = 21,
  bg                 ="BLUE",
  cex                = 0.5,
  lwd                = 0.5
)


TT.text(
  tri.data           = Comparing.Samples[15,],
  geo                = geo.ALLP,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, 
  labels             = Comparing.Samples[15,"SAMPLE"],
  pos                = 4,
  cex                = 0.4,
  offset             = 0.2,
  font               =2
)

TT.text(
  tri.data           = Comparing.Samples[6,],
  geo                = geo.ALLP,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, 
  labels             = Comparing.Samples[6,"SAMPLE"],
  pos                = 2,
  cex                = 0.4,
  offset             = 0.2,
  font               =2
)

TT.text(
  tri.data           = Comparing.Samples[11,],
  geo                = geo.ALLP,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, 
  labels             = Comparing.Samples[11,"SAMPLE"],
  pos                = 3,
  cex                = 0.4,
  offset             = 0.2,
  font               =2
)

dev.off()   













        
