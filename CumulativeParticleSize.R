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


#install.packages("soiltexture")
library('soiltexture')


########################################################################################################
# 
#                      Get the data from the lasser diffraction instrument for plotting
#
#########################################################################################################


library(XLConnect) ;


LassDiff.name<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_FM20180702.xlsx", sheet="correct (6)",startRow=1, endRow=1, header=F)[,-1] ; 

str(LassDiff.name[1,1])
head(LassDiff.name)

sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1)

LassDiff.1<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_FM20180702.xlsx", sheet="correct (6)",startRow=4, endRow=103, header=F) ; 

head(LassDiff.1)
tail(LassDiff.1)



################ getting the total sample mass and the mass of Sand #################


MassANDSand<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_FM20180702.xlsx", sheet="correct (6)",startRow=109, endRow=110, startCol=3, header=F) ;



names(LassDiff.1)<-c('Size',sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1))

head(MassANDSand)

barplot(LassDiff.1[,2],names.arg =LassDiff.1[,1], horiz = F)




#####################################
# clay is less than 6 microns
# Silt is bethween 6 and 100 microns
#####################################


###### Trying cumulative distribution

cumsum(LassDiff.1[1:74,2])


barplot(cumsum(LassDiff.1[1:74,2]),names.arg =LassDiff.1[1:74,1], horiz = T, xlim=c(0,100))



###### Trying size distribution




LassDiff.1[1:51,c(1,2)]

LassDiff.1[52:74,c(1,2)]

max(LassDiff.1[52:74,c(2,3)])

barplot(height=LassDiff.1[1:74,2],width=rep(1,74), names.arg=LassDiff.1[1:74,1], col=NA, border=gray(0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,5))
barplot(height=LassDiff.1[1:74,3],width=rep(1,74), names.arg=LassDiff.1[1:74,1],beside=T, col=gray(0.3,alpha=0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,max(LassDiff.1[52:74,c(2,3)])),add=T)
barplot(as.matrix(LassDiff.1[1:74,3]),beside=T,add=T, col=rgb(1,0,1, alpha=0.5) , horiz = T)



barplot(height=LassDiff.1[1:74,2],width=diff(LassDiff.1[1:74,1],differences = 1), names.arg=LassDiff.1[1:74,1],beside=T, col=rgb(0,0,1, alpha=0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,max(LassDiff.1[52:74,c(2,3)])))
barplot(height=LassDiff.1[1:74,3],width=diff(LassDiff.1[1:74,1],differences = 1), names.arg=LassDiff.1[1:74,1],beside=T, col=rgb(1,0,0, alpha=0.5), horiz = T, ylim=c(0.01,100), xlim=c(0,max(LassDiff.1[52:74,c(2,3)])),add=T)
barplot(as.matrix(LassDiff.1[1:74,3]),beside=T,add=T, col=rgb(1,0,1, alpha=0.5) , horiz = T)

diff(LassDiff.1[1:74,1],differences = 1)



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

ScalingFactor<-(1-(MassANDSand[2,2:length(MassANDSand)]/MassANDSand[1,2:length(MassANDSand)]))  ;



ScaledLD.data<-data.frame(LassDiff.1[1:74,1],as.matrix(LassDiff.1[1:74,2:length(MassANDSand)]) %*% diag(ScalingFactor));


names(ScaledLD.data)<-c('Size',LassDiff.name)

head(ScaledLD.data)

#Try plot the clay and silt in separate colors

# create a data series only with clay

ClayScaled.data<-ScaledLD.data ;
ClayScaled.data[55:74,-1]<-0 ;

head(ClayScaled.data)
# create a data series only with silt

SiltScaled.data<-ScaledLD.data ;
SiltScaled.data[1:54,-1]<-0 ;


################ Ploting the scaled fraction with sand ##########  

######## changing the margin parameter prr mar
par(mar=c(5.1, 10, 4.1, 2.1))

# creating an adequate size labels numbering for the bar plot
SizeLabels<-c(as.character(signif(ScaledLD.data[1:18,1],2)), as.character(signif(ScaledLD.data[19:38,1],2)), as.character(signif(ScaledLD.data[39:55,1],2)), as.character(signif(ScaledLD.data[56:74,1],2)))  ;

i=2


# plotting the figures as a high resolution tiff

for (i in seq(2,length(MassANDSand))) {
  
  tiff(filename=paste0("../Manuscript/Figures/LDPArticleSizeDist_", i,".tiff"), width=3840 , height=3840, pointsize = 80  )
  
  #initilaizing the horizontal bar plot with the first scaled LD results
  
  barplot(height=ClayScaled.data[,i], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(1,0,0,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))
  
  barplot(height=SiltScaled.data[,i], width=rep(1.4,74), space=0.2, col=rgb(0,0,1,0.5), horiz = T,las=1,add=T)
  
  
  
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

############################################# PLot 1  2013-109 and 2016-111 #################################################################

tiff(filename=paste0("../Manuscript/Figures/Comparison", 1,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ClayScaled.data[,16], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(1,1,1,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))

barplot(height=SiltScaled.data[,16], width=rep(1.4,74), space=0.2, col=rgb(1,1,1,0.5), horiz = T,las=1,add=T)

barplot(height=ClayScaled.data[,14], width=rep(1.4,74), space=0.2, col=rgb(1,0,0,0.5), horiz = T,las=1,add=T)

barplot(height=SiltScaled.data[,14], width=rep(1.4,74), space=0.2, col=rgb(1,0,0,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('2013-111','2016-111'), pch=c( 22, 22), pt.bg = c(rgb(1,1,1,0.5), rgb(1,0,0,0.5)), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()

############################################# PLot 2  SRS-1508  and SRS-1604 ####################################################################

tiff(filename=paste0("../Manuscript/Figures/Comparison", 2,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ClayScaled.data[,24], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(0,0,1,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))

barplot(height=SiltScaled.data[,24], width=rep(1.4,74), space=0.2, col=rgb(0,0,1,0.5), horiz = T,las=1,add=T)

barplot(height=ClayScaled.data[,20], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.5), horiz = T,las=1,add=T)

barplot(height=SiltScaled.data[,20], width=rep(1.4,74), space=0.2, col=rgb(0,1,0,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('SRS-1508','SRS-1604'), pch=c( 22, 22), pt.bg = c(rgb(0,0,1,0.5), rgb(0,1,0,0.5)), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

abline(h=91,lty=2, col=rgb(1,0,0,1), lwd=10)


dev.off()




############################################# PLot 3  2013-109 and SRS-1604 ####################################################################

tiff(filename=paste0("../Manuscript/Figures/Comparison", 3,".tiff"), width=3840 , height=3840, pointsize = 80  )

#initilaizing the horizontal bar plot with the first scaled LD results

barplot(height=ClayScaled.data[,14], width=rep(1.4,74),names.arg=SizeLabels[seq(1,74)], space=0.2, col=rgb(1,1,0,0.5), horiz = T, ylim=c(0.01,110),las=1,cex.names = 0.5, cex.axis=1,xlab="Particle Size Fraction (%)", ylab=expression(paste("Equivalent particle size ( ", mu, "m)")))

barplot(height=SiltScaled.data[,14], width=rep(1.4,74), space=0.2, col=rgb(1,1,0,0.5), horiz = T,las=1,add=T)

barplot(height=ClayScaled.data[,20], width=rep(1.4,74), space=0.2, col=rgb(1,0,1,0.5), horiz = T,las=1,add=T)

barplot(height=SiltScaled.data[,20], width=rep(1.4,74), space=0.2, col=rgb(1,0,1,0.5), horiz = T,las=1,add=T)

legend("bottomright", legend = c('2013-109','SRS-1604'), pch=c( 22, 22), pt.bg = c(rgb(1,1,0,0.5), rgb(1,0,1,0.5)), pt.lwd=3)

text(2.5,112, "SILT", col="black")

text(2.5,40, "CLAY", col="black")

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


