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
#                      Get the NAPT soil texture data and format it for plotting
#
#########################################################################################################


library(XLConnect) ;





ALLP.data.1<-readWorksheetFromFile("Results_data_all.xlsx", sheet="ALP",startCol= 1, endCol=7) ;
str(ALLP.data.1)
head(ALLP.data.1)


ALLP.data.2<-ALLP.data.1[,c('Sand_Mean' , 'Silt_Mean' , 'Clay_Mean', 'Sand_MAD' ,'Silt_MAD' , 'Clay_MAD' ,'Sample')] ;


names(ALLP.data.2)<-c('SAND' , 'SILT' , 'CLAY', 'MAD_SAND' , 'MAD_SILT' , 'MAD_CLAY', 'SAMPLE')  ;


ALLP.norm<-TT.normalise.sum(ALLP.data.2)   ;


ALLP<-data.frame(ALLP.norm,ALLP.data.2 )  ;




str(ALLP)
head(ALLP)

###########PLot the Data ######################


TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = ALLP,
  main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="gray",
  cex                = 0.5
)

########################################################################################################
# 
#                      Get the data from the lasser diffraction instrument for plotting
#
#########################################################################################################

LassDiff.name<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_FM20180702.xlsx", sheet="correct (6)",startRow=1, endRow=1, header=F)[,-1] ; 

str(LassDiff.name[1,1])
head(LassDiff.name)

sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1)

LassDiff.1<-readWorksheetFromFile("../Manuscript/USDA Standards_PSA_Mastersizer_FM20180702.xlsx", sheet="correct (6)",startRow=4, endRow=103, header=F) ; 


head(LassDiff.1)
tail(LassDiff.1)

names(LassDiff.1)<-c('Size (??m)',sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1))
