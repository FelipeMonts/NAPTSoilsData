##############################################################################################################
# 
# 
# Program to verify the data from the table that it is going to be published as part of the Laser diffraction paper
# 
# Felipe Montes 2019  05 07
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;

#  Set Working directory

setwd("C:\\Felipe\\LaserDifractionSoilTextureAnalysis\\Manuscript") ;


#Loand And install packages 
#install.packages('tibble', dependencies = T)
#install.packages('readxl', dependencies = T)


library(readxl) ;
library(XLConnect) ;
library(openxlsx) ;





###############################################################################################################
#                         Read data sets into R            
###############################################################################################################


LaserDS.data<-read.xlsx("./Review/Second Review/Laser and Std Results for SSSAJ.xlsx", sheet="Laser Diffraction-Sieving Data", colNames=T) ;


names(LaserDS.data)



############################################## Read the names of the samples from the spreadsheet ######################################

LassDiff.name<-read.xlsx("C:/Felipe/LaserDifractionSoilTextureAnalysis/Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=2, colNames=F, rows=c(2)) ; 




#grep("SRS",LassDiff.name)

############################################# Read the LD data from the spreadsheet  ########################################################




LassDiff.1<-read.xlsx("C:/Felipe/LaserDifractionSoilTextureAnalysis/Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=5, colNames=F, rows=c(5:104),skipEmptyCols=F) ; 

tail(LassDiff.1)
str(LassDiff.1)

names(LassDiff.1)<-c('Row','Size', 'nothing' ,sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1))


#####################################################  Organizing column names to compare them withw the spreadsheet names ##################

LassDiff.names1<-sapply(strsplit(names(LassDiff.1)[4:54]," "), "[",2) ;

LassDiff.names2<-sapply(strsplit(LassDiff.names1,"-5"),"[",1) ;


names(LaserDS.data) %in% LassDiff.names2

LassDiff.names2 %in% names(LaserDS.data)


str(names(LaserDS.data))
str(LassDiff.names2)

names(LaserDS.data)[c(1,21,31,43,46)]

match(names(LaserDS.data)[-1],LassDiff.names2)

match(LassDiff.names2,names(LaserDS.data)[-1])



names(LaserDS.data)[!(names(LaserDS.data) %in% LassDiff.names2)]
 

match(names(LaserDS.data)[!(names(LaserDS.data) %in% LassDiff.names2)], names(LaserDS.data))


##############################################################################################################################################












################ getting the total sample mass and the mass of Sand #################


MassANDSand<-read.xlsx("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=111, rows=c(111,112), cols=(4:55), colNames=F) ;



 
