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

setwd("C:\\Felipe\\LaserDifractionSoilTextureAnalysis\\NAPTSoilsData") ;


#Loand And install packages 
#install.packages('tibble', dependencies = T)
#install.packages('readxl', dependencies = T)


library(readxl) ;
library(XLConnect) ;
library(openxlsx) ;





###############################################################################################################
#                         Read Standard data into R and compare them.          
###############################################################################################################

StandardDataSSJ<-read.xlsx("Laser and Std Results for SSSAJ.xlsx", sheet="Standard Data", colNames=T, startRow = 2 ) ;

names(StandardDataSSJ)

names(StandardDataSSJ)<-c("Sample", "P_CLAY",   "P_SILT" ,  "P_SAND" ,  "H_CLAY"  , "H_SILT"  , "H_SAND"  )

Ordered.DataSSJ<-StandardDataSSJ[order(StandardDataSSJ$Sample),]




Table1.names<-read.xlsx("../Manuscript/Table1_20190510.xlsx", sheet="New (5)", colNames=F, startRow = 1, rows=c(1,2) ) ;

as.character(Table1.names[1,])


Table1.data<-read.xlsx("../Manuscript/Table1_20190510.xlsx", sheet="New (5)", colNames=F, startRow = 3 ) ;

names(Table1.data)<-as.character(Table1.names[1,])

names(Table1.data)<-c("Sample" , "Location",   "Soil classification" ,"P_Sand",  "P_Silt" , "P_Clay" , "n" ,"H_Sand", "H_Silt" , "H_Clay",                "n" , "TOC " , "CaCO3" )

Ordered.Table1<-Table1.data[order(Table1.data$Sample),]


plot(as.numeric(Ordered.DataSSJ$P_CLAY)*100, type="p", pch=21, col="RED", cex=1.5, bg=NA)
points(Ordered.Table1$P_Clay, type="p" , pch=21, col="BLUE", bg="BLUE")

plot(as.numeric(Ordered.DataSSJ$P_SILT)*100, type="p", pch=21, col="RED", cex=1.5, bg=NA)
points(Ordered.Table1$P_Silt, type="p" , pch=21, col="BLUE", bg="BLUE")

plot(as.numeric(Ordered.DataSSJ$P_SAND)*100, type="p", pch=21, col="RED", cex=1.5, bg=NA)
points(Ordered.Table1$P_Sand, type="p" , pch=21, col="BLUE", bg="BLUE")


plot(as.numeric(Ordered.DataSSJ$H_CLAY)*100, type="p", pch=21, col="RED", cex=1.5, bg=NA)
points(Ordered.Table1$H_Clay, type="p" , pch=21, col="BLUE", bg="BLUE")

plot(as.numeric(Ordered.DataSSJ$H_SILT)*100, type="p", pch=21, col="RED", cex=1.5, bg=NA)
points(Ordered.Table1$H_Silt, type="p" , pch=21, col="BLUE", bg="BLUE")

plot(as.numeric(Ordered.DataSSJ$H_SAND)*100, type="p", pch=21, col="RED", cex=1.5, bg=NA)
points(Ordered.Table1$H_Sand, type="p" , pch=21, col="BLUE", bg="BLUE")


###############################################################################################################
#                         Read Laser Diff  data into R and compare them.          
###############################################################################################################



############################################## Read the names of the samples from the spreadsheet ######################################

LaserSSAJ.data<-read.xlsx("Laser and Std Results for SSSAJ.xlsx", sheet="Laser Diffraction-Sieving Data", colNames=T) ;


names(LaserSSAJ.data)


LaserSSAJ<-LaserSSAJ.data[,order(names(LaserSSAJ.data))]


LassDiff.name<-read.xlsx("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=2, colNames=F, rows=c(2)) ; 



LassDiff.1<-read.xlsx("C:/Felipe/LaserDifractionSoilTextureAnalysis/Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=5, colNames=F, rows=c(5:104),skipEmptyCols=F) ; 

tail(LassDiff.1)
str(LassDiff.1)

names(LassDiff.1)<-c('Row','Size', 'nothing' ,sapply(strsplit(as.character(LassDiff.name[1,]),"/"),"[", 1))


LassDiff.names1<-sapply(strsplit(names(LassDiff.1)[4:54]," "), "[",2) ;

LassDiff.names2<-sapply(strsplit(LassDiff.names1,"-5"),"[",1) ;


names(LassDiff.1)<-c('Row','Size', 'nothing' ,LassDiff.names2)


names(LassDiff.1[,c(2,4:54)])


LassDiff.2<-LassDiff.1[,order(names(LassDiff.1))]


# names(LaserDS.data) %in% LassDiff.names2
# 
# LassDiff.names2 %in% names(LaserDS.data)
# 
# 
# str(names(LaserDS.data))
# str(LassDiff.names2)
# 
# names(LaserDS.data)[c(1,21,31,43,46)]
# 
# match(names(LaserDS.data)[-1],LassDiff.names2)
# 
# match(LassDiff.names2,names(LaserDS.data)[-1])
# 
# 
# 
# names(LaserDS.data)[!(names(LaserDS.data) %in% LassDiff.names2)]
#  
# 
# match(names(LaserDS.data)[!(names(LaserDS.data) %in% LassDiff.names2)], names(LaserDS.data))






##############################################################################################################################################












################ getting the total sample mass and the mass of Sand #################


MassANDSand<-read.xlsx("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=111, rows=c(111,112), cols=(4:55), colNames=F) ;



 
