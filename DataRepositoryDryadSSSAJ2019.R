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

LassDiff.3<-LassDiff.2[,-17]

for (i in seq(1,38)) {
  
  barplot(LassDiff.3[,i], names.arg=LassDiff.3$Size, col="RED", ylab=names(LassDiff.3)[i], main = i)
  barplot(LaserSSAJ[,i], names.arg=LaserSSAJ$Size, add=T, col=NA, border= "BLUE", ylab=names(LaserSSAJ)[i], mgp=c(2,1,0))   
  
}

for (j in seq(39,47)) {
  
  barplot(LassDiff.3[,j], names.arg=LassDiff.3$Size, col="RED", ylab=names(LassDiff.3)[j], main = c(j,j+4))
  barplot(LaserSSAJ[,j+4], names.arg=LaserSSAJ$Size, add=T, col=NA, border= "BLUE", ylab=names(LaserSSAJ)[j+4], mgp=c(2,1,0))   
  
  
}


for (h in seq(51,53)) {
  
  barplot(LassDiff.3[,h], names.arg=LassDiff.3$Size, col="RED", ylab=names(LassDiff.3)[h], main = c(h,h+2))
  barplot(LaserSSAJ[,h+2], names.arg=LaserSSAJ$Size, add=T, col=NA, border= "BLUE", ylab=names(LaserSSAJ)[h+2], mgp=c(2,1,0))     
  
  
}



###############################################################################################################
#                         Read Sand  data into R and compare them.          
###############################################################################################################


SandSSAJ.data<-read.xlsx("Laser and Std Results for SSSAJ.xlsx", sheet="Sand Content", colNames=T) ;


names(SandSSAJ.data)


SandSSAJ.1<-SandSSAJ.data[,order(names(SandSSAJ.data))]


names(SandSSAJ)

str(SandSSAJ)



################ getting the total sample mass and the mass of Sand #################


MassANDSand<-read.xlsx("../Manuscript/USDA Standards_PSA_Mastersizer_Felipe_20180824.xlsx", sheet="correct (6) fraction",startRow=111, rows=c(111,112), cols=(4:55), colNames=F) ;


names(LassDiff.1)

names(MassANDSand)<-names(LassDiff.1)[4:54]   ;

str(MassANDSand)

MassANDSand[3,]<-t(MassANDSand)[,2]/t(MassANDSand)[,1]  ;


MassANDSand.1<-MassANDSand[,order(names(MassANDSand))]


str(MassANDSand.1)

plot(t(MassANDSand.1)[1:21,3]*100,  pch=21, col="RED", cex=1.5, bg=NA)
points(t(SandSSAJ.1)[1:21,2], type="p" , pch=21, col="BLUE", bg="BLUE")


plot(t(MassANDSand.1)[1:17,3]*100-as.numeric(t(SandSSAJ.1)[1:17,2]))
text(t(MassANDSand.1)[,3]*100-as.numeric(t(SandSSAJ.1)[,2]),names(MassANDSand.1), pos=3, cex=0.5, col="RED" )
text(t(MassANDSand.1)[,3]*100-as.numeric(t(SandSSAJ.1)[,2]),names(SandSSAJ.1), pos=1, cex=0.5, col="BLUE" )


plot(t(MassANDSand.1)[18:39,3]*100-as.numeric(t(SandSSAJ.1)[17:38,2]))
text((t(MassANDSand.1)[18:39,3]*100-as.numeric(t(SandSSAJ.1)[17:38,2])),names(MassANDSand.1)[18:39], pos=3, cex=0.8, col="RED" )
text(t(MassANDSand.1)[18:39,3]*100-as.numeric(t(SandSSAJ.1)[17:38,2]),names(SandSSAJ.1)[17:38], pos=1, cex=0.8, col="BLUE" )


plot(t(MassANDSand.1)[40:50,3]*100-as.numeric(t(SandSSAJ.1)[43:53,2]))
text((t(MassANDSand.1)[40:50,3]*100-as.numeric(t(SandSSAJ.1)[43:53,2])),names(MassANDSand.1)[40:50], pos=3, cex=0.8, col="RED" )
text(t(MassANDSand.1)[40:50,3]*100-as.numeric(t(SandSSAJ.1)[43:53,2]),names(SandSSAJ.1)[43:53], pos=1, cex=0.8, col="BLUE" )
