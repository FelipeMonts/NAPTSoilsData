#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;



###### Introduction to Web Scraping #####

# Preliminaries
rm(list = ls())
# Set your working directory to some place you can find

setwd("C:/Felipe/LaserDifractionSoilTextureAnalysis/NAPTSoilsData") ;

# First we will need to install the packages we plan to use for this exercise (
# if they are not already installed on your computer).
# install.packages("httr", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)

# httr is a package for downloading html
library(httr)
# A package for manipulating strings
library(stringr)
library(XML)
#install.packages('rvest')

#install.packages("magrittr")

library(rvest)
library(magrittr)








##########################################################################################################################
# 
#       Gaylon S. Campbell;John M. Norman. An Introduction to Environmental Biophysics (p. 131). Kindle Edition. 
# 
# 
# TABLE 9.1.     Hydraulic properties of soils as a function of soil texture 
# (recomputed  from Rawls et al. 1992).
# 
# Texture                 Silt     Clay     - Y,e       b     Ks
# sand                   0.05     0.03      0.7      1.7     0.0058           0.09        0.03
# loamy sand           0.12     0.07      0.9      2.1     0.0017          0.13        0.06
# sandy loam            0.25     0.10      1.5     3.1     0.00072         0.21        0.1
# loam                   0.40     0.18      I.I  4 . 5   0.00037        0.27        0.12
# silt loam              0.65     0.15     2.1     4.7     0.00019         0.33        0.13
# sandy clay loam    0.13     0.27      2.8     4        0.00012         0.26        0.15
# clay loam             0.34     0.34      2.6     5.2     0.000064       0.32        0.2
# silty clay loam      0.58     0.33      3.3      6.6     0.000042       0.37        0.32
# sandy clay           0.07     0.40      2.9     6        0.000033      0.34        0.24
# silty clay             0.45     0.45      3.4      7.9     0.000025      0.39        0.25
# clay                    0.20     0.60      3.7      7.6     0.000017      0.4          0.27
# 
# TABLE 9.1. Hydraulic properties of soils as a function of soil texture
# (recomputed from Rawls et al. 1992).
# Texture Silt Clay - Y,e b Ks 8_33 0_ 1500
# J/kg kg s m-3 ml/ml ml/ml
# sand 0.05 0.03 0.7 1.7 0.0058 0.09 0.03
# loamy sand 0.12 0.07 0.9 2.1 0.0017 0. I 3 0.06
# sandy loam 0.25 0.10 1.5 3.1 0.00072 0.21 0.1
# loam 0.40 0.18 I.I 4.5 0.00037 0.27 0.12
# silt loam 0.65 0.15 2.1 4.7 0.00019 0.33 0.13
# sandy clay loam 0.13 0.27 2.8 4 0.00012 0.26 0.15
# clay loam 0.34 0.34 2.6 5.2 0.000064 0.32 0.2
# silty clay loam 0.58 0.33 3.3 6.6 0.000042 0.37 0.32
# sandy clay 0.07 0.40 2.9 6 0.000033 0.34 0.24
# silty clay 0.45 0.45 3.4 7.9 0.000025 0.39 0.25
# clay 0.20 0.60 3.7 7.6 0.000017 0.4 0.27

##########################################################################################################################

#install.packages("soiltexture")
library('soiltexture')

TT.plot(class.sys ='none' )

TT.plot(class.sys="USDA.TT")

TT.plot(class.sys = "USDA-NCSS.TT",
        class.p.bg.col=T
)

#######Cambell Soil Texture Data##############

silt<-c(0.05,
        0.12,
        0.25,
        0.4,
        0.65,
        0.13,
        0.34,
        0.58,
        0.07,
        0.45,
        0.2
)
clay<-c(0.03,
        0.07,
        0.1,
        0.18,
        0.15,
        0.27,
        0.34,
        0.33,
        0.4,
        0.45,
        0.6
)

Campbell.Soil<-data.frame(clay*100, silt*100) ;
Campbell.Soil.2<-data.frame(clay*101, silt*101) ;

Campbell.Soil$sand<-100-(Campbell.Soil$silt+Campbell.Soil$clay);
Campbell.Soil.2$sand<-100-(Campbell.Soil.2$silt+Campbell.Soil.2$clay);
names(Campbell.Soil)<-c('CLAY', 'SILT' , 'SAND')
names(Campbell.Soil.2)<-c('CLAY', 'SILT' , 'SAND')

TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = Campbell.Soil,
  main               ="Campbell & NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="RED",
  cex                = 0.5
  )

TT.plot(
  class.sys          ="USDA-NCSS.TT",
  add                = T,
  tri.data           = Campbell.Soil.2,
  main               ="Campbell & NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="BLUE",
  cex                = 0.5
)

########################################################################################################
# 
#                      Get the NAPT soil texture data and format it for plotting
#
#########################################################################################################


library(XLConnect) ;

NAPT.data<-readWorksheetFromFile("Results_data_all.xlsx", sheet="Combined") ;
str(NAPT.data)
head(NAPT.data)


NAPT.data.n<-NAPT.data[(which(NAPT.data[,1] == 'n')),] ;
str(NAPT.data.n)
head(NAPT.data.n)

NAPT.data.Not_n<-NAPT.data[(which(NAPT.data[,1] != 'n')),] ;

str(NAPT.data.Not_n)
head(NAPT.data.Not_n)

NAPT.data.nohead<-NAPT.data.Not_n[-c(1,2),]
head(NAPT.data.nohead)


NAPT.data.Median<-NAPT.data.nohead[grep('Median$',NAPT.data.nohead$X),]

NAPT.Texture.Hygrom<-NAPT.data.Median[,c(1,2,3,4)]
names(NAPT.Texture.Hygrom)<-c('Sample','SAND' , 'SILT' , 'CLAY') ;




str(NAPT.Texture.Hygrom)
head(NAPT.Texture.Hygrom)

NAPT.Texture.Pipet<-NAPT.data.Median[,c(1,5,6,7)] ;

str(NAPT.Texture.Pipet)
head(NAPT.Texture.Pipet,20)

names(NAPT.Texture.Pipet)<-c('Sample', 'SAND' , 'SILT' , 'CLAY') ;


NAPT.all<-rbind(NAPT.Texture.Hygrom,NAPT.Texture.Pipet)

str(NAPT.all)
head(NAPT.all,20)

NAPT.all.notNA<-NAPT.all[which(!(is.na.data.frame(NAPT.all))),]

str(NAPT.all.notNA)
head(NAPT.all.notNA,20)

NAPT.Texture.data<-data.frame(as.numeric(NAPT.all.notNA$SAND),as.numeric(NAPT.all.notNA$SILT),as.numeric(NAPT.all.notNA$CLAY)) ;

str(NAPT.Texture.data)
head(NAPT.Texture.data)
names(NAPT.Texture.data)<-c('SAND' , 'SILT' , 'CLAY') ;
NAPT.Texture.data

NAPT.Texture.data$Sample<-NAPT.all.notNA$Sample ;


which(is.na.data.frame(NAPT.Texture.data),arr.ind = T)



NAPT.Texture.data.norm.1<-TT.normalise.sum(NAPT.Texture.data[c( 1:118),])

NAPT.Texture.data.norm.1$Sample<-NAPT.Texture.data[c( 1:118),4] 

which(is.na.data.frame(NAPT.Texture.data.norm.1),arr.ind = T)

NAPT<-NAPT.Texture.data.norm.1

head(NAPT)


###########PLot the Data ######################

TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = NAPT,
  main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="gray",
  cex                = 0.5
)


########################################################################################################
# 
#                      Get the ALLP soil texture data and format it for plotting
#
#########################################################################################################




ALLP.data.1<-readWorksheetFromFile("Results_data_all.xlsx", sheet="ALP") ;
str(ALLP.data.1)
head(ALLP.data.1)


ALLP.data.2<-ALLP.data.1[,c('Sand_Mean' , 'Silt_Mean' , 'Clay_Mean', 'Sample')] ;


names(ALLP.data.2)[1:3]<-c('SAND' , 'SILT' , 'CLAY')  ;


ALLP<-TT.normalise.sum(ALLP.data.2)

ALLP$Sample<-ALLP.data.2$Sample


###########PLot the Data ######################


TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = ALLP,
  main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="gray",
  cex                = 0.5
)

########### Plot all the data together the Data ######################

TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = NAPT,
  main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="BLUE",
  cex                = 0.5
)

# geo<-TT.plot(
#   class.sys          ="USDA-NCSS.TT",
#   #tri.data           = NAPT,
#   #main               ="NAPT Texture Data"
#   #class.p.bg.col     =T,
# )

  
  
TT.points(
  geo,
  tri.data           = ALLP,
  #main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="RED",
  cex                = 0.5
)


TT.points(
  geo,
  tri.data           = Paper.Samples,
  #main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  col                ="GREEN",
  cex                = 0.6
)

########### Clasiffy the data into textures ######################


NAPT.Texture.clases<-TT.points.in.classes(
  tri.data = NAPT,
  class.sys = "USDA-NCSS.TT",
  PiC.type  = "t"
  
)


NAPT$TextureClass<-NAPT.Texture.clases  ;
head(NAPT)




ALLP.Texture.clases<-TT.points.in.classes(
  tri.data = ALLP,
  class.sys = "USDA-NCSS.TT",
  PiC.type  = "t"
  
)


ALLP$TextureClass<-ALLP.Texture.clases ;

head(ALLP)


All.Data<-rbind(NAPT,ALLP) ;

head(All.Data)
tail(All.Data)

All.Data$TextureFactor<-as.factor(All.Data$TextureClass) ;
levels(All.Data$TextureFactor)

for (i in levels(All.Data$TextureFactor)) {
  #i=levels(All.Data$TextureFactor)[1]
  
  assign(paste0('Texture_',i),All.Data[All.Data$TextureFactor==i,])
  
}


################### Select the Samples

Selected_Samples<-c(Texture_C$Sample, Texture_CL$Sample[1:6] , Texture_L$Sample[1:6], Texture_LS$Sample[1:6], Texture_S$Sample[1:6],Texture_SCL$Sample, Texture_SICL$Sample[1:6],Texture_SIL$Sample[1:6], Texture_SL$Sample[1:6])



Paper.Samples<-All.Data[All.Data$Sample %in% Selected_Samples, ]


head(Paper.Samples)


writeWorksheetToFile("Results_data_all.xlsx",Paper.Samples, sheet="Selected") ;


################### comine data from selected samples with the original data


str(NAPT.data)
head(NAPT.data)

str(Paper.Samples)
head(Paper.Samples)


str(NAPT.data.n)
head(NAPT.data.n)
