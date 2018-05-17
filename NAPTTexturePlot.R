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

NAPT.data<-readWorksheetFromFile("Results_data_all.xlsx", sheet="Combined", startRow=1, endRow=864) ;
str(NAPT.data)
head(NAPT.data)

NAPT.data.head<-NAPT.data[c(1,2),] ;

NAPT.data.nohead<-NAPT.data[-c(1,2),] ;


NAPT.data.nohead$SAMPLE<-str_split(str_split(NAPT.data.nohead$X, " ", simplify = T)[,2], "_" ,simplify = T)[,1] ;

NAPT.data.nohead$VALUE<-str_split(str_split(NAPT.data.nohead$X, " ", simplify = T)[,2], "_" ,simplify = T)[,2] ;


NAPT.data.nohead.n_rows<-which(NAPT.data.nohead[,1] == 'n', arr.ind = T)  ;

NAPT.data.nohead[NAPT.data.nohead.n_rows, 'SAMPLE']<-NAPT.data.nohead[NAPT.data.nohead.n_rows+1, 'SAMPLE'] ;

NAPT.data.nohead[NAPT.data.nohead.n_rows, 'VALUE']<-NAPT.data.nohead[NAPT.data.nohead.n_rows, 'X'] ;


str(NAPT.data.nohead)
head(NAPT.data.nohead)

NAPT.data.Median<-NAPT.data.nohead[NAPT.data.nohead$VALUE == 'Median',]
str(NAPT.data.Median)
head(NAPT.data.Median)

NAPT.Texture.Hydrom<-NAPT.data.Median[,c(1,2,3,4,8,9)] ;

NAPT.Texture.Hydrom$ANALYSIS<-c('Hydrometer') ;
head(NAPT.Texture.Hydrom)


names(NAPT.Texture.Hydrom)[1:4]<-c('Sample','SAND' , 'SILT' , 'CLAY') ;


str(NAPT.Texture.Hydrom)
head(NAPT.Texture.Hydrom)

NAPT.Texture.Pipet<-NAPT.data.Median[,c(1,5,6,7,8,9)] ;

NAPT.Texture.Pipet$ANALYSIS<-c('Pipette')  ;

str(NAPT.Texture.Pipet)
head(NAPT.Texture.Pipet,20)

names(NAPT.Texture.Pipet)[1:4]<-c('Sample', 'SAND' , 'SILT' , 'CLAY') ;


NAPT.all<-rbind(NAPT.Texture.Hydrom,NAPT.Texture.Pipet)  ;

str(NAPT.all)
head(NAPT.all,20)



NAPT.Texture.data<-data.frame(as.numeric(NAPT.all$SAND),as.numeric(NAPT.all$SILT),as.numeric(NAPT.all$CLAY),NAPT.all[,c(1,5,6,7)]) ;

str(NAPT.Texture.data)

names(NAPT.Texture.data)[c(1,2,3)]<-c('SAND' , 'SILT' , 'CLAY') ;

head(NAPT.Texture.data)
str(NAPT.Texture.data)

which(is.na(NAPT.Texture.data$SAND))


NAPT.Texture.data.norm.1<-TT.normalise.sum(NAPT.Texture.data[1:511,])

NAPT.Texture.data.norm.2<-data.frame(NAPT.Texture.data.norm.1,NAPT.Texture.data[1:511,])

which(is.na.data.frame(NAPT.Texture.data.norm.2),arr.ind = T)

NAPT<-NAPT.Texture.data.norm.2

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




ALLP.data.1<-readWorksheetFromFile("Results_data_all.xlsx", sheet="ALP",startCol= 1, endCol=7) ;
str(ALLP.data.1)
head(ALLP.data.1)


ALLP.data.2<-ALLP.data.1[,c('Sand_Mean' , 'Silt_Mean' , 'Clay_Mean', 'Sample')] ;


names(ALLP.data.2)[1:3]<-c('SAND' , 'SILT' , 'CLAY')  ;


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
  tri.data           = ALLP.norm,
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

ALLP$SAMPLE<-ALLP$Sample ;

ALLP$VALUE<-c('Median') ;

ALLP$ANALYSIS<-c('Hydrometer') ;

head(ALLP)
str(ALLP)

head(NAPT)
str(NAPT)



All.Data<-rbind(NAPT,ALLP) ;

head(All.Data)
tail(All.Data)
str(All.Data)

All.Data$TextureFactor<-as.factor(All.Data$TextureClass) ;
levels(All.Data$TextureFactor)

for (i in levels(All.Data$TextureFactor)) {
  #i=levels(All.Data$TextureFactor)[1]
  
  assign(paste0('Texture_',i),All.Data[All.Data$TextureFactor==i,])
  
}


################### Select the Samples

Selected_Samples<-c(Texture_C$SAMPLE, Texture_CL$SAMPLE[1:6] , Texture_L$SAMPLE[1:6], Texture_LS$SAMPLE[1:6], Texture_S$SAMPLE[1:6],Texture_SCL$SAMPLE, Texture_SICL$SAMPLE[1:6],Texture_SIL$SAMPLE[1:6], Texture_SL$SAMPLE[1:6])



Paper.Samples<-All.Data[All.Data$SAMPLE %in% Selected_Samples,c(1,2,3,4,5,6,8,9,10,11) ]


head(Paper.Samples)

names(Paper.Samples)[1:6]<-c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm', 'SAND' , 'SILT' , 'CLAY')  ;

##### Sample 2011-112 was not available  ######

Paper.Samples[!which(Paper.Samples$SAMPLE == "2011-112"),]

writeWorksheetToFile("Results_data_all.xlsx",Paper.Samples[-c(49,113),], sheet="Selected_Original") ;





