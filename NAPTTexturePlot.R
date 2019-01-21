##############################################################################################################
# 
# 
# Program to Extract soil Texture data from the NAPT pdf document data base and plot texture plots from them.
# 
# Felipe Montes 2018 
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

# First we will need to install the packages we plan to use for this exercise (
# if they are not already installed on your computer).
# install.packages("httr", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)
# install.packages("soiltexture")


# httr is a package for downloading html
library(httr)
# A package for manipulating strings
library(stringr)
library(XML)
#install.packages('rvest')

#install.packages("magrittr")

library(rvest)
library(magrittr)

# Package for soil texture plotting

library('soiltexture')

# Package for writing and reading excel files

library(XLConnect) ;

########################################################################################################
# 
#                      Get the NAPT soil texture data and format it for plotting
#
#########################################################################################################


NAPT.data<-readWorksheetFromFile("Results_data_all.xlsx", sheet="Combined", startRow=1, endRow=864) ;
str(NAPT.data)
head(NAPT.data)

NAPT.data.head<-NAPT.data[c(1,2),] ;

NAPT.data.nohead<-NAPT.data[-c(1,2),] ;

head(NAPT.data.nohead)

############################## Separate the data into different samples and Values per sample ###########################

#### Get the sample and the value names separated

NAPT.data.nohead$SAMPLE<-str_split(str_split(NAPT.data.nohead$X, " ", simplify = T)[,2], "_" ,simplify = T)[,1] ;

NAPT.data.nohead$VALUE<-str_split(str_split(NAPT.data.nohead$X, " ", simplify = T)[,2], "_" ,simplify = T)[,2] ;


#### Get the rows with n 



NAPT.data.nohead.n_rows<-which(NAPT.data.nohead[,1] == 'n', arr.ind = T)  ;

NAPT.data.nohead[NAPT.data.nohead.n_rows, 'SAMPLE']<-NAPT.data.nohead[NAPT.data.nohead.n_rows+1, 'SAMPLE'] ;

NAPT.data.nohead[NAPT.data.nohead.n_rows, 'VALUE']<-NAPT.data.nohead[NAPT.data.nohead.n_rows, 'X'] ;


str(NAPT.data.nohead)
head(NAPT.data.nohead)



##### separate the Median, MAD and N rows into different groups



NAPT.data.Median<-NAPT.data.nohead[NAPT.data.nohead$VALUE == 'Median',]

str(NAPT.data.Median)
head(NAPT.data.Median)

NAPT.data.MAD<-NAPT.data.nohead[NAPT.data.nohead$VALUE == 'MAD',]

str(NAPT.data.MAD)
head(NAPT.data.MAD)


NAPT.data.n<-NAPT.data.nohead[NAPT.data.nohead$VALUE == 'n',]

head(NAPT.data.n)


#### Caluclate the mode of the number of samples reported for each method

names(table(unlist(NAPT.data.n[,c('V1' , 'V2' , 'V3')])))[table(unlist(NAPT.data.n[,c('V1' , 'V2' , 'V3')]))==max(table(unlist(NAPT.data.n[,c('V1' , 'V2' , 'V3')])))]


names(table(unlist(NAPT.data.n[,c('V4' , 'V5' , 'V6')])))[table(unlist(NAPT.data.n[,c(c('V4' , 'V5' , 'V6'))]))==max(table(unlist(NAPT.data.n[,c(c('V4' , 'V5' , 'V6'))])))]


str(NAPT.data.n)
head(NAPT.data.n)

#### Merge the groups back together by SAMPLE

#### First Merge the Hydrometer group of measurements

NAPT.Texture.Hydrometer<-merge(NAPT.data.Median[,c('X', 'V1' , 'V2' , 'V3' , 'SAMPLE' , 'VALUE')],NAPT.data.MAD[,c('X', 'V1' , 'V2' , 'V3' , 'SAMPLE' , 'VALUE')], by= 'SAMPLE');


NAPT.Texture.Hydrometer$ANALYSIS<-c('Hydrometer') ;
names(NAPT.Texture.Hydrometer)[c(3,4,5,8,9,10)]<-c('SAND.Med', 'SILT.Med','CLAY.Med' ,'SAND.MAD', 'SILT.MAD','CLAY.MAD' )

str(NAPT.Texture.Hydrometer)
head(NAPT.Texture.Hydrometer,30)

which(is.na(NAPT.Texture.Hydrometer),arr.ind = T)

#### Second Merge the Pipette group of measurements

NAPT.Texture.Pipette<-merge(NAPT.data.Median[,c('X', 'V4' , 'V5' , 'V6' , 'SAMPLE' , 'VALUE')],NAPT.data.MAD[,c('X', 'V4' , 'V5' , 'V6' , 'SAMPLE' , 'VALUE')], by= 'SAMPLE');

NAPT.Texture.Pipette$ANALYSIS<-c('Pipette') ;

names(NAPT.Texture.Pipette)[c(3,4,5,8,9,10)]<-c('SAND.Med', 'SILT.Med','CLAY.Med' ,'SAND.MAD', 'SILT.MAD','CLAY.MAD' )

str(NAPT.Texture.Pipette)
head(NAPT.Texture.Pipette,30)


#### Find the rows with NA in the texture analysis and exclude them ###

NAPT.Texture.Pipette.NA_rows<-unique(which(is.na(NAPT.Texture.Pipette),arr.ind = T)[,1]);


NAPT.Texture.Pipette[-NAPT.Texture.Pipette.NA_rows,]


#### Combine the Hydrometer and pipette data into one 


NAPT.all<-rbind(NAPT.Texture.Hydrometer,NAPT.Texture.Pipette[-NAPT.Texture.Pipette.NA_rows,])

str(NAPT.all)
head(NAPT.all,20)
tail(NAPT.all,20)

### Convert character data to numeric data 

str(NAPT.all)
head(NAPT.all,20)
tail(NAPT.all,20)

NAPT.all$SAND.Med<-as.numeric(NAPT.all$SAND.Med) ;
NAPT.all$SILT.Med<-as.numeric(NAPT.all$SILT.Med) ;
NAPT.all$CLAY.Med<-as.numeric(NAPT.all$CLAY.Med) ;

NAPT.all$SAND.MAD<-as.numeric(NAPT.all$SAND.MAD) ;
NAPT.all$SILT.MAD<-as.numeric(NAPT.all$SILT.MAD) ;
NAPT.all$CLAY.MAD<-as.numeric(NAPT.all$CLAY.MAD) ;


str(NAPT.all)
head(NAPT.all,20)
tail(NAPT.all,20)




###########PLot the Data ######################

TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = NAPT.all,
  css.names          =c('CLAY.Med' , 'SILT.Med' , 'SAND.Med'),
  tri.sum.tst        =F,
  main               ="NAPT  and ALP Samples Texture Data",
  #class.p.bg.col     =T,
  #pch                = "circles",
  #col                ="black",
  #cex                = 1,
  frame.bg.col       ="gray75"
)


########################################################################################################
# 
#                      Get the ALLP soil texture data and format it for plotting
#
#########################################################################################################




ALLP.data<-readWorksheetFromFile("Results_data_all.xlsx", sheet="ALP",startCol= 1, endCol=7) ;
str(ALLP.data)
head(ALLP.data)


names(ALLP.data)<-c('SAMPLE', 'SAND.MEAN' , 'SAND.MAD', 'SILT.MEAN' ,  'SILT.MAD',  'CLAY.MEAN', 'CLAY.MAD' )  ;



str(ALLP.data)
head(ALLP.data)

###########PLot the Data ######################


TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = ALLP.data,
  css.names          =c('CLAY.MEAN' , 'SILT.MEAN' , 'SAND.MEAN'),
  tri.sum.tst        =F
  # #class.p.bg.col     =T,
  # pch                = "O",
  # col                ="white",
  # cex                = 1,
)



########### Clasiffy the data into textures ######################


NAPT.Texture.clases<-TT.points.in.classes(
  tri.data = NAPT.all,
  class.sys = "USDA-NCSS.TT",
  css.names = c('CLAY.Med' , 'SILT.Med' , 'SAND.Med'),
  tri.sum.tst = F,
  PiC.type  = "t"
  
)


NAPT.all$TextureClass<-NAPT.Texture.clases  ;
head(NAPT.all)




ALLP.Texture.clases<-TT.points.in.classes(
  tri.data = ALLP.data,
  class.sys = "USDA-NCSS.TT",
  css.names = c('CLAY.MEAN' , 'SILT.MEAN' , 'SAND.MEAN'),
  tri.sum.tst = F,
  PiC.type  = "t"
  
)


ALLP.data$ANALYSIS<-c('Hydrometer') ;


ALLP.data$TextureClass<-ALLP.Texture.clases ;




####### Get all the data together to plot


head(ALLP.data)
str(ALLP.data)

head(NAPT.all)
str(NAPT.all)



NAPT<-NAPT.all[,c('SAMPLE', 'SAND.Med' , 'SILT.Med', 'CLAY.Med', 'TextureClass')];

names(NAPT)<-c('SAMPLE', 'SAND' , 'SILT', 'CLAY', 'TextureClass' );

head(NAPT)


ALLP<-ALLP.data[,c('SAMPLE', 'SAND.MEAN' , 'SILT.MEAN', 'CLAY.MEAN', 'TextureClass')];

names(ALLP)<-c('SAMPLE', 'SAND' , 'SILT', 'CLAY', 'TextureClass');

head(ALLP)




All.Data<-rbind(NAPT,ALLP) ;

head(All.Data)
tail(All.Data)
str(All.Data)


######Transform the Texture Class variabl into a Factor 


All.Data$TextureFactor<-as.factor(All.Data$TextureClass) ;
levels(All.Data$TextureFactor)

for (i in levels(All.Data$TextureFactor)) {
  #i=levels(All.Data$TextureFactor)[1]
  
  assign(paste0('Texture_',i),All.Data[All.Data$TextureFactor==i,])
  
}


head(Texture_C)










################### Select the Samples

Selected_Samples<-rbind(Texture_C, Texture_CL[1:6,] , Texture_LS[1:6,], Texture_S[1:6,],Texture_SCL, Texture_SICL[1:6,],Texture_SIL[1:6,], Texture_SL[1:6,])


head(Selected_Samples)

names(Selected_Samples)[1:6]<-c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm', 'SAND' , 'SILT' , 'CLAY')  ;

##### Sample 2011-112 was not available  ######

#Paper.Samples<-Selected_Samples[,c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm')] ;


# writeWorksheetToFile("Results_data_all.xlsx",Selected_Samples, sheet="Selected_Original") ;


########################################################################################################################################
# 
# 
#                     !IMPORTANT NOTE!
#       AFTER WORKING ON THE CODE TO GET THE MOST OF THE DATA READ, A FEW POINTS AND SUMARIES COULD NOT BE READ FOMR THE
#       DO TO THE QUALITY OF THE PDF'S.  THEREFORE THE "Selected_Original" SPREADSHEET IN EXCELL WAS UPDATED AND SAVED WITH 
#       THE NAME "Selected_Final".  THAT IS THE TABLE THAT HAS THE FINAL SAMPLE SELECTION
# 
# 
########################################################################################################################################



##### Reading the table with the final selection of the samples and their data ################################


Paper.Samples<-readWorksheetFromFile("Results_data_all.xlsx", sheet="Selected_Final",startCol= 1, endCol=10) ;

head(Paper.Samples)

########### Plot all the data together the Data ######################

geo.ALLP<-TT.plot(
  class.sys          ="USDA-NCSS.TT",
  frame.bg.col       ="gray75",
  bg                 ="white"
  
)

###### Plot Option 1 ##################



TT.plot(
  #geo.ALLP,
  class.sys          ="USDA-NCSS.TT",
  main               ="USDA - NCRCS Texture classification for the NAPT, ALP and selected soil samples",
  tri.data           = Paper.Samples,
  css.names          =c('CLAY' , 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  pch                =1,
  #bg                 =1,
  col                ="black",
  cex                = 2,
  lwd                = 1,
  frame.bg.col       ="gray75"
  )



TT.points(
  geo.ALLP,
  # class.sys          ="USDA-NCSS.TT",
  tri.data           = NAPT,
  css.names          =c('CLAY' , 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  #main               ="NAPT Texture Data",
  pch                = 24,
  bg                 ="white",
  col                ="black",
  cex                = 1.2,
  lwd                = 0.5
)


TT.points(
  geo.ALLP,
  tri.data           = ALLP,
  css.names          =c('CLAY' , 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  #main               ="NAPT Texture Data",
  #class.p.bg.col     =T,
  pch                = 25,
  bg                 ="grey50",
  col                ="black",
  cex                = 1.2,
  lwd                = 0.5
)


#############  Plot option 2 ############# 

# This is the beter option and therefore will be created as tiff 

tiff(filename="../Manuscript/Figures/AllData.tiff", width=3840 , height=3840, pointsize = 80  )


TT.plot(
  #geo.ALLP,
  class.sys          ="USDA-NCSS.TT",
  main               ='USDA - NCRCS Texture classification for the NAPT, \nALP and selected soil samples',
  tri.data           = NAPT,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  #main               ="NAPT Texture Data",
  pch                = 24,
  bg                 ="WHITE",
  col                ="BLACK",
  cex                = 1.5,
  lwd                = 0.5,
  frame.bg.col       ="gray75"
)




TT.points(
  geo.ALLP,
  tri.data           = ALLP,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  #class.p.bg.col     =T,
  pch                = 25,
  bg                 ="grey50",
  col                ="black",
  cex                = 1.5,
  lwd                = 0.5
)



TT.points(
  geo.ALLP,
  tri.data           = Paper.Samples,
  css.names          =c('CLAY', 'SILT' , 'SAND'),
  tri.sum.tst        =F, # allows toplot texture fraction that do not all to 100 as in the NAPT and ALP databases
  pch                =21,
  bg                 =NA,
  col                ="BLACK",
  cex                = 2.5,
  lwd                = 4
)


legend("topleft", legend = c("NAPT", "ALP", "This Study"), pch=c(24, 25, 21), col=c( 'BLACK', 'BLACK', 'BLACK'), pt.bg = c("white", "grey50", NA), pt.cex=c(1.5,1.5,2.5), pt.lwd=c(0.5,0.5,4))

dev.off()


##################### PDF that need to be saved for the selected samples ################

# Paper.Samples<-Selected_Samples ;
# 
# str(Paper.Samples)
# 
# Selected_Samples.names<-data.frame(Paper.Samples[-c(49,113,120), 'SAMPLE'], as.numeric(str_split(Paper.Samples[-c(49,113,120), 'SAMPLE'], "-", simplify = T) [,1]), as.numeric(str_split(Paper.Samples[-c(49,113,120), 'SAMPLE'], "-", simplify = T) [,2]), stringsAsFactors = F)  ;
# 
# names(Selected_Samples.names)<-c( 'SAMPLE', 'YEAR' , 'No' )
# 
# head(Selected_Samples.names,18)
# 
# str(Selected_Samples.names)
# 
# Selected_Samples.names[order(Selected_Samples.names$YEAR),]
# 
# Selected_Samples.names$Quarter<-cut(Selected_Samples.names$No, breaks=c(100,106,111,116,120),labels=c('Q1' , 'Q2' ,'Q3' , 'Q4') ) ;
# 
# cut(Selected_Samples.names$No, breaks=c(100,106,111,116,120),labels=c('Q1' , 'Q2' ,'Q3' , 'Q4') )
# 
# Selected_Samples.pdfs<-Selected_Samples.names[with(Selected_Samples.names, order(YEAR,Quarter)),]
# 
# 
# 
# writeWorksheetToFile("Results_data_all.xlsx",Selected_Samples.pdfs[-c(49,113),], sheet="Selected_PDF") ;
# 



#################################### Retrieve Soil organic Matter and Carbonates  from PDFS of selected samples  ############################

# Soil organic Matter and Carbonates were copied into an excell sheet and then read into R for formatting  ###

SoilCarbonCarbonates<-readWorksheetFromFile("Results_data_all.xlsx", sheet="Carbon and carbonates", header=T, startRow=2) ;

head(SoilCarbonCarbonates)
str(SoilCarbonCarbonates)

# SCC.t<-data.frame(t(SoilCarbonCarbonates[,7:16]), stringsAsFactors = F);
# str(SCC.t)

#rep(seq(SoilCarbonCarbonates[2,2],SoilCarbonCarbonates[2,3]),each=2)

########### Going  along the analysis direction (Horizontal)  First and then along the year (vertical) second  #########

i=1
j=1

SCC.Data.0<-data.frame(SoilCarbonCarbonates[j,1], rep(seq(SoilCarbonCarbonates[j,2],SoilCarbonCarbonates[j,3]),each=2), SoilCarbonCarbonates[j,(4+(13*(i-1)))], rep(c('Median', 'MAD'),5),t(SoilCarbonCarbonates[j,seq(7+((i-1)*13),7+9+((i-1)*13))])[,1] , SoilCarbonCarbonates[j,6+((i-1)*13)] ) ;

names(SCC.Data.0)<-c('YEAR' , 'SAMPLE' , 'ANALYSIS' ,'TYPE' , 'VALUE' , 'n');

SCC.Data.0

for (j in seq(1,20))  {
  
  for ( i in seq(1,5) ) {
    
    SCC.Data<-data.frame(SoilCarbonCarbonates[j,1], rep(seq(SoilCarbonCarbonates[j,2],SoilCarbonCarbonates[j,3]),each=2), SoilCarbonCarbonates[j,(4+(13*(i-1)))], rep(c('Median', 'MAD'),5),t(SoilCarbonCarbonates[j,seq(7+((i-1)*13),7+9+((i-1)*13))])[,1] , SoilCarbonCarbonates[j,6+((i-1)*13)] ) ;
    
    names(SCC.Data)<-c('YEAR' , 'SAMPLE' , 'ANALYSIS' ,'TYPE' , 'VALUE' , 'n');
    
    print(SCC.Data)
    
    SCC.Data.1<-rbind(SCC.Data.0,SCC.Data)
    
    SCC.Data.0<-SCC.Data.1
    
    rm(SCC.Data,SCC.Data.1)
    
  }
  
  
}


################### Add The SRS-1604 SRS1508 sample data ####################################################

SCC.Data.0

head(SCC.Data.0)


SRS_Samples<-readWorksheetFromFile("Results_data_all.xlsx", sheet="Carbon and carbonatesOriginal", header=T, startRow=1, endRow=25, startCol=42, endCol=48 ) ;


SCC.Data.all<-rbind(SCC.Data.0, SRS_Samples)

head(SCC.Data.all,20)

tail(SCC.Data.all,30)

##############Printout the results to an excell spreadsheet  ###########




writeWorksheetToFile("Results_data_all.xlsx",SCC.Data.all[-c(1:10),], sheet="Carbon and Carbonates table") ;


############## Format the data into a table that is easy to incorporate into Giovani's data structure ####################################

CarbonANDCarbonates.data<-SCC.Data.all[-c(1:10),] ;

CarbonANDCarbonates.data$Sample.ID<-paste0(CarbonANDCarbonates.data$YEAR,"-",CarbonANDCarbonates.data$SAMPLE) ;

CarbonANDCarbonates.noMAD.data<-CarbonANDCarbonates.data[which(CarbonANDCarbonates.data$TYPE == c('Median')),] ;

str(CarbonANDCarbonates.noMAD.data)

head(CarbonANDCarbonates.noMAD.data)

tail(CarbonANDCarbonates.noMAD.data)

levels(CarbonANDCarbonates.noMAD.data$ANALYSIS)

TOC<-CarbonANDCarbonates.data[which(CarbonANDCarbonates.data$TYPE == c('Median') & CarbonANDCarbonates.data$ANALYSIS == c('Soil TOC (Combustion)')),]  ;

TC<-CarbonANDCarbonates.data[which(CarbonANDCarbonates.data$TYPE == c('Median') & CarbonANDCarbonates.data$ANALYSIS == c('Soil Total C (Combustion)')),]  ;

SOM_Walkley<-CarbonANDCarbonates.data[which(CarbonANDCarbonates.data$TYPE == c('Median') & CarbonANDCarbonates.data$ANALYSIS == c('SOM - Walkley-Black')),]  ;


SOM_LOI<-CarbonANDCarbonates.data[which(CarbonANDCarbonates.data$TYPE == c('Median') & CarbonANDCarbonates.data$ANALYSIS == c('SOM - LOI (% Wt loss)')),]  ;

CaCO3<-CarbonANDCarbonates.data[which(CarbonANDCarbonates.data$TYPE == c('Median') & CarbonANDCarbonates.data$ANALYSIS == c('CaCO3 Content')),]  ;


##### Agregating the table

Soil_C_CaCO3.1<-merge(TOC[,c('ANALYSIS', 'VALUE' , 'Sample.ID')],TC[,c('ANALYSIS', 'VALUE' , 'Sample.ID')] , by="Sample.ID", all= T) ;

Soil_C_CaCO3.2<-merge(SOM_Walkley[,c('ANALYSIS', 'VALUE' , 'Sample.ID')],SOM_LOI[,c('ANALYSIS', 'VALUE' , 'Sample.ID')] , by="Sample.ID", all= T ) ;


Soil_C_CaCO3.3<-merge(Soil_C_CaCO3.2, CaCO3[,c('ANALYSIS', 'VALUE' , 'Sample.ID')] , by="Sample.ID", all= T) ;


Soil_C_CaCO3.4<-merge(Soil_C_CaCO3.3, Soil_C_CaCO3.1, by="Sample.ID", all= T) ;

head(Soil_C_CaCO3.4)

tail(Soil_C_CaCO3.4)

Soil_C_CaCO3<-Soil_C_CaCO3.4[,c('Sample.ID', 'VALUE.x.x' , 'VALUE.y.x' , 'VALUE' , 'VALUE.x.y' , 'VALUE.y.y')] ;

names(Soil_C_CaCO3)<-c('Sample.ID','SOM - Walkley-Black', 'SOM - LOI (% Wt loss)','CaCO3 Content' ,'Soil TOC (Combustion)' , 'Soil Total C (Combustion)')

head(Soil_C_CaCO3)

tail(Soil_C_CaCO3,20)




writeWorksheetToFile("Results_data_all.xlsx",Soil_C_CaCO3, sheet="C_CaCO3") ;


###### Check with the data that Giovani has

Giovani_C_CaCO3<-readWorksheetFromFile("../Manuscript/Location of samples completed FM 20180629.xlsx", 'Sheet1', header=T ) ;

str(Giovani_C_CaCO3)
str(Soil_C_CaCO3)

Soil_C_CaCO3[which(!Soil_C_CaCO3$Sample.ID %in% Giovani_C_CaCO3$Sample),'Sample.ID']

Soil_C_CaCO3[which(Soil_C_CaCO3$Sample.ID == c('2016-111')),]


Soil_C_CaCO3[which(Soil_C_CaCO3$Sample.ID == c('2016-114')),]



# ##########################################################################################################################################
# 
# 
# 
#                 Plot Texture data for samples selected for comparison between sedimentation and LDM
# 
# 
# 
# 
##########################################################################################################################################

###### Select the interesting samples to compare

# Original c('2011-118' ,'2011-119' , '2017-113' , '2012-103' , '2013-119' , 'SRS1709' , ' SRSSRS1508')

SamplesToCompare<-c('2011-118' , '2017-113' , '2011-119' , '2012-103' , '2013-119' , 'SRS1709' , ' SRSSRS1508' ,'2013-114', '2013-102', '2015-103' , '2013-109' , '2016-111','2015-113' , '2014-103' , ' SRSSRS1604', 'SRSSRS1508') ;

#SamplesToCompare<-c('2011-106' ,'2011-109' , '2016-111' , '2012-101' , '2013-109' , 'SRS1709' , ' SRSSRS1508') ;

Comparing.Samples.all<-Paper.Samples[which(Paper.Samples$SAMPLE %in% SamplesToCompare),]

Comparing.Samples<-unique(Comparing.Samples.all[which(Comparing.Samples.all$ANALYSIS == 'Hydrometer'), ])

Comparing.Samples



#############  Plot   Updated ############# 


tiff(filename="../Manuscript/Figures/ComparisonTexture.tiff", width=3840 , height=3840, pointsize = 80  )

TT.plot(
  class.sys          ="USDA-NCSS.TT",
  main               ="Sample selected for comparison",
  tri.data           = Comparing.Samples[7,],
  css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  frame.bg.col       ="gray75",
  pch                =16,
  col                 ="Black",
  cex                = 3,
  lwd                = 1
)


TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[5,],
  css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  pch                = 21, 
  bg                 ="GREEN",
  cex                = 3,
  lwd                = 0.5
)



TT.points(
  geo.ALLP,
  tri.data           = Comparing.Samples[2,],
  css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
  pch                = 21, 
  bg                 ="BLUE",
  cex                = 3,
  lwd                = 0.5
)




# TT.points(
#   geo.ALLP,
#   tri.data           = Comparing.Samples[1,],
#   css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
#   pch                = 21, 
#   bg                 ="Yellow",
#   cex                = 1.2,
#   lwd                = 0.5
# )
# 
# 
# TT.points(
#   geo.ALLP,
#   tri.data           = Comparing.Samples[5,],
#   css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
#   pch                = 21, 
#   bg                 ="MAGENTA",
#   cex                = 1.2,
#   lwd                = 0.5
# )
# 
# 
# TT.points(
#   geo.ALLP,
#   tri.data           = Comparing.Samples[7,],
#   css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
#   pch                = 21, 
#   bg                 ="BLACK",
#   cex                = 1.2,
#   lwd                = 0.5
# )
# 
# 
# TT.points(
#   geo.ALLP,
#   tri.data           = Comparing.Samples[6,],
#   css.names          =c('CLAY_Norm' , 'SILT_Norm' , 'SAND_Norm'),
#   pch                = 21, 
#   bg                 ="RED",
#   cex                = 1.2,
#   lwd                = 0.5
# )
# 
# 

#legend("topright", legend = c('SRS-1709', '2013-119', '2011-118' ), pch=c( 22, 22, 22), pt.bg = c(rgb(0,0,0,1) , rgb(0,1,0,1), rgb(0,0,1,1) ), pt.lwd=3)


dev.off()


save.image(file='NAPTTexturePlot.RData');

