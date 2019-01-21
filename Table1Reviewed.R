##############################################################################################################
# 
# 
# Program to Plot table 1 inthe laser diffraction paper, with all the information requested by the reviewwers
# 
# Felipe Montes 2018  12 13
# 
############################################################################################################### 



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;



# Preliminaries
rm(list = ls())
# Set your working directory to some place you can find

setwd("C:/Felipe/LaserDifractionSoilTextureAnalysis/NAPTSoilsData") ;

# Load the data from (file='NAPTTexturePlot.RData') obtained from runing the NAPTTexturePlot.R

# Package for writing and reading excel files

library(XLConnect) ;

library(stringr) ;


load(file='NAPTTexturePlot.RData');


Table1.data<-readWorksheetFromFile("C:\\Felipe\\LaserDifractionSoilTextureAnalysis\\Manuscript\\Table120180927.xlsx", sheet="New",startCol= 1, endCol=6) ;

head(Table1.data)
str(Table1.data)


head(Paper.Samples)
str(Paper.Samples)

Table1Papersamples<-merge(Table1.data, Paper.Samples, by.x='Sample',by.y='SAMPLE', all.x=T) ;

writeWorksheetToFile("C:\\Felipe\\LaserDifractionSoilTextureAnalysis\\Manuscript\\Table120180927.xlsx",Table1Papersamples, sheet="Table1Papersamples") ;

head(NAPT.all)

str(NAPT.all)


# Get all the data from the NAPT.all files, screened for the selected samples

#first transform the sample name to a compatible name

NAPT.all$YEAR<-str_split(NAPT.all$SAMPLE, '-', simplify=T )[,1] ;
NAPT.all$SampleNo<-str_split(NAPT.all$SAMPLE, '-', simplify=T )[,2] ;
NAPT.all$Sample<-paste0(NAPT.all$YEAR,'-',NAPT.all$SampleNo) ;


head(NAPT.all)

Table1NAPTall<-unique(NAPT.all[which(NAPT.all$Sample %in% Table1.data$Sample),]) ;

head(Table1NAPTall)

# write it into an excell spreadsheet 

writeWorksheetToFile("C:\\Felipe\\LaserDifractionSoilTextureAnalysis\\Manuscript\\Table120180927.xlsx",Table1NAPTall, sheet="Table1NAPTalls") ;

