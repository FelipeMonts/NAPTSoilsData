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
# install.packages('tabulizer', dependencies = TRUE)
# install.packages('stringi', dependencies = TRUE)


# httr is a package for downloading html
library(httr)
# A package for manipulating strings
library(stringr)
library(stringi)



dir.create("../ALPP_PDFs");

# for (i in seq(30,34)) {
#   download.file(paste0('https://www.collaborativetesting.com/assets/news/', i ,'_WebSum.pdf'), destfile= paste0('../ALPP_PDFs/','ALLP', i, '.pdf'), mode = 'wb' )
# 
# }
  


library(tabulizer)
library(dplyr)



  ALPP.Tables <- extract_tables(paste0('../ALPP_PDFs/','ALLP',32, '.pdf'), pages=c(131,133,135)) ;
  
  
  
  Sample.ID<-c('SRS1701',
               'SSR1702',
               'SRS1703',
               'SRS1704',
               'SRS1615'  ) ;
  # 
  # for (k in seq(1,length(NAPT.archive.paths))) {
  #   download.file(NAPT.archive.paths[k], destfile = paste0("../NAPT_ARCHIVE_PDFs/pdf_",k,".pdf"), mode='wb')
  #   }
  
  ########## Extract the Sand Data Summary ########################
  
  
Texture.data.0<-ALPP.Texture
  
sss<-ALPP.Tables[[1]];
  
Median.row<-which(sss[,1] == "Grand Median") ;
MAD.row<-which(sss[,1] == "Median Abs Dev") ;

 
data.sss<-data.frame(as.numeric(sss[Median.row,which(!is.na(as.numeric(sss[Median.row,])))]), as.numeric(sss[MAD.row,which(!is.na(as.numeric(sss[MAD.row,])))]))

str(data.sss)

data.sss$Sample<-Sample.ID  ;

names(data.sss)[1:2]<-c('Sand_Mean','Sand_MAD' ) ;
 
 
  
########## Extract the Silt Data Summary ########################
  
lll<-ALPP.Tables[[2]];

Median.row<-which(lll[,1] == "Grand Median") ;
MAD.row<-which(lll[,1] == "Median Abs Dev") ;




data.lll<-data.frame(as.numeric(lll[Median.row,which(!is.na(as.numeric(lll[Median.row,])))]), as.numeric(lll[MAD.row,which(!is.na(as.numeric(lll[MAD.row,])))]))

str(data.lll)


data.lll$Sample<-Sample.ID  ;

names(data.lll)[1:2]<-c('Silt_Mean' , 'Silt_MAD' ) ;
  
  
 
  
  ########## Extract the Clay Data Summary ########################
  

ccc<-ALPP.Tables[[3]];

Median.row<-which(ccc[,1] == "Grand Median") ;
MAD.row<-which(ccc[,1] == "Median Abs Dev") ;




data.ccc<-data.frame(as.numeric(ccc[Median.row,which(!is.na(as.numeric(ccc[Median.row,])))]), as.numeric(ccc[MAD.row,which(!is.na(as.numeric(ccc[MAD.row,])))]))

str(data.ccc)


data.ccc$Sample<-Sample.ID  ;

names(data.ccc)[1:2]<-c('Clay_Mean' , 'Clay_MAD') ;


  ########### combine the data together ##################
  
  Texture.data<-merge(merge(data.sss, data.lll),data.ccc) ;
  
  ALPP.Texture<-rbind(Texture.data.0,Texture.data)  ;
  
  # ALPP.Texture<-Texture.data  ;
  
  rm(Texture.data , Texture.data.0)
  
  
  


