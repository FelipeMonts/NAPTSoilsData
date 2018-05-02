#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;



###### Introduction to Web Scraping #####

# Preliminaries
# rm(list = ls())
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

# packages for extracting pdf data and manipulating data
library(tabulizer)
library(dplyr)



dir.create("../ALPP_PDFs");

######################################################################################################################## 
# 
# Download all the pdf files from the https://www.collaborativetesting.com/store/main.aspx?DepartmentId=40
# 
######################################################################################################################## 

# for (i in seq(30,34)) {
#   download.file(paste0('https://www.collaborativetesting.com/assets/news/', i ,'_WebSum.pdf'), destfile= paste0('../ALPP_PDFs/','ALLP', i, '.pdf'), mode = 'wb' )
# 
# }



######################################################################################################################## 
# 
# Extract the information on the tables inside each PDF file
# 
########################################################################################################################


for (i in seq(32,33)) {
  
  i=33
  Texture.data.0<-ALPP.Texture   ;
  
  ALPP.Tables <- extract_tables(paste0('../ALPP_PDFs/','ALLP', i, '.pdf'), pages=c(127:137) ) ;
  
  
  ########## Extract the Sand Data Summary ########################
  
  sss<-ALPP.Tables[grepl('(SubTestCode 189)',ALPP.Tables)][[2]] ;
  
  data.sss<-data.frame(sss[2:4,2:4], stringsAsFactors = F) ;
  
  data.sss$X4<-str_split(sss[2,1], " ")[[1]][1] ;
  
  data.sss$X4[2]<-str_split(sss[3,1], " ")[[1]][3] ;
  
  data.sss$X4[3]<-str_split(sss[4,1], " ")[[1]][4] ;
  
  
  
  
  data.sss$X5<-str_split(sss[2,1], " ")[[1]][2] ;
  
  data.sss$X5[2]<-str_split(sss[3,1], " ")[[1]][4] ;
  
  data.sss$X5[3]<-str_split(sss[4,1], " ")[[1]][5] ;
  
  #str(data.sss)
  
  
  Sand.data<-data.frame(as.numeric(t(data.sss)[,2]),as.numeric(t(data.sss)[,3]))  ;
  Sand.data$Sample<-t(data.sss)[,1]
  names(Sand.data)[1:2]<-c( 'Sand_Mean' , 'Sand_MAD') ;
  
  
  ########## Extract the Silt Data Summary ########################
  
  
  
  sil<-ALPP.Tables[grepl('(SubTestCode 190)',ALPP.Tables)] [[3]] ;
  
  data.sil<-data.frame(sil[2:4,3:5], stringsAsFactors = F) ;
  
  data.sil$X4<-str_split(sil[2,2], " ")[[1]][1] ;
  
  data.sil$X4[2]<-str_split(sil[3,2], " ")[[1]][1] ;
  
  data.sil$X4[3]<-str_split(sil[4,2], " ")[[1]][1] ;
  
  
  data.sil$X5<-str_split(sil[2,2], " ")[[1]][2] ;
  
  data.sil$X5[2]<-str_split(sil[3,2], " ")[[1]][2] ;
  
  data.sil$X5[3]<-str_split(sil[4,2], " ")[[1]][2] ;
  
  #str(data.sil)
  
  
  Silt.data<-data.frame(as.numeric(t(data.sil)[,2]),as.numeric(t(data.sil)[,3]))  ;
  
  Silt.data$Sample<-t(data.sil)[,1]
  names(Silt.data)[1:2]<-c('Silt_Mean' , 'Silt_MAD') ;
  
  
  ########## Extract the Clay Data Summary ########################
  
  
  
  ccc<-ALPP.Tables[grepl('(SubTestCode 191)',ALPP.Tables)] [[3]] ;
  
  data.ccc<-data.frame(ccc[2:4,3:5], stringsAsFactors = F) ;
  
  data.ccc$X4<-str_split(ccc[2,2], " ")[[1]][1] ;
  
  data.ccc$X4[2]<-str_split(ccc[3,2], " ")[[1]][1] ;
  
  data.ccc$X4[3]<-str_split(ccc[4,2], " ")[[1]][1] ;
  
  
  data.ccc$X5<-str_split(ccc[2,2], " ")[[1]][2] ;
  
  data.ccc$X5[2]<-str_split(ccc[3,2], " ")[[1]][2] ;
  
  data.ccc$X5[3]<-str_split(ccc[4,2], " ")[[1]][2] ;
  
  
  #str(data.ccc)
  
  Clay.data<-data.frame(as.numeric(t(data.ccc)[,2]),as.numeric(t(data.ccc)[,3]))  ;
  
  Clay.data$Sample<-t(data.ccc)[,1] ;
  
  names(Clay.data)[1:2]<-c('Clay_Mean' , 'Clay_MAD') ;
  
  
  
  ########### combine the data together ##################
  
  Texture.data<-merge(merge(Sand.data, Silt.data),Clay.data) ;
  
  Texture.data.0<-Texture.data  ;
  
  ALPP.Texture<-rbind(Texture.data.0,Texture.data)  ;
  
  #ALPP.Texture<-Texture.data
  
  rm(Texture.data , Texture.data.0)
  
  
}












#### Check the consistency of the data

Texture.data$SUM<-Texture.data$Sand_Mean + Texture.data$Silt_Mean + Texture.data$Clay_Mean  ;

str(Sand.data)