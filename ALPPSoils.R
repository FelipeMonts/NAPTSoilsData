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



dir.create("../ALPP_PDFs");

for (i in seq(30,34)) {
  download.file(paste0('https://www.collaborativetesting.com/assets/news/', i ,'_WebSum.pdf'), destfile= paste0('../ALPP_PDFs/','ALLP', i, '.pdf'), mode = 'wb' )

}
  


library(tabulizer)
library(dplyr)


ALPP.Tables <- extract_tables(paste0('../ALPP_PDFs/','ALLP', 34, '.pdf')) ;

# 
# for (k in seq(1,length(NAPT.archive.paths))) {
#   download.file(NAPT.archive.paths[k], destfile = paste0("../NAPT_ARCHIVE_PDFs/pdf_",k,".pdf"), mode='wb')
#   }


str(ALPP.Tables[157])

Sand<-ALPP.Tables[157][1]

str(Sand[[1]])

ALPP.Tables['Sand 2000-50$']
