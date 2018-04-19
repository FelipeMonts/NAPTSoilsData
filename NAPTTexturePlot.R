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

Campbell.Soil$sand<-100-(Campbell.Soil$silt+Campbell.Soil$clay);

names(Campbell.Soil)<-c('CLAY', 'SILT' , 'SAND')


TT.plot(
  class.sys          ="USDA-NCSS.TT",
  tri.data           = Campbell.Soil,
  main               ="Campbell Soil Texture Data",
  class.p.bg.col     =T
)

