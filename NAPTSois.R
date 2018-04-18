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

# Lets start by downloading an example web page:
url <- "http://www.naptprogram.org/lab-results/program-archive"

# We start by using the httr package to download the source html
page <- httr::GET(url)

# As we can see, this produces a great deal of information
str(page)

# To get at the actual content of the page, we use the content() function:
page_content <- httr::content(page, "text") 

# Now lets print it out
cat(page_content)


### Web Scraping NAPT Soils ###

url.NAPT<-"http://www.naptprogram.org/lab-results/program-archive" 

# we start by using the httr package to download the source html

NAPT.page<-httr::GET(url.NAPT)

 
#######################################################################################################################
# 
#       Boehmke, Bradley. 2016. Data Wrangling with R. New York, NY: Springer Science+Business Media.
# 
#       chapter 16  Scrapping data
# 
# 
####################################################################################################################### 

#install.packages("XML")

library(XML)
#install.packages('rvest')

#install.packages("magrittr")

library(rvest)
library(magrittr)


##########################################################################################################################
# 
#                               Trying with the NAPT website
# 
##########################################################################################################################


##########          Getting the most recent results in the page Laboratory Results   #################################

# Examples of the addres where the pdf files can be downloaded  

# http://www.naptprogram.org/files/napt/lab-results/2017-q1-soil-general-report.pdf
# 
# http://www.naptprogram.org/files/napt/lab-results/2017-q2-soil-report.pdf



#########     gettting all the date from  http://www.naptprogram.org/lab-results


scraping_NAPT <- read_html ("http://www.naptprogram.org/lab-results")
Lab_Results   <- scraping_NAPT %>%
                 html_nodes ("td>a")   
  
length(Lab_Results)

#########     using the node name text to filter soil results only


Node.names.scraping_NAPT<-html_text(Lab_Results) ;
str(Node.names.scraping_NAPT)


Soils.NAPT<-which(Node.names.scraping_NAPT == "Soil") ;

length(Lab_Results[Soils.NAPT])

head(Lab_Results[Soils.NAPT])

######    Use the node text to extract the file paths to the pdf files with the data

Soil.NAPT.paths<-strsplit(as.character(Lab_Results[Soils.NAPT]), split='"') ;

str(Soil.NAPT.paths)

NAPT.pdfs.1<-sapply(Soil.NAPT.paths,'[',2) ;

NAPT.paths<-paste0('http://www.naptprogram.org/',NAPT.pdfs.1) ;







########### read the pdf to extract the columns and row names   #################


library(tabulizer)
library(dplyr)


out.1 <- extract_tables(NAPT.paths[1]) ;

str(out.1)
length(out.1)

out.1[[1]]
str(out.1[[1]])

soil.names<-out.1[[1]][1,grep("Soil .*",out.1[[1]][1,])]

temp<-matrix(c(paste(soil.names,c("Median"),sep='_'),paste(soil.names,c("MAD"),sep='_')),nrow=length(soil.names),ncol=2)

Soil_names<-as.vector(t(temp))


analysis.names<-c("Analysis" , "Units" , "n", as.vector(t(temp)))

############## get the data from the table ###############


pdf.data.1<-t(out.1[[length(out.1)]]);

row.names(pdf.data.1)<-analysis.names
str(pdf.data.1)

select.columns<-sort(c(grep('Sand.*',pdf.data.1[1,]),grep('Silt.*',pdf.data.1[1,]),grep('Clay.*',pdf.data.1[1,])));

Results.data.1<-pdf.data.1[,select.columns]


########  Extract all the texture data in the Soils Results web page  ###########

Results.data.all<-Results.data.1

for (i in seq(2,length(NAPT.paths))) {
#i=29
  ########### read the pdf to extract the columns and row names   #################

  data.1<-Results.data.all

  out<-extract_tables(NAPT.paths[i]) ;

  str(out)
  length(out)

  out[[1]]
  str(out[[1]])

  soil.names<-out[[1]][1,grep("Soil .*",out[[1]][1,])]

  temp<-matrix(c(paste(soil.names,c("Median"),sep='_'),paste(soil.names,c("MAD"),sep='_')),nrow=length(soil.names),ncol=2)

  Soil_names<-as.vector(t(temp))


  analysis.names<-c("Analysis" , "Units" , "n", as.vector(t(temp)))
  
  
  ############## get the data from the table ###############
  
  pdf.data<-t(out[[length(out)]]);
  row.names(pdf.data)<-analysis.names
  str(pdf.data)
  
  select.columns<-sort(c(grep('Sand.*',pdf.data[1,]),grep('Silt.*',pdf.data[1,]),grep('Clay.*',pdf.data[1,])));
  
  Results.data<-pdf.data[,select.columns]
  
  ############# combine the data with the previous pdf data ###########
  str(data.1)
  str(Results.data)
  
  Results.data.all<-rbind(data.1,Results.data)
  
  str(Results.data.all)
  }


######## The code above failed at 2011  Because of formatting differences in the pdf files#####


####### Using another way, downloading the files into a directory and taking the information from there #

dir.create("../NAPT_PDFs");

for (j in seq(29,32)) {
  download.file(NAPT.paths[j], destfile = paste0("../NAPT_PDFs/pdf_",j,".pdf"), mode='wb')

}

#######  Again format differences  at Results for 2010  ###############

NAPT.pdfs.1[33:40]

strsplit(NAPT.pdfs.1[33:40],split="/f") 


List.NAPT.pdfs_33_40<-strsplit(NAPT.pdfs.1[33:40],split="/f")

NAPT.pdfs_33_40<-sapply(List.NAPT.pdfs_33_40,'[',2) ;

NAPT.paths_33_40<-paste0('http://www.naptprogram.org/f',NAPT.pdfs_33_40)

for (h in seq(1,length(NAPT.paths_33_40))) {
  download.file(NAPT.paths_33_40, destfile = paste0("../NAPT_PDFs/pdf_",h+32,".pdf"), mode='wb')
  
}



############# download the archive files #######################################














# Old Records stop being readable in 2005


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



