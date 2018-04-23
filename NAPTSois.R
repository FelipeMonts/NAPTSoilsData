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


# ########### Download the pdf files to a local drive to get the data from them ##################
# 
# dir.create("../NAPT_PDFs");
# 
# for (j in seq(1,length(NAPT.paths))) {
#   download.file(NAPT.paths[j], destfile = paste0("../NAPT_PDFs/pdf_",j,".pdf"), mode='wb')
#   
# }
# 
# NAPT.pdfs.1[seq(33,length(NAPT.paths))]
# 
# 
# List.NAPT.pdfs_33_40<-strsplit(NAPT.pdfs.1[seq(33,length(NAPT.paths))],split="/f")
# 
# NAPT.pdfs_33_40<-sapply(List.NAPT.pdfs_33_40,'[',2) ;
# 
# NAPT.paths_33_40<-paste0('http://www.naptprogram.org/f',NAPT.pdfs_33_40)
# 
# for (h in seq(1,length(NAPT.paths_33_40))) {
#   download.file(NAPT.paths_33_40[h], destfile = paste0("../NAPT_PDFs/pdf_",h+32,".pdf"), mode='wb')
#   
# }
# 



########  Extract all the texture data in the Soils Results web page  ###########

#Results.data.all<-Results.data.1



# ############# download the archive files #######################################
# 
# scraping_NAPT_archive <- read_html ("http://www.naptprogram.org/lab-results/program-archive")
# Lab_Results_archive   <- scraping_NAPT_archive %>%
#                  html_nodes ("a")   
# 
# 
# length(Lab_Results_archive)
# 
# grep('<a href="/files/napt/publications/program-archive/soils/',Lab_Results_archive)
# 
# 
# NAPT.archive.Soils<-Lab_Results_archive[grep('<a href="/files/napt/publications/program-archive/soils/',Lab_Results_archive)];
# 
# 
# 
# Soil.archive.NAPT.paths<-strsplit(as.character(NAPT.archive.Soils), split='"') ;
# 
# str(Soil.archive.NAPT.paths)
# 
# NAPT.archive.pdfs.1<-sapply(Soil.archive.NAPT.paths,'[',2) ;
# 
# NAPT.archive.paths<-paste0('http://www.naptprogram.org/',NAPT.archive.pdfs.1) ;
# 
# 
# dir.create("../NAPT_ARCHIVE_PDFs");
# 
# for (k in seq(1,length(NAPT.archive.paths))) {
#   download.file(NAPT.archive.paths[k], destfile = paste0("../NAPT_ARCHIVE_PDFs/pdf_",k,".pdf"), mode='wb')
#   }
# 
# #################### Read the pdf files that were images and needed to be transfromed to be able to incorporate
# ##################### into the NAPT Soildatabase ################################################

Archive.files<-list.files("../NAPT_PDFs")


#######################################################################################################################
# 
#                                   Files that have one method of soil analysis
# 
# 
####################################################################################################################### 

#################### Files that have one method of soil analysis ##########################


Archive.files[(grep("Copy.pdf$",Archive.files))] ;

Archive.files.comp<-Archive.files[which( !(seq(1,length(Archive.files)) %in% grep("Copy.pdf$",Archive.files)) )] ;

Hygrometer.Soils<-c(Archive.files[grep("Copy.pdf$",Archive.files)], Archive.files.comp[c(27:33, 35)]) ;

########### read the pdf to extract the columns and row names   #################


library(tabulizer)
library(dplyr)


out.1 <- extract_tables(paste0("../NAPT_PDFs/", Hygrometer.Soils[1])) ;

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

Results.data.all<-pdf.data.1[,select.columns]



##############Get the rest of the data and combine it #####################
for (i in seq(2,length(Hygrometer.Soils))) {
  #i=16
  rm('out', 'Results.data') 
  data.1<-Results.data.all
  
  out<- extract_tables(paste0("../NAPT_PDFs/", Hygrometer.Soils[i])) ;
  
  str(out)
  length(out)
  
  out[[2]]
  str(out[[2]])
  
  soil.names<-out[[2]][1,grep("Soil .*",out[[2]][1,])]
  
  temp<-matrix(c(paste(soil.names,c("Median"),sep='_'),paste(soil.names,c("MAD"),sep='_')),nrow=length(soil.names),ncol=2)
  
  Soil_names<-as.vector(t(temp))
  
  
  analysis.names<-c("Analysis" , "Units" , "n", as.vector(t(temp)))
  
  ############## get the data from the table ###############
  
  
  pdf.data<-t(out[[length(out)]]);
  
  row.names(pdf.data)<-analysis.names[seq(1,dim(pdf.data)[1])]
  str(pdf.data)
  
  select.columns<-sort(c(grep('Sand.*',pdf.data[1,]),grep('Silt.*',pdf.data[1,]),grep('Clay.*',pdf.data[1,])));
  
  Results.data<-pdf.data[,select.columns]
  
  
  ############# combine the data with the previous pdf data ###########
  str(Results.data)
  str(Results.data)
  
  Results.data.all<-rbind(Results.data,Results.data.all)
  
  str(Results.data.all)
  
}

#write.csv(Results.data.all,file='Results_data_all.csv',quote=F)
#Results.data.all<-read.csv('Results_data_all.csv',header=T, as.is=T)
library(XLConnect) ;

#writeWorksheetToFile("Results_data_all.xlsx",Results.data.all,sheet="Results_data_all")

#######################################################################################################################
# 
#                                   Files that have two methods of soil analysis
# 
# 
####################################################################################################################### 


########### read the pdf to extract the columns and row names   #################


TwoMethods.Soils<-Archive.files.comp[-c(22,24,25)] ;

rm('out.1', 'out' , 'Results.data', 'Results.data.all') 


out.1 <- extract_tables(paste0("../NAPT_PDFs/", TwoMethods.Soils[1])) ;

str(out.1)
length(out.1)

out.1[[1]]
str(out.1[[1]])

soil.names<-out.1[[1]][1,grep("Soil.*",out.1[[1]][1,])]

temp<-matrix(c(paste(soil.names,c("Median"),sep='_'),paste(soil.names,c("MAD"),sep='_')),nrow=length(soil.names),ncol=2)

Soil_names<-as.vector(t(temp))


analysis.names<-c("Analysis" , "Units" , "n", as.vector(t(temp[2:6,])))

############## get the data from the table ###############


pdf.data.1<-t(out.1[[length(out.1)]]);

row.names(pdf.data.1)<-analysis.names
str(pdf.data.1)

select.columns<-sort(c(grep('Sand.*',pdf.data.1[1,]),grep('Silt.*',pdf.data.1[1,]),grep('Clay.*',pdf.data.1[1,])));

Results.data.all<-pdf.data.1[,select.columns]




##############Get the rest of the data and combine it #####################
for (i in seq(2,length(TwoMethods.Soils))) {
  #i=26
  rm('out', 'Results.data') 
  data.1<-Results.data.all
  
  out<- extract_tables(paste0("../NAPT_PDFs/", TwoMethods.Soils[i])) ;
  
  str(out)
  length(out)
  
  out[[1]]
  str(out[[1]])
  
  soil.names<-out[[1]][1,grep("Soil.*",out[[1]][1,])] ;
  
  temp<-matrix(c(paste(soil.names,c("Median"),sep='_'),paste(soil.names,c("MAD"),sep='_')),nrow=length(soil.names),ncol=2)
  
  Soil_names<-as.vector(t(temp))
  
  
  analysis.names<-c("Analysis" , "Units" , "n", as.vector(t(temp[2:6,])))
  
  ############## get the data from the table ###############
  
  
  pdf.data<-t(out[[length(out)]]);
  
  row.names(pdf.data)<-analysis.names[seq(1,dim(pdf.data)[1])]
  str(pdf.data)
  
  select.columns<-sort(c(grep('Sand.*',pdf.data[1,]),grep('Silt.*',pdf.data[1,]),grep('Clay.*',pdf.data[1,])));
  
  Results.data<-pdf.data[,select.columns]
  
  
  ############# combine the data with the previous pdf data ###########
  str(Results.data)
  str(Results.data)
  
  Results.data.all<-rbind(Results.data,Results.data.all)
  
  str(Results.data.all)
  
}


#write.csv(Results.data.all,file='Results_data_all_two.csv',quote=F)
#Results.data.all<-read.csv('Results_data_all_two.csv',header=T, as.is=T)
library(XLConnect) ;

#writeWorksheetToFile("Results_data_all.xlsx",Results.data.all,sheet="Results_data_all_two")








