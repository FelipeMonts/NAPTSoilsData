#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;



###### Introduction to Web Scraping #####

# Preliminaries
rm(list = ls())
# Set your working directory to some place you can find
setwd("C:/Users/frm10/Desktop/WebScrapping") ;

# First we will need to install the packages we plan to use for this exercise (
# if they are not already installed on your computer).
# install.packages("httr", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)

# httr is a package for downloading html
library(httr)
# A package for manipulating strings
library(stringr)

# Lets start by downloading an example web page:
url <- "http://www.mjdenny.com/Rcpp_Intro.html"

# We start by using the httr package to download the source html
page <- httr::GET(url)

# As we can see, this produces a great deal of information
str(page)

# To get at the actual content of the page, we use the content() function:
page_content <- httr::content(page, "text") 

# Now lets print it out
cat(page_content)

# and write it to a file for easier viewing
write.table(x = page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "Example_1.html")

# lets try a more complicated example page for a peice of legislation in the
# U.S. Congress
url <- "https://www.congress.gov/bill/103rd-congress/senate-bill/486/text"

# we start by using the httr package to download the source html
page <- httr::GET(url)

# as we can see, this produces a great deal of information
str(page)

# to get at the actaul content of the page, we use the content() function
page_content <- httr::content(page, "text")

# now lets print it out
cat(page_content)

# and write it to a file for easier viewing
write.table(x = page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "Example_2.html")



### Web Scraping Example, Part 1 ###

url <- "https://scholar.google.com/scholar?hl=en&q=https://scholar.google.com/scholar?hl=en&q=laurel+smith-doerr"

url.NAPT<-"http://www.naptprogram.org/lab-results/program-archive" 

# we start by using the httr package to download the source html
page <- httr::GET(url)

NAPT.page<-httr::GET(url.NAPT)

# as we can see, this produces a great deal of information
str(page)

# to get at the actaul content of the page, we use the content() function
page_content <- httr::content(page, "text")

NAPT.page_content <- httr::content(NAPT.page, "text")

# now lets print it out
cat(page_content)
cat(NAPT.page_content)

# and write it to a file for easier viewing

write.table(x = page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "Example_3.html")

write.table(x = NAPT.page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "NAPT_Lab_Results.html")

# now lets think about what we might like to extract from this page?



# lets look inside the function
string <- "Laurel Smith-Doerr"
return_source <- FALSE

# Write a function to go get the number of results that pop up for a given name
# in google scholar.
get_google_scholar_results <- function(string,
                                       return_source = FALSE){
  
  # print out the input name
  cat(string, "\n")
  
  # make the input name all lowercase
  string <- tolower(string)
  
  # split the string on spaces
  str <- stringr::str_split(string," ")[[1]]
  
  # combine the resulting parts of the string with + signs so "Matt Denny"
  # will end up as "matt+denny" which is what Google Scholar wants as input
  str <- paste0(str,collapse = "+")
  
  # add the name (which is now in the correct format) to the search querry and
  # we have our web address.
  str <- paste("https://scholar.google.com/scholar?hl=en&q=",str,sep = "")
  
  # downloads the web page source code
  page <- httr::GET(str)
  page <- httr::content(page, "text")
  

  
  ### Web Scraping Example, Part 2 ###
  
  
  
  # search for the 'Scholar</a><div id="gs_ab_md">' string which occurs
  # uniquely right before google Scholar tells you how many results your
  # querry returned
  num_results <- str_split(page,'<div id=\\"gs_ab_md\\"><div class=\\"gs_ab_mdw\\">')[[1]][2]
  
  # split the resulting string on the fist time you see a "(" as this will
  # signify the end of the text string telling you how many results were
  # returned.
  num_results <- str_split(num_results,'\\(')[[1]][1]
  
  # Print out the number of results returned by Google Scholar
  cat("Querry returned", tolower(num_results), "\n")
  
  # Look to see if the "User profiles" string is present -- grepl will return
  # true if the specified text ("User profiles") is contained in the web page
  # source.
  if (grepl("User profiles",page)) {
    
    # split the web page source (which is all one string) on the "Cited by "
    # string and then take the second chunk of the resulting vector of
    # substrings (so we can get at the number right after the first mention
    # of "Cited by ")
    num_cites <- str_split(page,"Cited by ")[[1]][2]
    
    # now we want the number before the < symbol in the resulting string
    # (which will be the number of cites)
    num_cites <- str_split(num_cites,"<")[[1]][1]
    
    # now let the user know how many we found
    cat("Number of Cites:",num_cites,"\n")
  } else {
    # If we could not find the "User profiles" string, then the person
    # probably does not have a profile on Google Scholar and we should let
    # the user know this is the case
    cat("This user may not have a Google Scholar profile \n")
  }
  
  # If we specified the option at the top that we wanted to return the HTML
  # source, then return it, otherwise dont.'
                           
  if (return_source) {
    return(page)
  }
}

#now lets have some fun...
get_google_scholar_results("Joya Misra")

get_google_scholar_results("Laurel Smith-Doerr")

get_google_scholar_results("Nilanjana Dasgupta")

page_source <- get_google_scholar_results("Gary Becker",return_source = TRUE)



#######################################################################################################################
# 
#       Boehmke, Bradley. 2016. Data Wrangling with R. New York, NY: Springer Science+Business Media.
# 
#       chapter 16  Scrapping data
# 
# 
####################################################################################################################### 

library(XML)
#install.packages('rvest')

#install.packages("magrittr")

library(rvest)
library(magrittr)

# url hosting multiple links to data sets
#url <- "https://download.bls.gov/pub/time.series/ap/"


url <-'https://www.naptprogram.org/lab-results/program-archive'



# identify the links available

links <- getHTMLLinks (url) 


library (rvest)
scraping_wiki <- read_html ("https://en.wikipedia.org/wiki/Web_scraping")
scraping_wiki %>%
  html_nodes ("h1")
## {xml:nodeset (1)}
## [1] <h1 id="fi rstHeading" class="fi rstHeading" lang="en">Web scraping</h1>

scraping_wiki %>%
  html_nodes ("p") %>%
  html_text ()
## [1] "Web scraping"

ul_text <- scraping_wiki %>%
  html_nodes ("ul") %>%
  html_text ()
length (ul_text)
## [1] 21

ul_text[1]



li_text <- scraping_wiki %>%
  html_nodes ("li") %>%
  html_text ()

li_text[1:8]


all_text <- scraping_wiki %>%
  html_nodes ("div") %>%
  html_text ()

##########################################################################################################################
# 
#                               Trying with the NAPT website
# 
##########################################################################################################################

# scraping_NAPT <- read_html ("http://www.naptprogram.org/lab-results/program-archive")
# Lab_Results<-scraping_NAPT %>%
#   html_nodes ("li")  %>%
#   html_text ()
# 

scraping_NAPT <- ("http://www.naptprogram.org/lab-results")
Lab_Results<-scraping_NAPT %>%
  html_nodes ("li")  %>%
  html_text ()




length(Lab_Results)

Lab_Results[[11]]


links <- getHTMLLinks (scraping_NAPT) 


library(tabulizer)
library(dplyr)

SoilREsults<-"http://www.naptprogram.org/files/napt/publications/program-archive/soils/2008-116-120.pdf"

out <- extract_tables(SoilREsults)
str(out[[1]])

as.data.frame(out[[5]])


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
 