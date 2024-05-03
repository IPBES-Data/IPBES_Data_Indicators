################################################
# Settings for indicator extraction and analysis
################################################

## Settings----
# check for nodename (or username) and set directories accordingly
set_working_dir <- function(your_user,your_node){
  
  # get current user and node
  user <- Sys.info()["user"]
  node <- Sys.info()["nodename"]
  
  print(user)
  
  if (node == your_node && user == your_user){ 
    workdir <- your_workdir
    print(workdir)
    
    #set working directory
    setwd(workdir)
  }
 
  # Install required packages
  listOfPackages <- c("readr", "dplyr", "stringr", "tidyr", "data.table", 
                      "gtools", "pdftools","tesseract","purrr",
                      "googlesheets4")
  
  
  # Install Packages, if needed
  for (i in listOfPackages){
    if(! i %in% installed.packages()){
      print('Installing packages')
      install.packages(i, dependencies = TRUE)
    }
  }
  
  #return(workdir)
}


# Install tabulizer from https://stackoverflow.com/questions/70036429/having-issues-installing-tabulizer-package-in-r
# run following these steps:
# 1- Download java 64-bit from https://www.java.com/en/download/
# 2- install.packages("rJava")
# 3- Create the Java environment through the command (check path to Java): Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre-1.8/")
# 4- library(rJava)
# 5- devtools::install_github("ropensci/tabulizer")
# 6- library(tabulizer)
#library(rJava)
#library(tabulizer)
#library(purrr)