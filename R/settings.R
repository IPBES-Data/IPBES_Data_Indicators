################################################
# Settings for indicator extraction and analysis
# working directory
# packages
################################################

## Settings----
settings <- function(){

  # 1. Type the working directory of your choose 
  workdir <- your_dir # already set in previous file
  # workdir <- 'set_your_own_working_directory'
  
  cat('Your working dir: ', workdir, '\n')
  
  # 1.a. Set working directory
  setwd(workdir)
  
  # 2. List of packages needed
  listOfPackages <- c("readr", "dplyr", "stringr", "tidyr", "data.table", 
                      "gtools", "pdftools","tesseract","purrr",
                      "googlesheets4","circlize","ggalluvial")
  

  # 3.a. Install Packages  (if needed)
  for (i in listOfPackages){
    if(! i %in% installed.packages()){
      print('Installing packages')
      install.packages(i, dependencies = TRUE)
    }
  }
  cat('All packages installed')

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