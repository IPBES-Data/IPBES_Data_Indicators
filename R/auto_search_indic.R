# clean you environment
rm(list=ls())

##############################################################################
# Extract indicators from global assessments (IPBES, IPCC, GEO)
# 1- Automatically extract paragraphs from assessments based on keywords
# 2- Manually assess those paragraph and classify into indices or variables
# 3- Join and harmonize all indicators and variables in the assessments
##############################################################################
# Created by Yanina Sica in January 2023
# Updated May 2024

### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_where_code_is" # complete accordingly

## Source useful functions from folder downloaded from GitHub
source(paste0(your_dir,"/useful_functions_indic.R"))
source(paste0(your_dir,"/settings.R"))

## Set working directory and install required libraries
settings()

library("pdftools")
#library(tesseract)

library(stringr)
library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(purrr)

#install.packages("googlesheets4")
library(googlesheets4)


# 1- Automatic extraction of paragraphs----

# Set data directories for automatic extraction
input_dir = './input/automated_search/assessments_to_search' #ignored in Git so that data is not uploaded to GitHub
#output_dir = './input/automated_search/extracted_paragraphs'

# output_dir is set to Google Drive for easy sharing and manual assessment
output_dir = './input/automated_search/extracted_paragraphs/matches'

# Set from which assessment you want to extract indicators 
organization = 'IPBES'
assessment = 'ias'

# List chapters in assessment
chapters = list.files(file.path(input_dir,organization,assessment), pattern = '.pdf', full.names = TRUE)

# Set where you want to save the extracted indicators 
save_dir = file.path(output_dir,'matches',organization)
if (dir.exists(save_dir)){
  print('Directory exists')} else {dir.create(save_dir, recursive = TRUE)}

## Explore and set pdf parameters----

# Explore pdf
pdf <- pdftools::pdf_data(chapters[1], font_info = TRUE, opw = "", upw = "") %>% 
  # convert each list element into a data frame
  map(as.data.frame) %>% 
  # bind all data frames adding the page number
  bind_rows( .id = "page") %>% 
  mutate(page = as.integer(page))

# If 2 columns: explore x
pdf %>% 
  group_by(x) %>% count() %>% arrange(desc(n)) %>% View()
# Aprox: 60+ starting position of column 1
# Aprox: 300+ starting position of column 2
col_separation = 300

range(pdf$x)
pdf[which(pdf$x == 8),]$text # position of chapter label
pdf[which(pdf$x == 10),]$text # position of chapter numbering
pdf[which(pdf$x == 20),]$text # position of chapter label

pdf[which(pdf$x == 598),]$text # position of heading
pdf[which(pdf$x == 580),]$text # position of chapter label
hist(pdf$x)

min_x = 20
max_x = 580
  
# If 2 columns: explore y
pdf %>% 
  group_by(y) %>% count() %>% arrange(desc(n)) %>% View()#not very informative

range(pdf$y)
pdf[which(pdf$y == 26),]$text # heading
pdf[which(pdf$y == 55),]$text 

pdf[which(pdf$y == 751),]$text #position of page numbering
pdf[which(pdf$y == 740),]$text #position of page numbering

hist(pdf$y)

min_y = 30
max_y = 720

diffY = pdf %>% 
  # identify position (x) of second column
  mutate(column=if_else(x<col_separation,
                        true = 1000,
                        false = 2000)) %>% 
  # order data frame in the proper way to read (page, column, y=line,x=position in line)
  arrange(page,column,y,x) %>%
  # create a grouping class for sentences (page + line + column)
  mutate(page_y_col = paste0(page,'_',y,'_',column)) %>% 
  mutate(diffy = y - lag(y, default = 0)) %>% 
  group_by(diffy) %>% count() %>% 
  filter(diffy>0) %>% 
  filter(diffy<50)
plot(diffY$diffy,diffY$n) # differences between lines ~12 everything above that will be considered a new paragraph
paragraph_separation = 20

# Column settings
pdf_col = 1 # assign number of columns in the pdf

# explore fonts
pdf %>% 
  group_by(font_size) %>% count() %>% arrange(desc(n)) %>% View()
# 12 is main text

# Font settings
min_font = 11.9
max_font = 20 #so we keep titles

rm(pdf)
rm(diffY)

## Set search patterns----

# Keywords to search in assessments
keywords<-c("index", "Index","INDEX", "indic", "Indic", "INDIC")#do we want indicates/indicated?? check with current results..
pattern = paste0(keywords, collapse="|")

## Start search----

# Start search depending on type of pdf
if(pdf_col == 2){
  matches = data.frame()
  
  # Loop through chapters
  for (ch in 1:length(chapters)){
    ch_n = word(str_extract(chapters[ch], "\\d+.pdf"), 1, sep = '[.]')
    cat('Iteration: ', ch,'\n', ' Chapter: ',ch_n,'\n')
    #  Create data frame using position information and build sentences 
    pdf.text <- pdftools::pdf_data(chapters[ch], font_info = TRUE, opw = "", upw = "") %>% 
      # convert each list element into a data frame
      map(as.data.frame) %>% 
      # bind all data frames adding the page number
      bind_rows( .id = "page") %>% 
      mutate(page=as.integer(page)) %>% 
      # identify position (y) of new paragraph
      mutate(diffY = y - lag(y, default = 0)) %>% 
      mutate(new_paragraph = ifelse(diffY >= paragraph_separation, TRUE, FALSE)) %>% 
      # identify position (x) of second column
      mutate(column=if_else(x < col_separation,
                          true = 1000,
                          false = 2000)) %>% 
      # order data frame in the proper way to read (page, column, y=line,x=position in line)
      arrange(page,column,y,x) %>%
      # create a grouping class for sentences (page + line + column)
      mutate(page_y_col = paste0(page,'_',y,'_',column)) %>% 
      # Form sentences
      group_by(page_y_col) %>% 
      mutate(sentence=paste(text,collapse =" ")) %>% 
      ungroup() %>% 
      # remove min/max x and y
      filter(x > min_x) %>% 
      filter(x < max_x) %>% 
      filter(y > min_y) %>% 
      filter(y < max_y) %>% 
      # remove by font size
      filter(font_size >= min_font) %>% 
      filter(font_size <= max_font)
      
    # remove first pages (authors, table of contents, executive summary) 
    if(any(grepl("Executive Summary",pdf.text$sentence, ignore.case = TRUE))){
      es = tail(subset(pdf.text, grepl("Executive Summary",pdf.text$sentence, ignore.case = TRUE)),n=1)
    pdf.text = pdf.text[pdf.text$page > (es$page + 1),] 
    }else{print('no Executive summary')}
    # remove references and appendices
    pdf.text = pdf.text[pdf.text$page < tail(pdf.text[which(pdf.text$sentence == "References"), ],n=1)$page,] 

    # Build paragraphs
    pdf.text_p = pdf.text %>% 
      dplyr::distinct(page_y_col, .keep_all = TRUE) %>% 
      dplyr::select(-width, -height,-x,-y,-space,-text,-column,-diffY) %>% 
      dplyr::group_by(page) %>%
      dplyr::mutate(paragraph_n = cumsum(new_paragraph == TRUE)) %>% 
      dplyr::ungroup() %>% 
      # Form paragraphs
      dplyr::group_by(page,paragraph_n) %>% 
      dplyr::summarise(paragraph=paste(sentence,collapse =" ")) %>% 
      dplyr::select(-paragraph_n)
      
    # Extract matches
    pdf.text_p_matches = pdf.text_p %>% 
      dplyr::mutate(chapter = ch_n) %>% 
      # find indicators in paragraphs
      dplyr::mutate(matches = if_else(grepl(pattern, paragraph, ignore.case = FALSE),
                               true = TRUE,
                               false = FALSE)) %>% 
      dplyr::filter(matches==TRUE) %>% 
      # highlight keywords in paragraphs ("index", "Index","INDEX", "indic", "Indic", "INDIC")
      dplyr::mutate(paragraph = gsub('indicator', 'INDICATOR', paragraph)) %>%
      dplyr::mutate(paragraph = gsub('Indicator', 'INDICATOR', paragraph)) %>%
      dplyr::mutate(paragraph = gsub('indicators', 'INDICATORS', paragraph)) %>%
      dplyr::mutate(paragraph = gsub('Indicators', 'INDICATORS', paragraph)) %>%
      dplyr::mutate(paragraph = gsub('index', 'INDEX', paragraph)) %>% 
      dplyr::mutate(paragraph = gsub('Index', 'INDEX', paragraph)) %>% 
      dplyr::mutate(paragraph = gsub('indices', 'INDICES', paragraph)) %>% 
      dplyr::mutate(paragraph = gsub('Indices', 'INDICES', paragraph)) %>% 
      dplyr::mutate(paragraph = gsub('indic', 'INDIC', paragraph)) %>% 
      dplyr::mutate(paragraph = gsub('Indic', 'INDIC', paragraph)) %>% 
      # add match ID
      tibble::rowid_to_column(., "ID") %>% 
      dplyr::mutate(indic_id = paste0(assessment,'_',ch_n,'_',ID)) %>% 
      dplyr::select(indic_id, chapter, page,paragraph) 
        
    cat('# Matches ', length(pdf.text_p_matches$paragraph),'\n')
    
    matches = rbind(matches, pdf.text_p_matches)
  }
  
  write_csv(matches, paste0(save_dir,'/',assessment,'_matches.csv'))
  
}else{ #pdf with no columns
  
  pdf.text.matches = data.frame()
  for (ch in 1:length(chapters)){
    cat('Chapter: ',ch,'\n')
    
    # convert pdf to text
    pdf.text <- pdftools::pdf_text(chapters[ch])
    cat('# Pages ',length(pdf.text),'\n')
    # remove first pages until executive summary and references
    start = tail(grep('\\bexecutive summary\n', pdf.text, ignore.case = TRUE), n=1)
    end = tail(grep('\\breferences\n', pdf.text, ignore.case = TRUE),n=1)
    if (length(end)==0){end = tail(grep('\\breference\n', pdf.text, ignore.case = TRUE),n=1)}
    if (length(start)==0){pdf.text = pdf.text[1:(end-1)]}else{pdf.text = pdf.text[start:(end-1)]}
    cat('# Pages after removing content and references ', length(pdf.text),'\n')
    
    # extract paragraphs from pages
    for (page in 1:length(pdf.text)) {
      pdf.text[page] = str_split(pdf.text[[page]], "\n\n")
      for (paragraph in 1:length(pdf.text[[page]])){
        # add page number
        pdf.text[[page]][[paragraph]] = paste0('Page', page, '--p ', pdf.text[[page]][[paragraph]])
        pdf.text[[page]][[paragraph]] = gsub('\n',' ', pdf.text[[page]][[paragraph]])
        pdf.text[[page]][[paragraph]] = gsub('    ',' ', pdf.text[[page]][[paragraph]])
        pdf.text[[page]][[paragraph]] = gsub('   ',' ', pdf.text[[page]][[paragraph]])
        pdf.text[[page]][[paragraph]] = gsub('  ',' ', pdf.text[[page]][[paragraph]])
        
      }
    }
    
    # extract elements from list
    pdf.text<-unlist(pdf.text)
    
    # lower case so search of terms is easier
    #pdf.text<-tolower(pdf.text) # removed for easy reading (keywords were extended)
    
    # look for multiple keywords and extract (convert to data frame and add chapter)
    matches <- data.frame(matches = pdf.text[grep(pattern, pdf.text, ignore.case = TRUE)], chapter = ch)
    
    # get page numbers
    matches = matches %>%  tidyr::separate(matches,into = c('page','paragraphs'), sep = '--p ') %>% 
      dplyr::mutate(page = gsub('Page', '',page)) %>% 
      # select columns to keep
      dplyr::select(paragraphs, page, chapter)
    cat('# Matches ', length(matches$paragraphs),'\n')
    pdf.text.matches = rbind(pdf.text.matches,matches)
    
  }
  # Capitalize keywords so it easier to find them in paragraphs
  pdf.text.matches= pdf.text.matches %>% dplyr::mutate(paragraphs = gsub('indicator', 'INDICATOR', paragraphs)) %>%
    dplyr::mutate(paragraphs = gsub('Indicator', 'INDICATOR', paragraphs)) %>%
    dplyr::mutate(paragraphs = gsub('index', 'INDEX', paragraphs)) %>% 
    dplyr::mutate(paragraphs = gsub('Index', 'INDEX', paragraphs)) %>% 
    dplyr::mutate(paragraphs = gsub('indeces', 'INDECES', paragraphs)) %>% 
    dplyr::mutate(paragraphs = gsub('Indeces', 'INDECES', paragraphs))
  
  # save
  write_csv(pdf.text.matches, paste0(save_dir,'/',assessment,'_matches.csv'))
  
}



# 2- Manual inspection of paragraphs----
# Manual inspection of paragraphs done in shared Google sheets by 3 people independently
# validated datasets are stored './input/automated_search/extracted_paragraphs/manual_validation'

# 3- Harmonization of manually extracted indicators----


# Load indicators from extracted var/indices/indicators in assessments----
## Indicators extracted from IPBES------
# Automatic extraction of 'indicator', 'index', 'indicates', 'indices' from IPBES assessments
# Manual validation

## Global Assessment
ga = readxl::read_excel("../input/assessments/automated_search/extracted_paragraphs/manual_validation/validated_global_matches2.xlsx",
                sheet = "global_matches2") %>% 
  # re do ID to include page
  dplyr::mutate(indic_id = paste0('IPBES_GA_',chapter,'_',page,'_', row_number())) %>% 
  dplyr::select(indic_id,agree_val = `Agreement_validation (1,2,3)`,agree_indic = `Agreement_extracted`, ILK = ILK_indicators)

ipbes_ga_ext = ga %>% 
  # remove no hits
  dplyr::filter(!is.na(agree_val)) %>% 
  dplyr::filter(!is.na(agree_indic)) %>% 
  # remove no indicators (category 3)
  dplyr::filter(agree_val != 3) %>% 
  # Convert 'ILK' in 1
  dplyr::mutate(ILK= gsub('ILK', 1, ILK)) %>% 
  # spread long
  mutate(indicator = strsplit(as.character(agree_indic), ";")) %>% 
  unnest(indicator) %>% 
  # remove duplicates
  dplyr::distinct(indicator, .keep_all = TRUE) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', ' ',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator) %>%
  # clean columns
  dplyr::select(indicator_harmonized, indic_id, var_indic  = agree_val, ILK) %>% 
  # save
  write_csv('../input/assessments/automated_search/extracted_paragraphs/ga_extracted.csv')

# Prepare indicators for analysis
indic_ga = ipbes_ga_ext %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))

rm(ga, ipbes_ga_ext)

## Sustainable Use

sua = readxl::read_excel("../input/assessments/automated_search/extracted_paragraphs/manual_validation/validated_sua_matches2.xlsx",
                 sheet = "sua_matches2") %>% 
  # re do ID to include page
  dplyr::mutate(indic_id = paste0('IPBES_SUA_',chapter,'_',page,'_', row_number())) %>% 
  dplyr::select(indic_id,agree_val = `Agreement_validation (1,2,3)`,agree_indic = `Agreement_extracted`, ILK = ILK_indicators)

ipbes_sua_ext = sua %>% 
  # remove no hits
  dplyr::filter(!is.na(agree_val)) %>% 
  dplyr::filter(!is.na(agree_indic)) %>% 
  # remove no indicators (category 3)
  dplyr::filter(agree_val != 3) %>% 
  # Convert 'ILK' in 1
  dplyr::mutate(ILK = gsub('ILK', 1, ILK)) %>% 
  # spread long
  mutate(indicator = strsplit(as.character(agree_indic), ";")) %>% 
  unnest(indicator) %>% 
  # remove duplicates
  dplyr::distinct(indicator, .keep_all = TRUE) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', ' ',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator) %>%
  # clean columns
  dplyr::select(indicator_harmonized, indic_id, var_indic  = agree_val, ILK) %>% 
  # save
  write_csv('../input/assessments/automated_search/extracted_paragraphs/sua_extracted.csv')

# Prepare indicators for analysis
indic_sua = ipbes_sua_ext %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))

rm(sua,ipbes_sua_ext)

## values

va = readxl::read_excel("../input/assessments/automated_search/extracted_paragraphs/manual_validation/validated_values_matches.xlsx",
                sheet = "values_matches2") %>% 
  # re do ID to include page
  dplyr::mutate(indic_id = paste0('IPBES_VA_',chapter,'_',page,'_', row_number())) %>% 
  dplyr::select(indic_id,agree_val = `Agreement_validation (1,2,3)`,agree_indic = `Agreement_extracted`, ILK = ILK_indicators)

ipbes_va_ext = va %>% 
  # remove no hits
  dplyr::filter(!is.na(agree_val)) %>% 
  dplyr::filter(!is.na(agree_indic)) %>% 
  # remove no indicators (category 3)
  dplyr::filter(agree_val != 3) %>% 
  # Convert 'ILK' in 1
  dplyr::mutate(ILK = gsub('ILK', 1, ILK)) %>% 
  # spread long
  mutate(indicator = strsplit(as.character(agree_indic), ";")) %>% 
  unnest(indicator) %>% 
  # remove duplicates
  dplyr::distinct(indicator, .keep_all = TRUE) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', ' ',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator) %>%
  # clean columns
  dplyr::select(indicator_harmonized, indic_id, var_indic  = agree_val, ILK) %>% 
  # save
  write_csv('../input/assessments/automated_search/extracted_paragraphs/va_extracted.csv')

# Prepare indicators for analysis
indic_va = ipbes_va_ext %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))

rm(va,ipbes_va_ext)
## ias

ias = readxl::read_excel("../input/assessments/automated_search/extracted_paragraphs/manual_validation/validated_ias_matches.xlsx",
                 sheet = "ias_matches") %>% 
  # re do ID to include page
  dplyr::mutate(indic_id = paste0('IPBES_IAS_',chapter,'_',page,'_', row_number())) %>% 
  dplyr::select(indic_id,agree_val = `Agreement_validation (1,2,3)`,agree_indic = `Agreement_extracted`, ILK = ILK_indicators)

ipbes_ias_ext = ias %>% 
  # remove no hits
  dplyr::filter(!is.na(agree_val)) %>% 
  dplyr::filter(!is.na(agree_indic)) %>% 
  # remove no indicators (category 3)
  dplyr::filter(agree_val != 3) %>% 
  # Convert 'ILK' in 1
  dplyr::mutate(ILK = gsub('ILK', 1, ILK)) %>% 
  # spread long
  mutate(indicator = strsplit(as.character(agree_indic), ";")) %>% 
  unnest(indicator) %>% 
  # remove duplicates
  dplyr::distinct(indicator, .keep_all = TRUE) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', ' ',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator) %>%
  # clean columns
  dplyr::select(indicator_harmonized, indic_id, var_indic  = agree_val, ILK) %>% 
  # save
  write_csv('../input/assessments/automated_search/extracted_paragraphs/ias_extracted.csv')

# Prepare indicators for analysis
indic_ias = ipbes_ias_ext %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))

rm(ias, ipbes_ias_ext)

# combine all IPBES indicators from extracted paragraphs
indic_ipbes = indic_ga %>% 
  rbind(indic_sua) %>% 
  rbind(indic_va) %>% 
  rbind(indic_ias) %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_ids, collapse = ";")) %>% 
  #create source
  dplyr::mutate(ipbes_extracted = 1) %>% 
  # clean columns
  dplyr::select(indicator_harmonized,indic_ids,ipbes_extracted) %>% 
  # save input data
  write_csv('../input/assessments/automated_search/ipbes_extracted_indicators.csv')

rm(indic_ga, indic_sua,indic_va,indic_ias)

## Indicators from manual extraction in GEO------

geo = readxl::read_excel("../input/assessments/automated_search/extracted_paragraphs/manual_validation/validated_GEO6_matches.xlsx",
                 sheet = "GEO6_matches") %>% 
  # re do ID to include page
  dplyr::mutate(indic_id = paste0('GEO_',chapter,'_',page,'_', row_number())) %>% 
  dplyr::select(indic_id,agree_val = `Agreement_validation (1,2,3)`,agree_indic = `Agreement_extracted`, ILK = ILK_indicators)

geo_ext = geo %>% 
  # remove no hits
  dplyr::filter(!is.na(agree_val)) %>% 
  dplyr::filter(!is.na(agree_indic)) %>% 
  # remove no indicators (category 3)
  dplyr::filter(agree_val != 3) %>% 
  # Convert 'ILK' in 1
  dplyr::mutate(ILK = gsub('ILK', 1, ILK)) %>% 
  # spread long
  mutate(indicator = strsplit(as.character(agree_indic), ";")) %>% 
  unnest(indicator) %>% 
  # remove duplicates
  dplyr::distinct(indicator, .keep_all = TRUE) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', ' ',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator) %>%
  # clean columns
  dplyr::select(indicator_harmonized, indic_id, var_indic  = agree_val, ILK) %>% 
  # save
  write_csv('../input/assessments/automated_search/extracted_paragraphs/geo_extracted.csv')

# Prepare indicators for analysis
indic_geo = geo_ext %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))%>% 
  #create source
  dplyr::mutate(geo_extracted = 1) %>% 
  # save
  write_csv('../input/assessments/automated_search/geo_extracted_indicators.csv')

rm(geo, geo_ext)

## Indicators from manual extraction in IPCC------

ipcc = readxl::read_excel("../input/assessments/automated_search/extracted_paragraphs/manual_validation/validated_AR6_WG1_matches.xlsx",
                  sheet = "AR6_WG1_matches") %>% 
  # re do ID to include page
  dplyr::mutate(indic_id = paste0('IPCC_',chapter,'_',page,'_', row_number())) %>% 
  dplyr::select(indic_id,agree_val = `Agreement_validation (1,2,3)`,agree_indic = `Agreement_extracted`, ILK = ILK_indicators)

ipcc_ext = ipcc %>% 
  # remove no hits
  dplyr::filter(!is.na(agree_val)) %>% 
  dplyr::filter(!is.na(agree_indic)) %>% 
  # remove no indicators (category 3)
  dplyr::filter(agree_val != 3) %>% 
  # Convert 'ILK' in 1
  dplyr::mutate(ILK = gsub('ILK', 1, ILK)) %>% 
  # spread long
  mutate(indicator = strsplit(as.character(agree_indic), ";")) %>% 
  unnest(indicator) %>% 
  # remove duplicates
  dplyr::distinct(indicator, .keep_all = TRUE) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', ' ',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator) %>%
  # clean columns
  dplyr::select(indicator_harmonized, indic_id, var_indic  = agree_val, ILK) %>% 
  # save
  write_csv('../input/assessments/automated_search/extracted_paragraphs/ipcc_extracted.csv')

# Prepare indicators for analysis
indic_ipcc = ipcc_ext %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))%>% 
  #create source
  dplyr::mutate(ipcc_extracted = 1) %>% 
  # save
  write_csv('../input/assessments/automated_search/ipcc_extracted_indicators.csv')

rm(ipcc, ipcc_ext)



# ### Append all extracted and harmonized indicators----
# indic_va_ext = read_csv(paste0(git_dir,'input/automated_search/va_extracted.csv'))
# indic_sua_ext = read_csv(paste0(git_dir,'input/automated_search/sua_extracted.csv'))
# indic_ga_ext = read_csv(paste0(git_dir,'input/automated_search/ga_extracted.csv'))
# indic_ias_ext = read_csv(paste0(git_dir,'input/automated_search/ias_extracted.csv'))
# indic_geo_ext = read_csv(paste0(git_dir,'input/automated_search/geo_extracted.csv'))
# indic_ipcc_ext = read_csv(paste0(git_dir,'input/automated_search/ipcc_extracted.csv'))
# 
# # Check independent files
# dup = check_dup(indic_ias_ext, indicators_harmonized) # no exact duplicates
# dup = check_dup(indic_sua_ext, indicators_harmonized) # no exact duplicates
# rm(dup)
# 
# # Join
# indicators_extracted = indic_ga_ext %>% 
#   full_join(indic_sua_ext, by = 'indicators_harmonized') %>% 
#   full_join(indic_va_ext, by = 'indicators_harmonized') %>% 
#   full_join(indic_ias_ext, by = 'indicators_harmonized') %>% 
#   full_join(indic_ipcc_ext, by = 'indicators_harmonized') %>% 
#   full_join(indic_geo_ext, by = 'indicators_harmonized') %>% 
#   dplyr::mutate(ILK_ext = if_else(ILK.x==1 | ILK.y==1 | ILK.x.x==1 | ILK.y.y==1 | ILK.x.x.x==1 | ILK.y.y.y==1 ,
#                                   true = 1,
#                                   false = NA)) %>% 
#   dplyr::mutate(var_ext = if_else(var_indic.x==1 | var_indic.y==1 | var_indic.x.x==1 | var_indic.y.y==1 | var_indic.x.x.x==1 | var_indic.y.y.y==1 ,
#                                   true = 1,
#                                   false = NA)) %>% 
#   dplyr::mutate(indic_ext = if_else(var_indic.x==2 | var_indic.y==2 | var_indic.x.x==2 | var_indic.y.y==2 | var_indic.x.x.x==2 | var_indic.y.y.y==2 ,
#                                     true = 1,
#                                     false = NA)) %>% 
#   dplyr::select(indicators_harmonized, ga_extracted, sua_extracted, va_extracted, ias_extracted, ipcc_extracted, geo_extracted, ILK_ext, indic_ext, var_ext) 
# 
# # checks joined indicators
# dup = check_dup(indicators_extracted, indicators_harmonized) # no exact duplicates
# 
# indicators_extracted = indicators_extracted %>% 
#   # harmonize indicators
#   dplyr::mutate(indicators_h = tolower(indicators_harmonized)) %>%
#   dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
#   harmonize_indic(indicators_h)
# dup = check_dup(indicators_extracted, indicators_h) # no exact duplicates
# 
# # save
# indicators_extracted = indicators_extracted %>% 
#   # clean table
#   dplyr::select(-indicators_h, -field_harm) %>%
#   # save data
#   write_csv(paste0(git_dir,'input/automated_search/automated_search_indicators.csv'))



