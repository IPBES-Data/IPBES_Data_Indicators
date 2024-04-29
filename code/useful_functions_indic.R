## Functions to harmonize indicators

##### Check for duplicates
check_dup <- function(file_name,field){
  dup_df = file_name %>% 
    dplyr::mutate(dup1 = duplicated({{field}}, fromLast = TRUE)) %>%  
    dplyr::mutate(dup2 = duplicated({{field}}, fromLast = FALSE)) %>% 
    dplyr::mutate(dup = ifelse(dup1 == TRUE,
                               yes=dup1,
                               no=dup2)) %>% 
    dplyr::filter(dup == TRUE)

  if (nrow(dup_df) > 0){
    print("Check duplicates")
  }else{
    print("No duplicates")
  }
  return(dup_df)
}

##### Check character in other fields (e.g. authorship where there is more flexibility-------
check_odd_chr <- function(file_name = file_name,
                          field = field){
  regexp1 <- "([^a-zA-Z\u00C0-\u024F\u1E00-\u1EFF])" #using extended latin characters
  odd_chr = file_name %>%
    dplyr::mutate(field1 = gsub('\\(', '',{{field}})) %>%
    dplyr::mutate(field1 = gsub('\\)', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\[', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\]', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\.', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\;', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\:', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\,', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\&', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\–', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\/', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\%', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\<', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\>', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\+', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\‑', '',field1)) %>%
    dplyr::mutate(field1 = gsub(' ', '',field1)) %>%
    dplyr::mutate(field1 = gsub('  ', '',field1)) %>%
    dplyr::mutate(field1 = gsub('[0-9]', '',field1)) %>%
    dplyr::mutate(field1 = gsub('-', '',field1)) %>%
    dplyr::mutate(field1 = gsub("'", "",field1)) %>%
    dplyr::mutate(field1 = gsub("’", "",field1)) %>%
    dplyr::mutate(field1 = gsub("’", "",field1)) %>%
    dplyr::mutate(bad_chr = grepl(pattern = regexp1, x = field1))  %>%
    dplyr::mutate(field1 = gsub(' ', '',field1)) %>%
    #dplyr::mutate(bad_chr = grepl("[^-'`’;,.äàáâaÁÄåãëèéêÉïíîÍöóòôõøÓØÖüùúûÜÚß&ñÑçÇssSSýYa-zA-Z 0-9]",field1))  %>%
    filter(bad_chr == TRUE)
  
  if (nrow(odd_chr) > 0){
    cat('Check characters')
  }else{
    cat('No odd characters')
  }
  return(odd_chr)
}

##### Harmonize indicators-------
harmonize_indic <- function(file_name = file_name,
                            field = field){
  #regexp1 <- "([^a-zA-Z\u00C0-\u024F\u1E00-\u1EFF])" #using extended latin characters
  
  harmonized = file_name %>%
    # general
    dplyr::mutate(field_harm = gsub('&', 'and',{{field}})) %>%
    dplyr::mutate(field_harm = gsub('†', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('–', '-',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('[.]$', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub(' [(][$][)]', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('[(]tonnes[)]', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('[(]tonnes[/]ha[)]', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('no[.]', 'number',field_harm)) %>%
    dplyr::mutate(field_harm = gsub(' [(]overall[)]', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('the ramsar sites', 'ramsar sites',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('above ground', 'above-ground',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('aboveground', 'above-ground',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('forstry', 'forestry',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('fossil fuel', 'fossil-fuel',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("\\bnumber of extinctions\\b", "number of species extinctions",field_harm)) %>%     
    # dplyr::mutate(field_harm = gsub(" [(]$[)]\\b", "sustainable development goals index",field_harm)) %>% 
    
    # red list/iucn
    dplyr::mutate(field_harm = gsub('rli ', 'red list index ',field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('^rli$', 'red list index',field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('total extinction risk', 'extinction risk',field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('red list index [(]overall[)]', 'red list index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]impacts of utilization[/]wild relatives of domesticated animals[)]', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]impacts of utilisation[)]', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]impact of pollution[)]', 'red list index (impacts of pollution)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]impacts of utilisation[)]', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]impacts of utilization[)]', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]species used for food and medicine[)]', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index for vertebrate pollinators', 'red list index (vertebrate pollinators)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]wild species used for food and medicine[)]', 'red list index (wild relatives of farmed and domesticated species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]wild relatives of farmed and domesticated species[)]', 'red list index (wild relatives of farmed and domesticated species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (internationally traded species and migratory species)', 'red list index (internationally traded species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('[(]species used in food [&] medicine[)]', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]forest specialist species[)]', 'red list index (forest specialists)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]forest tree specialist species[)]', 'red list index (forest specialists)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('[(]internationally traded birds[)]', 'red list index (internationally traded birds)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]for internationally traded species and for migratory species[)]', 'red list index (internationally traded species and migratory species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]for internationally traded species[)]', 'red list index (internationally traded species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]internationally traded wild species[)]', 'red list index (internationally traded species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]pollinating species[)]', 'red list index (pollinators)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]pollinator species[)]', 'red list index (pollinators)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]wild relatives of domesticated animals[)]','red list index (wild relatives of farmed and domesticated species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index internationally[-]traded species', 'red list index (internationally traded species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index pollinating vertebrate species', 'red list index (vertebrate pollinators)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index species used in food and medicine', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index wild relatives of farmed and domesticated mammals and birds','red list index (wild relatives of farmed and domesticated mammals and birds)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index for birds and mammals','red list index (birds and mammals)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index [(]wild relatives[)]','red list index (wild relatives of farmed and domesticated species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("red list index of species survival for cms[-]listed bird species", "red list index (cms-listed bird species)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("red list index of species survival for migratory bird species", "red list index (migratory bird species)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("red list index of species survival for cms[-]listed bird and mammal species", "red list index (cms-listed bird and mammalspecies)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("red list of threatened species", "red list index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("iucn[']s red list of ecosystems",'red list of ecosystems',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("iucn red list of ecosystems",'red list of ecosystems',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("iucn red list",'red list index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("iucn red list index",'red list index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("iucn red list of threatened species index",'red list index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("iucn red list of threatened species of threatened species",'red list index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("iucn red list of threatened species[.]", "red list index",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("migratory birds threatened or near threatened on the iucn red list of threatened species",'red list index (migratory bird species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("taxa assessed for the iucn red list of threatened species",'proportion of known species assessed through the iucn red list of threatened species',field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("iucn red list of threatened species[.]", "iucn red list of threatened species",field_harm)) %>%
    
    # SDGs
    #dplyr::mutate(field_harm = gsub('[(]a[)] damage to critical infrastructure and', 'damage to critical infrastructure and',field_harm)) %>%
    #dplyr::mutate(field_harm = gsub('[(]a[)] food loss index and', 'food loss index and',field_harm)) %>%
    #dplyr::mutate(field_harm = gsub('[(]a[)] hazardous waste generated per capita[;]', 'hazardous waste generated per capita',field_harm)) %>%
    #dplyr::mutate(field_harm = gsub('[(]a[)] index of coastal eutrophication[;]', 'index of coastal eutrophication',field_harm)) %>%
    #dplyr::mutate(field_harm = gsub('[(]a[)] number of commercial bank branches per 1adults', 'number of commercial bank branches per 1adults',field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("[(]a[)] number of countries that have established national", "number of countries that have established national",field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("[(]a[)] official development assistance on conservation and sustainable use of biodiversity[;]", "official development assistance on conservation and sustainable use of biodiversity",field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("[(]a[)] proportion of total agricultural population with ownership or secure rights over agricultural land[,] by sex[;]", "proportion of total agricultural population with ownership or secure rights over agricultural land, by sex",field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("proportion of total agricultural population with ownership or secure tenure rights over agricultural land[,] by sex[;]", "proportion of total agricultural population with ownership or secure rights over agricultural land, by sex",field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("1adults", "adults",field_harm)) %>% 
    #dplyr::mutate(field_harm = gsub("national pared list indexaments and", "national parliaments and",field_harm)) %>% 
    
    # indeces
    dplyr::mutate(field_harm = gsub('gross domestic product [(]gdp[)]', 'gross domestic product',field_harm)) %>%	
    dplyr::mutate(field_harm = gsub('gdp', ' gross domestic product',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gross value added [(]gva[)]',  'gross value added',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gva\\b', 'gross value added',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gross value added',  'gross value added (gva)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mti [(]marine trophic index[)]', 'marine trophic index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('marine trophic index [(]mti[)]', 'marine trophic index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mti\\b', 'marine trophic index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('marine trophic index', 'marine trophic index (mti)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mean species abundance [(]msa[)]', 'mean species abundance',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mean species abundance index', 'mean species abundance',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mean species abundance', 'mean species abundance (msa)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("genuine progress indicator [(]gpi[)]", "genuine progress indicator",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("index of coastal eutrophication (icep)", "index of coastal eutrophication potential (icep)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("index of coastal eutrophication", "index of coastal eutrophication potential (icep)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("index of coastal eutrophication potential", "index of coastal eutrophication potential",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("[(]icep[)] [(]icep[)]", "(icep)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sustainable economic welfare (isew)", "index of sustainable economic welfare (isew)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("index of sustainable economic welfare", "index of sustainable economic welfare (isew)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("[(]isew[)] [(]isew[)]", "(isew)",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("extent of suitable habitat esh index", "extent of suitable habitat index (esh)",field_harm)) %>%    
    dplyr::mutate(field_harm = gsub("index of linguistic diversity", "linguistic diversity index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('wetland extent trend index', 'wetland extent trends index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('species status information index', 'species status index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub(' [(]lai[)]', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('leaf area index lai', 'leaf area index (lai)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('leaf area index', 'leaf area index (lai)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('vegetatio leaf area index [(]lai[)]', 'leaf area index (lai)',field_harm)) %>%    
    dplyr::mutate(field_harm = gsub("vegetation leaf area index [(]lai[)]", "leaf area index (lai)",field_harm)) %>%
    dplyr::mutate(field_harm = gsub('[(]lai[)] [(]lai[)]', '(lai)',field_harm)) %>%
    

    dplyr::mutate(field_harm = gsub('atlantic multi[-]decadal variability [(]amv[)]', 'atlantic multi-decadal variability (amv) index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('atlantic multi[-]decadal variability amv index', 'atlantic multi-decadal variability (amv) index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('atlantic multi[-]decadal variability amv', 'atlantic multi-decadal variability (amv) index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('net primary production [(]npp[)]', 'net primary production',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('npp\\b', 'net primary production',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("marine net primary production [(]remote sensing[)]", "marine net primary production",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("marine npp", "marine net primary production",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("terrestrial npp", "terrestrial net primary production",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("bird species per grid cell [(]csar[)]", "bird species per grid cell",field_harm)) %>%    
    dplyr::mutate(field_harm = gsub("el niño[-]southern oscillation [(]enso]", "el niño-southern oscillation (enso) index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("north atlantic oscillation nao index", "north atlantic oscillation nao",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("northern annular mode nam index", "northern annular mode (nam) index",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("northern annular mode nam", "northern annular mode (nam) index",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("human development index [(]hdi[)]", "human development index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("human development index", "human development index (hdi)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("iuu [(]indicator of unsustainable fisheries[)]", "indicator of unsustainable fisheries (iuu)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the agriculture orientation index for government expenditures", "agriculture orientation index for government expenditures",field_harm)) %>%    
    dplyr::mutate(field_harm = gsub("sdg index", "sustainable development goals (sdg) index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("standardized precipitation evapotranspiration index spei", "standardized precipitation evapotranspiration index (spei)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("standardized precipitation evapotranspiration index", "standardized precipitation evapotranspiration index (spei)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("[(]spei[)] [(]spei[)]", "(spei)",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("heat index hi[>]41[°]c", "heat index (hi)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("global land monsoon precipitation index", "land monsoon precipitation index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("bhi [(]overall habitat integrity[)]", "biodiversity habitat index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface temperature [(]sst index[)]", "sea surface temperature",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface temperature [(]sst[)]", "sea surface temperature",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface temperature [(]sst[)] index", "sea surface temperature",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface air temperature", "sea surface temperature",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface temperature index", "sea surface temperature",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface temperature", "sea surface temperature (sst) index",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("pacific decadal oscillation [(]pdo[)] index", "pacific decadal oscillation (pdo)",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("pacific decadal variability [(]pdv[)] index", "pacific decadal variability (pdv)",field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("sdg index", "sustainable development goals (sdg) index",field_harm)) %>% 
    #dplyr::mutate(field_harm = gsub("sdg index", "sustainable development goals (sdg) index",field_harm)) %>% 
    
    # harmonize names
    dplyr::mutate(field_harm = gsub('amount of fossil[-]fuel subsidies [(]production and consumption[)] per unit of  gross domestic product', 'amount of fossil-fuel subsidies per unit of gross domestic product (production and consumption)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('nmps\\b', 'national medicines policies',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('human appropriation of net primary production [(]hanet primary production[)]', 'human appropriation of net primary production',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('area of mangrove forest cover [(]km2[)]', 'mangrove forest area',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('agricultural organic area','area of agricultural land under organic production (million ha)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('secure rights over agricultural land[,] by sex[;] and [(]b[)] ','secure rights over agricultural land, by sex and (b) ', field_harm)) %>%
    dplyr::mutate(field_harm = gsub('or secure tenure rights over agricultural land[,] by sex[;] and [(]b[)] share ','secure rights over agricultural land, by sex and (b) share ',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('proportion of known species assessed through the iucn red list[.]', 'proportion of known species assessed through the iucn red list',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('fpic related to conservation would work here for ips [(]not necessarily lcs[)][,] if spatial planning was substituted for conservation[.]', 'fpic related to conservation',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('plans3', 'plans',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('number of countries with nationally determined contributions[,] long[-]term strategies[,] national adaptation plans[,] strategies as reported in adaptation communications and national communications', 'number of countries with nationally determined contributions, long-term strategies, national adaptation plans and adaptation communications, as reported to the secretariat of the united nations framework convention on climate change',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('number of least developed countries and small island developing states with nationally determined contributions[,] long[-]term strategies, national adaptation plans[,] strategies as reported in adaptation communications and national communications', 'number of least developed countries and small island developing states with nationally determined contributions, long-term strategies, national adaptation plans and adaptation communications, as reported to the secretariat of the united nations framework convention on climate change',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('total amount of approved funding for developing countries','total amount of funding for developing countries',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('number of plant and animal genetic resources for food and agriculture secured in medium[-] or longterm conservation facilities','number of plant and animal genetic resources for food and agriculture secured in either medium- or long-term conservation facilities',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('number of plant genetic resources for food and agriculture secured in conservation facilities','number of plant and animal genetic resources secured in medium or long-term conservation facilities', field_harm)) %>%
    dplyr::mutate(field_harm = gsub('land not cultivated or urban (global)','land not cultivated or urban',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('student assessments [(]sdg 4[.]7[.]1[)]', 'student assessments',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('warm spell duration index[:] annual count of days with at least six consecutive days when tx [>]90th percentile', 'warm spell duration index (wsdi)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub(" [(]kg[/]km2[)]", "",field_harm)) %>%
    
    dplyr::mutate(field_harm = gsub('area of agricultural land under conservation', 'area of agricultural land under conservation agriculture',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('area of agricultural land under conservation agriculture [(]thousand ha[)]', 'area of agricultural land under conservation agriculture',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('areas of agricultural land under convervation agriculture', 'area of agricultural land under conservation agriculture',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('area of agricultural land under organic production [(]million ha[)]', 'area of agricultural land under organic production',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('area of forest under sustainable management[:] total fsc and pefc forest management certification [(]million ha[)]', 'area of forest under sustainable management: total forest management certification by forest stewardship council and programme for the endorsement of forest certification',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('area of tree cover loss [(]ha[)]', 'area of tree cover loss',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('extent of marine vegetation[,] e[.]g[.] net primary production and seaweed aquaculture', 'extent of marine vegetation',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('extent of marine vegetation[:] net primary production seaweed aquaculture', 'extent of marine vegetation',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('extent of natural ecosystems by type', 'extent of natural ecosystems',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("forest area as a proportion of total land area", "forest area as a percentage of total land area",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("forest area as a percentage of land area", "forest area as a percentage of total land area",field_harm)) %>%  
    dplyr::mutate(field_harm = gsub("mangrove forest cover", "mangrove forest area",field_harm)) %>%    
    
    dplyr::mutate(field_harm = gsub("atmospheric co2 concentration\\b", "atmospheric co2 concentrations",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("atmospheric concentrations", "atmospheric co2 concentrations",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("catch[-]per[-]unit effort of harvest species[.]", "catch per unit effort",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("catch[-]per[-]unit effort", "catch per unit effort",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("coastal[:] protection", "coastal protection habitats",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("community weighted mean of traits", "community-weighted mean trait value (cwm)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('fertilizers used per unit area', 'fertilizers used',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('fish stocks biologically sustainable', 'fish stocks within biologically sustainable levels',field_harm)) %>%
    dplyr::mutate(field_harm = gsub("floating plastic debris density [[]by micro and macro plastics[]]", "floating plastic debris density",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("cumulative introduced invasive aliens species", "cumulative introduced invasive aliens",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("cumulative number of alien species", "cumulative introduced invasive aliens",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("ecological footprint [(]number of earths needed to support human society[)]", "ecological footprint",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("ghg emissions [(]in tonnes co2 eq and tonnes per capita[)]", "ghg emissions",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("glacial mass balance [(]mm water equivalent[)]", "glacial mass balance",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("global effort in bottom[-]trawling [(]kw sea[-]days[)]", "global effort in bottom-trawling",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("growth in species occurrence records accessible through gbif", "growth in species occurrence records accessible through the global biodiversity information facility",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("human appropriation of fresh water [(]water footprint[)] [(]thousand km3)]", "human appropriation of fresh water",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("human appropriation of net primary production [(]hanpp[)]", "human appropriation of net primary production",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("human appropriation of net primary productivity [(]pg c[)]", "human appropriation of net primary production",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("level of water stress[:] freshwater withdrawal as a proportion of available freshwater resources", "level of water stress (freshwater withdrawal as a proportion of available freshwater resources)",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("msc certified fisheries", "marine stewardship council certified fisheries",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("official development assistance provided in support of cbd objectives", "official development assistance provided in support of the cbd objectives",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("foreign direct investment[,] official development assistance and south[-]south cooperation as a proportion of gross national income", "foreign direct investment (fdi), official development assistance and south-south cooperation as a proportion of total domestic budget",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("mean polar sea ice extent [(]million km2[)]", "mean polar sea ice extent",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("growth in species occurrence records accessible through the global biodiversity information facility", "species occurrence records accessible through the global biodiversity information facility",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("number of species occurrence records in the global biodiversity information facility", "species occurrence records accessible through the global biodiversity information facility",field_harm)) %>%

    dplyr::mutate(field_harm = gsub("number of invasive alien species introductions", "number of invasive alien species introduction events",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("official development assistance provided in support of the cbd objectives [(]$[)]", "official development assistance provided in support of cbd objectives",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("percentage of category 1 nations in cites", "percentage of countries that are category 1 cites parties",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("percentage of threatened species that are improving in status according to the red list", "percentage of threatened species that are improving in status",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("pesticide use [(]tonnes[)]", "pesticide use",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("pesticides used per unit area", "pesticide use",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("prevalence of moderate or severe food insecurity in the population, based on the food insecurity experience scale [(]fies[)]", "prevalence of moderate or severe food insecurity in the population, based on the food insecurity experience scale",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of domestic and industrial wastewater flow safely treated", "proportion of domestic and industrial wastewater flows safely treated",field_harm)) %>%
    
    dplyr::mutate(field_harm = gsub("soil moisture anomalies [(]sma[)]", "soil moisture anomalies",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("species richness per grid cell [(]aim[)]", "species richness per grid cell",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("total ghg emissions", "total greenhouse gas emissions per year",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("total number of internationally recognized certificates published in the abs clearing[-]house", "total number of internationally recognized certificates of compliance published in the abs clearing-house",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("under[-]five mortality", "under-five child mortality",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("under[-]five mortality rate", "under-five child mortality",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("under[‑]5 mortality rate", "under-five child mortality",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("united nations framework convention on climate change [(]unfccc[)]", "united nations framework convention on climate change",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("water footprint [(]human appropriation of fresh water[)]", "water footprint",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("world trade organisation green box agricultural subsidies [(]$[)]", "world trade organisation greenbox agricultural subsidies",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea surface temperature [(]sst[)] index", "world trade organisation greenbox agricultural subsidies",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("community weighted mean of traits", "community-weighted mean trait value (cwm)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("annual mean levels of fine particulate matter [(]e[.]g[.] pm2[.]5 and pm10[)] in cities [(]population weighted[)]", "annual mean levels of fine particulate matter in cities",field_harm)) %>%
    

    dplyr::mutate(field_harm = gsub("proportion of fish stocks in safe biological limits", "proportion of fish stocks within safe biological limits",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("proportion of fish stocks within sustainable levels", "proportion of fish stocks within biologically sustainable levels",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("proportion of land degraded over total land area", "proportion of land degraded over total land",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("proportion of local breeds classified as being at risk extinction", "proportion of local breeds classified as being at risk of extinction",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of predatory fish in the community", "proportion of predatory fish",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of the rural population who live within of an all[-]season road", "proportion of the rural population who live within 2 km of an all-season road",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of countries where the legal framework [(]including customary law[)] guarantees women[’]s equal rights to land ownership and[/]or control", "proportion of countries where the legal framework (including customary law) guarantees women equal rights to land ownership and/or control",field_harm)) %>%     
    dplyr::mutate(field_harm = gsub("proportion of total agricultural population with ownership secure rights over agricultural land", "proportion of total agricultural population with ownership or secure rights over agricultural land",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("percentage of key biodiversity areas covered by protected areas", "protected area coverage of key biodiversity areas",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("percentage of terrestrial areas covered by protected areas", "percentage of terrestrial ecoregions covered by protected areas",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("natural habitat extent", "extent of suitable habitat",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("percentage natural habitat extent", "extent of suitable habitat",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("percentage extent of suitable habitat", "extent of suitable habitat",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of fish stocks within safe biological limits", "proportion of fish stocks within biologically sustainable levels",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("fish stocks within biologically sustainable levels", "proportion of fish stocks within biologically sustainable levels",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("trends in the proportion of proportion of fish stocks within biologically sustainable levels", "proportion of fish stocks within biologically sustainable levels",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of proportion of fish stocks within biologically sustainable levels", "proportion of fish stocks within biologically sustainable levels",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of fish stocks at or below a target biomass that can undergo recovery", "proportion of fish stocks within biologically sustainable levels",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of population using an improved drinking water source", "proportion of population using safely managed drinking water services",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of population using safely managed sanitation services[,] including a hand[-]washing facility with soap and water", "proportion of population using safely managed sanitation services",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("proportion of population using safely managed sanitation services[,] including a hand[-]washing facility with soap and water", "proportion of population using a hand-washing facility with soap and water",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("proportion of local breeds[,] classified as being at risk[,] not[-]at[-]risk or unknown level of risk of extinction", "proportion of local breeds classified as being at risk of extinction",field_harm)) %>%    
    dplyr::mutate(field_harm = gsub("proportion of land degraded over total land", "proportion of land that is degraded over total land area",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("biodiversity richness at global level [(]plants[,] animals and fungi[,] terrestrial and marine)", "species richness",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("changes in local terrestrial diversity [(]predicts[)]", "local species richness (predicts)",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("biodiversity richness", "species richness",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("expected loss of phylogenetic diversity", "loss of phylogenetic diversity",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("loss of phylogenetic diversity [(]pd[)]", "loss of phylogenetic diversity",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("tl of the landed catch", "trophic level of the landed catch",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("landed catch [(]capture fisheries production[)]", "landed catch",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("maximum fish catch potential", "fish catch potential",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("tl of the community", "trophic level of the community",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("number of deaths[,] missing persons and directly affected persons attributed to disasters per 1population", "number of deaths, missing persons and directly affected persons attributed to disasters per 100,000 population",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("phylobeta diversity", "phylogenetic betadiversity",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("\\bobis\\b", "ocean biodiversity information system (obis)",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("north atlantic oscillation nao", "north atlantic oscillation (nao) index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("el niño[-]southern oscillation [(]enso[)]", "el niño-southern oscillation (enso) index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("summer north atlantic oscilation index", "north atlantic oscillation (nao) index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("gross domestic happiness index", "gross national happiness index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("standardized precipitation evapotranspiration index spei", "standardized precipitation evapotranspiration index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("arctic sea ice area [(]sia[)]", "arctic sea ice area",field_harm)) %>% 

    dplyr::mutate(field_harm = gsub("culturally salient wild species populations", "culturally salient species",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("willingness[-]to[-]give[-]up[-]time", "willingness to allocate time for nature´s contributions to people conservation",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("willingness[-]to[-]pay", "willingness to pay to protect nature or improve ecosystem services",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("willingness to pay for nature[´]s contributions to people conservation", "willingness to pay to protect nature or improve ecosystem services",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("under[-]five child mortality", "under-five child mortality rate",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("rate rate", "rate",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("tree ring reconstructions", "tree rings",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the atlantic meridional overturning circulation", "atlantic meridional overturning circulation",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the expansion of human economic activities and the competing interest for land resources", "expansion of human economic activities and the competing interest for land resources",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the first and last days of snow cover", "first and last days of snow cover",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the fraction of precipitation falling as snow", "fraction of precipitation falling as snow",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the number of countries implementing natural resource accounts", "number of countries implementing natural resource accounts",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("the proportion of terrestrial protected area as a share of the total land area expressed as a percentage", "proportion of terrestrial protected area as a share of the total land area",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("area of tree cover loss", "area of tree cover",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("annual tree cover loss", "annual rate of net forest loss",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("live coral cover on reefs", "live coral cover",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("percentage live coral cover", "live coral cover",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("percentage of areas covered by protected areas[-] marine[,] coastal[,] terrestrial[,] inland water", "protected area coverage",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("westland extent trend index", "wetland extent trends index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea[-]ice", "sea ice",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("sea ice extent/area", "sea ice extent",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("extent of forested land", "extent of forests",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("trends in forest extent [(]tree cover[)]", "trends in forest extent",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("tide gauges", "tide gauge",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("terrestrial net primary productivity[,] evapotranspiration", "terrestrial net primary production",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("terrestrial net primary production net primary production", "terrestrial net primary production",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("temperature[/]ocean heat content", "ocean heat content",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("above[-]ground biomass stock in forest [(]tonnes[/]ha[)]", "above-ground biomass in forests",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("area of tree cover", "tree cover",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("gross national happiness measure", "gross national happiness index",field_harm)) %>%     
    dplyr::mutate(field_harm = gsub("gross domestic product per capita", "gross domestic product",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("build up area per capita", "build up area",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("agriculture agriculture [(]thousand ha[)]", "agriculture",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("hectares of mangroves", "mangrove forest area",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("domestic material consumption per capita", "domestic material consumption",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("floating debris density", "floating plastic debris density",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("bottom trawling", "bottom-trawling",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("human appropriation of net primary production", "human appropriation of net primary productivity",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("marine stewardship council fish catch", "marine stewardship council certified catch",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("phylogenetic diversity (pd)", "phylogenetic diversity",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("world trade organisation greenbox agricultural subsidies sst", "world trade organisation greenbox agricultural subsidies",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("world trade organisation green box agricultural subsidies", "world trade organisation greenbox agricultural subsidies",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub("index of coastal eutrophication potential [(]icep[)] potential", "index of coastal eutrophication potential (icep)",field_harm)) %>% 
    # dplyr::mutate(field_harm = gsub("gross domestic product per capita", "gross domestic product",field_harm)) %>% 
    # dplyr::mutate(field_harm = gsub("gross domestic product per capita", "gross domestic product",field_harm)) %>% 
    # dplyr::mutate(field_harm = gsub("gross domestic product per capita", "gross domestic product",field_harm)) %>% 
    # dplyr::mutate(field_harm = gsub("gross domestic product per capita", "gross domestic product",field_harm)) %>% 
    # dplyr::mutate(field_harm = gsub("gross domestic product per capita", "gross domestic product",field_harm)) %>% 
  

    
    # protected areas
    dplyr::mutate(field_harm = gsub("protected area connectedness index [(]parc[-]connectedness[)]", "protected area connectedness index",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("protected area connectedness index", "protected area connectedness (parc-connectedness) index",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("protected area coverage of key biodiversity areas [(]including important bird and biodiversity areas[,] alliance for zero extinction sites[)]", "protected area coverage of key biodiversity areas",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("protected area management effectiveness [(]pame[)]", "protected area management effectiveness",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("percentage of terrestrial key biodiversity areas covered by protected areas", "protected area coverage of key biodiversity areas",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("terrestrial protected areas as a percentage of total land area", "proportion of terrestrial protected area as a share of the total land area",field_harm)) %>%
    #dplyr::mutate(field_harm = gsub("protected area coverage", "proportion of terrestrial, freshwater and marine ecological regions which are conserved by protected areas or other effective area-based conservation measures",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("management effectiveness in national protected areas", "number of protected area management effectiveness assessments",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("protected area management effectiveness", "protected area management effectiveness (pame)",field_harm)) %>%
    
    dplyr::mutate(field_harm = gsub("percentage of mountain key biodiversity areas covered by protected areas", "coverage by protected areas of important sites for mountain biodiversity",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("coverage of marine protected areas", "coverage of protected areas in relation to marine areas",field_harm)) %>%
    
    dplyr::mutate(field_harm = gsub("average functional intactness", "functional intactness (madingley)",field_harm)) %>%   
    dplyr::mutate(field_harm = gsub("\\bfunctional intactness\\b", "functional intactness (madingley)",field_harm)) %>% 
    
    dplyr::mutate(field_harm = gsub("red list index red list index [(]internationally traded birds[)]", "red list index (internationally traded birds)",field_harm)) %>%
    dplyr::mutate(field_harm = gsub("index index", "index",field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('  ', ' ',field_harm))

  
  return(harmonized)
}
