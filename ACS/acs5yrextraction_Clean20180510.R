#Extract block and ZCTA level variables from ACS using tidy census
#This file should be utilized as a templace with some modification necessary
#Must extract block by calling intdividual counties or missing blocks if calling by state.
#Code blocked in extractions due to memory restrictions

#Package/Version information
#R version 3.4.3 (2017-11-30)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 7 x64 (build 7601) Service Pack 1
#other attached packages:
# [1] forcats_0.2.0       stringr_1.2.0       dplyr_0.7.4         purrr_0.2.4         readr_1.1.1        
#[6] tidyr_0.7.2         tibble_1.4.1        ggplot2_2.2.1       tidyverse_1.2.1     bindrcpp_0.2       
#[11] tidycensus_0.4      data.table_1.10.4-3


#libraries
library(tidycensus)
library(tidyverse)

#see the tables with variables to download
#v13 <- load_variables(2015, "acs5", cache = TRUE) 
#v14 <- load_variables(2016, "acs5", cache = TRUE) 
#v15 <- load_variables(2015, "acs5", cache = TRUE) 
#v16 <- load_variables(2016, "acs5", cache = TRUE) 


#import state and county list from census for United States 
#https://www.census.gov/geo/reference/codes/cou.html
states<-read.csv(file="state.county.codes.all.csv",
                 colClasses="character",
                 na.strings = c("NA",NULL,""," "),
                 header = T) #or txt file
names(states)<-tolower(names(states))

#compiled  list of variables into CSV for reference of tables to extract
vars<-read.csv(file="acs_var_key.csv",
               colClasses="character",
               na.strings = c("NA",NULL,""," "),
               header = T)
years<-c(2013,2014,2015,2016)
tables<-unique(vars$table)
tables<-tables[3:length(tables)] #remove the first two (unadjusted pop and households, just use adj with moe)


##########
#FORMULAS#
##########
#remove list elements where everything is na
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

##########################################
# ZCTAS - MUST REQUST FOR ENTIRE COUNTRY #
##########################################
#https://www.google.com/search?q=tidy+census+get+zcta&rlz=1C1NHXL_enUS694US694&oq=tidy+census+get+zcta&aqs=chrome..69i57.3343j0j4&sourceid=chrome&ie=UTF-8

#empty lists
zc<-list()
yrs<-list()
#extract zctas for all years loop
for (k in 1:length(years)){
  year.pull<-as.numeric(gsub("var","",years[k]))
for (i in 1:length(tables)){
  census_api_key("insert your key") # use `install = TRUE` to install the key
  options(tigris_use_cache = TRUE) # optional - to cache the Census shapefile
zc[[i]]<- get_acs(geography = "zip code tabulation area", 
              table=tables[i],
              survey="acs5",
              year = year.pull,
              geometry = F)
#see progress
print(tables[i])
}
yrs[[k]]<-as.data.frame(do.call("rbind", na.omit.list(zc)))
yrs[[k]]$yr<-year.pull
}
#yrsfinal<-as.data.frame(do.call("rbind", na.omit.list(yrs)))


###################################################################################
#BLOCK GROUPS - REQUEST BY STATE AND COUNTY
###################################################################################
#https://www.census.gov/geo/reference/codes/cou.html
#it appearsmust request by county otherwise it
#returns nothing if even only one is missing
#states.sub<-#get  2010uscounties.txt

#empty lists tp hold
#due to modification to query by county, names such as yrs, and sts don't actually align with
#content and should be treated as purely list holders
yrs<-list()
sts<-list()
tbspre<-list()
tbs<-list()
for (k in 1:length(years)){
  year.pull<-as.numeric(years[k])
  for (j in 1:length(states.sub)){
    tbs<-list()   #empty list for tables for each state - limited memory
    census_api_key("insert your key") 
    options(tigris_use_cache = TRUE) # optional - to cache the Census shapefile
  for (i in 1:length(tables)){
    for(f in 1:nrow(states.sub)){  
      #get tables by county
      tbspre[[f]]<- as.data.frame(tryCatch(get_acs(geography = "block group", 
                                                state=states.sub[,"state"][f],
                                                table=tables[i], #get all vars in tables
                                                year = year.pull, #this year
                                                geometry = F, # no mapping geometry
                                                survey = "acs5",
                                                cache_table=T,
                                                county=states.sub[,"countyfp"][f]
                                                ), error=function(e) NA))
      
      
    }
      #clean up step by step, merge all counti
      tbspre<-tbspre[!grepl("tryCatch",tbspre)] #remove empty tables
      if(length(tbspre)<1){tbs[[i]]<-NA}else{tbs[[i]]<- as.data.frame(do.call("rbind", na.omit.list(tbspre)))}
       }
    if(length(tbs)<1){sts[[j]]<-NA}else{sts[[j]]<- as.data.frame(do.call("rbind", na.omit.list(tbs)))}
  }
  save(sts,file=paste0("pathfile",year.pull,".RData")) 
  #throw into list for all states corresponding to each year
  #yrs[[k]]<-as.data.frame(do.call("rbind", na.omit.list(sts)))
  #yrs[[k]]$yr<-year.pull
  #save(yrs,file=paste0("pathfile",year.pull,".RData"))
}








