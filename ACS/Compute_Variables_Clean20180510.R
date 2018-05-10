#Calculate Variables 
#Previous step  to download ACS states and tables of interest via tidycensus into lists
#BLOCK AND ZCTA variables were utilized, slight variation in output from tidycensus for consideration
#retrieve data - Saved by years for block group, saved in one list for zctas
#Use this file as a guide - not all variables calculated below were utilized an thus not all
#variables/tables were checked rigorously, please double check if using this code. 
#This code runs for subsets by year and is to be used as a template only.  
#Some modification may be necessary.

# sessionInfo()
# R version 3.4.4 (2018-03-15)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# #
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
#   [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
#   [5] LC_TIME=English_United States.1252    
#   
#   attached base packages:
#     [1] stats     graphics  grDevices utils     datasets  methods   base     
#   
#   other attached packages:
#     [1] dplyr_0.7.4         plyr_1.8.4          reshape2_1.4.3      data.table_1.10.4-3 stringr_1.3.0      
#   
#   loaded via a namespace (and not attached):
#     [1] Rcpp_0.12.16     assertthat_0.2.0 R6_2.2.2         magrittr_1.5     pillar_1.2.1     rlang_0.2.0     
#   [7] stringi_1.1.7    bindrcpp_0.2     tools_3.4.4      glue_1.2.0       yaml_2.1.18      compiler_3.4.4  
#   [13] pkgconfig_2.0.1  bindr_0.1.1      tibble_1.4.2  

#libraries
library(stringr)
library(data.table)
library(reshape2)
library(plyr)
library(dplyr)

#Manuals from ACS for Derived Variables
#https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2016StatisticalTesting5year.pdf 
#https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2016.pdf
#function to remove anything that is absent
#acs<-(#insert path with object name sts or years)
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
acs<-as.data.frame(do.call("rbind", na.omit.list(sts)))
acs<-as.data.frame(do.call("rbind", na.omit.list(yrs)))


#add columns
yrs$county<-NA
yrs$state<-NA
yrs$year<-"insert year"
geos<-acs[!duplicated(acs$GEOID),c("GEOID","NAME")]


##########
#FUNCTION##FORMULA FOR PERCENT/RATIO
##########

#calculate peproportions/percents - where the number is a subset of the denominator
fpercent<-function(data, #data object
                   table, #toable for numerator estimate (and denominator estimate)
                   table.den=NULL, #if the denominator is from another table
                   est.num, #numerator variable number for estimate
                   est.den, #denominator variable number used to normalize estimate
                   out.name) #what to name it
{

  if(nchar(est.num)==2){
  num.vec<-data[data$variable %in% paste0(table,"_0",est.num),]$estimate
  num.moe<-data[data$variable %in% paste0(table,"_0",est.num),]$moe
  block<-data[data$variable %in% paste0(table,"_0",est.num),]$GEOID
  }else{num.vec<-data[data$variable %in% paste0(table,"_00",est.num),]$estimate
        num.moe<-data[data$variable %in% paste0(table,"_00",est.num),]$moe
        block<-data[data$variable %in% paste0(table,"_00",est.num),]$GEOID}
  
#if no total "universe" variable in data set, use total pop or housing from different table
if(!is.null(table.den)){  
  den.vec<-data[data$variable %in% paste0(table.den,"_00",est.den),]$estimate
  den.moe<-data[data$variable %in% paste0(table.den,"_00",est.den),]$moe
}else{
  den.vec<-data[data$variable %in% paste0(table,"_00",est.den),]$estimate
  den.moe<-data[data$variable %in% paste0(table,"_00",est.den),]$moe}
  
  num.moe95<-num.moe*(1.96/1.645)
  den.moe95<-den.moe*(1.96/1.645)
  num.se<-num.moe/1.645
  den.se<-den.moe/1.645

  #DERIVED PROPRTION/PERCENTAGE for estimate
  est.vec<-as.numeric(rep(NA, length(block)))
  for(i in 1:length(block)){
    #if the numerator or denom is na, or if the denominator is 0, but estimate is not, 
    #then give the vector NA
    if((is.na(num.vec[i]) | is.na(den.vec[i]))|
       (!is.na(num.vec[i]) & num.vec[i]>0 & den.vec[i]==0)){
      est.vec[i]<-NA}else{
        #if both are 0, give it a 0 instead of inf
        if(num.vec[i]==0 & den.vec[i]==0){
          est.vec[i]<-0
          #other
        }else{
          est.vec[i]<-round(num.vec[i]/den.vec[i]*100,2)
          cat(i,"\n")
        }}}
  
  #empty holders for calculations below
  moe90<-as.numeric(rep(NA, length(block)))
  moe95<-as.numeric(rep(NA, length(block)))
  form90<-as.character(rep(NA, length(block))) 
  form95<-as.character(rep(NA, length(block)))
  se90<-as.numeric(rep(NA, length(block)))
  seform90<-as.character(rep(NA, length(block)))

  #90% MOE 
  i<-NULL
  for(i in 1:length(block)){
    #if anything is na, then give it NA. If den is 0, formula will also give NA
    if(is.na(num.moe[i]) | is.na(den.moe[i]) | is.na(den.vec[i]) | is.na(num.vec[i]) | den.vec[i]==0){
      moe90[i]<-NA
      form90[i]<-NA
    }else{
      #if negative is under sqrt is negative, use ratio
      if(!is.na((num.moe[i]^2)<((num.vec[i]/den.vec[i])^2*(den.moe[i]^2))) & 
         (num.moe[i]^2)<((num.vec[i]/den.vec[i])^2*(den.moe[i]^2))){
        #if area under sqrt is negative (rare), calculate conservative estimate
        moe90[i]<-(sqrt(num.moe[i]^2+((num.vec[i]/den.vec[i])^2)*den.moe[i]^2)/den.vec[i])*100
        form90[i]<-"ratio"
      }else{
        #perform normal proprotion 95% moe calculation
        moe90[i]<-(sqrt(num.moe[i]^2-((num.vec[i]/den.vec[i])^2)*den.moe[i]^2)/den.vec[i])*100
        form90[i]<-"prop"
      }}
    cat(i,"\n")}
  
  #95% MOE 
  i<-NULL
  for(i in 1:length(block)){
    #if anything is na, then give it NA. If den is 0, formula will also give NA
    if(is.na(num.moe95[i]) | is.na(den.moe95[i]) | is.na(den.vec[i]) | is.na(num.vec[i]) | den.vec[i]==0){
      moe95[i]<-NA
      form95[i]<-NA
    }else{
      #if negative is under sqrt is negative, use ratio
      if(!is.na((num.moe95[i]^2)<((num.vec[i]/den.vec[i])^2*(den.moe95[i]^2))) & 
         (num.moe95[i]^2)<((num.vec[i]/den.vec[i])^2*(den.moe95[i]^2))){
        #if area under sqrt is negative (rare), calculate conservative estimate
        moe95[i]<-(sqrt(num.moe95[i]^2+((num.vec[i]/den.vec[i])^2)*den.moe95[i]^2)/den.vec[i])*100
        form95[i]<-"ratio"
      }else{
        #perform normal proprotion 95% moe calculation
        moe95[i]<-(sqrt(num.moe95[i]^2-((num.vec[i]/den.vec[i])^2)*den.moe95[i]^2)/den.vec[i])*100
        form95[i]<-"prop"
      }}
    cat(i,"\n")} 

  
  #90% SE - will be the same as 95% SE and very similar to moe90/1.645, 
  #but would rather derive by formula in case of extreme values
  i<-NULL
  for(i in 1:length(block)){
    #if anything is na, then give it NA. If den is 0, formula will also give NA
    if(is.na(num.se[i]) | is.na(den.se[i]) | is.na(den.vec[i]) | is.na(num.vec[i]) | den.vec[i]==0){
      se90[i]<-NA
      form90[i]<-NA
    }else{
      #if negative is under sqrt is negative
      if(!is.na((num.se[i]^2)<((num.vec[i]/den.vec[i])^2*(den.se[i]^2))) & 
         (num.se[i]^2)<((num.vec[i]/den.vec[i])^2*(den.se[i]^2))){
        #check den.vec<-85041778, den.se<-125610, num.se<-77381, num.vec<-39563948, should get .06
        #if area under sqrt is negative (rare), calculate more conservative estimate
        se90[i]<-(sqrt(num.se[i]^2+((num.vec[i]/den.vec[i])^2)*den.se[i]^2)/den.vec[i])*100
        seform90[i]<-"ratio"
      }else{
        #perform normal proprotion calculation
        se90[i]<-(sqrt(num.se[i]^2-((num.vec[i]/den.vec[i])^2)*den.se[i]^2)/den.vec[i])*100
        seform90[i]<-"prop"
      }}
    cat(i,"\n")} 


  #90 will be the same as se/est.vec and the 95% calculation
  cv90<-((moe90/1.645)/est.vec)*100 
  #remove infinity caused by 0 in denominator
  cv90[mapply(is.infinite, cv90)] <- NA

   #COMBINE
   out<-as.data.frame(cbind(block,est.vec,moe90,moe95, 
                          se90,cv90, form90, form95, seform90))
   #format
   out[]<-lapply(out, as.character)
   out[,c("est.vec", "moe90", "moe95", "se90",
         "cv90")]<-lapply( out[,c("est.vec", "moe90", "moe95", "se90", "cv90")],
                                               function(x) round(as.numeric(x),digits=2))
   #name
   names(out)<-c("GEOID",paste0(out.name,"_est"),
                paste0(out.name,("_moe90")), 
                paste0(out.name,("_moe95")),  
                paste0(out.name,("_se")),
                paste0(out.name,("_cv")),  
                paste0(out.name,("_form90")), 
                paste0(out.name,("_form95")),
                paste0(out.name,("_seform90")))
  return(out)
}




#####################
#CALCULATE VARIABLES#
#####################
#start building vars
#vars with no proportion or calculation necessary
library(dplyr)
#total population, median age, and income are unchanged 
pop<-acs[grepl("B01003", acs$variable),c("GEOID","estimate","moe")]
pop$moe90<-pop$moe
pop$moe95<-pop$moe*(1.96/1.645)
pop$se<-pop$moe/1.645 #critical value for 90% is 1.645
pop$cv<-pop$se/pop$estimate*100
pop$moe<-NULL
names(pop)[2:ncol(pop)]<-paste0("pop_",names(pop)[2:ncol(pop)])

#housesues
house<-acs[grepl("B25001", acs$variable),c("GEOID","estimate","moe","variable")]
house<-house[!duplicated(house$GEOID),c("GEOID","estimate","moe")]
house$moe90<-house$moe
house$moe95<-house$moe*(1.96/1.645)
house$se<-house$moe/1.645
house$cv<-house$se/house$estimate*100
house$moe<-NULL
names(house)[2:ncol(house)]<-paste0("house_",names(house)[2:ncol(house)])

#join
geosjoin1<-dplyr::inner_join(x=geos, y=pop, by="GEOID")
geosjoin2<-dplyr::inner_join(x=geosjoin1, y=house, by="GEOID")


#median age
age<-acs[(acs$variable %in% "B01002_001"),c("GEOID", "estimate","moe")]
age$moe90<-age$moe
age$moe95<-age$moe*(1.96/1.645)
age$se<-age$moe/1.645
age$cv<-age$se/age$estimate*100
age$moe<-NULL
names(age)[2:ncol(age)]<-paste0("age_",names(age)[2:ncol(age)])

#median income
inc<-acs[(acs$variable %in% "B19013_001"),c("GEOID", "estimate","moe")]
inc$moe90<-inc$moe
inc$moe95<-inc$moe*(1.96/1.645)
inc$se<-inc$moe/1.645 
inc$cv<-inc$se/inc$estimate*100
inc$moe<-NULL
names(inc)[2:ncol(inc)]<-paste0("income_",names(inc)[2:ncol(inc)])

#combine above
geosjoin3<-dplyr::inner_join(x=geosjoin2, y=age, by="GEOID")
geosjoin4<-dplyr::inner_join(x=geosjoin3, y=inc, by="GEOID")

#format - round
geosjoin4[4:ncol(geosjoin4)]<-
  lapply(geosjoin4[4:ncol(geosjoin4)], function(x) round(x,1))

#################
#CALCULATED VARS#
#################
#Use universe total to normalize
#no prelim calculation necessary
males<-fpercent(data=acs, table="B01001", table.den=NULL, est.num=2, est.den=1, out.name="male") 
females<-fpercent(data=acs, table="B01001" , est.num=26, est.den=1, out.name="female") 
      #similar to calculation  100-%male check
white_alone<-fpercent(data=acs, table="B02001" , est.num=2, est.den=1, out.name="white_alone")
black_alone<-fpercent(data=acs, table="B02001" , est.num=3, est.den=1, out.name="black_alone")
#use total pop to normalize - No universe total, alone variables have universe var
black_any<-fpercent(data=acs, table="B02009" ,table.den="B01003", est.num=1, est.den=1, out.name="black_any") 
ai_ak<-fpercent(data=acs, table="B02001" , est.num=4, est.den=1, out.name="ai_ak")  
asian<-fpercent(data=acs, table="B02001" , est.num=5, est.den=1, out.name="asian")  
nh_pi<-fpercent(data=acs, table="B02001" , est.num=6, est.den=1, out.name="nh_pi")  
other<-fpercent(data=acs, table="B02001" , est.num=7, est.den=1, out.name="other")  
two_race<-fpercent(data=acs, table="B02001" , est.num=8, est.den=1, out.name="two_race")  
hisp<-fpercent(data=acs, table="B03003" , est.num=3, est.den=1, out.name="hisp") #ethnicity
vet<-fpercent(data=acs, table="B21001" , est.num=2, est.den=1, out.name="vet")  #veteran
vacant<-fpercent(data=acs, table="B25002" , est.num=3, est.den=1, out.name="vacant") #vacancy - housing units
ownership<-fpercent(data=acs, table="B25003" , est.num=2, est.den=1, out.name="ownership") #home own, of occupied units
poverty<-fpercent(data=acs, table="B17017" , est.num=2, est.den=1, out.name="poverty") #households below poverty
#income in the past 12 months below poverty levvel
live.alone<-fpercent(data=acs, table="B11001" , est.num=8, est.den=1, out.name="live_alone") #householder living alone over, households
unemployed<-fpercent(data=acs, table="B23025" , est.num=5, est.den=1, out.name="unemployed") #unemployed (population 16+)
transport<-fpercent(data=acs, table="B08134" , est.num=10, est.den=1, out.name="transport")      
#Universe:  Workers 16 years and over who did not work at home who commute 60 minutes or more

#join
library(plyr)
geosjoin6<-join_all(list(geosjoin4,males[1:7],females[1:7],white_alone[1:7],black_alone[1:7],
                      black_any[1:7],
                      ai_ak[1:7],
                      asian[1:7],
                      nh_pi[1:7],
                      other[1:7],
                      two_race[1:7],
                    hisp[1:7],
                    vet[1:7],
                    vacant[1:7],
                    ownership[1:7],
                    poverty[1:7],
                    live.alone[1:7],
                    unemployed[1:7],
                    transport[1:7]), by='GEOID', type='left')



#extra calculations for derived variables prior to proportion calculation
#caution as more calc introduces more error
#Optional variables
###############################
#education, high school or less
#EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
edu<-acs[grepl("B15003",acs$variable),]
edu<-edu[order(edu$GEOID),]
geo<-edu[!duplicated(edu$GEOID),]$GEOID
#which vars to derive estimate
add<-paste0("B15003_",str_pad(seq(2,18,1),pad="0",side="left", width=3)) #or just above
#derived estimate
vars<-paste0("edu[edu$variable==","'",add,"'",",]$estimate")
edu.est<- eval(parse(text=paste0(vars,collapse="+")))
#derived moe
vars<-paste0("edu[edu$variable==","'",add,"'",",]$moe^2")
edu.moe<- sqrt(eval(parse(text=paste0(vars,collapse="+"))))

#table of derived estimates with denominator
temp.table.edu<-as.data.frame(cbind(geo, rep("B15003_00extra", length(geo)), edu.est, edu.moe))
temp.table.edu[]<-lapply(temp.table.edu,as.character)
names(temp.table.edu)<-c("GEOID","variable","estimate","moe")
edu.denom<-edu[edu$variable %in% "B15003_001",names(temp.table.edu)]
temp.edu<-rbind(edu.denom,temp.table.edu)
temp.edu[,c("estimate","moe")]<-lapply(temp.edu[,c("estimate","moe")], as.numeric)
edu.total<-fpercent(data=temp.edu, table="B15003" , est.num="extra", est.den=1, out.name="edu")



##############
#poverty ratio  # fairly similar to poverty
#Ratios below 1.00 indicate that the income for the
#respective family or unrelated individual is 
#below the official definition of poverty,
pov.rat<-acs[grepl("C17002",acs$variable),]
pov.rat<-pov.rat[order(pov.rat$GEOID),]
geo<-pov.rat[!duplicated(pov.rat$GEOID),]$GEOID
#which vars to derive estimate
add<-paste0("C17002_",str_pad(c(2,3),pad="0",side="left", width=3))
#derived estimate
vars<-paste0("pov.rat[pov.rat$variable==","'",add,"'",",]$estimate")
pov.rat.est<- eval(parse(text=paste0(vars,collapse="+")))
#derived moe
vars<-paste0("pov.rat[pov.rat$variable==","'",add,"'",",]$moe^2")
pov.rat.moe<- sqrt(eval(parse(text=paste0(vars,collapse="+"))))
#table of derived estimates with denominator
temp.table.pov.rat<-as.data.frame(cbind(geo, rep("C17002_00extra", length(geo)), pov.rat.est, pov.rat.moe))
temp.table.pov.rat[]<-lapply(temp.table.pov.rat,as.character)
names(temp.table.pov.rat)<-c("GEOID","variable","estimate","moe")
pov.rat.denom<-pov.rat[pov.rat$variable %in% "C17002_001",names(temp.table.pov.rat)]
temp.pov.rat<-rbind(pov.rat.denom,temp.table.pov.rat)
temp.pov.rat[,c("estimate","moe")]<-lapply(temp.pov.rat[,c("estimate","moe")], as.numeric)
pov.rat.total<-fpercent(data=temp.pov.rat, table="C17002" , est.num="extra", est.den=1, out.name="pov.rat")


###########################################################
#english - hosehold level.  Limited english speaking households
#use C16002 for 2016, Different table with more categories
#2016
eng<-acs[grepl("C16002",acs$variable),]
geo<-eng[!duplicated(eng$GEOID),]$GEOID
add<-paste0("C16002_",str_pad(c(4,7,10,13),pad="0",side="left", width=3))
#add<-paste0("C16002_",str_pad(c(4,7,10,13,16,19,22,25,28,31,34,37),pad="0",side="left", width=3)) #FOR 2016
#derived estimate
vars<-paste0("eng[eng$variable==","'",add,"'",",]$estimate")
eng.est<- eval(parse(text=paste0(vars,collapse="+")))
#derived moe
vars<-paste0("eng[eng$variable==","'",add,"'",",]$moe^2")
eng.moe<- sqrt(eval(parse(text=paste0(vars,collapse="+"))))
#table of derived estimates with denominator
temp.table.eng<-as.data.frame(cbind(geo, rep("C16002_00extra", length(geo)), eng.est, eng.moe))
temp.table.eng[]<-lapply(temp.table.eng,as.character)
names(temp.table.eng)<-c("GEOID","variable","estimate","moe")
eng.denom<-eng[eng$variable %in% "C16002_001",names(temp.table.eng)]
temp.eng<-rbind(eng.denom,temp.table.eng)
temp.eng[,c("estimate","moe")]<-lapply(temp.eng[,c("estimate","moe")], as.numeric)
eng.total<-fpercent(data=temp.eng, table="C16002" , est.num="extra", est.den=1, out.name="eng")
#universe is households
#END 2016

#2013-2015
eng<-acs[grepl("B16002",acs$variable),]
eng<-eng[order(eng$GEOID),]
#which vars to derive estimate
add<-paste0("B16002_",str_pad(c(4,7,10,13),pad="0",side="left", width=3)) #2013-2015
#derived estimate
vars<-paste0("eng[eng$variable==","'",add,"'",",]$estimate")
eng.est<- eval(parse(text=paste0(vars,collapse="+")))
#derived moe
vars<-paste0("eng[eng$variable==","'",add,"'",",]$moe^2")
eng.moe<- sqrt(eval(parse(text=paste0(vars,collapse="+"))))
#table of derived estimates with denominator
temp.table.eng<-as.data.frame(cbind(geo, rep("B16002_00extra", length(geo)), eng.est, eng.moe))
temp.table.eng[]<-lapply(temp.table.eng,as.character)
names(temp.table.eng)<-c("GEOID","variable","estimate","moe")
eng.denom<-eng[eng$variable %in% "B16002_001",names(temp.table.eng)]
temp.eng<-rbind(eng.denom,temp.table.eng)
temp.eng[,c("estimate","moe")]<-lapply(temp.eng[,c("estimate","moe")], as.numeric)
eng.total<-fpercent(data=temp.eng, table="B16002" , est.num="extra", est.den=1, out.name="eng")
#universe is households

#######################################################
#language - language spoken at home other than english:  #apparently not at zcta level or didn't download
#non english speakers
#language
lang<-acs[grepl("C16001",acs$variable),]
lang<-lang[order(lang$GEOID),]
geo<-lang[!duplicated(lang$GEOID),]$GEOID
#which vars to derive estimate
#derived estimate - subtraction (total population 5 yrs and older ) #subtract english only from total to get non-english
lang.est<-lang[lang$variable=="C16001_001",]$estimate-lang[lang$variable=="C16001_002",]$estimate
#derived moe
lang.moe<- sqrt((lang[lang$variable=="C16001_001",]$moe)^2+(lang[lang$variable=="C16001_002",]$moe)^2)
#table of derived estimates with denominator
temp.table.lang<-as.data.frame(cbind(geo, rep("C16001_00extra", length(geo)), lang.est, lang.moe))
temp.table.lang[]<-lapply(temp.table.lang,as.character)
names(temp.table.lang)<-c("GEOID","variable","estimate","moe")
lang.denom<-lang[lang$variable %in% "C16001_001",names(temp.table.lang)]
temp.lang<-rbind(lang.denom,temp.table.lang)
temp.lang[,c("estimate","moe")]<-lapply(temp.lang[,c("estimate","moe")], as.numeric)
lang.total<-fpercent(data=temp.lang, table="C16001" , est.num="extra", est.den=1, out.name="lang")



###########################
#MINORITY - NON WHITE ONLY
#non english speakers
#mincation, high school or less
min<-acs[grepl("B02001",acs$variable),]
min<-min[order(min$GEOID),]
geo<-min[!duplicated(min$GEOID),]$GEOID
#which vars to derive estimate
#derived estimate - subtraction (total population 5 yrs and older )
min.est<-min[min$variable=="B02001_001",]$estimate-min[min$variable=="B02001_002",]$estimate
#derived moe
min.moe<- sqrt((min[min$variable=="B02001_001",]$moe)^2+(min[min$variable=="B02001_002",]$moe)^2)
#table of derived estimates with denominator
temp.table.min<-as.data.frame(cbind(geo, rep("B02001_00extra", length(geo)), min.est, min.moe))
temp.table.min[]<-lapply(temp.table.min,as.character)
names(temp.table.min)<-c("GEOID","variable","estimate","moe")
min.denom<-min[min$variable %in% "B02001_001",names(temp.table.min)]
temp.min<-rbind(min.denom,temp.table.min)
temp.min[,c("estimate","moe")]<-lapply(temp.min[,c("estimate","moe")], as.numeric)
min.total<-fpercent(data=temp.min,table="B02001", table.den=NULL, est.num="extra", est.den=1, out.name="min")


############################
#disability - all age groups 
dis<-acs[grepl("C18101",acs$variable),]
dis<-dis[order(dis$GEOID),]
geo<-dis[!duplicated(dis$GEOID),]$GEOID
#which vars to derive estimate
add<-paste0("C18101_",str_pad(c(4,7,10,14,17,20),pad="0",side="left", width=3))
#derived estimate
vars<-paste0("dis[dis$variable==","'",add,"'",",]$estimate")
dis.est<- eval(parse(text=paste0(vars,collapse="+")))
#derived moe
vars<-paste0("dis[dis$variable==","'",add,"'",",]$moe^2")
dis.moe<- sqrt(eval(parse(text=paste0(vars,collapse="+"))))
#table of derived estimates with denominator
temp.table.dis<-as.data.frame(cbind(geo, rep("C18101_00extra", length(geo)), dis.est, dis.moe))
temp.table.dis[]<-lapply(temp.table.dis,as.character)
names(temp.table.dis)<-c("GEOID","variable","estimate","moe")
dis.denom<-dis[dis$variable %in% "C18101_001",names(temp.table.dis)]
temp.dis<-rbind(dis.denom,temp.table.dis)
temp.dis[,c("estimate","moe")]<-lapply(temp.dis[,c("estimate","moe")], as.numeric)
dis.total<-fpercent(data=temp.dis, table="C18101" , est.num="extra", est.den=1, out.name="dis")

##############
#COMPILE VARS#
##############
#anything else to join 
geosjoin7<-join_all(list(geosjoin6,
                         min.total[1:7],
                         eng.total[1:7],
                         edu.total[1:7]), by='GEOID', type='left')

#block group categories
geosjoin7$year<-"insert year"
geosjoin7$state<-substr(geosjoin7$GEOID, start=1, stop=2)
geosjoin7$county<-substr(geosjoin7$GEOID, start=3, stop=5)
geosjoin7$tract<-substr(geosjoin7$GEOID, start=6, stop=11)
geosjoin7$block_group<-substr(geosjoin7$GEOID, start=12, stop=12)

#######
#CLEAN#
#######
cleanacs<-geosjoin7
numerics<-sapply(cleanacs,is.numeric)[sapply(cleanacs,is.numeric)==T]
num<-names(numerics)
cleanacs[,num][is.na(cleanacs[,num])]<--1
names(cleanacs)<-gsub("form90","form",names(cleanacs))
hold<-names(cleanacs)[grepl("form",names(cleanacs))]
cleanacs[hold][is.na(cleanacs[hold])]<-"not_calculated"
cleanacs[hold][cleanacs[hold]=="prop"]<-"proportion"
names(cleanacs)<-tolower(names(cleanacs))
#acs2013<-cleanacs[,c(149,1:2,150:ncol(cleanacs), 3:148)]
cleanacs$geo_type<-"block_group"
cleanacs$acs_est<-"acs5yr"
cleanacs[cleanacs=="Inf"]<--1
write.table(cleanacs, "pathfilename.txt", sep="|",quote = FALSE, row.names=F)
