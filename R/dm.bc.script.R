######################
#MERGE DATA FROM I2B2#
######################
#import i2b2 file, merge and clean data in preparation for model 
# data setpredefined as patients with diabetes type II between ages of 40 and 90 on any medication
  #outcome- bladder cancer
  #exposure - tzd
  #covariates - gender, race, age, bmi, smoking status
  #baseline - 2015-01-01 
 

###################
#About R interface#
###################


#import file into workspace
#install.packages("readxl")
library(readxl)

#finding help
#https://cran.r-project.org/web/packages/
#https://cran.r-project.org/web/packages/readxl/readxl.pdf
??readxl 

#Write function to read multiple sheets
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, 
                    col_types ="text",  #import all as characters
                    col_names=T, #give it column names
                    na=c(""," "), #designate white space as na
                    trim_ws=T )) #trim extra spaces
  names(x) <- sheets
  x
}

#call function, apply it to your file
bc.data<-read_excel_allsheets("~/workshop/dm.bladder.cancer.xlsx")
  #bc.data<-read_excel_allsheets("Z:/BWells/Clinical Informatics Short Course/R/klenoir/dm.bladder.cancer.xlsx")
  #bc.data<-read_excel_allsheets("Z:/klenoir/dm.bladder.cancer.xlsx") #path example
  #this does not require a function
  #bc.data<-read.csv(file="Z:/klenoir/dm.bladder.cancer.csv", header=T, colClasses="character", na.strings=c("", "#N/A", "NULL")))


##############
#DEMOGRAPHICS#
##############
#pull out patients and clean demographics, calculate age at baseline

#############
#REVIEW DATA#
#############

#separate into data frames and make names user friendly
#list vs data frame (class)
pts<-as.data.frame(bc.data$Patients)
names(pts)<-c("MRN","DOB","gender","race")

#General data views, checks
head(pts) #see top of data
tail(pts) #see bottom of data
##########
#PRACTICE#  class, str, how to call "columns", vector
##########




    #Do we have unique set of pts (no duplicates)?
    length(unique(pts$MRN))
    nrow(pts)
    #or
    length(unique(pts$MRN))==nrow(pts)

###################
#Set baseline date#
###################
#If medication is active on this date, time to first occurance of bladder diagnosis
pts$baseline<-as.Date("2015-01-01")


#Date format and examples
#https://www.stat.berkeley.edu/~s133/dates.html
#handy date packages: lubridate (add/subtract years/months/days), chron
#Calculate Age at baseline
  pts$dob<-as.Date(as.POSIXlt(pts$DOB, format="%m/%d/%Y %H:%M:%S"))
  pts$age<-pts$baseline-pts$dob 
  #look at the top of the data set, what's wrong?
  
  
  
#get years,  make it numeric, round it (no decimals)
pts$age<-round(as.numeric((pts$baseline-pts$dob)/365.25),0)
  #Formatting
    #as.numeric
    #as.character
    #as.Date
  str(pts$age) #numeric type
  #histogram of age
  ##########
  #PRACTICE#
  ##########
  
  
#visualize race
#install.packages("ggplot2")
library(ggplot2)  #http://www.cookbook-r.com/Graphs/
  #review composition
  table(pts$race)
  #visualize with gplot
  g <- ggplot(pts, aes(race)) +geom_bar()
  

    
###########
#DIAGNOSES#
###########
#use list of icd9 and 10 codes to find patients who were diagnosed with bladder cancer
    #merge this into demographic file
#calculate CCI from icd codes
  
diagcodes<-as.data.frame(bc.data$Diagnoses)
names(diagcodes)<-c("mrn", "visit", "visit_date", "diag9", "diag10", "diag_desc")

#Find those with bladder cancer
#http://www.icd10data.com/
#icd9 codes: "188" 
#icd10 codes: "C67"

#default grepl function catches 188 and x188.x 
#returns T/F argument
#says: go row by row, and if there is a 188 code in diag9 column or a C67 in the diag10 column, 
                #return "true". If not, return "false"
                #make a new column called "bc" for the result
diagcodes$bc<-sapply(1:nrow(diagcodes), function(x)  ifelse(grepl("188", diagcodes[x,"diag9"],  ignore.case=T) |
                                          grepl("C67", diagcodes[x,"diag10"],  ignore.case=T),T,F))

#check logic
  #for all that are true, did I get the expected codes?
  check<-diagcodes[diagcodes$bc==T,] #get these where bladder cancer is T
  table(check$diag9) 
  #################
  #PRACTICE#  #check icd10 codes that we identified
  ##########

#get bladder cancer cases only (format date first)
diagcodes$diag_date<-as.Date(as.POSIXlt(diagcodes$visit_date, format="%m/%d/%Y %H:%M:%S"))
#get where my bc identification column is T and where there is not and NA value
bc.cases<-diagcodes[!is.na(diagcodes$bc) & diagcodes$bc==T,]
#get earliest date of diagnosis: order by date to bring earliest to top
bc.cases<-bc.cases[order(bc.cases$diag_date,decreasing=F),]
#get only the top instance
bc.cases<-bc.cases[!duplicated(bc.cases$mrn),]
  #bc.cases<-bc.cases[duplicated(bc.cases$mrn)==F,] #some people like this alternative to the ! sign


#merge with patients
pts.bc<-merge(pts,bc.cases[,c("mrn","bc","diag_date")], by.x="MRN", by.y="mrn", all.x=T, all.y=F)
summary(pts.bc)
#only get cases where diagnosis date occurs after the baseline (we want to keep those without a diagnosis date)
  nrow(pts.bc)
  pts.bc<-pts.bc[is.na(pts.bc$diag_date) | (pts.bc$diag_date>pts.bc$baseline),]
  

  
#Calculate CCI
#https://github.com/bostasie/WFCTSI-Public
library(WFCTSI)
??CCI
pts.bc<-CCI(patients=pts.bc, diagcodes=diagcodes, ID.patients = "MRN",
    ID.diagcodes = "mrn", dx.diagcodes = "diag9",
    dx.diagcodes2 = "diag10", dx.date = "diag_date",
    st.date = "1800-01-01", end.date = "baseline", 
    weights = "original", method = "quan")
  #everyone should have a fake score of at least 2 (diabetes)...
  #but this is fake data, fewer codes so it doesn't crash

######
#LABS#
######
#Find most recent creatinine value (will be used to calculated GFR) that occured within a year prior to baseline
#merge it with demographics

labs<-as.data.frame(bc.data$Labs)
names(labs)<-c("mrn", "visit", "date_time", "lab_code", "lab_desc", 
               "txt_value", "num_value", "units")
labs$lab_date<-as.Date(as.POSIXlt(labs$date_time, format="%m/%d/%Y %H:%M:%S"))

#view codes
table(labs$lab_code)
creat<-c("CMEP:CREATININE","CREATININE")
creat_labs<-labs[labs$lab_code %in% creat,] #matches exact



#install.packages("lubridate")
library(lubridate)
#install.packages("survival")
library(survival)
#neardate function gives closes date before baseline (or/and after)
indx1<-neardate(pts.bc$MRN, creat_labs$mrn, pts.bc$baseline, creat_labs$lab_date, best= "prior")
temp1<-creat_labs[indx1, c("num_value","units","lab_date")]
pts.bc<-cbind(pts.bc,temp1)	#cbind - order must be same for two 	
names(pts.bc)[names(pts.bc)=="num_value"]<-"creat"
#specify 30 days before baseline
#if the lab date is within one year prior to baseline, use it, otherwise, give it NA
pts.bc$creat_year<-ifelse(pts.bc$lab_date>=(pts.bc$baseline-years(1)), pts.bc$creat,NA)
pts.bc$creat_year<-as.numeric(pts.bc$creat_year)
  #how do you test/break it down? Test second row
  #pts.bc$lab_date[2]
  #pts.bc$baseline[2]
  #pts.bc$baseline[2]-years(1)
  #pts.bc$lab_date[2]>=(pts.bc$baseline[2]-years(1))
  
#normal range 0.5-1.2mg/dL, check for outliers
##########
#PRACTICE#
##########


#replace outliers with na, anything over 100 for now
pts.bc$creat_year<-ifelse(pts.bc$creat_year>100,NA,pts.bc$creat_year)


######
#MEDS#
######
#identify if patient was on tzd med at baseline (if active), merge information with pts
#In this data set, all meds are presumed active without end date

meds<-as.data.frame(bc.data$Meds)
names(meds)<-c("mrn", "visit", "start", "end", 
               "ndc_rxnorm", "desc", "txt_val", "num_val", "units")
#format date
meds$date_med<-as.Date(as.POSIXlt(meds$start, format="%m/%d/%Y %H:%M:%S"))

#helpful medication information
#https://mor.nlm.nih.gov/download/rxnav/RxNavDoc.html

#meds list for which to search:
#pioglitazone/Actos 
#rosiglitazone/Avandia  #search by drug or Rxnorm

#look for med descriptions "like" these names
tzdmeds<-c("piogli","rosigli")
meds$tzd<-sapply(1:nrow(meds), function(x)  grepl(paste(tzdmeds, collapse="|"), meds[x,"desc"],  ignore.case=T))
  #does the same thing, but might use the method above in the case for searching for many alternatives
  #meds$tzdsame<-sapply(1:nrow(meds), function(x)  grepl("pioglit|rosiglit", meds[x,"desc"],  ignore.case=T))
 
  table(meds[meds$tzd==T,][,"desc"]) #looks like we got the expected
  unique(meds[meds$tzd==F,][,"desc"]) #anything  missed?

  
#get earliest instance and remove everything else
#subset where tzd is T, and do not get NA's
tzdmeds<-meds[!is.na(meds$tzd) & meds$tzd==T,]
tzdmeds<-tzdmeds[order(tzdmeds$date_med,decreasing=F),]
tzdmeds<-tzdmeds[!duplicated(tzdmeds$mrn),]

#neardate function gives closes date before baseline (or/and after)
indx2<-neardate(pts.bc$MRN, tzdmeds$mrn, pts.bc$baseline, tzdmeds$date_med, best= "prior")
temp2<-tzdmeds[indx2, c("tzd", "date_med")]
pts.bc<-cbind(pts.bc,temp2)	#cbind - order must be same for two 	

################
#SMOKING STATUS#
################
#find if patient has ever smoked before (categorize, as prior, never, or missing data) and merge with data

smoke<-as.data.frame(bc.data$Smoking)
names(smoke)<-c("mrn", "visit", "date_time", "history", "desc", 
                  "txt", "num", "unit")
#format date
smoke$date_smoke<-as.Date(as.POSIXlt(smoke$date_time, format="%m/%d/%Y %H:%M:%S"))
unique(smoke$desc)
#only get smoking status before baseline
smoke<-smoke[smoke$date_smoke<=as.Date("2015-01-01"),]


#categorize into ever/never/missing
#Decide how to code, unknown and missing category?
#if unknown or missing, assume non-smoking?
#for these purposes we'll code as missing
unique(smoke$desc)
unknown<-"Unknown"
never<-c("Never", "Never Used", "Never Smoker")
ever<-c("Former User","Former Smoker", "Quit", "Yes", "Current Some Day Smoker", "Current User", 
           "Current Every Day Smoker")

smoke$smoke<-ifelse(smoke$desc %in% never, "never",
                    ifelse(smoke$desc %in% ever, "smoke",
                           ifelse(is.na(smoke$desc) | smoke$desc %in% unknown, "missing","oops")))
  #anything with an oops means you missed it.  You could just stop at the second ifelse statement
    #and code all else as missing
  #table(smoke$smoke)

#if multiple instances per person, get smoke over never, and never over unknown
  smoke<-smoke[order(smoke$smoke,decreasing=T),]
  #demonstration of ordering if not alphabetical, missing on bottom, never in middle, smoke on top
  #smoke<-smoke[order(smoke$smoke=="missing",smoke$smoke=="never",smoke$smoke=="smoke"),]
  #remove duplicates (get first instance on top)
    length(unique(smoke$mrn))==nrow(smoke)
  smoke<-smoke[!duplicated(smoke$mrn),]
    length(unique(smoke$mrn))==nrow(smoke)
  
  pts.bc<-merge(pts.bc,smoke[,c("mrn","smoke")], by.x="MRN", by.y="mrn", all.x=T, all.y=T)

###########
#SUMMARIZE#
###########

#we now have diabetes patient with exposure and covariates
#ready for cleaning and cph model

  
