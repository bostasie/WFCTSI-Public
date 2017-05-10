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
#histogram of pts.bc

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
smoke<-smoke[(!is.na(smoke$date_smoke) & smoke$date_smoke<=as.Date("2015-01-01")),]


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





##########################
# SAVE WORK AND SUMMARIZE#
##########################
  #we now have diabetes patient with exposure and covariates
  #ready for cleaning and cph model

#Save the data frame with which you have been working as an Rdata object
#save(pts.bc,file="~/workshop/pts.bc.Rdata")
  
#saves all of the objects you have created/loads the same way as above
#save.image("Z:/R/klenoir/pts.bc.workspace.Rdata")
  
#load that data frame you previously saved
load("~/workshop/pts.bc.Rdata")





#Review Data
summary(pts.bc)
#are there any na's that we need to fix for model? How many are true?
summary(is.na(pts.bc))
#is everything in the format that we want for model?
str(pts.bc)
  #reformat a few things

##############
#CLEAN/FORMAT#
##############
#create outcome variable - time to development of bc in daya
pts.bc$time.to<-as.numeric(pts.bc$diag_date-pts.bc$baseline)
#fictitious end date due to lack of last follow-up data
#2016-12-28 (last diagnosis date) use 1/1/2017 as end of follow up date
pts.bc$time.to.bc<-ifelse(is.na(pts.bc$time.to),
                              as.numeric(as.Date("2017-01-01")-pts.bc$baseline), #time difference from when stop following
                              pts.bc$time.to) #otherwise use the time to diagnosis


#outcome - develop bc (TRUE) or not (FALSE)
pts.bc$bc<-ifelse(pts.bc$bc==F | is.na(pts.bc$bc),F, T)
pts.bc$bc<-as.factor(pts.bc$bc)

#smoking status: NA's become "missing
pts.bc$smoke<-ifelse(is.na(pts.bc$smoke), "missing", pts.bc$smoke)
pts.bc$smoke<-as.factor(pts.bc$smoke)

#make gender a factor
pts.bc$gender<-as.factor(pts.bc$gender)

#exposure - make factor and NA's become FALSE
pts.bc$tzd<-as.factor(ifelse(pts.bc$tzd==F | is.na(pts.bc$tzd),F, T))

summary(pts.bc)
#missing values for creat_year - impute

########
#IMPUTE#
########
#select variables for imputation
dput(names(pts.bc)) #can be helpful to copy and paste
row.names(pts.bc)<-pts.bc$MRN
pts.bc.preimp<-pts.bc[,c("gender", "race","age", "bc",
                 "CCIo", "creat_year", "tzd", "smoke",
                 "time.to.bc")] 


#install.packages("mice")
library(mice) #multivariate imp by chained equations
set.seed(454) #allows you to reproduce the imputation
pts.bc.imp<-mice(pts.bc.preimp,10) #10 imputations
#complete with 1st iteration
pts.imp<-complete(pts.bc.imp,1)
#model hates attributes of factor variables caused by imputation function
#correct all factors to remove extra attributes
pts.imp[,c("tzd","smoke","bc","gender")]<-lapply(pts.imp[,c("tzd","smoke","bc","gender")],
                                        function(x) factor(x,ordered=F))


#####
#GFR#
#####
#calculate GFR after imputation of creatinine
pts.imp$MRN<-rownames(pts.imp)
gfrset<-pts.imp[,c("MRN","creat_year")]
#just need a place holder date - quirk of function to be fixed
gfrset$date<-as.Date(Sys.Date())
#remove creat_year in pts, another quirk
pts.imp$creat_year<-NULL

#apply gfr function
library(WFCTSI)
pts.imp.gfr<-gfr(patients = pts.imp, labs = gfrset, ID.patients = "MRN",
    ID.labs = "MRN", lab.name="none",
    ord.value = "creat_year", result.date = "date",
    gender = "gender", race = "race", age = "age")




#######
#MODEL#
#######
#When multiple imputations are available,
#we typically pool these sets for the final model.
#for the sake of demonstration, we will use the first imputed set only
#muliple imputed sets are demonstrated below

#rms not working with newest R version
#installed directly from harrel's git hub as the version not yet updated on CRAN
#install.packages("rms")
#install.packages("devtools")
library(devtools)
#install_github("harrelfe/rms") #prompts install of Rtools
library(rms)

dd<-datadist(pts.imp.gfr)
options(datadist='dd')
bc.develop<-cph(formula=Surv(time=time.to.bc, bc=="TRUE") ~ rcs(age, 3) + gender
      + rcs(CCIo, 3) + smoke + rcs(GFR,3)
      +tzd,  #exposure
      ,data=pts.imp.gfr, x=T, y=T, surv=T)

#view model
bc.develop
bc.develop$terms  #has many parts with $
#Hazard Ratios
summary(bc.develop)




########################
#DESCRIPTIVE STATISTICS#
########################
#install.packages("epiDisplay")
library(epiDisplay)
#examine by tzd expsure
pts.imp.gfr$tzd<-as.character(pts.imp.gfr$tzd)
describebc<-tableStack(dataFrame=pts.imp.gfr,
                           c(gender, race, age, bc,
                              CCIo, smoke,
                              time.to.bc, GFR),
                            by=tzd, #breakdown by this variable
                            total.column=TRUE,
                            var.labels = TRUE,
                            #means=T,
                            medians=T,
                            na.rm =T)
#write descriptive statistics to csv file
#write.csv(x=describebc,file= "~/workshop/describebc_20170508.csv", row.names=T, col.names=T)





######################################
#ADVANCED CPH with MULIPLE IMPUTATION#
######################################
#add a for loop to utilize all imputed sets
#use pts.bc.imp - imputed data sets created above


#create empty list to hold iterations of cph models
#for each imputed data set
bc.multi<-list()


#creat for loop the length of number of data sets
for (i in 1:10){
#complete set i and then complete loop
#complete set i+1 and then complete loop
#and so on
pts.imp<-complete(pts.bc.imp,i)

#reformat all factors to remove attributes generated in mice
#quirk of new R
pts.imp[,c("tzd","smoke","bc","gender")]<-lapply(pts.imp[,c("tzd","smoke","bc","gender")],
                                                 function(x) factor(x,ordered=F))

#calculate GFR after imputation of creatinine
#for each imputed set
pts.imp$MRN<-rownames(pts.imp)
gfrset<-pts.imp[,c("MRN","creat_year")]
#just need a place holder date - quirk of function to be fixed
gfrset$date<-as.Date(Sys.Date())
#remove creat_year in pts, another quirk
pts.imp$creat_year<-NULL
#apply gfr function
pts.imp.gfr<-gfr(patients = pts.imp, labs = gfrset, ID.patients = "MRN",
                 ID.labs = "MRN", lab.name="none",
                 ord.value = "creat_year", result.date = "date",
                 gender = "gender", race = "race", age = "age")


#prepare for cph
dd<-datadist(pts.imp.gfr)
options(datadist='dd')
#throw models into i in list of 1:10
bc.multi[[i]]<-cph(formula=Surv(time=time.to.bc, bc=="TRUE")
                           ~ rcs(age, 3) + gender
                           + rcs(CCIo, 3) + smoke + rcs(GFR,3)
                           +tzd,  #exposure
                           ,data=pts.imp.gfr, x=T, y=T, surv=T)

}

#import pool.mi function from a txt file
Sys.setlocale('LC_ALL','C') #fix warning below with this
source(file="~/workshop/poolMI.txt")
    #Warning messages:
    #1: In grepl("\n", lines, fixed = TRUE) :
    #input string 4 is invalid in this locale  # text file you are reading in contains a character that is not available
pool.cph<-poolMI(bc.multi)
#hazard ratios
summary(pool.cph)


###############
#VIEWING MODEL#
###############
#view parts of pool.cph
pool.cph$sformula
#view first cph generated from first imputed data set
bc.multi[[1]]




####################################################
#ADVANCED CPH with MULIPLE IMPUTATION# ALTERNATIVE#
####################################################
#another option for multiple imputed data sets
#is the use the fit.mult.imput function
#quirk is that gfr should be imputed instead of calculated after imputation
#demonstrated without GFR
longdata<-complete(pts.bc.imp, action="long", include=T)
dd<-datadist(longdata)
options(datadist="dd")
patients.imp2<-as.mids(longdata)

fit.mult.impute(formula=Surv(time=time.to.bc, bc=="TRUE")
                ~ rcs(age, 3) + gender
                + rcs(CCIo, 3) + smoke #+ rcs(GFR,3)
                +tzd,
                fitter=cph, x=T, y=T,  xtrans=patients.imp2)


