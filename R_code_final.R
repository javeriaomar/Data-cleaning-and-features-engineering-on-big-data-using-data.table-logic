library(foreign)
library(data.table)
library(magrittr)
library(stringr)
library(dplyr)
library(mice) 
library(plyr)
library(tidyverse)
library(Hmisc)
library(openxlsx)

options(scipen = 999)

#### Read Main data in raw format

raw_data <- read.xlsx("path name/Global_2019.xlsx") %>%
  setDT()

#### Data Cleaning

#### Remove -ve values/""/"-" values in numeric and integer columns replace it with NA. As a test of this step check
#### values in columns d2, before there was -ve values in that column. Now they are replaced with NAs
for (j in seq_along(raw_data)) {
  data.table::set(raw_data, i = which((is.integer(raw_data[[j]]) | is.numeric(raw_data[[j]]))
                                      & (raw_data[[j]] < 0) | raw_data[[j]] == "" | raw_data[[j]] == "-"), j = j, value = NA)
}


#INCONSISTANCIES
#Step 1
#1.	Extract estimated interview year from the country variable . Compare it with the interview year ( a14y) and complete the 
#one that is empty of the two with the data of the other. In the case of being both empty they will remain empty. If both have
#data, if it is a difference greater than 2 years, we delete the record, if not, we are left with the year less.

#Extract estimated interview year from the country variable . Compare it with the interview year ( a14y) and complete the 
#one that is empty of the two with the data of the other. In the case of being both empty they will remain empty.

raw_data[,`:=`(
  estimated_year_of_interview = as.integer(str_extract_all(country,"[0-9]{1,4}", simplify = TRUE)[[1]]),
countries= gsub("[0-9]","",country)  
)] %>% .[, `:=` (
  a14y = ifelse(is.na(a14y),estimated_year_of_interview,a14y),
  estimated_year_of_interview= ifelse(is.na(estimated_year_of_interview),a14y,estimated_year_of_interview),
  country= countries
)] %>% .[,countries := NULL]

#"If both have data, if the difference is greater than 2 years, we delete the record(row)".we used 
#Difference between years: temporal diff col

raw_data <- raw_data[,temporal_year_dif:=abs(a14y - estimated_year_of_interview )][temporal_year_dif <=2,]
                     

##### "#In difference of less than two years, choose min year" part of code

raw_data[,interview_year:=min(a14y,estimated_year_of_interview)]
  
#2. Validation year of registration and year of commencement of operations . In case of both being null, leave as null. 
#If they are equal, or one year apart is considered correct. If either is empty we will put a year before or after 
#respectively. If there is more than 3 years of difference we will delete the record.

raw_data[,`:=` (year_of_begin=b5,year_of_registration=b6b)]
raw_data[,`:=` (b5= ifelse(is.na(year_of_begin),year_of_registration+1,b5), 
                b6b= ifelse(is.na(year_of_registration),year_of_begin-1, b6b))
         ]

raw_data[,temporal_year_dif:=abs(b6b-b5)]
dim(raw_data[temporal_year_dif>3])[1]/dim(raw_data)[1]*100
raw_data <- raw_data[temporal_year_dif<3,][,c("temporal_year_dif", "estimated_year_of_interview", "a14y"):=NULL]

#####
##### Removing of features with too many missing values
#####

#Number of missing values in each feature
number_of_nas <-sapply(names(raw_data), function(i) sum(is.na(raw_data[,i, with= FALSE])))
DT_nas<-data.table(names=names(raw_data), number_of_nas)[order(-number_of_nas),]

#Features with 20% or more of the values missing
threshold_value=round(nrow(raw_data)*0.2)
features_to_remove<-dplyr::filter(DT_nas, number_of_nas>=threshold_value) %>% setDT()
keep_features<-c("b6b", "n2b", "n3", "interview_year")
features_to_remove <- features_to_remove[!names %in% keep_features,]
first_removal_data<-raw_data[ , !names(raw_data) %in% features_to_remove$names, with=FALSE]

####
#### Exploratory analysis and Input reduction part
#####
####
#Variables to eliminate
#part 1
features_to_remove<-c("d1a1x", "h3x", "h4x", "h6x", "h7x", "h3hdx", "b1x", "k5hdJx", "a6b", "a16", "n2e", "n2p", "n7a", "methodologyx", "a5", "a7", "c12", "c22a", "j13", "b8x", "g5bx", "e17", "a18", "l30a", "l30b", "k3hdx", "c3", "c10", "k21", "k8", "k7", "k6", "k4", "k3a", "k3bc",  "k3e", "k3f",  "k3hd", "c6", "g2", "j3", "j6a", "j10", "k2c", "k16", "methodologyx", "d2x", "l10")
first_removal_data<- first_removal_data[,!names(first_removal_data) %in% features_to_remove, with = FALSE]
#part 2
undecided_features<-c("k16", "b2a", "b2b", "b2c", "b2d.", "e11", "b3", "b4", "b6a", "b8", "c30a", "d1a2", "d1a3", "j30a", "j30b", "j30c", "j30e", "j30f","h30", "i1", "i3", "i30", "j2", "g2","k3a", "k3bc","k3e", "k3f", "k3hd", "k30", "j10", "isic", "d30a", "d30b", "idstd", "a17", "a3ax", "g30a", "h7a", "b2d", "a2x")
DT_simp<- first_removal_data[, !names(first_removal_data)%in% undecided_features, with =FALSE]
#part 3
unused_time<-c("a14d", "a14h", "a14m", "a14min", "a15d", "a15h", "a15m", "a15min")
DT_simp<-DT_simp[,!names(DT_simp)%in% unused_time, with= FALSE]

#variables to try and stay
#Rename

setnames(DT_simp, old = c("d3a","d3b","d3c","d2","n3","a0","a3","b1","b5","b6","b6b","b7","c22b","e30","l1","l2","l6","m1a","n2a","n2b"),
         new =  c("national_sales","indirect_exports","direct_export","total_sales_1y","total_sales_3y","industry" ,  "size_of_locality" ,  "legal_status" ,  "year_of_begin" ,  "workers_y0",  "year_of_registration" ,  "top_experience" ,  "website" ,  "degree_of_competition" ,  "workers_1y",  "workers_3y",  "temporal_workers",  "bussines_enviroment " ,  "cost_labor",  "cost_production"), skip_absent = TRUE)
         

#####
#####  Feature engineering
#####

# part 1 
features_to_round=c("top_experience", "national_sales", "direct_export", "indirect_exports", "workers_y0", "workers_1y", "workers_3y", "legal_status")
DT_simp<- DT_simp[,c("top_experience", "national_sales", "direct_export", "indirect_exports", "workers_y0", "workers_1y", "workers_3y", "legal_status"):= 
                    lapply(.SD, function(x) round(as.numeric(x))),.SDcols= c("top_experience", "national_sales", "direct_export", "indirect_exports", "workers_y0", "workers_1y", "workers_3y", "legal_status")]

#part 2 #To Categorical
years_and_char=c("year_of_begin", "year_of_registration", "interview_year","country", "stra_sector", "sector_MS", "sample")
DT_simp<- DT_simp[,c("year_of_begin", "year_of_registration", "interview_year","country", "stra_sector", "sector_MS", "sample"):=
                    lapply(.SD, function(x)as.factor(x)),.SDcols=c("year_of_begin", "year_of_registration", "interview_year","country", "stra_sector", "sector_MS", "sample")]

numeric_to_categorical<-c("legal_status",  "website", "degree_of_competition")
DT_simp <- DT_simp[,c("legal_status",  "website", "degree_of_competition"):=
                     lapply(.SD, function(x) as.factor(x)), .SDcols=c("legal_status",  "website", "degree_of_competition")]

#To numeric
features_to_numeric<-c("workers_3y", "workers_1y", "workers_y0", "temporal_workers", "top_experience", "national_sales", "indirect_exports", "direct_export")
DT_simp <- DT_simp[,c("workers_3y", "workers_1y", "workers_y0", "temporal_workers", "top_experience", "national_sales", "indirect_exports", "direct_export"):=
                     lapply(.SD, function(x) as.numeric(x)), .SDcols=c("workers_3y", "workers_1y", "workers_y0", "temporal_workers", "top_experience", "national_sales", "indirect_exports", "direct_export")]

#Part 3
DT_simp <- DT_simp[!legal_status %in% c("10","7")][!degree_of_competition %in% c("19")]

#part 4
DT_simp$website <- revalue(DT_simp$website, c("1"="Yes", "2"="No"))
DT_simp$degree_of_competition <- revalue(DT_simp$degree_of_competition, c("0"="None", "1"="Minor", "2"="Moderate", "3"="Major", "4"="Very severe"))
DT_simp<-DT_simp[!is.na(DT_simp$interview_year), ]
df_iy<-data.table(interview_year=DT_simp$interview_year)

###### Percentage of NAs in total
number_of_nas<-sapply(names(DT_simp), function(i) sum(is.na(DT_simp[,i, with=FALSE])))
DT_nas<-data.table(names=names(DT_simp), number_of_nas)[order(-number_of_nas),]

DT_nas<-filter(DT_nas, DT_nas$names %in% names(DT_simp))
DT_incomplete<-filter(DT_nas, number_of_nas!=0)
DT_complete<-filter(DT_nas, number_of_nas==0)
DT_incomplete$proportion=paste(round((DT_incomplete$number_of_nas)/nrow(DT_simp)*100,2), "%")

####### Additional Engineered Features

###### Stage of development  

#Define develop level of each country
stage_1<-c("Bangladesh", "Benin", "Burundi", "Cambodia", "Cameroon", "Chad", "Congo", "Ethiopia", "Gambia", "Ghana", "Guinea", "Haiti", "India", "Kenya", "Kyrgyz Republic", "Lao PDR", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Moldova", "Mozambique", "Nepal", "Pakistan", "Rwanda", "Senegal", "Sierra Leone", "Tajikistan", "Tanzania", "Uganda", "Yemen", "Zambia", "Zimbabwe")

transition_12<-c("Algeria", "Azerbaijan", "Bhutan", "Botswana", "Brunei Darussalam", "Honduras", "Kazakhstan", "Kuwait", "Mongolia", "Nicaragua", "Nigera", "Philippines", "Ukraine", "Venezuela", "Vietnam")

stage_2<-c("Albania",  "Armenia", "Bosnia and Herzegovina", "Brazil", "Bulgaria", "CapeVerde", "China", "Colombia", "DominicanRecpublic", "Ecuador", "Egypt", "ElSalvador", "Georgia", "Guatemala", "Indonesia", "Iran, Islamic Rep.", "Jamaica", "Jordan", "Mexico", "Montenegro", "Morocco", "Namibia", "Paraguay", "Peru", "Russia", "Serbia", "SouthAfrica", "SriLanka", "Swaziland", "Thailand", "Tunisia")

transition_23<-c("Argentina", "Chile", "Costarica", "Croatia", "Hungary", "Latvia", "Lebanon", "Lithuania", "Malaysia", "Mauritius", "Oman", "Panama", "Poland", "Romania", "Saudi Arabia", "Seychelles", "Slovak Republic", "TrinidadandTobago", "Turkey", "Uruguay")

stage_3<-c("Australia","Austria", "Bahrain", "Belgium", "Canada", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hong Kong", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea, Rep.","Luxembourg", "Malta", "Netherlands", "New Zealand", "Norway", "Portugal", "Qatar", "Singapore","Slovenia", "Spain", "Sweden", "Switzerland", "China", "United Arab Emirates", "United Kingdom","United States")

#not included: Antiguandbarbuda, Eswatini, Mali, Myanmar, Niger, St Vincent and Grenadina, Dominica, Afghanistan, Belarus, Burundi, Suriname, Bardabos, Bolivia, BurkinaFaso, Central African Republic, Djibouti, Angola, Iraq, Guyana, Gabon, Eritrea, Cote d'Ivore, Belize, Vanuatn, Timor-Leste, StkittsandNeus, Togo, Bahamas, DRC, Grenada, Kosovo, Micronesia, Papua New Guinea, Solomon Islands, Tonga, Uzbekistam, Sudan, South Sudan, SIerra Leone, North Macedonia, Lao PDR, India. 

DT_simp[country %in% stage_1,develop_level:="stage_1"]
DT_simp[country %in% transition_12,develop_level:="transition_12"]
DT_simp[country %in% stage_2,develop_level:="stage_2"]
DT_simp[country %in% transition_23,develop_level:="transition_23"]
DT_simp[country %in% stage_3,develop_level:="stage_3"]
DT_simp[!country %in% c(stage_1,transition_12,stage_2,transition_23,stage_3), develop_level:="not_included"]

##### Additional Cost to sales variable
DT_simp[,Cost_to_sales_1y:=(cost_labor+cost_production)/total_sales_1y]
DT_simp[,Cost_to_sales_3y:=(cost_labor+cost_production)/total_sales_3y]

write.xlsx(DT_simp, "path/Output.xlsx", overwrite=TRUE)
