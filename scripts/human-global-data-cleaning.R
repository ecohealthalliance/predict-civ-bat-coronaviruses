### PREDICT Global Data Cleaning ###
## Author: Shannon Ball ##
## Date: July 19, 2022 ##

### Loading Packages ####

library(tidyverse)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(summarytools)
library(splitstackshape)
library(gtsummary)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tableone)
library(expss)
library(gmodels)
library(binom)
library(geosphere)
library(measurements)
library(haven)
library(psych)
library(ggeasy)
library(labelled)
library(paletteer)
library(here)
library(broom)



### Setting up Directory ####

#Run to create path to top-level directory where current R project is saved
here()
# /Users/shannonball/Dropbox (EHA)/Shannon's Documents/EHA Projects/PREDICT/PREDICT Data Analysis/PREDICT-shannon

### Global Data Loading ####

#Create connection to SQLite database
con <- dbConnect(SQLite(),here("Data", "eidith_db.sqlite"))

# Show list of tables in SQLite database
as.data.frame(dbListTables(con))

# Get tables from database connection
#Human Questionnaires
human_2 <- dbReadTable(con, 'human_2', human_2)
human_ehp_2 <- dbReadTable(con, 'human_ehp_2', human_ehp_2)
human_extractive_industry_2 <- dbReadTable(con, 'human_extractive_industry_2', human_extractive_industry_2)
human_hospital_worker_2 <- dbReadTable(con, 'human_hospital_worker_2', human_hospital_worker_2)
human_restaurant_2 <- dbReadTable(con, 'human_restaurant_2', human_restaurant_2)
human_zoo_2 <- dbReadTable(con, 'human_zoo_2', human_zoo_2)
human_sick_person_2 <- dbReadTable(con, 'human_sick_person_2', human_sick_person_2)
human_animal_production_2 <- dbReadTable(con, 'human_animal_production_2', human_animal_production_2)
human_animal_production_ehp_2 <- dbReadTable(con, 'human_animal_production_ehp_2', human_animal_production_ehp_2)
human_crop_production_2 <- dbReadTable(con, 'human_crop_production_2', human_crop_production_2)
human_temporary_settlements_2 <- dbReadTable(con, 'human_temporary_settlements_2', human_temporary_settlements_2)
human_market_2 <- dbReadTable(con, 'human_market_2', human_market_2)
human_hunter_2 <- dbReadTable(con, 'human_hunter_2', human_hunter_2)
human_hunter_ehp_2 <- dbReadTable(con, 'human_hunter_ehp_2', human_hunter_ehp_2)

#Site and Event Forms
events_2 <- dbReadTable(con, 'events_2', events_2)
dwellings_2 <- dbReadTable(con, 'dwellings_2', dwellings_2)
extractive_industry_2 <- dbReadTable(con, 'extractive_industry_2', extractive_industry_2)
natural_areas_2 <- dbReadTable(con, 'natural_areas_2', natural_areas_2)
wildlife_restaurant_2 <- dbReadTable(con, 'wildlife_restaurant_2', wildlife_restaurant_2)
crop_production_2 <- dbReadTable(con, 'crop_production_2', crop_production_2)
zoo_sanctuary_2 <- dbReadTable(con, 'zoo_sanctuary_2', zoo_sanctuary_2)
market_value_chain_2 <- dbReadTable(con, 'market_value_chain_2', market_value_chain_2)
animal_production_2 <- dbReadTable(con, 'animal_production_2', animal_production_2)

#Other Tables
specimens_2 <- dbReadTable(con, 'specimens_2', specimens_2)
tests_2 <- dbReadTable(con, 'tests_2', tests_2)
test_data_serology_2 <- dbReadTable(con, 'test_data_serology_2', test_data_serology_2)
test_data_interpreted_2 <- dbReadTable(con, 'test_data_interpreted_2', test_data_interpreted_2)
training_2 <- dbReadTable(con, 'training_2', training_2)

#Disconnect from database
dbDisconnect(con)

### Global Human 2 Data Cleaning ####

#Reclassifying and sorting variables
#Get lists of variables and class in order of questionnaire
#as.data.frame(sapply(human_2, class))
#as.data.frame(sapply(events_2, class))
#Dates
human_2$date_of_interview <- as.Date(human_2$date_of_interview)
#Numbers
names_numeric <- c('age', 'live_latitude', 'live_longitude', 'people_in_dwelling', 'children_in_dwelling', 'males_in_dwelling', 'rooms_in_dwelling', 'work_location_latitude',
                   'work_location_longitude', 'latitude_loc_1', 'longitude_loc_1', 'latitude_loc_2', 'longitude_loc_2', 'latitude_loc_3', 'longitude_loc_3', 'latitude_loc_4',
                   'longitude_loc_4', 'latitude_loc_5', 'longitude_loc_5', 'latitude_loc_6', 'longitude_loc_6', 'interview_longitude', 'interview_latitude')
human_2[names_numeric] <-lapply(human_2[names_numeric], as.numeric)

#Length Lived
#table(human_2$length_lived)
human_2$length_lived <- factor(human_2$length_lived, levels = c("<1 month", "1 month - 1 year", ">1 - 5 years", ">5 - 10 years", ">10 years", "missing")) #factoring and sorting
#table(human_2$length_lived)
human_2 = apply_labels(human_2, length_lived = "Length of time living at location") #applying var label

#Gender
#table(human_2$gender)
human_2$gender <- factor(human_2$gender, levels = c("male", "female", "other")) #factoring and sorting
#table(human_2$gender)
human_2 = apply_labels(human_2, gender = "Gender") #applying var label

#Highest Education
#table(human_2$highest_education)
human_2$highest_education <- factor(human_2$highest_education, levels = c("none", "primary school", "secondary school", "college/university/professional", "missing"),
                                    labels = c("None", "Primary school", "Secondary school", "College/university/professional", "Missing")) #factoring and sorting
#table(human_2$highest_education)
human_2 = apply_labels(human_2, highest_education = "Education (highest level completed") #applying var label

#Mother's Highest Education
#table(human_2$highest_education_mother)
human_2$highest_education_mother <- factor(human_2$highest_education_mother, levels = c("none", "primary school", "secondary school", "college/university/professional", "missing"),
                                           labels = c("None", "Primary school", "Secondary school", "College/university/professional", "Missing")) #factoring and sorting
#table(human_2$highest_education_mother)
human_2 = apply_labels(human_2, highest_education_mother = "Mother's Education (highest level completed)") #applying var label
#Creating second variable that groups Secondary school and College/university/professional into Secondary school and higher
human_2$highest_education_mother2 <- ifelse(human_2$highest_education_mother=="College/university/professional","Secondary school and higher",
                                            ifelse(human_2$highest_education_mother=="Secondary school","Secondary school and higher",
                                                   ifelse(human_2$highest_education_mother=="None","None",
                                                          ifelse(human_2$highest_education_mother=="Primary school","Primary school","Missing")))) #factoring and sorting
human_2$highest_education_mother2 <- factor(human_2$highest_education_mother2, levels = c("None", "Primary school", "Secondary school and higher", "Missing"))
#table(human_2$highest_education_mother2)
human_2 = apply_labels(human_2, highest_education_mother2 = "Mother's Education (highest level completed)") #applying var label

#Age
#summary(human_2$age)
#Creating 10-yr categories
human_2$agecat10yr <-ifelse(0 <= human_2$age & human_2$age <10, 0,
                            ifelse(10 <= human_2$age & human_2$age < 20, 1,
                                   ifelse(20 <= human_2$age & human_2$age < 30, 2,
                                          ifelse(30 <= human_2$age & human_2$age < 40, 3,
                                                 ifelse(40 <= human_2$age & human_2$age < 50, 4,
                                                        ifelse(50 <= human_2$age & human_2$age < 60, 5,
                                                               ifelse(60 <= human_2$age & human_2$age < 70, 6,
                                                                      ifelse(70 <= human_2$age & human_2$age < 80, 7,
                                                                             ifelse(80 <= human_2$age & human_2$age < 90, 8,
                                                                                    ifelse(90 <= human_2$age & human_2$age < 100, 9,
                                                                                           ifelse(human_2$age >=100, 10, NA))))))))))) #creating categories
human_2$agecat10yr <- factor(human_2$agecat10yr, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                             labels = c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')) #factoring, sorting, and labeling
human_2 = apply_labels(human_2, agecat10yr = "Age") #applying var label
#freq(human_2$agecat10yr)

#Creating 20-yr categories
human_2$agecat20yr <- ifelse(0 <= human_2$age & human_2$age < 20, 1,
                             ifelse(20 <= human_2$age & human_2$age < 40, 2,
                                    ifelse(40 <= human_2$age & human_2$age < 60, 3,
                                           ifelse(60 <= human_2$age & human_2$age < 80, 4,
                                                  ifelse(human_2$age >=80, 5, NA))))) #creating categories
human_2$agecat20yr <- factor(human_2$agecat20yr, levels = c(1, 2, 3, 4, 5),
                             labels = c('0-19','20-39','40-59','60-79','80+')) #factoring, sorting, and labeling
human_2 = apply_labels(human_2, agecat20yr = "Age") #applying var label
#freq(human_2$agecat20yr)

#Creating 4 categories (0-19, 20-39, 40-59, 60+)
human_2$agecat4 <-  ifelse(0 <= human_2$age & human_2$age < 20, 1,
                           ifelse(20 <= human_2$age & human_2$age < 40, 2,
                                  ifelse(40 <= human_2$age & human_2$age < 60, 3,
                                         ifelse(human_2$age >=60, 4, NA)))) #creating categories
human_2$agecat4 <- factor(human_2$agecat4, levels = c(1, 2, 3, 4),
                          labels = c('0-19','20-39','40-59','60+')) #factoring, sorting, and labeling
human_2 = apply_labels(human_2, agecat4 = "Age") #applying var label
#freq(human_2$agecat4)

#Creating 10-yr categories but separating <12 (<12, 12-19, 20-29, 30-39, 40-49, 50-59, 60+)
human_2$agecat12 <-ifelse(0 <= human_2$age & human_2$age <12, 0,
                          ifelse(12 <= human_2$age & human_2$age < 20, 1,
                                 ifelse(20 <= human_2$age & human_2$age < 30, 2,
                                        ifelse(30 <= human_2$age & human_2$age < 40, 3,
                                               ifelse(40 <= human_2$age & human_2$age < 50, 4,
                                                      ifelse(50 <= human_2$age & human_2$age < 60, 5,
                                                             ifelse(60 <= human_2$age, 6, NA))))))) #creating categories
human_2$agecat12 <- factor(human_2$agecat12, levels = c(0, 1, 2, 3, 4, 5, 6),
                           labels = c('<12','12-19','20-29','30-39','40-49','50-59','60+')) #factoring, sorting, and labeling
human_2 = apply_labels(human_2, agecat12 = "Age") #applying var label
#freq(human_2$agecat12)

#Creating 5 categories (0-19, 20-29, 30-39, 40-49, 50-59, 60+)
human_2$agecat5 <-ifelse(0 <= human_2$age & human_2$age < 20, 0,
                         ifelse(20 <= human_2$age & human_2$age < 30, 1,
                                ifelse(30 <= human_2$age & human_2$age < 40, 2,
                                       ifelse(40 <= human_2$age & human_2$age < 50, 3,
                                              ifelse(50 <= human_2$age & human_2$age < 60, 4,
                                                     ifelse(60 <= human_2$age, 5, NA)))))) #creating categories
human_2$agecat5 <- factor(human_2$agecat5, levels = c(0, 1, 2, 3, 4, 5),
                          labels = c('0-19','20-29','30-39','40-49','50-59','60+')) #factoring, sorting, and labeling
human_2 = apply_labels(human_2, agecat5 = "Age") #applying var label
#freq(human_2$agecat5)

#Creating 6 categories (<10, 10-19, 20-29, 30-39, 40-49, 50-59, 60+)
human_2$agecat6 <-ifelse(0 <= human_2$age & human_2$age <10, 0,
                         ifelse(10 <= human_2$age & human_2$age < 20, 1,
                                ifelse(20 <= human_2$age & human_2$age < 30, 2,
                                       ifelse(30 <= human_2$age & human_2$age < 40, 3,
                                              ifelse(40 <= human_2$age & human_2$age < 50, 4,
                                                     ifelse(50 <= human_2$age & human_2$age < 60, 5,
                                                            ifelse(60 <= human_2$age, 6, NA))))))) #creating categories
human_2$agecat6 <- factor(human_2$agecat6, levels = c(0, 1, 2, 3, 4, 5, 6),
                          labels = c('<10','1-19','20-29','30-39','40-49','50-59','60+')) #factoring, sorting, and labeling
human_2 = apply_labels(human_2, agecat6 = "Age") #applying var label
#freq(human_2$agecat6)

#Water Source
#table(human_2$drinking_water_source)
#sum(human_2$drinking_water_source=="MISSING") #checking number of missing
#Creating binary variables for water source types using grepl to search for matching strings and coding missings as NA
human_2$drinking_water_piped <- ifelse(human_2$drinking_water_source=="MISSING",NA,
                                       ifelse(grepl("piped in water/water taps", human_2$drinking_water_source),1,0))
human_2$drinking_water_covered <- ifelse(human_2$drinking_water_source=="MISSING",NA,
                                         ifelse(grepl("covered well", human_2$drinking_water_source),1,0))
human_2$drinking_water_uncovered <- ifelse(human_2$drinking_water_source=="MISSING",NA,
                                           ifelse(grepl("uncovered well/pond/river", human_2$drinking_water_source),1,0))
human_2$drinking_water_truck_rainwater <- ifelse(human_2$drinking_water_source=="MISSING",NA,
                                                 ifelse(grepl("water truck/rainwater harvest", human_2$drinking_water_source),1,0))
human_2$drinking_water_other <- ifelse(human_2$drinking_water_source=="MISSING",NA,
                                       ifelse(grepl("other", human_2$drinking_water_source),1,0))
watersource <- c('drinking_water_piped', 'drinking_water_covered', 'drinking_water_uncovered', 'drinking_water_truck_rainwater', 'drinking_water_other') #concatenating binary water source vars into vector
human_2[watersource] <- lapply(human_2[watersource], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all water source vars using concatenated vector
#freq(human_2[c('drinking_water_piped', 'drinking_water_covered', 'drinking_water_uncovered', 'drinking_water_truck_rainwater', 'drinking_water_other')], report.nas = FALSE)

#Water Treated
#freq(human_2$water_treated)
human_2$water_treated <- ifelse(human_2$water_treated=="yes",1,
                                ifelse(human_2$water_treated=="no",0,NA)) #re coding binary string as integers
human_2$water_treated <- factor(human_2$water_treated, levels = c(0,1), labels = c("No", "Yes")) #factoring, sorting, labeling
human_2 = apply_labels(human_2, water_treated = "Drinking Water Treated") #applying var label
#freq(human_2$water_treated)

#Water Treatment
#freq(human_2$water_treatment)
#sum(human_2$water_treatment=="N/A") #checking number of N/As
human_2$water_treatment <- ifelse(human_2$water_treatment=="N/A",NA,human_2$water_treatment) #re coding "N/A" as NA
#sum(is.na(human_2$water_treatment)) #checking that NAs successfully re coded
#Creating binary variables for water treatment types using grepl to search for matching strings
human_2$water_treatment_boil <- ifelse(grepl("boil", human_2$water_treatment),1,0)
human_2$water_treatment_filter <- ifelse(grepl("filter", human_2$water_treatment),1,0)
human_2$water_treatment_chlorine <- ifelse(grepl("add chlorine or bleach", human_2$water_treatment),1,0)
human_2$water_treatment_solar <- ifelse(grepl("solar disinfection", human_2$water_treatment),1,0)
human_2$water_treatment_other <- ifelse(grepl("other", human_2$water_treatment),1,0)
watertx <- c('water_treatment_boil', 'water_treatment_filter', 'water_treatment_chlorine', 'water_treatment_solar', 'water_treatment_other') #concatenating binary water tx vars into vector
human_2[watertx] <- lapply(human_2[watertx], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all water tx vars using concatenated vector
#freq(human_2[c('water_treatment_boil', 'water_treatment_filter', 'water_treatment_chlorine', 'water_treatment_solar', 'water_treatment_other')], report.nas = FALSE)

#Food Storage
#table(human_2$food_storage_containers)
#sum(human_2$food_storage_containers=="MISSING") #checking number of missing
#Creating binary variables for food storage and coding missings as NA
human_2$food_storage_covered <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                       ifelse(grepl("yes, with covers", human_2$food_storage_containers),1,0)) #using grepl to identify if food storage containers are ever covered
human_2$food_storage_uncovered <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                         ifelse(grepl("yes, without covers", human_2$food_storage_containers),1,0)) #using grepl to identify if food storage containers are ever uncovered
human_2$food_storage_no_containers <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                             ifelse(grepl("no", human_2$food_storage_containers),1,0)) #using grepl to identify if food is ever not stored in containers
human_2$food_storage_always_covered <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                              ifelse(human_2$food_storage_containers=="yes, with covers",1,0)) #always use covered containers
human_2$food_storage_always_uncovered <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                                ifelse(human_2$food_storage_containers=="yes, without covers",1,0)) #always use uncovered containers
human_2$food_storage_always_no_containers <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                                    ifelse(human_2$food_storage_containers=="no",1,0)) #never use containers
human_2$food_storage_covered_and_uncovered <- ifelse(human_2$food_storage_containers=="MISSING",NA,
                                                     ifelse(grepl("yes", human_2$food_storage_containers),1,0)) #using grepl to identify is food is stored in covered and/or uncovered containers
foodstorage <- c('food_storage_covered', 'food_storage_uncovered', 'food_storage_no_containers', 'food_storage_always_covered', 'food_storage_always_uncovered', 'food_storage_covered_and_uncovered') #concatenating food storage vars into vector
human_2[foodstorage] <- lapply(human_2[foodstorage], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all food storage vars using concatenated vector
#freq(human_2[c('food_storage_covered', 'food_storage_uncovered', 'food_storage_no_containers', 'food_storage_always_covered', 'food_storage_always_uncovered', 'food_storage_covered_and_uncovered')])

#Waste
human_2$dedicated_location_for_waste <- ifelse(human_2$dedicated_location_for_waste=="no", 0,
                                               ifelse(human_2$dedicated_location_for_waste=="yes", 1,NA)) #re coding binary string as integers
human_2$dedicated_location_for_waste <- factor(human_2$dedicated_location_for_waste, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, labeling

#Permanent Structure
human_2$dwelling_permanent_structure <- ifelse(human_2$dwelling_permanent_structure=="no", 0,
                                               ifelse(human_2$dwelling_permanent_structure=="yes", 1,NA)) #re coding binary string as integers
human_2$dwelling_permanent_structure <- factor(human_2$dwelling_permanent_structure, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, labeling

#Worried Disease
human_2$worried_about_disease <- ifelse(human_2$worried_about_disease=="no", 0,
                                        ifelse(human_2$worried_about_disease=="yes", 1,NA)) #re coding binary string as integers
human_2$worried_about_disease <- factor(human_2$worried_about_disease, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, labeling

#Crowding
human_2$crowding_index <- human_2$people_in_dwelling / human_2$rooms_in_dwelling #calculating crowding index
human_2$crowding_index <- ifelse(human_2$crowding_index==Inf, NA, human_2$crowding_index) #re coding infinite values to NA
#summary(human_2$crowding_index)
human_2$crowding_index_cat <- ifelse(human_2$crowding_index < 1, "<1",
                                     ifelse(human_2$crowding_index >=1 & human_2$crowding_index <= 2, "1-2",
                                            ifelse(human_2$crowding_index >2, ">2", NA))) #creating categories
human_2$crowding_index_cat <- factor(human_2$crowding_index_cat, levels = c("<1", "1-2", ">2")) #factoring, sorting, labeling
#freq(human_2$crowding_index_cat)

#Treatment
#freq(human_2$treatment)
#Creating binary variables for treatment types
human_2$tx_clinic <- ifelse(grepl("clinic", human_2$treatment),1,0)
human_2$tx_chw <- ifelse(grepl("community", human_2$treatment),1,0)
human_2$tx_hospital <- ifelse(grepl("hospital", human_2$treatment),1,0)
human_2$tx_traditional <- ifelse(grepl("traditional", human_2$treatment),1,0)
human_2$tx_mobile <- ifelse(grepl("mobile", human_2$treatment),1,0)
human_2$tx_pharmacy <- ifelse(grepl("pharmacy", human_2$treatment),1,0)
txtypes <- c('tx_clinic', 'tx_chw', 'tx_hospital', 'tx_traditional', 'tx_mobile', 'tx_pharmacy') #concatenating treatment variables into vector
human_2[txtypes] <- lapply(human_2[txtypes], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all vars using concatenated vector
#freq(human_2[c('tx_clinic', 'tx_chw', 'tx_hospital', 'tx_traditional', 'tx_mobile', 'tx_pharmacy')], report.nas = FALSE)

#Scratched Bitten Action
#freq(human_2$scratched_bitten_action)
#Creating binary variables for action taken after scratched/bitten/cut
human_2$scratched_takeover <- ifelse(grepl("take over", human_2$scratched_bitten_action),1,0)
human_2$scratched_doctor <- ifelse(grepl("doctor", human_2$scratched_bitten_action),1,0)
human_2$scratched_wash <- ifelse(grepl("wash", human_2$scratched_bitten_action),1,0)
human_2$scratched_rinse <- ifelse(grepl("rinse", human_2$scratched_bitten_action),1,0)
human_2$scratched_nothing <- ifelse(grepl("nothing", human_2$scratched_bitten_action),1,0)
human_2$scratched_neverbutcher <- ifelse(grepl("never butcher", human_2$scratched_bitten_action),1,0)
human_2$scratched_bandage <- ifelse(grepl("bandage", human_2$scratched_bitten_action),1,0)
scratchedaction <- c('scratched_takeover', 'scratched_doctor', 'scratched_wash', 'scratched_rinse', 'scratched_nothing', 'scratched_neverbutcher', 'scratched_bandage') #concatenating scratched/bitten vars into vector
human_2[scratchedaction] <- lapply(human_2[scratchedaction], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all vars using concatenated vector
#freq(human_2[c('scratched_takeover', 'scratched_doctor', 'scratched_wash', 'scratched_rinse', 'scratched_nothing', 'scratched_neverbutcher', 'scratched_bandage')], report.nas = FALSE)

#Risk Associated with Slaughtering/Butchering with Open Wound
#freq(human_2$risk_open_wound)
#Creating binary variables for perceived risks associated with slaughtering/butchering with open wound
human_2$woundrisk_no <- ifelse(grepl("no", human_2$risk_open_wound),1,0)
human_2$woundrisk_yes_dk <- ifelse(grepl("what they are", human_2$risk_open_wound),1,0)
human_2$woundrisk_yes_sick <- ifelse(grepl("sick", human_2$risk_open_wound),1,0)
human_2$woundrisk_yes_poison <- ifelse(grepl("poison", human_2$risk_open_wound),1,0)
human_2$woundrisk_yes_infect <- ifelse(grepl("infect", human_2$risk_open_wound),1,0)
human_2$woundrisk_dk <- ifelse(grepl("don't  know",human_2$risk_open_wound),1,0) #note that there's an extra space in between don't and know
human_2$woundrisk_other <- ifelse(grepl("other", human_2$risk_open_wound),1,0)
woundrisk <- c('woundrisk_no', 'woundrisk_yes_dk', 'woundrisk_yes_sick', 'woundrisk_yes_poison', 'woundrisk_yes_infect', 'woundrisk_dk', 'woundrisk_other') #concatenating woundrisk vars into vector
human_2[woundrisk] <- lapply(human_2[woundrisk], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all vars using concatenated vector
#freq(human_2[c('woundrisk_no', 'woundrisk_yes_dk', 'woundrisk_yes_sick', 'woundrisk_yes_poison', 'woundrisk_yes_infect', 'woundrisk_dk', 'woundrisk_other')], report.nas = FALSE)


#Sickness Cause
#freq(human_2$sickness_cause)
#Creating binary variables for perceived cause of sickness
human_2$sickcause_sickppl <- ifelse(grepl("sick", human_2$sickness_cause),1,0)
human_2$sickcause_wildanimals <- ifelse(grepl("wild animals", human_2$sickness_cause),1,0)
human_2$sickcause_otheranimals <- ifelse(grepl("other animals", human_2$sickness_cause),1,0)
human_2$sickcause_foodwater <- ifelse(grepl("bad food", human_2$sickness_cause),1,0)
human_2$sickcause_witchcraft <- ifelse(grepl("witchcraft", human_2$sickness_cause),1,0)
human_2$sickcause_wound <- ifelse(grepl("wound", human_2$sickness_cause),1,0)
human_2$sickcause_dk <- ifelse(grepl("don't know", human_2$sickness_cause),1,0)
human_2$sickcause_other <- ifelse(grepl("other", human_2$sickness_cause),1,0)
sickcause <- c('sickcause_sickppl', 'sickcause_wildanimals', 'sickcause_otheranimals', 'sickcause_foodwater', 'sickcause_witchcraft', 'sickcause_wound', 'sickcause_dk', 'sickcause_other') #concatenating sickness cause vars into vector
human_2[sickcause] <- lapply(human_2[sickcause], factor, levels=c(0,1), labels=c("No", "Yes")) #factoring, sorting, and labeling all vars using concatenated vector
#freq(human_2[c('sickcause_sickppl', 'sickcause_wildanimals', 'sickcause_otheranimals', 'sickcause_foodwater', 'sickcause_witchcraft', 'sickcause_wound', 'sickcause_dk', 'sickcause_other')], report.nas = FALSE)


# Symptoms
#Creating binary vars for symptoms in the last year using grepl to find matching strings
human_2$sx_encephalitis <- ifelse(grepl("encephalitis", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_encephalitis)
human_2$sx_hemorrhagic <- ifelse(grepl("hemorrhagic", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_hemorrhagic)
human_2$sx_sari <- ifelse(grepl("SARI", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_sari)
human_2$sx_ili <- ifelse(grepl("(ILI)", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_ili)
human_2$sx_fever_gi <- ifelse(grepl("fever with diarrhea or vomiting", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_fever_gi)
human_2$sx_fever_rash <- ifelse(grepl("fever with rash", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_fever_rash)
human_2$sx_rash_sores <- ifelse(grepl("persistent rash", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_rash_sores)
human_2$sx_other <- ifelse(grepl("other", human_2$symptoms_in_last_year),1,0)
#freq(human_2$sx_other)

#Factoring remaining vars
names_factor <- c('season', 'dwelling_permanent_structure', 'water_used_by_animals', 'dedicated_location_for_waste', 'primary_livelihood', 'job_position', 'had_symptoms_in_last_year',
                  'had_symptoms_in_last_year_other_people', 'illness_death', 'worried_about_disease', 'travelled', 'more_than_6') #concatenating var names into vector
human_2[names_factor] <-lapply(human_2[names_factor], factor) #factoring all vars using concatenated vector

## Livelihoods ####
#table(human_2$livelihoods)
#sum(human_2$livelihoods=="MISSING") #checking number of missing
#as.data.frame(table(human_2$livelihoods)) #get df of livelihood response strings
#Creating binary livelihood vars using grepl to identify matching strings
human_2$livelihoods_extraction <- ifelse(human_2$livelihoods=="MISSING",NA,
                                         ifelse(grepl("extraction of minerals", human_2$livelihoods),1,0))
human_2$livelihoods_crops <- ifelse(human_2$livelihoods=="MISSING",NA,
                                    ifelse(grepl("crop production", human_2$livelihoods),1,0))
human_2$livelihoods_wildlife_rest <- ifelse(human_2$livelihoods=="MISSING",NA,
                                            ifelse(grepl("wildlife restaurant", human_2$livelihoods),1,0))
human_2$livelihoods_animal_trade <- ifelse(human_2$livelihoods=="MISSING",NA,
                                           ifelse(grepl("wild/exotic animal trade", human_2$livelihoods),1,0))
human_2$livelihoods_animal_production <- ifelse(human_2$livelihoods=="MISSING",NA,
                                                ifelse(grepl("rancher/farmer animal", human_2$livelihoods),1,0))
human_2$livelihoods_meat_processing <- ifelse(human_2$livelihoods=="MISSING",NA,
                                              ifelse(grepl("meat processing, slaughterhouse", human_2$livelihoods),1,0))
human_2$livelihoods_zoo <- ifelse(human_2$livelihoods=="MISSING",NA,
                                  ifelse(grepl("zoo/sanctuary", human_2$livelihoods),1,0))
human_2$livelihoods_protected_area <- ifelse(human_2$livelihoods=="MISSING",NA,
                                             ifelse(grepl("protected area", human_2$livelihoods),1,0))
human_2$livelihoods_hunter <- ifelse(human_2$livelihoods=="MISSING",NA,
                                     ifelse(grepl("hunter/trapper", human_2$livelihoods),1,0))
human_2$livelihoods_forager <- ifelse(human_2$livelihoods=="MISSING",NA,
                                      ifelse(grepl("forager/gatherer", human_2$livelihoods),1,0))
human_2$livelihoods_migrant_laborer <- ifelse(human_2$livelihoods=="MISSING",NA,
                                              ifelse(grepl("migrant laborer", human_2$livelihoods),1,0))
human_2$livelihoods_hcw <- ifelse(human_2$livelihoods=="MISSING",NA,
                                  ifelse(grepl("nurse, doctor", human_2$livelihoods),1,0))
human_2$livelihoods_construction <- ifelse(human_2$livelihoods=="MISSING",NA,
                                           ifelse(grepl("construction", human_2$livelihoods),1,0))
human_2$livelihoods_homemaker <- ifelse(human_2$livelihoods=="MISSING",NA,
                                        ifelse(grepl("homemaker", human_2$livelihoods),1,0))
human_2$livelihoods_child <- ifelse(human_2$livelihoods=="MISSING",NA,
                                    ifelse(grepl("child", human_2$livelihoods),1,0))
human_2$livelihoods_livestock <- ifelse(human_2$livelihoods=="MISSING",NA,
                                        ifelse(grepl("livestock/domestic animal/product trade", human_2$livelihoods),1,0))
human_2$livelihoods_non_animal <- ifelse(human_2$livelihoods=="MISSING",NA,
                                         ifelse(grepl("non-animal business", human_2$livelihoods),1,0))
human_2$livelihoods_student <- ifelse(human_2$livelihoods=="MISSING",NA,
                                      ifelse(grepl("student", human_2$livelihoods),1,0))
human_2$livelihoods_unemployed <- ifelse(human_2$livelihoods=="MISSING",NA,
                                         ifelse(grepl("unemployed", human_2$livelihoods),1,0))
human_2$livelihoods_other <- ifelse(human_2$livelihoods=="MISSING",NA,
                                    ifelse(grepl("other", human_2$livelihoods),1,0))
#freq(human_2[c('livelihoods_extraction', 'livelihoods_crops', 'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production','livelihoods_meat_processing', 'livelihoods_zoo', 'livelihoods_protected_area', 'livelihoods_hunter', 'livelihoods_forager', 'livelihoods_migrant_laborer','livelihoods_hcw', 'livelihoods_construction', 'livelihoods_homemaker', 'livelihoods_child', 'livelihoods_livestock', 'livelihoods_non_animal','livelihoods_student', 'livelihoods_unemployed', 'livelihoods_other')], report.nas = FALSE, cumul = FALSE)
human_2[c('livelihoods_extraction', 'livelihoods_crops', 'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production',
          'livelihoods_meat_processing', 'livelihoods_zoo', 'livelihoods_protected_area', 'livelihoods_hunter', 'livelihoods_forager', 'livelihoods_migrant_laborer',
          'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_homemaker', 'livelihoods_child', 'livelihoods_livestock', 'livelihoods_non_animal',
          'livelihoods_student', 'livelihoods_unemployed', 'livelihoods_other')] <- lapply(human_2[c('livelihoods_extraction', 'livelihoods_crops', 'livelihoods_wildlife_rest',
                                                                                                     'livelihoods_animal_trade', 'livelihoods_animal_production', 'livelihoods_meat_processing',
                                                                                                     'livelihoods_zoo', 'livelihoods_protected_area', 'livelihoods_hunter', 'livelihoods_forager',
                                                                                                     'livelihoods_migrant_laborer', 'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_homemaker',
                                                                                                     'livelihoods_child', 'livelihoods_livestock', 'livelihoods_non_animal', 'livelihoods_student',
                                                                                                     'livelihoods_unemployed', 'livelihoods_other')],
                                                                                           factor,
                                                                                           levels=c(0, 1),
                                                                                           labels = c("No", "Yes")) #factoring, sorting, and labeling livelihood vars
human_2 = apply_labels(human_2, livelihoods_extraction = "Extraction of minerals, gas, oil, timber",
                       livelihoods_crops = "Crop production",
                       livelihoods_wildlife_rest = "Wildlife restaurant business",
                       livelihoods_animal_trade = "Wild/exotic animal trade/market business",
                       livelihoods_animal_production = "Rancher/farmer animal production business",
                       livelihoods_meat_processing = "Meat processing, slaughterhouse, abattoir",
                       livelihoods_zoo = "Zoo/sanctuary animal healthcare",
                       livelihoods_protected_area = "Protected area worker",
                       livelihoods_hunter = "Hunter/trapper/fisher",
                       livelihoods_forager = "Forager/gatherer/non-timber forest product collector",
                       livelihoods_migrant_laborer = "Migrant laborer",
                       livelihoods_hcw = "Nurse, doctor, traditional healer, community health worker",
                       livelihoods_construction = "Construction",
                       livelihoods_homemaker = "Homemaker",
                       livelihoods_child = "Child",
                       livelihoods_livestock = "Livestock/domestic animal/product trade",
                       livelihoods_non_animal = "Non-animal business",
                       livelihoods_student = "Student",
                       livelihoods_unemployed = "Unemployed",
                       livelihoods_other = "Other") #applying var labels

## Animal Contact ####
#Bat Contact
#table(human_2$bats_contact)
#sum(human_2$bats_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$bats_contact_pet <- ifelse(grepl("pet", human_2$bats_contact),1,0)
human_2$bats_contact_handled <- ifelse(grepl("handled", human_2$bats_contact),1,0)
human_2$bats_contact_raised <- ifelse(grepl("raised", human_2$bats_contact),1,0)
human_2$bats_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$bats_contact),1,0)
human_2$bats_contact_house <- ifelse(grepl("in house", human_2$bats_contact),1,0)
human_2$bats_contact_cooked <- ifelse(grepl("cooked/handled", human_2$bats_contact),1,0)
human_2$bats_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$bats_contact),1,0)
human_2$bats_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$bats_contact),1,0)
human_2$bats_contact_found_dead <- ifelse(grepl("found dead", human_2$bats_contact),1,0)
human_2$bats_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$bats_contact),1,0)
human_2$bats_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$bats_contact),1,0)
human_2$bats_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$bats_contact),1,0)
#freq(human_2[c('bats_contact_pet', 'bats_contact_handled', 'bats_contact_raised', 'bats_contact_feces_food', 'bats_contact_house', 'bats_contact_cooked', 'bats_contact_eaten_raw','bats_contact_eaten_sick', 'bats_contact_found_dead', 'bats_contact_scratched_bitten', 'bats_contact_hunted', 'bats_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$bats_contact_dummy <- ifelse(grepl("none", human_2$bats_contact),0,1) #creating binary var for any contact
#freq(human_2$bats_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Rodent Contact
#table(human_2$rodents_contact)
#sum(human_2$rodents_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$rodents_contact_pet <- ifelse(grepl("pet", human_2$rodents_contact),1,0)
human_2$rodents_contact_handled <- ifelse(grepl("handled", human_2$rodents_contact),1,0)
human_2$rodents_contact_raised <- ifelse(grepl("raised", human_2$rodents_contact),1,0)
human_2$rodents_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$rodents_contact),1,0)
human_2$rodents_contact_house <- ifelse(grepl("in house", human_2$rodents_contact),1,0)
human_2$rodents_contact_cooked <- ifelse(grepl("cooked/handled", human_2$rodents_contact),1,0)
human_2$rodents_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$rodents_contact),1,0)
human_2$rodents_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$rodents_contact),1,0)
human_2$rodents_contact_found_dead <- ifelse(grepl("found dead", human_2$rodents_contact),1,0)
human_2$rodents_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$rodents_contact),1,0)
human_2$rodents_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$rodents_contact),1,0)
human_2$rodents_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$rodents_contact),1,0)
#freq(human_2[c('rodents_contact_pet', 'rodents_contact_handled', 'rodents_contact_raised', 'rodents_contact_feces_food', 'rodents_contact_house', 'rodents_contact_cooked', 'rodents_contact_eaten_raw', 'rodents_contact_eaten_sick', 'rodents_contact_found_dead', 'rodents_contact_scratched_bitten', 'rodents_contact_hunted', 'rodents_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$rodents_contact_dummy <- ifelse(grepl("none", human_2$rodents_contact),0,1) #creating binary var for any contact
#freq(human_2$rodents_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Non-Human Primate Contact
#table(human_2$nhp_contact)
#sum(human_2$nhp_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$nhp_contact_pet <- ifelse(grepl("pet", human_2$nhp_contact),1,0)
human_2$nhp_contact_handled <- ifelse(grepl("handled", human_2$nhp_contact),1,0)
human_2$nhp_contact_raised <- ifelse(grepl("raised", human_2$nhp_contact),1,0)
human_2$nhp_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$nhp_contact),1,0)
human_2$nhp_contact_house <- ifelse(grepl("in house", human_2$nhp_contact),1,0)
human_2$nhp_contact_cooked <- ifelse(grepl("cooked/handled", human_2$nhp_contact),1,0)
human_2$nhp_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$nhp_contact),1,0)
human_2$nhp_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$nhp_contact),1,0)
human_2$nhp_contact_found_dead <- ifelse(grepl("found dead", human_2$nhp_contact),1,0)
human_2$nhp_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$nhp_contact),1,0)
human_2$nhp_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$nhp_contact),1,0)
human_2$nhp_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$nhp_contact),1,0)
#freq(human_2[c('nhp_contact_pet', 'nhp_contact_handled', 'nhp_contact_raised', 'nhp_contact_feces_food', 'nhp_contact_house', 'nhp_contact_cooked', 'nhp_contact_eaten_raw','nhp_contact_eaten_sick', 'nhp_contact_found_dead', 'nhp_contact_scratched_bitten', 'nhp_contact_hunted', 'nhp_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$nhp_contact_dummy <- ifelse(grepl("none", human_2$nhp_contact),0,1) #creating binary var for any contact
#freq(human_2$nhp_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Bird Contact
#table(human_2$birds_contact)
#sum(human_2$birds_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$birds_contact_pet <- ifelse(grepl("pet", human_2$birds_contact),1,0)
human_2$birds_contact_handled <- ifelse(grepl("handled", human_2$birds_contact),1,0)
human_2$birds_contact_raised <- ifelse(grepl("raised", human_2$birds_contact),1,0)
human_2$birds_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$birds_contact),1,0)
human_2$birds_contact_house <- ifelse(grepl("in house", human_2$birds_contact),1,0)
human_2$birds_contact_cooked <- ifelse(grepl("cooked/handled", human_2$birds_contact),1,0)
human_2$birds_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$birds_contact),1,0)
human_2$birds_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$birds_contact),1,0)
human_2$birds_contact_found_dead <- ifelse(grepl("found dead", human_2$birds_contact),1,0)
human_2$birds_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$birds_contact),1,0)
human_2$birds_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$birds_contact),1,0)
human_2$birds_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$birds_contact),1,0)
#freq(human_2[c('birds_contact_pet', 'birds_contact_handled', 'birds_contact_raised', 'birds_contact_feces_food', 'birds_contact_house', 'birds_contact_cooked', 'birds_contact_eaten_raw','birds_contact_eaten_sick', 'birds_contact_found_dead', 'birds_contact_scratched_bitten', 'birds_contact_hunted', 'birds_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$birds_contact_dummy <- ifelse(grepl("none", human_2$birds_contact),0,1) #creating binary var for any contact
#freq(human_2$birds_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Carnivore Contact
#table(human_2$carnivores_contact)
#sum(human_2$carnivores_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$carnivores_contact_pet <- ifelse(grepl("pet", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_handled <- ifelse(grepl("handled", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_raised <- ifelse(grepl("raised", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_house <- ifelse(grepl("in house", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_cooked <- ifelse(grepl("cooked/handled", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_found_dead <- ifelse(grepl("found dead", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$carnivores_contact),1,0)
human_2$carnivores_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$carnivores_contact),1,0)
#freq(human_2[c('carnivores_contact_pet', 'carnivores_contact_handled', 'carnivores_contact_raised', 'carnivores_contact_feces_food', 'carnivores_contact_house', 'carnivores_contact_cooked', 'carnivores_contact_eaten_raw','carnivores_contact_eaten_sick', 'carnivores_contact_found_dead', 'carnivores_contact_scratched_bitten', 'carnivores_contact_hunted', 'carnivores_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$carnivores_contact_dummy <- ifelse(grepl("none", human_2$carnivores_contact),0,1) #creating binary var for any contact
#freq(human_2$carnivores_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Ungulate Contact
#table(human_2$ungulates_contact)
#sum(human_2$ungulates_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$ungulates_contact_pet <- ifelse(grepl("pet", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_handled <- ifelse(grepl("handled", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_raised <- ifelse(grepl("raised", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_house <- ifelse(grepl("in house", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_cooked <- ifelse(grepl("cooked/handled", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_found_dead <- ifelse(grepl("found dead", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$ungulates_contact),1,0)
human_2$ungulates_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$ungulates_contact),1,0)
#freq(human_2[c('ungulates_contact_pet', 'ungulates_contact_handled', 'ungulates_contact_raised', 'ungulates_contact_feces_food', 'ungulates_contact_house', 'ungulates_contact_cooked', 'ungulates_contact_eaten_raw', 'ungulates_contact_eaten_sick', 'ungulates_contact_found_dead', 'ungulates_contact_scratched_bitten', 'ungulates_contact_hunted', 'ungulates_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$ungulates_contact_dummy <- ifelse(grepl("none", human_2$ungulates_contact),0,1) #creating binary var for any contact
#freq(human_2$ungulates_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Pangolins Contact
#table(human_2$pangolins_contact)
#sum(human_2$pangolins_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$pangolins_contact_pet <- ifelse(grepl("pet", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_handled <- ifelse(grepl("handled", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_raised <- ifelse(grepl("raised", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_house <- ifelse(grepl("in house", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_cooked <- ifelse(grepl("cooked/handled", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_found_dead <- ifelse(grepl("found dead", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$pangolins_contact),1,0)
human_2$pangolins_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$pangolins_contact),1,0)
#freq(human_2[c('pangolins_contact_pet', 'pangolins_contact_handled', 'pangolins_contact_raised', 'pangolins_contact_feces_food', 'pangolins_contact_house', 'pangolins_contact_cooked', 'pangolins_contact_eaten_raw','pangolins_contact_eaten_sick', 'pangolins_contact_found_dead', 'pangolins_contact_scratched_bitten', 'pangolins_contact_hunted', 'pangolins_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$pangolins_contact_dummy <- ifelse(grepl("none", human_2$pangolins_contact),0,1) #creating binary var for any contact
#freq(human_2$pangolins_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Poultry Contact
#table(human_2$poultry_contact)
#sum(human_2$poultry_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$poultry_contact_pet <- ifelse(grepl("pet", human_2$poultry_contact),1,0)
human_2$poultry_contact_handled <- ifelse(grepl("handled", human_2$poultry_contact),1,0)
human_2$poultry_contact_raised <- ifelse(grepl("raised", human_2$poultry_contact),1,0)
human_2$poultry_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$poultry_contact),1,0)
human_2$poultry_contact_house <- ifelse(grepl("in house", human_2$poultry_contact),1,0)
human_2$poultry_contact_cooked <- ifelse(grepl("cooked/handled", human_2$poultry_contact),1,0)
human_2$poultry_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$poultry_contact),1,0)
human_2$poultry_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$poultry_contact),1,0)
human_2$poultry_contact_found_dead <- ifelse(grepl("found dead", human_2$poultry_contact),1,0)
human_2$poultry_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$poultry_contact),1,0)
human_2$poultry_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$poultry_contact),1,0)
human_2$poultry_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$poultry_contact),1,0)
#freq(human_2[c('poultry_contact_pet', 'poultry_contact_handled', 'poultry_contact_raised', 'poultry_contact_feces_food', 'poultry_contact_house', 'poultry_contact_cooked', 'poultry_contact_eaten_raw','poultry_contact_eaten_sick', 'poultry_contact_found_dead', 'poultry_contact_scratched_bitten', 'poultry_contact_hunted', 'poultry_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$poultry_contact_dummy <- ifelse(grepl("none", human_2$poultry_contact),0,1) #creating binary var for any contact
#freq(human_2$poultry_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Goats Sheep Contact
#table(human_2$goats_sheep_contact)
#sum(human_2$goats_sheep_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$goats_sheep_contact_pet <- ifelse(grepl("pet", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_handled <- ifelse(grepl("handled", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_raised <- ifelse(grepl("raised", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_house <- ifelse(grepl("in house", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_cooked <- ifelse(grepl("cooked/handled", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_found_dead <- ifelse(grepl("found dead", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$goats_sheep_contact),1,0)
human_2$goats_sheep_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$goats_sheep_contact),1,0)
#freq(human_2[c('goats_sheep_contact_pet', 'goats_sheep_contact_handled', 'goats_sheep_contact_raised', 'goats_sheep_contact_feces_food', 'goats_sheep_contact_house', 'goats_sheep_contact_cooked', 'goats_sheep_contact_eaten_raw','goats_sheep_contact_eaten_sick', 'goats_sheep_contact_found_dead', 'goats_sheep_contact_scratched_bitten', 'goats_sheep_contact_hunted', 'goats_sheep_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$goats_sheep_contact_dummy <- ifelse(grepl("none", human_2$goats_sheep_contact),0,1) #creating binary var for any contact
#freq(human_2$goats_sheep_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Camel Contact
#table(human_2$camels_contact)
#sum(human_2$camels_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$camels_contact_pet <- ifelse(grepl("pet", human_2$camels_contact),1,0)
human_2$camels_contact_handled <- ifelse(grepl("handled", human_2$camels_contact),1,0)
human_2$camels_contact_raised <- ifelse(grepl("raised", human_2$camels_contact),1,0)
human_2$camels_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$camels_contact),1,0)
human_2$camels_contact_house <- ifelse(grepl("in house", human_2$camels_contact),1,0)
human_2$camels_contact_cooked <- ifelse(grepl("cooked/handled", human_2$camels_contact),1,0)
human_2$camels_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$camels_contact),1,0)
human_2$camels_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$camels_contact),1,0)
human_2$camels_contact_found_dead <- ifelse(grepl("found dead", human_2$camels_contact),1,0)
human_2$camels_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$camels_contact),1,0)
human_2$camels_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$camels_contact),1,0)
human_2$camels_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$camels_contact),1,0)
#freq(human_2[c('camels_contact_pet', 'camels_contact_handled', 'camels_contact_raised', 'camels_contact_feces_food', 'camels_contact_house', 'camels_contact_cooked', 'camels_contact_eaten_raw','camels_contact_eaten_sick', 'camels_contact_found_dead', 'camels_contact_scratched_bitten', 'camels_contact_hunted', 'camels_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$camels_contact_dummy <- ifelse(grepl("none", human_2$camels_contact),0,1) #creating binary var for any contact
#freq(human_2$camels_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Swine Contact
#table(human_2$swine_contact)
#sum(human_2$swine_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$swine_contact_pet <- ifelse(grepl("pet", human_2$swine_contact),1,0)
human_2$swine_contact_handled <- ifelse(grepl("handled", human_2$swine_contact),1,0)
human_2$swine_contact_raised <- ifelse(grepl("raised", human_2$swine_contact),1,0)
human_2$swine_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$swine_contact),1,0)
human_2$swine_contact_house <- ifelse(grepl("in house", human_2$swine_contact),1,0)
human_2$swine_contact_cooked <- ifelse(grepl("cooked/handled", human_2$swine_contact),1,0)
human_2$swine_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$swine_contact),1,0)
human_2$swine_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$swine_contact),1,0)
human_2$swine_contact_found_dead <- ifelse(grepl("found dead", human_2$swine_contact),1,0)
human_2$swine_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$swine_contact),1,0)
human_2$swine_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$swine_contact),1,0)
human_2$swine_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$swine_contact),1,0)
#freq(human_2[c('swine_contact_pet', 'swine_contact_handled', 'swine_contact_raised', 'swine_contact_feces_food', 'swine_contact_house', 'swine_contact_cooked', 'swine_contact_eaten_raw','swine_contact_eaten_sick', 'swine_contact_found_dead', 'swine_contact_scratched_bitten', 'swine_contact_hunted', 'swine_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$swine_contact_dummy <- ifelse(grepl("none", human_2$swine_contact),0,1) #creating binary var for any contact
#freq(human_2$swine_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Cattle Contact
#table(human_2$cattle_contact)
#sum(human_2$cattle_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$cattle_contact_pet <- ifelse(grepl("pet", human_2$cattle_contact),1,0)
human_2$cattle_contact_handled <- ifelse(grepl("handled", human_2$cattle_contact),1,0)
human_2$cattle_contact_raised <- ifelse(grepl("raised", human_2$cattle_contact),1,0)
human_2$cattle_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$cattle_contact),1,0)
human_2$cattle_contact_house <- ifelse(grepl("in house", human_2$cattle_contact),1,0)
human_2$cattle_contact_cooked <- ifelse(grepl("cooked/handled", human_2$cattle_contact),1,0)
human_2$cattle_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$cattle_contact),1,0)
human_2$cattle_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$cattle_contact),1,0)
human_2$cattle_contact_found_dead <- ifelse(grepl("found dead", human_2$cattle_contact),1,0)
human_2$cattle_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$cattle_contact),1,0)
human_2$cattle_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$cattle_contact),1,0)
human_2$cattle_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$cattle_contact),1,0)
#freq(human_2[c('cattle_contact_pet', 'cattle_contact_handled', 'cattle_contact_raised', 'cattle_contact_feces_food', 'cattle_contact_house', 'cattle_contact_cooked', 'cattle_contact_eaten_raw','cattle_contact_eaten_sick', 'cattle_contact_found_dead', 'cattle_contact_scratched_bitten', 'cattle_contact_hunted', 'cattle_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$cattle_contact_dummy <- ifelse(grepl("none", human_2$cattle_contact),0,1) #creating binary var for any contact
#freq(human_2$cattle_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Horses Contact
#table(human_2$horses_contact)
#sum(human_2$horses_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$horses_contact_pet <- ifelse(grepl("pet", human_2$horses_contact),1,0)
human_2$horses_contact_handled <- ifelse(grepl("handled", human_2$horses_contact),1,0)
human_2$horses_contact_raised <- ifelse(grepl("raised", human_2$horses_contact),1,0)
human_2$horses_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$horses_contact),1,0)
human_2$horses_contact_house <- ifelse(grepl("in house", human_2$horses_contact),1,0)
human_2$horses_contact_cooked <- ifelse(grepl("cooked/handled", human_2$horses_contact),1,0)
human_2$horses_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$horses_contact),1,0)
human_2$horses_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$horses_contact),1,0)
human_2$horses_contact_found_dead <- ifelse(grepl("found dead", human_2$horses_contact),1,0)
human_2$horses_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$horses_contact),1,0)
human_2$horses_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$horses_contact),1,0)
human_2$horses_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$horses_contact),1,0)
#freq(human_2[c('horses_contact_pet', 'horses_contact_handled', 'horses_contact_raised', 'horses_contact_feces_food', 'horses_contact_house', 'horses_contact_cooked', 'horses_contact_eaten_raw','horses_contact_eaten_sick', 'horses_contact_found_dead', 'horses_contact_scratched_bitten', 'horses_contact_hunted', 'horses_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$horses_contact_dummy <- ifelse(grepl("none", human_2$horses_contact),0,1) #creating binary var for any contact
#freq(human_2$horses_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Cats Contact
#table(human_2$cats_contact)
#sum(human_2$cats_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$cats_contact_pet <- ifelse(grepl("pet", human_2$cats_contact),1,0)
human_2$cats_contact_handled <- ifelse(grepl("handled", human_2$cats_contact),1,0)
human_2$cats_contact_raised <- ifelse(grepl("raised", human_2$cats_contact),1,0)
human_2$cats_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$cats_contact),1,0)
human_2$cats_contact_house <- ifelse(grepl("in house", human_2$cats_contact),1,0)
human_2$cats_contact_cooked <- ifelse(grepl("cooked/handled", human_2$cats_contact),1,0)
human_2$cats_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$cats_contact),1,0)
human_2$cats_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$cats_contact),1,0)
human_2$cats_contact_found_dead <- ifelse(grepl("found dead", human_2$cats_contact),1,0)
human_2$cats_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$cats_contact),1,0)
human_2$cats_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$cats_contact),1,0)
human_2$cats_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$cats_contact),1,0)
#freq(human_2[c('cats_contact_pet', 'cats_contact_handled', 'cats_contact_raised', 'cats_contact_feces_food', 'cats_contact_house', 'cats_contact_cooked', 'cats_contact_eaten_raw','cast_contact_eaten_sick', 'cats_contact_found_dead', 'cats_contact_scratched_bitten', 'cats_contact_hunted', 'cats_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$cats_contact_dummy <- ifelse(grepl("none", human_2$cats_contact),0,1) #creating binary var for any contact
#freq(human_2$cats_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Dog Contact
#table(human_2$dogs_contact)
#sum(human_2$dogs_contact=="MISSING")
#Creating binary contact type vars using grepl to identify matching strings
human_2$dogs_contact_pet <- ifelse(grepl("pet", human_2$dogs_contact),1,0)
human_2$dogs_contact_handled <- ifelse(grepl("handled", human_2$dogs_contact),1,0)
human_2$dogs_contact_raised <- ifelse(grepl("raised", human_2$dogs_contact),1,0)
human_2$dogs_contact_feces_food <- ifelse(grepl("feces in or near food", human_2$dogs_contact),1,0)
human_2$dogs_contact_house <- ifelse(grepl("in house", human_2$dogs_contact),1,0)
human_2$dogs_contact_cooked <- ifelse(grepl("cooked/handled", human_2$dogs_contact),1,0)
human_2$dogs_contact_eaten_raw <- ifelse(grepl("eaten raw", human_2$dogs_contact),1,0)
human_2$dogs_contact_eaten_sick <- ifelse(grepl("eaten sick", human_2$dogs_contact),1,0)
human_2$dogs_contact_found_dead <- ifelse(grepl("found dead", human_2$dogs_contact),1,0)
human_2$dogs_contact_scratched_bitten <- ifelse(grepl("scratched/bitten", human_2$dogs_contact),1,0)
human_2$dogs_contact_hunted <- ifelse(grepl("hunted/trapped", human_2$dogs_contact),1,0)
human_2$dogs_contact_slaughtered <- ifelse(grepl("slaughtered", human_2$dogs_contact),1,0)
#freq(human_2[c('dogs_contact_pet', 'dogs_contact_handled', 'dogs_contact_raised', 'dogs_contact_feces_food', 'dogs_contact_house', 'dogs_contact_cooked', 'dogs_contact_eaten_raw','dogs_contact_eaten_sick', 'dogs_contact_found_dead', 'dogs_contact_scratched_bitten', 'dogs_contact_hunted', 'dogs_contact_slaughtered')], report.nas = FALSE, cumul=FALSE)
human_2$dogs_contact_dummy <- ifelse(grepl("none", human_2$dogs_contact),0,1) #creating binary var for any contact
#freq(human_2$dogs_contact_dummy, report.nas = FALSE, cumul=FALSE)

#Animal Contact
#Creating binary var for any animal contact
human_2$animal_contact_dummy <- ifelse(human_2$bats_contact_dummy==1,1,
                                       ifelse(human_2$rodents_contact_dummy==1,1,
                                              ifelse(human_2$nhp_contact_dummy==1,1,
                                                     ifelse(human_2$birds_contact_dummy==1,1,
                                                            ifelse(human_2$carnivores_contact_dummy==1,1,
                                                                   ifelse(human_2$ungulates_contact_dummy==1,1,
                                                                          ifelse(human_2$pangolins_contact_dummy==1,1,
                                                                                 ifelse(human_2$poultry_contact_dummy==1,1,
                                                                                        ifelse(human_2$goats_sheep_contact_dummy==1,1,
                                                                                               ifelse(human_2$camels_contact_dummy==1,1,
                                                                                                      ifelse(human_2$swine_contact_dummy==1,1,
                                                                                                             ifelse(human_2$cattle_contact_dummy==1,1,
                                                                                                                    ifelse(human_2$horses_contact_dummy==1,1,
                                                                                                                           ifelse(human_2$dogs_contact_dummy==1,1,
                                                                                                                                  ifelse(human_2$cats_contact_dummy==1,1,0)))))))))))))))
#freq(human_2$animal_contact_dummy)
human_2[c("rodents_contact_dummy", "bats_contact_dummy", "nhp_contact_dummy", "birds_contact_dummy", "carnivores_contact_dummy", "ungulates_contact_dummy", "pangolins_contact_dummy",
          "poultry_contact_dummy", "goats_sheep_contact_dummy", "camels_contact_dummy", "swine_contact_dummy", "cattle_contact_dummy", "horses_contact_dummy", "dogs_contact_dummy",
          "cats_contact_dummy", "animal_contact_dummy")] <- lapply(human_2[c("rodents_contact_dummy", "bats_contact_dummy", "nhp_contact_dummy", "birds_contact_dummy", "carnivores_contact_dummy",
                                                                             "ungulates_contact_dummy", "pangolins_contact_dummy", "poultry_contact_dummy", "goats_sheep_contact_dummy", "camels_contact_dummy",
                                                                             "swine_contact_dummy", "cattle_contact_dummy", "horses_contact_dummy", "dogs_contact_dummy", "cats_contact_dummy", "animal_contact_dummy")],
                                                                   factor,
                                                                   levels=c(0, 1),
                                                                   labels = c("No", "Yes")) #factoring, sorting, and labeling all binary animal contact vars

## Animal Contact Types ####

#Factoring contact in life
contactlife <- c("pet_in_dwelling_life","handle_animals_life","raised_animals_life","shared_water_life","animal_feces_food_life","animals_in_food_life","animals_in_dwelling_life",
                 "cooked_meat_life","eaten_raw_meat_life","eaten_sick_animal_life","eaten_dead_animal_life","sold_dead_animal_life","scratched_bitten_life","hunted_animal_life","slaughtered_animal_life") #concatenating animal contact life vars into vector
human_2[contactlife]<-lapply(human_2[contactlife],factor) #factoring contact in life vars

#Last Year
#Creating binary vars for animal contact types within the last year, coding N/As as 0 (only N/A if answered no to contact in life, which also means no contact in last year)
human_2$pet_in_dwelling_last_year<-ifelse(human_2$pet_in_dwelling_last_year=="N/A",0,
                                          ifelse(human_2$pet_in_dwelling_last_year=="no",0,
                                                 ifelse(human_2$pet_in_dwelling_last_year=="yes",1,NA)))
human_2$handle_animals_last_year<-ifelse(human_2$handle_animals_last_year=="N/A",0,
                                         ifelse(human_2$handle_animals_last_year=="no",0,
                                                ifelse(human_2$handle_animals_last_year=="yes",1,NA)))
human_2$raised_animals_last_year<-ifelse(human_2$raised_animals_last_year=="N/A",0,
                                         ifelse(human_2$raised_animals_last_year=="no",0,
                                                ifelse(human_2$raised_animals_last_year=="yes",1,NA)))
human_2$shared_water_last_year<-ifelse(human_2$shared_water_last_year=="N/A",0,
                                       ifelse(human_2$shared_water_last_year=="no",0,
                                              ifelse(human_2$shared_water_last_year=="yes",1,
                                                     ifelse(human_2$shared_water_last_year=="don't know",2, NA))))
human_2$animal_feces_food_last_year<-ifelse(human_2$animal_feces_food_last_year=="N/A",0,
                                            ifelse(human_2$animal_feces_food_last_year=="no",0,
                                                   ifelse(human_2$animal_feces_food_last_year=="yes",1,NA)))
human_2$animals_in_food_last_year<-ifelse(human_2$animals_in_food_last_year=="N/A",0,
                                          ifelse(human_2$animals_in_food_last_year=="no",0,
                                                 ifelse(human_2$animals_in_food_last_year=="yes",1,
                                                        ifelse(human_2$animals_in_food_last_year=="don't know",2, NA))))
human_2$animals_in_dwelling_last_year<-ifelse(human_2$animals_in_dwelling_last_year=="N/A",0,
                                              ifelse(human_2$animals_in_dwelling_last_year=="no",0,
                                                     ifelse(human_2$animals_in_dwelling_last_year=="yes",1,NA)))
human_2$cooked_meat_last_year<-ifelse(human_2$cooked_meat_last_year=="N/A",0,
                                      ifelse(human_2$cooked_meat_last_year=="no",0,
                                             ifelse(human_2$cooked_meat_last_year=="yes",1,NA)))
human_2$eaten_raw_meat_last_year<-ifelse(human_2$eaten_raw_meat_last_year=="N/A",0,
                                         ifelse(human_2$eaten_raw_meat_last_year=="no",0,
                                                ifelse(human_2$eaten_raw_meat_last_year=="yes",1,NA)))
human_2$eaten_sick_animal_last_year<-ifelse(human_2$eaten_sick_animal_last_year=="N/A",0,
                                            ifelse(human_2$eaten_sick_animal_last_year=="no",0,
                                                   ifelse(human_2$eaten_sick_animal_last_year=="yes",1,
                                                          ifelse(human_2$eaten_sick_animal_last_year=="don't know",2, NA))))
human_2$eaten_dead_animal_last_year<-ifelse(human_2$eaten_dead_animal_last_year=="N/A",0,
                                            ifelse(human_2$eaten_dead_animal_last_year=="no",0,
                                                   ifelse(human_2$eaten_dead_animal_last_year=="yes",1,NA)))
human_2$sold_dead_animal_last_year<-ifelse(human_2$sold_dead_animal_last_year=="N/A",0,
                                           ifelse(human_2$sold_dead_animal_last_year=="no",0,
                                                  ifelse(human_2$sold_dead_animal_last_year=="yes",1,NA)))
human_2$scratched_bitten_last_year<-ifelse(human_2$scratched_bitten_last_year=="N/A",0,
                                           ifelse(human_2$scratched_bitten_last_year=="no",0,
                                                  ifelse(human_2$scratched_bitten_last_year=="yes",1,NA)))
human_2$hunted_animal_last_year<-ifelse(human_2$hunted_animal_last_year=="N/A",0,
                                        ifelse(human_2$hunted_animal_last_year=="no",0,
                                               ifelse(human_2$hunted_animal_last_year=="yes",1,NA)))
human_2$slaughtered_animal_last_year<-ifelse(human_2$slaughtered_animal_last_year=="N/A",0,
                                             ifelse(human_2$slaughtered_animal_last_year=="no",0,
                                                    ifelse(human_2$slaughtered_animal_last_year=="yes",1,NA)))
contactlastyear <- c("pet_in_dwelling_last_year","handle_animals_last_year","raised_animals_last_year","shared_water_last_year","animal_feces_food_last_year","animals_in_food_last_year","animals_in_dwelling_last_year",
                     "cooked_meat_last_year","eaten_raw_meat_last_year","eaten_sick_animal_last_year","eaten_dead_animal_last_year","sold_dead_animal_last_year","scratched_bitten_last_year","hunted_animal_last_year","slaughtered_animal_last_year") #concatenating contact type binary into vector
human_2[c("pet_in_dwelling_last_year","handle_animals_last_year","raised_animals_last_year","animal_feces_food_last_year","animals_in_dwelling_last_year",
          "cooked_meat_last_year","eaten_raw_meat_last_year","eaten_dead_animal_last_year","sold_dead_animal_last_year","scratched_bitten_last_year","hunted_animal_last_year",
          "slaughtered_animal_last_year")]<-lapply(human_2[c("pet_in_dwelling_last_year","handle_animals_last_year","raised_animals_last_year","animal_feces_food_last_year","animals_in_dwelling_last_year",
                                                             "cooked_meat_last_year","eaten_raw_meat_last_year","eaten_dead_animal_last_year","sold_dead_animal_last_year","scratched_bitten_last_year","hunted_animal_last_year","slaughtered_animal_last_year")],
                                                   factor,
                                                   levels=c(0,1),
                                                   labels=c("No","Yes")) #factoring, sorting, labeling binary contact type vars
human_2[c("shared_water_last_year","animals_in_food_last_year","eaten_sick_animal_last_year")]<-
  lapply(human_2[c("shared_water_last_year","animals_in_food_last_year","eaten_sick_animal_last_year")],
         factor,
         levels=c(0,1,2),
         labels=c("No","Yes","Don't Know")) #factoring, sorting, and labeling contact type vars that include "don't know" response

## Animal Contact Heatmap Dataframe ####
#Creating global df of animal taxa and contact type grouped by country

#Creating Subset, Pivoting, and Separating Rows
animal_contact_heatmap_df <- human_2 |> select(participant_id, country, rodents_contact, bats_contact, nhp_contact, birds_contact, carnivores_contact, ungulates_contact, pangolins_contact, poultry_contact, goats_sheep_contact, camels_contact, swine_contact, cattle_contact, horses_contact, cats_contact, dogs_contact) #creating subset with ID, country, and original animal contact vars
animal_contact_heatmap_df <- animal_contact_heatmap_df |> pivot_longer(-c(participant_id, country), names_to="Animal_Contact", values_to="Contact_Type") #pivoting animal contact vars long
animal_contact_heatmap_df <- separate_rows(animal_contact_heatmap_df, Contact_Type, sep="; ", convert=FALSE) #separating contact type options
#Creating pretty Animal taxa names
animal_contact_heatmap_df$Animals <- ifelse(animal_contact_heatmap_df$Animal_Contact=="rodents_contact", "Rodents/Shrews",
                                            ifelse(animal_contact_heatmap_df$Animal_Contact=="bats_contact", "Bats",
                                                   ifelse(animal_contact_heatmap_df$Animal_Contact=="nhp_contact", "Non-Human Primates",
                                                          ifelse(animal_contact_heatmap_df$Animal_Contact=="birds_contact", "Birds",
                                                                 ifelse(animal_contact_heatmap_df$Animal_Contact=="carnivores_contact", "Carnivores",
                                                                        ifelse(animal_contact_heatmap_df$Animal_Contact=="ungulates_contact", "Ungulates",
                                                                               ifelse(animal_contact_heatmap_df$Animal_Contact=="pangolins_contact", "Pangolins",
                                                                                      ifelse(animal_contact_heatmap_df$Animal_Contact=="poultry_contact", "Poultry/Other Fowl",
                                                                                             ifelse(animal_contact_heatmap_df$Animal_Contact=="goats_sheep_contact", "Goats/Sheep",
                                                                                                    ifelse(animal_contact_heatmap_df$Animal_Contact=="camels_contact", "Camels",
                                                                                                           ifelse(animal_contact_heatmap_df$Animal_Contact=="swine_contact", "Swine",
                                                                                                                  ifelse(animal_contact_heatmap_df$Animal_Contact=="cattle_contact", "Cattle/Buffalo",
                                                                                                                         ifelse(animal_contact_heatmap_df$Animal_Contact=="horses_contact", "Horses",
                                                                                                                                ifelse(animal_contact_heatmap_df$Animal_Contact=="cats_contact", "Cats",
                                                                                                                                       ifelse(animal_contact_heatmap_df$Animal_Contact=="dogs_contact", "Dogs",NA)))))))))))))))
#Capitalizing Contact Types
animal_contact_heatmap_df$Contact <- animal_contact_heatmap_df$Contact_Type
animal_contact_heatmap_df %>% mutate(Contact=sub("(.)","\\U\\1", animal_contact_heatmap_df$Contact, perl=TRUE)) -> animal_contact_heatmap_df
#Calculating Counts
animal_contact_heatmap_df_counts <- animal_contact_heatmap_df |> group_by(Animals, country) |> dplyr::count(Contact)
#Completing dataframe with missing rows
animal_contact_heatmap_df_counts %>%
  ungroup() %>%
  complete(country, Animals, Contact, fill=list(n=0)) -> animal_contact_heatmap_df_counts
#Removing rows where contact type is "None"
animal_contact_heatmap_df_counts <- subset(animal_contact_heatmap_df_counts, Contact != "None")

## Labels ####
#Getting list of names
#list_human_names <- names(human_2)
#Doing some preliminary cleaning
#str_replace_all(list_human_names, "_", " ") -> list_human_names2
#str_to_title(list_human_names2) -> list_human_names2
#Converting cleaned list to comma separated string to copy and paste below
#paste(shQuote(human_variables2), collapse = ", ")
#Listing cleaned variable names in order of dataframe
human_variables_clean <- list('Project', 'Gains4 Event Id', 'Event Name', 'Country', 'Gains4 Sample Unit Id', 'Season', 'Season Modelled', 'Season Modelled Deviation', 'Participant Id', 'Date Of Interview',
                              'Begin Time Interview', 'End Time Interview', 'Interview City', 'Interview State Prov', 'Interview Latitude', 'Interview Longitude', 'Gender', 'Age', 'Live City', 'Live State Prov',
                              'Live Latitude', 'Live Longitude', 'Length Lived', 'People in Dwelling', 'Children in Dwelling', 'Males in Dwelling', 'Rooms in Dwelling', 'Dwelling Permanent Structure',
                              'Drinking Water Source', 'Water Treated', 'Water Treatment', 'Water Used by Animals', 'Dedicated Location for Waste', 'Food Storage Containers', 'Demographic Notes',
                              'Highest Education', "Mother's Highest Education", 'Livelihoods', 'Primary Livelihood', 'Modules Completed', 'Job Position', 'Work Location City', 'Work Location Prov',
                              'Work Location Latitude', 'Work Location Longitude', 'Livelihood Notes', 'Treatment for Medical Problems', 'Ever Had Symptoms', 'Had Any Symptoms in Last Year', 'Symptoms in Last Year', 'Opinion  on Cause of Sickness',
                              'Had Symptoms in Last Year Other People', 'Symptoms in Last Year Other People', 'Illness Death', 'Medical History Notes', 'Travelled', 'More Than 6', 'Travelled City Loc 1',
                              'Latitude Loc 1', 'Longitude Loc 1', 'Travelled City Loc 2', 'Latitude Loc 2', 'Longitude Loc 2', 'Travelled City Loc 3', 'Latitude Loc 3', 'Longitude Loc 3', 'Travelled City Loc 4',
                              'Latitude Loc 4', 'Longitude Loc 4', 'Travelled City Loc 5', 'Latitude Loc 5', 'Longitude Loc 5', 'Travelled City Loc 6', 'Latitude Loc 6', 'Longitude Loc 6', 'Travel Reason',
                              'Movement Notes', 'Pet in Dwelling Life', 'Pet in Dwelling Last Year', 'Handle Animals Life', 'Handle Animals Last Year', 'Raised Animals Life', 'Raised Animals Last Year',
                              'Shared Water Life', 'Shared Water Last Year', 'Animal Feces Food Life', 'Animal Feces Food Last Year', 'Animals in Food Life', 'Animals in Food Last Year', 'Animals in Dwelling Life',
                              'Animals in Dwelling Last Year', 'Cooked Meat Life', 'Cooked Meat Last Year', 'Eaten Raw Meat Life', 'Eaten Raw Meat Last Year', 'Eaten Sick Animal Life', 'Eaten Sick Animal Last Year',
                              'Eaten Dead Animal Life', 'Eaten Dead Animal Last Year', 'Sold Dead Animal Life', 'Sold Dead Animal Last Year', 'Scratched Bitten Life', 'Scratched Bitten Last Year',
                              'Hunted Animal Life', 'Hunted Animal Last Year', 'Slaughtered Animal Life', 'Slaughtered Animal Last Year', 'Scratched Bitten Action', 'Risk Open Wound', 'Rodents Contact',
                              'Bats Contact', 'Nhp Contact', 'Birds Contact', 'Carnivores Contact', 'Ungulates Contact', 'Pangolins Contact', 'Poultry Contact', 'Goats Sheep Contact', 'Camels Contact',
                              'Swine Contact', 'Cattle Contact', 'Horses Contact', 'Dogs Contact', 'Cats Contact', 'Worried About Disease', 'Specimens Collected', "Mother's Highest Education", 'Age',
                              'Age', 'Age', 'Age', 'Age', 'Age', 'Piped', 'Covered', 'Uncovered', 'Truck Rainwater',
                              'Other', 'Boil', 'Filter', 'Chlorine', 'Solar', 'Other', 'Covered',
                              'Uncovered', 'No Containers', 'Always Covered', 'Always Uncovered', 'Covered And Uncovered', 'Crowding Index',
                              'Crowding Index', 'Clinic', 'Community Health Worker', 'Hospital', 'Traditional Healer', 'Mobile Clinic', 'Dispensary or Pharmacy', 'Let Someone Else Take Over', 'Visit Doctor', 'Wash Wound with Soap and Water', 'Rinse Wound with Water',
                              'Nothing - Kept Working', 'Never Butcher or Slaughter', 'Bandage Wound', 'No', "Yes, but don't know what they are", 'Yes, it can make you sick', 'Yes, it can poison you', 'Yes, it can infect you with a disease',
                              "Don't know", 'Other', 'Contact with sick people', 'Contact with wild animals', 'Contact with other animals', 'Bad food or water', 'Bad spirits/witchcraft', 'Wound or injury',
                              "Don't know", 'Other', 'Encephalitis', 'Hemorrhagic fever', 'SARI', 'ILI', 'Fever with diarrhea or vomiting', 'Fever with rash', 'Persistent rash or sores on skin', 'Other', 'Livelihoods Extraction',
                              'Livelihoods Crops', 'Livelihoods Wildlife Rest', 'Livelihoods Animal Trade', 'Livelihoods Animal Production', 'Livelihoods Meat Processing', 'Livelihoods Zoo',
                              'Livelihoods Protected Area', 'Livelihoods Hunter', 'Livelihoods Forager', 'Livelihoods Migrant Laborer', 'Livelihoods Hcw', 'Livelihoods Construction', 'Livelihoods Homemaker',
                              'Livelihoods Child', 'Livelihoods Livestock', 'Livelihoods Non Animal', 'Livelihoods Student', 'Livelihoods Unemployed', 'Livelihoods Other', 'Bats Contact Pet', 'Bats Contact Handled',
                              'Bats Contact Raised', 'Bats Contact Feces Food', 'Bats Contact House', 'Bats Contact Cooked', 'Bats Contact Eaten Raw', 'Bats Contact Eaten Sick', 'Bats Contact Found Dead',
                              'Bats Contact Scratched Bitten', 'Bats Contact Hunted', 'Bats Contact Slaughtered', 'Bats Contact Dummy', 'Rodents Contact Pet', 'Rodents Contact Handled', 'Rodents Contact Raised',
                              'Rodents Contact Feces Food', 'Rodents Contact House', 'Rodents Contact Cooked', 'Rodents Contact Eaten Raw', 'Rodents Contact Eaten Sick', 'Rodents Contact Found Dead',
                              'Rodents Contact Scratched Bitten', 'Rodents Contact Hunted', 'Rodents Contact Slaughtered', 'Rodents Contact Dummy', 'Nhp Contact Pet', 'Nhp Contact Handled', 'Nhp Contact Raised',
                              'Nhp Contact Feces Food', 'Nhp Contact House', 'Nhp Contact Cooked', 'Nhp Contact Eaten Raw', 'Nhp Contact Eaten Sick', 'Nhp Contact Found Dead', 'Nhp Contact Scratched Bitten',
                              'Nhp Contact Hunted', 'Nhp Contact Slaughtered', 'Nhp Contact Dummy', 'Birds Contact Pet', 'Birds Contact Handled', 'Birds Contact Raised', 'Birds Contact Feces Food', 'Birds Contact House',
                              'Birds Contact Cooked', 'Birds Contact Eaten Raw', 'Birds Contact Eaten Sick', 'Birds Contact Found Dead', 'Birds Contact Scratched Bitten', 'Birds Contact Hunted',
                              'Birds Contact Slaughtered', 'Birds Contact Dummy', 'Carnivores Contact Pet', 'Carnivores Contact Handled', 'Carnivores Contact Raised', 'Carnivores Contact Feces Food',
                              'Carnivores Contact House', 'Carnivores Contact Cooked', 'Carnivores Contact Eaten Raw', 'Carnivores Contact Eaten Sick', 'Carnivores Contact Found Dead', 'Carnivores Contact Scratched Bitten',
                              'Carnivores Contact Hunted', 'Carnivores Contact Slaughtered', 'Carnivores Contact Dummy', 'Ungulates Contact Pet', 'Ungulates Contact Handled', 'Ungulates Contact Raised',
                              'Ungulates Contact Feces Food', 'Ungulates Contact House', 'Ungulates Contact Cooked', 'Ungulates Contact Eaten Raw', 'Ungulates Contact Eaten Sick', 'Ungulates Contact Found Dead',
                              'Ungulates Contact Scratched Bitten', 'Ungulates Contact Hunted', 'Ungulates Contact Slaughtered', 'Ungulates Contact Dummy', 'Pangolins Contact Pet', 'Pangolins Contact Handled',
                              'Pangolins Contact Raised', 'Pangolins Contact Feces Food', 'Pangolins Contact House', 'Pangolins Contact Cooked', 'Pangolins Contact Eaten Raw', 'Pangolins Contact Eaten Sick',
                              'Pangolins Contact Found Dead', 'Pangolins Contact Scratched Bitten', 'Pangolins Contact Hunted', 'Pangolins Contact Slaughtered', 'Pangolins Contact Dummy', 'Poultry Contact Pet',
                              'Poultry Contact Handled', 'Poultry Contact Raised', 'Poultry Contact Feces Food', 'Poultry Contact House', 'Poultry Contact Cooked', 'Poultry Contact Eaten Raw', 'Poultry Contact Eaten Sick',
                              'Poultry Contact Found Dead', 'Poultry Contact Scratched Bitten', 'Poultry Contact Hunted', 'Poultry Contact Slaughtered', 'Poultry Contact Dummy', 'Goats Sheep Contact Pet',
                              'Goats Sheep Contact Handled', 'Goats Sheep Contact Raised', 'Goats Sheep Contact Feces Food', 'Goats Sheep Contact House', 'Goats Sheep Contact Cooked', 'Goats Sheep Contact Eaten Raw',
                              'Goats Sheep Contact Eaten Sick', 'Goats Sheep Contact Found Dead', 'Goats Sheep Contact Scratched Bitten', 'Goats Sheep Contact Hunted', 'Goats Sheep Contact Slaughtered',
                              'Goats Sheep Contact Dummy', 'Camels Contact Pet', 'Camels Contact Handled', 'Camels Contact Raised', 'Camels Contact Feces Food', 'Camels Contact House', 'Camels Contact Cooked',
                              'Camels Contact Eaten Raw', 'Camels Contact Eaten Sick', 'Camels Contact Found Dead', 'Camels Contact Scratched Bitten', 'Camels Contact Hunted', 'Camels Contact Slaughtered',
                              'Camels Contact Dummy', 'Swine Contact Pet', 'Swine Contact Handled', 'Swine Contact Raised', 'Swine Contact Feces Food', 'Swine Contact House', 'Swine Contact Cooked',
                              'Swine Contact Eaten Raw', 'Swine Contact Eaten Sick', 'Swine Contact Found Dead', 'Swine Contact Scratched Bitten', 'Swine Contact Hunted', 'Swine Contact Slaughtered',
                              'Swine Contact Dummy', 'Cattle Contact Pet', 'Cattle Contact Handled', 'Cattle Contact Raised', 'Cattle Contact Feces Food', 'Cattle Contact House', 'Cattle Contact Cooked',
                              'Cattle Contact Eaten Raw', 'Cattle Contact Eaten Sick', 'Cattle Contact Found Dead', 'Cattle Contact Scratched Bitten', 'Cattle Contact Hunted', 'Cattle Contact Slaughtered',
                              'Cattle Contact Dummy', 'Horses Contact Pet', 'Horses Contact Handled', 'Horses Contact Raised', 'Horses Contact Feces Food', 'Horses Contact House', 'Horses Contact Cooked',
                              'Horses Contact Eaten Raw', 'Horses Contact Eaten Sick', 'Horses Contact Found Dead', 'Horses Contact Scratched Bitten', 'Horses Contact Hunted', 'Horses Contact Slaughtered',
                              'Horses Contact Dummy', 'Cats Contact Pet', 'Cats Contact Handled', 'Cats Contact Raised', 'Cats Contact Feces Food', 'Cats Contact House', 'Cats Contact Cooked',
                              'Cats Contact Eaten Raw', 'Cats Contact Eaten Sick', 'Cats Contact Found Dead', 'Cats Contact Scratched Bitten', 'Cats Contact Hunted', 'Cats Contact Slaughtered', 'Cats Contact Dummy',
                              'Dogs Contact Pet', 'Dogs Contact Handled', 'Dogs Contact Raised', 'Dogs Contact Feces Food', 'Dogs Contact House', 'Dogs Contact Cooked', 'Dogs Contact Eaten Raw', 'Dogs Contact Eaten Sick',
                              'Dogs Contact Found Dead', 'Dogs Contact Scratched Bitten', 'Dogs Contact Hunted', 'Dogs Contact Slaughtered', 'Dogs Contact Dummy', 'Animal Contact Dummy', 'Rodents Pretty', 'Bats Pretty',
                              'Nhp Pretty', 'Birds Pretty', 'Carnivores Pretty', 'Ungulates Pretty', 'Pangolins Pretty', 'Poultry Pretty', 'Goats Sheep Pretty', 'Camels Pretty', 'Swine Pretty', 'Cattle Pretty',
                              'Horses Pretty', 'Cats Pretty', 'Dogs Pretty')
var_label(human_2) <- human_variables_clean #applying labels to df
?var_label

