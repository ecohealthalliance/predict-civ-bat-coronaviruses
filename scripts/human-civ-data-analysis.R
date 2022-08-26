### PREDICT Côte d'Ivoire (CIV) Subset, Data Cleaning, and Analysis ###
## Author: Shannon Ball ##
## Date: July 19, 2022 ##

#### Creating CIV Subset ####

#Human Questionnaires
civ_human_2 <-subset(human_2,country=='Ivory Coast')
civ_human_ehp_2 <-subset(human_ehp_2,country=='Ivory Coast')
civ_human_extractive_industry_2 <-subset(human_extractive_industry_2,country=='Ivory Coast')
civ_human_hospital_worker_2 <-subset(human_hospital_worker_2,country=='Ivory Coast')
civ_human_restaurant_2 <-subset(human_restaurant_2,country=='Ivory Coast')
civ_human_zoo_2 <-subset(human_zoo_2,country=='Ivory Coast')
civ_human_sick_person_2 <-subset(human_sick_person_2,country=='Ivory Coast')
civ_human_animal_production_2 <-subset(human_animal_production_2,country=='Ivory Coast')
civ_human_animal_production_ehp_2 <-subset(human_animal_production_ehp_2,country=='Ivory Coast')
civ_human_crop_production_2 <-subset(human_crop_production_2,country=='Ivory Coast')
civ_human_temporary_settlements_2 <-subset(human_temporary_settlements_2,country=='Ivory Coast')
civ_human_market_2 <-subset(human_market_2,country=='Ivory Coast')
civ_human_hunter_2 <-subset(human_hunter_2,country=='Ivory Coast')
civ_human_hunter_ehp_2 <-subset(human_hunter_ehp_2,country=='Ivory Coast')
#Site and Event Forms
civ_events_2 <-subset(events_2,country=='Ivory Coast')
civ_dwellings_2 <-subset(dwellings_2,country=='Ivory Coast')
civ_extractive_industry_2 <-subset(extractive_industry_2,country=='Ivory Coast')
civ_natural_areas_2 <-subset(natural_areas_2,country=='Ivory Coast')
civ_wildlife_restaurant_2 <-subset(wildlife_restaurant_2,country=='Ivory Coast')
civ_crop_production_2 <-subset(crop_production_2,country=='Ivory Coast')
civ_zoo_sanctuary_2 <-subset(zoo_sanctuary_2,country=='Ivory Coast')
civ_market_value_chain_2 <-subset(market_value_chain_2,country=='Ivory Coast')
civ_animal_production_2 <-subset(animal_production_2,country=='Ivory Coast')
#Other Tables
civ_specimens_2 <-subset(specimens_2,country=='Ivory Coast')
civ_tests_2 <-subset(tests_2,country=='Ivory Coast')
civ_test_data_serology_2 <-subset(test_data_serology_2,country=='Ivory Coast')
civ_test_data_interpreted_2 <-subset(test_data_interpreted_2,country=='Ivory Coast')
civ_training_2 <-subset(training_2,participant_home_country=='Ivory Coast')

#### CIV Data Cleaning ####

#Gender
#table(civ_human_2$gender) #0 CIV participants reported gender as Other
civ_human_2$gender_civ <- ifelse(civ_human_2$gender=="male","male","female") #creating new gender var that doesn't include Other
civ_human_2$gender_civ <- factor(civ_human_2$gender_civ, levels = c("male", "female")) #factoring and sorting var
civ_human_2 = apply_labels(civ_human_2, gender_civ = "Gender") #applying var label

#Livelihoods
freq(civ_human_2$livelihoods_animal_production)
#Grouping livestock livelihood with animal production
civ_human_2$livelihoods_animal_production <- ifelse(civ_human_2$livelihoods_animal_production=="Yes",1,
                                                    ifelse(civ_human_2$livelihoods_livestock=="Yes",1,0))
freq(civ_human_2$livelihoods_animal_production) #one additional person now counted in animal production

#Recoding homemaker, child, non-animal business, student, unemployed, and extraction as Other
#freq(civ_human_2$livelihoods_other) #0 participants originally reported livelihood as Other
civ_human_2$livelihoods_other <- ifelse(civ_human_2$livelihoods_other=="Yes",1,
                                        ifelse(civ_human_2$livelihoods_homemaker=="Yes",1,
                                               ifelse(civ_human_2$livelihoods_child=="Yes",1,
                                                      ifelse(civ_human_2$livelihoods_non_animal=="Yes",1,
                                                             ifelse(civ_human_2$livelihoods_student=="Yes",1,
                                                                    ifelse(civ_human_2$livelihoods_unemployed=="Yes",1,
                                                                           ifelse(civ_human_2$livelihoods_extraction=="Yes",1,0)))))))
#freq(civ_human_2$livelihoods_other)

#Factoring and labelling new livelihood vars (to keep formatting compatible with others)
civ_human_2[c('livelihoods_animal_production','livelihoods_other')] <- lapply(civ_human_2[c('livelihoods_animal_production','livelihoods_other')],
                                                                              factor,
                                                                              levels=c(0, 1),
                                                                              labels = c("No", "Yes")) #factoring, sorting, and labeling livelihood vars
civ_human_2 = apply_labels(civ_human_2,
                           livelihoods_animal_production = "Rancher/farmer animal production business",
                           livelihoods_other = "Other") #applying var labels


#Symptoms Subset
freq(civ_human_2$had_symptoms_in_last_year)
#Creating subset that excludes N/A responses to symptoms in last year
civ_human_2_sx <- subset(civ_human_2, civ_human_2$had_symptoms_in_last_year == "yes" | civ_human_2$had_symptoms_in_last_year == "no")
freq(civ_human_2_sx$had_symptoms_in_last_year)
civ_human_2_sx$had_symptoms_in_last_year <- ifelse(civ_human_2_sx$had_symptoms_in_last_year=="yes", 1, 0)
freq(civ_human_2_sx$had_symptoms_in_last_year)

#Merging Events and Human Data
civ_human_site <- merge(civ_events_2, civ_human_2, by="gains4_event_id") #merging events and human dataframes by event id
#Creating new concurrent site names variable
civ_human_site$concurrent_sites <- ifelse(civ_human_site$concurrent_sampling_site == "Clinic Concurrent Site 1 and 2", "Bouafle Clinic Concurrent Site 2",
                                          ifelse(civ_human_site$concurrent_sampling_site == "Clinic Concurrent Site 1 and 3", "Bonon Clinic Concurrent Site 1",
                                                 ifelse(civ_human_site$concurrent_sampling_site =="Concurrent Site 1", "Bonon Concurrent Site 1",
                                                        ifelse(civ_human_site$concurrent_sampling_site == "Concurrent Site 2","Bouafle Concurrent Site 2", NA))))
civ_human_site$concurrent_sites <- factor(civ_human_site$concurrent_sites, levels = c("Bonon Concurrent Site 1", "Bouafle Concurrent Site 2", "Bonon Clinic Concurrent Site 1", "Bouafle Clinic Concurrent Site 2")) #factoring and sorting site var
#freq(civ_human_site$concurrent_sites)

#### Viewing Data ####

#Human Questionnaires
#nrow(civ_human_2)  #434
#20+ records
#nrow(civ_human_sick_person_2) #205
#nrow(civ_human_crop_production_2) #282
#nrow(civ_human_hunter_2) #129
#nrow(civ_human_animal_production_2) #25
#1-5 records
#nrow(civ_human_extractive_industry_2)
#nrow(civ_human_hospital_worker_2)
#nrow(civ_human_market_2)
#nrow(civ_human_restaurant_2)
#0 records
#nrow(civ_human_ehp_2)
#nrow(civ_human_zoo_2)
#nrow(civ_human_animal_production_ehp_2)
#nrow(civ_human_hunter_ehp_2)
#Events Tables
#nrow(civ_events_2) #19
#1-6 records
#nrow(civ_wildlife_restaurant_2)
#nrow(civ_market_value_chain_2)
#nrow(civ_crop_production_2)
#nrow(civ_animal_production_2)
#nrow(civ_dwellings_2)
#nrow(civ_natural_areas_2)
#0 records
#nrow(civ_extractive_industry_2)
#nrow(civ_zoo_sanctuary_2)
#Other Tables
#nrow(civ_specimens_2) #9107
#nrow(civ_tests_2) #14030
#nrow(civ_test_data_interpreted_2) #14030
#nrow(civ_training_2) #1768
#0 records
#nrow(civ_test_data_serology_2)

#Sites and Events
#freq(civ_human_2$interview_city)
#Creating subset of CIV event names to export
#CIV_events <- freq(civ_human_2$event_name, cumul=FALSE, totals = FALSE)
#write.csv(CIV_events, file = "CIV_events.csv")




#### Data Analysis ####
## Summary Tables ####

#Demographics
civdemos <- c('gender_civ', 'agecat5', 'highest_education', 'highest_education_mother2', 'length_lived', 'livelihoods_crops',
              'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production', 'livelihoods_meat_processing',
              'livelihoods_hunter', 'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_other')
CIVDemos <- CreateTableOne(vars=civdemos, data=civ_human_site, factorVars=civdemos)
print(CIVDemos, showAllLevels = F, varLabels = T, format="f")
CIVDemos_exp <- print(CIVDemos, showAllLevels = F, varLabels = T, format="f", test=FALSE,
                      noSpaces = TRUE, printToggle = FALSE)
#write.csv(CIVDemos_exp, file = here("Outputs", "Tables", "CIV Demographics Summary Table.csv"))

#Bat Contact
#Long
civ_bat_contact_types <- c('bats_contact_pet', 'bats_contact_handled', 'bats_contact_raised', 'bats_contact_feces_food', 'bats_contact_house', 'bats_contact_cooked', 'bats_contact_eaten_raw','bats_contact_eaten_sick', 'bats_contact_found_dead', 'bats_contact_scratched_bitten', 'bats_contact_hunted', 'bats_contact_slaughtered')
CIVBatContact <- CreateTableOne(vars=civ_bat_contact_types, data=civ_human_site, factorVars = civ_bat_contact_types)
print(CIVBatContact, showAllLevels = T, varLabels = T, format="f")
CIVBatContact_exp <- print(CIVBatContact, showAllLevels = F, varLabels = T, format="f", test=FALSE,
                           noSpaces = TRUE, printToggle = FALSE)
#write.csv(CIVBatContact_exp, file = here("Outputs", "Tables", "CIV Bat Contact Summary Table_Long.csv"))

#Pivoted
subsetcivbats <- civ_human_site |> select('participant_id', 'bats_contact_dummy', 'bats_contact_pet', 'bats_contact_handled', 'bats_contact_raised', 'bats_contact_feces_food', 'bats_contact_house', 'bats_contact_cooked', 'bats_contact_eaten_raw','bats_contact_eaten_sick', 'bats_contact_found_dead', 'bats_contact_scratched_bitten', 'bats_contact_hunted', 'bats_contact_slaughtered') #creating subset of all bat contact vars
subsetcivbats$bats_contact_any <- ifelse(subsetcivbats$bats_contact_dummy=='Yes',1,0) #recoding bats_contact_dummy as bats_contact_any, formatted as double instead of factor to be compatible with other vars
subsetcivbats <- subset(subsetcivbats, select = -c(bats_contact_dummy)) #dropping bats_contact_dummy var
subsetcivbatslong <- subsetcivbats |> pivot_longer(-c("participant_id"), names_to = "Bat_Contact", values_to="Response") #pivoting data long, but specifying that we don't want to pivot participant ID
subsetcivbatscounts <- subsetcivbatslong |> group_by(Bat_Contact) |> dplyr::count(Response) #calculating count of records
head(subsetcivbatscounts)
subsetcivbatscountswide <- subsetcivbatscounts |> pivot_wider(names_from="Response", values_from="n", values_fill = 0)#pivoting data back to wide, now that calculation completed
rename (subsetcivbatscountswide, No = '0', Yes = '1') #renaming response options
subsetcivbatscountswide
#write.csv(subsetcivbatscountswide, file = here("Outputs", "Tables", "CIV Bat Contact Summary Table_Pivoted.csv"))

## Statistical Analysis ####

# Demographics Stratified by Bat Contact
CIVBatDemos <- CreateTableOne(vars=civdemos, strata='bats_contact_dummy', data=civ_human_site, factorVars=civdemos) #creating table using civdemos vector and bat contact dummy
print(CIVBatDemos, showAllLevels = T, varLabels = T, format="f", testExact=fisher.test) #printing table in console
CIVBatDemos_exp <- print(CIVBatDemos, showAllLevels = T, varLabels = T, format="f", testExact=fisher.test,
                         noSpaces = TRUE, printToggle = FALSE) #preparing table to export
#write.csv(CIVBatDemos_exp, file = here("Outputs", "Tables", "CIV Demographics Stratified by Bat Contact.csv")) #exporting table

#Binomial Probability
#Creating vector of demographic vars of interest
civdemosbp <- c('gender_civ', 'agecat5', 'highest_education', 'highest_education_mother2', 'length_lived', 'livelihoods_crops',
                'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production', 'livelihoods_meat_processing',
                'livelihoods_hunter', 'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_other')

#Preliminary cleaning of list of var names
#str_replace_all(civdemosbp, "_", " ") -> civdemosbp2
#str_remove(civdemosbp2, "2| ind") -> civdemosbp2
#str_to_title(civdemosbp2) -> civdemosbp2
#Converting list to comma separated string to copy and paste
#paste(shQuote(civdemosbp2), collapse = ", ")

#Creating vector of clean var names
civdemosbp_clean <- c('Gender', 'Age', 'Highest level of education', "Mother's highest level of education", 'Length of time living at location',
                      'Crop production', 'Wildlife restaurant business', 'Wild/exotic animal trade/market business', 'Rancher/farmer/animal production business', 'Meat processing, slaughterhouse, abattoir', 'Hunter/trapper/fisher', 'Nurse, doctor, traditional healer, community health worker', 'Construction', 'Other')

#Creating lookup table for clean var names
names(civdemosbp_clean) <- civdemosbp
tibble(variable = c('gender_civ', 'agecat5', 'highest_education', 'highest_education_mother2', 'length_lived', 'livelihoods_crops',
                    'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production', 'livelihoods_meat_processing',
                    'livelihoods_hunter', 'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_other')) %>%
  mutate(variable_clean = civdemosbp_clean[variable]) -> civ_bp_lookup

#Creating variable groups
civ_bp_lookup <- civ_bp_lookup %>%
  mutate(group = ifelse(variable=="gender_civ" | variable=="agecat5" | variable=="highest_education"| variable=="highest_education_mother2"| variable=="length_lived", "Demographics","Livelihoods"))

#Calculating binomial confidence intervals
civbatbp <- map_dfr(civdemosbp,
                    function (col_name){ #define function within map function

                      dat_counts <- civ_human_site %>% #inputting full data frame
                        mutate(bats_contact_dummy = ifelse(bats_contact_dummy=="Yes", 1,0)) %>% #easier to convert it to 1s and 0s to add it up, could also do separately as pre-processing step, may try doing it as a factor
                        group_by(!!sym(col_name)) %>% #grouping by x ("gender" or "travelled"), !!sym means group by gender the column name (sym=symbol), rather than the string gender
                        summarize(bats_contact_positive = sum(bats_contact_dummy), #counting total number of people who reported contact
                                  total_pop = n()) %>% #counting total number of people within category
                        ungroup() #removing the group when you're done with it so that it doesn't stay grouped and potentially mess up future analysis

                      dat_counts_bc <- binom.confint(dat_counts$bats_contact_positive, dat_counts$total_pop, methods='wilson') %>% #calculated BP and CIs in new dataframe
                        mutate(variable = col_name) #add column named "variable" to indicate which variable we are summarizing

                      out <- bind_cols(dat_counts, dat_counts_bc) %>% #binding two dataframes together
                        rename(class = !!col_name)

                      return(out)

                    })

#Joining with lookup table to get clean var names
civbatbp <- left_join(civbatbp, civ_bp_lookup, by="variable")
#Selecting vars for df to export
civbatbp_exp <- civbatbp |> select(group, variable_clean, class, bats_contact_positive, total_pop, mean, lower, upper)
#write.csv(civbatbp_exp, file=here("Outputs", "Tables", "CIV Bats Binomial Probability.csv"))

#Unadjusted Odds Ratios

#ORs for Bat Contact by Demographics
#Creating subset of vars of interest for calculating ORs
civ_ors_df <- civ_human_site %>% select(participant_id, gender_civ, agecat5, highest_education, highest_education_mother2, length_lived, livelihoods_crops,
                                        livelihoods_wildlife_rest, livelihoods_animal_trade, livelihoods_animal_production, livelihoods_meat_processing,
                                        livelihoods_hunter, livelihoods_hcw, livelihoods_construction, livelihoods_other, bats_contact_dummy)
#identifying predictor cols
civdemos <- c('gender_civ', 'agecat5', 'highest_education', 'highest_education_mother2', 'length_lived', 'livelihoods_crops',
              'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production', 'livelihoods_meat_processing',
              'livelihoods_hunter', 'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_other')
civ_or_cols <- civ_ors_df[civdemos]
#Generating list of ORs for all demos using lapply
civ_ors_list <- lapply(as.list(civ_or_cols), function(x) glm(bats_contact_dummy ~ x, data=civ_ors_df, family=binomial(link="logit")))
#Creating tibble from list of ORs
do.call(rbind, lapply(civ_ors_list, broom::tidy, exponentiate=TRUE, conf.int=TRUE)) -> civ_ors
civ_ors
#write.csv(civ_ors, file=here("Outputs", "Tables", "CIV Bats Unadjusted Odds Ratios.csv"))

## Animal Contact Heat Map ####
#Subsetting global heatmap dataframe
#freq(animal_contact_heatmap_df_counts$country)
civ_animal_contact_heatmap_df_counts <- subset(animal_contact_heatmap_df_counts, country=="Ivory Coast")

#Generating heatmap with zeros NA
civ_animal_contact_heatmap_df_counts %>% mutate(n_nas = ifelse(n==0,NA,n)) -> civ_animal_contact_heatmap_df_counts #creating new count variable where zeros are NAs for scale gradient

ggplot(civ_animal_contact_heatmap_df_counts, aes(x=Contact, y=factor(Animals, levels = rev(levels(factor(Animals)))), #factoring Animals and setting levels as reverse of original to sort Y axis in desired alphabetical order
                                                 fill=n)) +
  geom_tile(color="white", size = 0.80) + #creating white borders between tiles
  geom_text(aes(label = n), size=4) + #labeling tiles with original count variable so that NAs still appear as 0s
  ggtitle("Participant-reported animal contact (n=60)") + ylab("Animal Taxa") + xlab("Contact Type") + #setting titles
  labs(fill = "Number of\nparticipants") + #labeling key and adding line break between "of" and "participants"
  scale_fill_gradient(low="gold", high="darkorchid") + #setting color scale for values >1 and specific fill color for values of 0
  theme(plot.title = element_text(color="black", size="14", face="bold"), #formatting title
        axis.title.y = element_text(face="bold", size="12", color="black"), axis.title.x = element_text(face="bold", size="12", color="black"), #formatting axis titles
        axis.text.y = element_text(color="black", size="10"), axis.text.x = element_text(angle = 45, hjust=1, color="black", size="10"), #formatting axis labels and adjusting angle of x-axis labels
        legend.title = element_text(color="black", size="10")) #formatting legend title





