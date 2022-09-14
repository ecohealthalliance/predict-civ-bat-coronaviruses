### PREDICT Côte d'Ivoire (CIV) De-identified Data Analysis ###
## Author: Shannon Ball ##
## Date: Sept 14, 2022 ##

#### Loading Packages ####
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

#### Setting up Directory ####

#Run to create path to top-level directory where current R project is saved
here()

#### Loading Data####

civ_human_site_zenodo <- read.csv(file = here("Data", "CIV Human Site Zenodo.csv"))


#### Data Analysis ####
  ## Summary Tables ####

  #Demographics
  civdemos <- c('gender_civ', 'agecat5', 'highest_education', 'highest_education_mother2', 'length_lived', 'livelihoods_crops',
                'livelihoods_wildlife_rest', 'livelihoods_animal_trade', 'livelihoods_animal_production', 'livelihoods_meat_processing',
                'livelihoods_hunter', 'livelihoods_hcw', 'livelihoods_construction', 'livelihoods_other')
  CIVDemos <- CreateTableOne(vars=civdemos, data=civ_human_site_zenodo, factorVars=civdemos)
  print(CIVDemos, showAllLevels = F, varLabels = T, format="f")
  CIVDemos_exp <- print(CIVDemos, showAllLevels = F, varLabels = T, format="f", test=FALSE,
                        noSpaces = TRUE, printToggle = FALSE)
  #write.csv(CIVDemos_exp, file = here("Outputs", "Tables", "CIV Demographics Summary Table.csv"))

  #Bat Contact
  #Long
  civ_bat_contact_types <- c('bats_contact_pet', 'bats_contact_handled', 'bats_contact_raised', 'bats_contact_feces_food', 'bats_contact_house', 'bats_contact_cooked', 'bats_contact_eaten_raw','bats_contact_eaten_sick', 'bats_contact_found_dead', 'bats_contact_scratched_bitten', 'bats_contact_hunted', 'bats_contact_slaughtered')
  CIVBatContact <- CreateTableOne(vars=civ_bat_contact_types, data=civ_human_site_zenodo, factorVars = civ_bat_contact_types)
  print(CIVBatContact, showAllLevels = T, varLabels = T, format="f")
  CIVBatContact_exp <- print(CIVBatContact, showAllLevels = F, varLabels = T, format="f", test=FALSE,
                             noSpaces = TRUE, printToggle = FALSE)
  #write.csv(CIVBatContact_exp, file = here("Outputs", "Tables", "CIV Bat Contact Summary Table_Long.csv"))

  #Pivoted
  subsetcivbats <- civ_human_site_zenodo |> select('participant_id', 'bats_contact_dummy', 'bats_contact_pet', 'bats_contact_handled', 'bats_contact_raised', 'bats_contact_feces_food', 'bats_contact_house', 'bats_contact_cooked', 'bats_contact_eaten_raw','bats_contact_eaten_sick', 'bats_contact_found_dead', 'bats_contact_scratched_bitten', 'bats_contact_hunted', 'bats_contact_slaughtered') #creating subset of all bat contact vars
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
  CIVBatDemos <- CreateTableOne(vars=civdemos, strata='bats_contact_dummy', data=civ_human_site_zenodo, factorVars=civdemos) #creating table using civdemos vector and bat contact dummy
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

                        dat_counts <- civ_human_site_zenodo %>% #inputting full data frame
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
  civ_ors_df <- civ_human_site_zenodo %>% select(participant_id, gender_civ, agecat5, highest_education, highest_education_mother2, length_lived, livelihoods_crops,
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





