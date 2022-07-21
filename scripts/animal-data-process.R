library(tidyverse)
library(stringr)
library(dbplyr)
library(eidith)


# Data from local EIDITH (not publicly available ) ------------------------------------------------------------

conn <- eidith:::eidith_db()

e_local <-  tbl(conn, "events_2") %>%
  filter(country == "Ivory Coast") %>%
  filter(concurrent_sampling_site %in% c("Concurrent Site 1", "Concurrent Site 2", "Concurrent Site 3"))  |>
  select(project, country, gains4_event_id, event_date, concurrent_sampling_site, site_name, disease_transmission_interfaces,site_latitude, site_longitude) |>
  collect()

a_local <- tbl(conn, "animals_2") %>%
  filter(country == "Ivory Coast") %>%
  filter(taxa_group == "bats") |>
  #select(project, country, gains4_event_id, season, gains4_sample_unit_id, animal_id, taxa_group, genus, species) |>
  collect()

s_local <- tbl(conn, "specimens_2") %>%
  select(project, country, gains4_sample_unit_id, gains4_specimen_id, specimen_id, specimen_type) |>
  collect()

t_local <- tbl(conn, "tests_2") %>%
  filter(test_requested == "Coronaviruses") |>
  select(project, country, gains4_specimen_id, gains4_test_id, test_requested, test_requested_protocol, confirmation_result) |>
  collect()

dat_local <- inner_join(e_local, a_local) |>
  inner_join(s_local) |>
  inner_join(t_local)

write_rds(dat_local, here::here("data/civ-bat-cov.rds"))


# DDL ---------------------------------------------------------------------
# Confirming DDL matches local version for n animals sampled
# Extracts were downloaded from https://data.usaid.gov/Global-Health-Security-in-Development-GHSD-/PREDICT-Emerging-Pandemic-Threats-Project/tqea-hwmr
# Tables:
# PREDICT Site/Event Characterization
# PREDICT Animals Sampled
# PREDICT PCR Tests


e_ddl <- read_csv(here::here("data/USAID-DDL-extracts/PREDICT_Site_Event_Characterization.csv")) |>
  filter(Country == "Cote d'ivoire") |>
  filter(ConcurrentSamplingSite %in% c("Concurrent Site 1", "Concurrent Site 2", "Concurrent Site 3"  ))

a_ddl <- read_csv(here::here("data/USAID-DDL-extracts/PREDICT_Animals_Sampled.csv")) |>
  filter(TaxaGroup == "bats") |>
  filter(PREDICT_EventID %in% e_ddl$PREDICT_EventID)  |>
  mutate(ScientificNameToLowestKnownRank = str_remove(ScientificNameToLowestKnownRank, "cf. ")) |>
  separate(ScientificNameToLowestKnownRank, sep = " ", into = c("genus", "species"))

t_ddl <- read_csv(here::here("data/USAID-DDL-extracts/PREDICT_PCR_Tests.csv")) |>
  select(-Latitude, -Longitude) |>
  filter(Country ==  "Cote d'Ivoire") |>
  mutate(Country =  "Cote d'ivoire" ) |>
  filter(TaxaGroup == "bats") |>
  filter(ViralFamilyTested == "Coronaviruses") |>
  mutate(AnimalID = str_extract(SpecimenID, "[^.]+"))

dat_ddl <- inner_join(e_ddl, a_ddl) |>
  inner_join(t_ddl) |>
  janitor::clean_names()

