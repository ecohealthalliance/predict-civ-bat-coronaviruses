[![DOI](https://zenodo.org/badge/146802522.svg)](https://zenodo.org/badge/latestdoi/146802522) [![License: CC0-1.0](https://img.shields.io/badge/License-CC0%201.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# Human interactions with bats and bat coronaviruses in rural Côte d’Ivoire (in development)

***Authors***: *Christian E. Lange, Julien Kalpy Coulibaly, Aristide Beranger Ako Ako, Sabine N’dri Vakou, Eugène Kouassi Koffi, Emma Mendelsohn, Shannon Ball, Stephanie Martinez, Leilani Francisco, Karen Saylers, Jean Manzan, Djeneba Bamba, Valère Kouakou, Stephane Tossea Koui, Frantz Jean Louis, Damien Joly, Cyprien Yapi, Peter Daszak, Mireille Dosso & Anne Laudisoit*

------------------------------------------------------------------------

This repository manuscript source code and data.

`scripts/` contains all code to reproduce the analysis:

-   `animal-data-process.R` produces a dataframe of the PREDICT bat coronavirus data from Côte d’Ivoire (`data/civ-bat-cov.rds`). It pulls data from a local copy of the database, and also includes methods for retrieving the data from the [USAID DDL](https://data.usaid.gov/Global-Health-Security-in-Development-GHSD-/PREDICT-Emerging-Pandemic-Threats-Project/tqea-hwmr).

-   `animal-data-summary.R` includes code to summarize the animal data results and to generate manuscript figures. 

For internal EcoHealth Alliance staff with access to the full eidith dataset:

- `human-global-data-cleaning.R` includes code for cleaning the global PREDICT eidith dataset, focusing on the human_2 and events_2 tables

- `human-civ-data-analysis.R` includes code for subsetting, cleaning, and analyzing the Côte d'Ivoire human and site data from the eidith dataset

For external collaborators without access to the full eidith dataset:

- `human-civ-deidentified-data-analysis.R` includes code for analyzing the de-identified Côte d'Ivoire subset available upon request on Zenodo
 
