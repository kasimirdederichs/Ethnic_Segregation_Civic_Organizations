# MASTER R SCRIPT: Brittle Bridges: Ethnic Segregation Across and Within Civic Organizations ####
# Dederichs, Kasimir and Dingeman Wiertz (2024): "Brittle Bridges: Ethnic Segregation Across and Within Civic Organizations", European Sociological Review

#  Set working directory / load libraries: ####
setwd(dir = "yourpath/BrittleBridges_replication")

#  libraries
packages_vector <- c("foreign", "haven", "dplyr", "ggplot2", "tidyfast", "tidyr", "purrr", "plyr",
                     "data.table", "sjlabelled", "DescTools", "grid", "psych", "tidyverse", "readxl",
                     "ggpubr", "stargazer", "patchwork", "forcats", "ggpattern", "effects", "corrplot",
                     "lmtest", "sandwich", "mediation")
lapply(packages_vector, library, character.only=T)

#First, run R scripts:
source("02_Scripts/BrittleBridges_LISS2017_data_preparation.R", echo = T) #data preparation of 2017 data
source("02_Scripts/BrittleBridges_LISS2011_data_preparation_and_mediation.R", echo = T) #data preparation of 2011 data and mediation analysis

#Second, run Stata do-files (LISS_2011_analysis.do and LISS_2017_analysis.do) to produce the analyses.

#Third, run R scripts to produce figures:
source("02_Scripts/BrittleBridges_LISS2017_results.R", echo = T) #analysis based on 2017 data
source("02_Scripts/BrittleBridges_SIM_benchmark.R", echo = T) #benchmarking civic organizations against other settings (SIM data)



#Where to find what:
# Main tables and figures:
## Fig 1: LISS2017_results.R
## Tab 1: LISS2017_analysis.do
## Tab 2: LISS2011_analysis.do
## Tab 3: LISS2011_data_preparation_and_mediation.R

# Supplement:
## Tab A1: LISS2017_analysis.do
## Tab A2: LISS2017_analysis.do
## Fig A3: SIM_benchmark.R
## Fig A4: LISS2017_results.R
## Fig A5: LISS2017_results.R
## Tab A6: LISS2017_data_preparation.R
## Tab B1: LISS2011_analysis.do
## Tab B2: LISS2011_analysis.do
## Tab B3: LISS2011_data_preparation_and_mediation.R