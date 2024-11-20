# Ethnic_Segregation_Civic_Organizations
This repository contains the replication materials for the paper "Brittle bridges: ethnic segregation across and within civic organizations". Please contact me if you have any questions.

To replicate our findings, please open the file 02_Scripts/BrittleBridges_MasterRScript.R”. 
There, you will find more detailed instructions about the order in which the separate scripts and Do-files need to be run. 

Our scripts run best after creating the following folder structure:
yourpath/BrittleBridges_replication/01_Data
yourpath/BrittleBridges_replication/01_Data/01_codebooks
yourpath/BrittleBridges_replication/01_Data/02_raw
yourpath/BrittleBridges_replication/01_Data/03_edit
yourpath/BrittleBridges_replication/02_Scripts
yourpath/BrittleBridges_replication/03_Outputs

The LISS data can be accessed through the LISS archive (https://www.dataarchive.lissdata.nl). Please populate the following subfolders in the replication package with the respective LISS data files:

02_raw / 2011: avars_201111_EN_2.0p.dta (background variables), cp11d_1.0p_EN.dta (Personality), cr11d_EN_1.0p.dta (Religion and Ethnicity), cs11d_EN_3.0p.dta (Social Integration and Leisure), cs12e_1.0p_EN.dta (Social Integration and Leisure), eu11_EN1.0p.dta (Civic Participation 2011, single-wave study)
02_raw / 2017_2018: ox17a_EN_1.0p.dta (Family Survey Dutch Population), oz18a_EN_1.0p.dta (Family Survey Dutch Population)

The SIM data can be accessed through the DANS archive (https://ssh.datastations.nl/dataset.xhtml?persistentId=doi:10.17026/dans-26h-xn4n). Please add the file SIM2020 CCF.sav to the following folder in the replication package: 02_raw / SIM
