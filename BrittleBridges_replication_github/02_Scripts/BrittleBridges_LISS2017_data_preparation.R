# In this do file we first prepare the 2017 and 2018 LISS data modules and then merge them together.

#  load datasets ####
d17 <- read_dta("01_Data/02_raw/2017_2018/ox17a_EN_1.0p.dta")
d18 <- read_dta("01_Data/02_raw/2017_2018/oz18a_EN_1.0p.dta")

#  2017 data ####
## Recoding basic variables (demographics, involvement in organizations) ####
d17 <- d17 %>%
  mutate(female = case_when(ox17a001==1 ~ 0, ox17a001==2 ~ 1),
         birthyear = ox17a002,
         age = ox17a003) %>%
  dplyr::select(-c("ox17a001", "ox17a002", "ox17a003"))

PercTable(d17$ox17a_m, margins = c(1,2))
#99.8% interviewed in October 2017

#Apply table command to different variables:
vars_3_ <- c("ox17a090", "ox17a095", "ox17a100", "ox17a105", "ox17a110", "ox17a115", 
             "ox17a120", "ox17a125", "ox17a130", "ox17a135", "ox17a140", "ox17a145", "ox17a150")
vars_4_ <- c("ox17a091", "ox17a096", "ox17a101", "ox17a106", "ox17a111", "ox17a116", 
             "ox17a121", "ox17a126", "ox17a131", "ox17a136", "ox17a141", "ox17a146", "ox17a151")

sapply(d17[vars_3_], table, useNA="always")
sapply(d17[vars_4_], table, useNA="always")
# All looks good; all of these variables have 3,090 respondents (i.e. 9 NA's).

# Creating involvement indicators by organization type:
setnames(d17, 
         old = c("ox17a090", "ox17a095", "ox17a100", "ox17a105", "ox17a110", "ox17a115", 
                 "ox17a120", "ox17a125", "ox17a130", "ox17a135", "ox17a140", "ox17a145", "ox17a150"), 
         new = c("memb_pol", "memb_rel", "memb_sch", "memb_cul", "memb_spo", "memb_nbh", 
                 "memb_wor", "memb_env", "memb_aid", "memb_dev", "memb_hum", "memb_fai", "memb_soc"))
setnames(d17, 
         old = c("ox17a091", "ox17a096", "ox17a101", "ox17a106", "ox17a111", "ox17a116", 
                 "ox17a121", "ox17a126", "ox17a131", "ox17a136", "ox17a141", "ox17a146", "ox17a151"), 
         new = c("volun_pol", "volun_rel", "volun_sch", "volun_cul", "volun_spo", "volun_nbh", 
                 "volun_wor", "volun_env", "volun_aid", "volun_dev", "volun_hum", "volun_fai", "volun_soc"))

#assign labels to variables:
d17 <- d17 %>%
  var_labels(memb_pol = "Member of a political association", 
             memb_rel = "Member of a religious association",
             memb_sch = "Member of a school or youth association",
             memb_cul = "Member of a music/drama/hobby association",
             memb_spo = "Member of a sports association",
             memb_nbh = "Member of a neighbourhood association",
             memb_wor = "Member of a work or interest association",
             memb_env = "Member of a environmental association",
             memb_aid = "Member of a emergency aid association",
             memb_dev = "Member of a development aid association",
             memb_hum = "Member of a human rights association",
             memb_fai = "Member of a fair trade association",
             memb_soc = "Member of a societal association",
             volun_pol = "Volunteer for a political association",
             volun_rel = "Volunteer for a religious association",
             volun_sch = "Volunteer for a school or youth association",
             volun_cul = "Volunteer for a music/drama/hobby association",
             volun_spo = "Volunteer for a sports association",
             volun_nbh = "Volunteer for a neighbourhood association",
             volun_wor = "Volunteer for a work or interest association",
             volun_env = "Volunteer for a environmental association",
             volun_aid = "Volunteer for a emergency aid association",
             volun_dev = "Volunteer for a development aid association",
             volun_hum = "Volunteer for a human rights association",
             volun_fai = "Volunteer for a fair trade association",
             volun_soc = "Volunteer for a societal association")

get_label(d17) #show labels

#create variables that indicate whether one is a member or volunteers for a type of organization:
d17 <- d17 %>%
  mutate(memvol_pol = case_when(memb_pol==0 & volun_pol==0 ~ 0, memb_pol==1 | volun_pol==1 ~ 1),
         memvol_rel = case_when(memb_rel==0 & volun_rel==0 ~ 0, memb_rel==1 | volun_rel==1 ~ 1),
         memvol_sch = case_when(memb_sch==0 & volun_sch==0 ~ 0, memb_sch==1 | volun_sch==1 ~ 1),
         memvol_cul = case_when(memb_cul==0 & volun_cul==0 ~ 0, memb_cul==1 | volun_cul==1 ~ 1),
         memvol_spo = case_when(memb_spo==0 & volun_spo==0 ~ 0, memb_spo==1 | volun_spo==1 ~ 1),
         memvol_nbh = case_when(memb_nbh==0 & volun_nbh==0 ~ 0, memb_nbh==1 | volun_nbh==1 ~ 1),
         memvol_wor = case_when(memb_wor==0 & volun_wor==0 ~ 0, memb_wor==1 | volun_wor==1 ~ 1),
         memvol_env = case_when(memb_env==0 & volun_env==0 ~ 0, memb_env==1 | volun_env==1 ~ 1),
         memvol_aid = case_when(memb_aid==0 & volun_aid==0 ~ 0, memb_aid==1 | volun_aid==1 ~ 1),
         memvol_dev = case_when(memb_dev==0 & volun_dev==0 ~ 0, memb_dev==1 | volun_dev==1 ~ 1),
         memvol_hum = case_when(memb_hum==0 & volun_hum==0 ~ 0, memb_hum==1 | volun_hum==1 ~ 1),
         memvol_fai = case_when(memb_fai==0 & volun_fai==0 ~ 0, memb_fai==1 | volun_fai==1 ~ 1),
         memvol_soc = case_when(memb_soc==0 & volun_soc==0 ~ 0, memb_soc==1 | volun_soc==1 ~ 1))

#Generating general membership and volunteering indicators:
d17 <- d17 %>%
  mutate(memb = case_when(memb_pol==0 & memb_rel==0 & memb_sch==0 & memb_cul==0 & memb_spo==0 & memb_nbh==0 & memb_wor==0 & 
                          memb_env==0 & memb_aid==0 & memb_dev==0 & memb_hum==0 & memb_fai==0 & memb_soc==0 ~ 0,
                          memb_pol==1 | memb_rel==1 | memb_sch==1 | memb_cul==1 | memb_spo==1 | memb_nbh==1 | memb_wor==1 | 
                          memb_env==1 | memb_aid==1 | memb_dev==1 | memb_hum==1 | memb_fai==1 | memb_soc==1 ~ 1),
         volun = case_when(volun_pol==0 & volun_rel==0 & volun_sch==0 & volun_cul==0 & volun_spo==0 & volun_nbh==0 & volun_wor==0 & 
                           volun_env==0 & volun_aid==0 & volun_dev==0 & volun_hum==0 & volun_fai==0 & volun_soc==0 ~ 0,
                           volun_pol==1 | volun_rel==1 | volun_sch==1 | volun_cul==1 | volun_spo==1 | volun_nbh==1 | volun_wor==1 | 
                           volun_env==1 | volun_aid==1 | volun_dev==1 | volun_hum==1 | volun_fai==1 | volun_soc==1 ~ 1),
         memvol = case_when(memb==0 & volun==0 ~ 0, memb==1 | volun==1 ~ 1)) %>%
  var_labels(memb = "Whether one is member of any organization",
             volun = "Whether one is volunteer for any organization",
             memvol = "Whether one is member / volunteer for any org")

PercTable(d17$memb, d17$volun, margins = c(1,2))
# 57% of all respondents are members (n = 1,764); 
# 25% of all respondents are volunteers (n = 770); 
# 64% of all respondents are members / volunteers (n = 1,991); 
# 18% of all respondents are members & volunteers (n = 543); 
# 17% of all non-members are volunteers (n = 227); 
# 31% of all members are volunteers (n = 543).
PercTable(d17$memvol, margins = c(1,2))

#Indicators for numbers of organizations:
d17 <- d17 %>%
  mutate(norg_memb = memb_pol + memb_rel + memb_sch + memb_cul + memb_spo + memb_nbh + memb_wor + memb_env + memb_aid + memb_dev + memb_hum + memb_fai + memb_soc,
         norg_volun = volun_pol + volun_rel + volun_sch + volun_cul + volun_spo + volun_nbh + volun_wor + volun_env + volun_aid + volun_dev + volun_hum + volun_fai + volun_soc,
         norg_memvol = memvol_pol + memvol_rel + memvol_sch + memvol_cul + memvol_spo + memvol_nbh + memvol_wor + memvol_env + memvol_aid + memvol_dev + memvol_hum + memvol_fai + memvol_soc) %>%
  var_labels(norg_memb = "Number of organizations one is member of",
             norg_volun = "Number of organizations one volunteers for",
             norg_memvol = "Number of orgs one is somehow involved in")
PercTable(d17$norg_memb, margins = c(1,2))
PercTable(d17$norg_volun, margins = c(1,2))
PercTable(d17$norg_memvol, margins = c(1,2))
# Membership: 26% one, 17% two, 8% three, 6% four or more; 
# Volunteering: 18% one, 5% two, 2% three or more; 
# Combined: 28% one, 19% two, 10% three, 8% four or more; 
# 57% of members/volunteers involved in >1 org type. */


#Indicator for main organization:
PercTable(d17$ox17a153, margins = c(1,2))
PercTable(d17$norg_memvol, d17$ox17a153, useNA = "a")
#There are 2 meaningful missings on this variable.

#label types of organization
PercTable(d17$ox17a153, margins=c(1,2))

d17 <- d17 %>%
  mutate(mo_1_org = as.numeric(ifelse(norg_memvol==1, case_when(memvol_pol==1 ~ 1, memvol_rel==1 ~ 2,
                                                      memvol_sch==1 ~ 3, memvol_cul==1 ~ 4,
                                                      memvol_spo==1 ~ 5, memvol_nbh==1 ~ 6,
                                                      memvol_wor==1 ~ 7, memvol_env==1 ~ 8,
                                                      memvol_aid==1 ~ 9, memvol_dev==1 ~ 10,
                                                      memvol_hum==1 ~ 11, memvol_fai==1 ~ 12,
                                                      memvol_soc==1 ~ 13), NA)),
         mo = as.factor(case_when(norg_memvol==1 ~ mo_1_org,
                                  norg_memvol!=1 ~ as.numeric(ox17a153))))
levels(d17$mo) <- c("Political association", "Religious association", 
                         "School / youth association", "Music / drama / hobby assoc", "Sports association",
                         "Neighbourhood association" , "Work / interest association" , "Environmental association",
                         "Emergency aid association", "Development aid association", "Human rights association", 
                         "Fair trade association" , "Societal association")
PercTable(d17$mo, margins=c(1,2))
#Most popular main organizations: Sports (40%), work/interest (17%), religious (11%).

d17 <- d17 %>%
  mutate(memb_mo = ifelse(is.na(memb), NA_real_, 
                               ifelse((norg_memvol==1 & memb==1) | 
                                      (norg_memvol>1 & ((mo=="Political association" & memb_pol==1) | (mo=="Religious association" & memb_rel==1) |
                                      (mo=="School / youth association" & memb_sch==1) | (mo=="Music / drama / hobby assoc" & memb_cul==1) |
                                      (mo=="Sports association" & memb_spo==1) | (mo=="Neighbourhood association" & memb_nbh==1) |
                                      (mo=="Work / interest association" & memb_wor==1) | (mo=="Environmental association" & memb_env==1) |
                                      (mo=="Emergency aid association" & memb_aid==1) | (mo=="Development aid association" & memb_dev==1) |
                                      (mo=="Human rights association" & memb_hum==1) | (mo=="Fair trade association" & memb_fai==1) |
                                      (mo=="Societal association" & memb_soc==1))), 1, 0)),
         volun_mo = ifelse(is.na(volun), NA_real_, 
                               ifelse((norg_memvol==1 & volun==1) | 
                                        (norg_memvol>1 & ((mo=="Political association" & volun_pol==1) | (mo=="Religious association" & volun_rel==1) |
                                                            (mo=="School / youth association" & volun_sch==1) | (mo=="Music / drama / hobby assoc" & volun_cul==1) |
                                                            (mo=="Sports association" & volun_spo==1) | (mo=="Neighbourhood association" & volun_nbh==1) |
                                                            (mo=="Work / interest association" & volun_wor==1) | (mo=="Environmental association" & volun_env==1) |
                                                            (mo=="Emergency aid association" & volun_aid==1) | (mo=="Development aid association" & volun_dev==1) |
                                                            (mo=="Human rights association" & volun_hum==1) | (mo=="Fair trade association" & volun_fai==1) |
                                                            (mo=="Societal association" & volun_soc==1))), 1, 0)),
         memvol_mo = ifelse(is.na(memvol), NA_real_, 
                                ifelse((norg_memvol==1 & memb==1 & volun==1) | 
                                         (norg_memvol>1 & ((mo=="Political association"& memb_pol==1  & volun_pol==1) | (mo=="Religious association" & memb_rel==1 & volun_rel==1) |
                                                             (mo=="School / youth association" & memb_sch==1 & volun_sch==1) | (mo=="Music / drama / hobby assoc" & memb_cul==1 & volun_cul==1) |
                                                             (mo=="Sports association" & memb_spo==1 & volun_spo==1) | (mo=="Neighbourhood association" & memb_nbh==1 & volun_nbh==1) |
                                                             (mo=="Work / interest association" & memb_wor==1 & volun_wor==1) | (mo=="Environmental association" & memb_env==1 & volun_env==1) |
                                                             (mo=="Emergency aid association" & memb_aid==1 & volun_aid==1) | (mo=="Development aid association" & memb_dev==1 & volun_dev==1) |
                                                             (mo=="Human rights association" & memb_hum==1 & volun_hum==1) | (mo=="Fair trade association" & memb_fai==1 & volun_fai==1) |
                                                             (mo=="Societal association" & memb_soc==1 & volun_soc==1))), 1, 0)),
         memvol1_mo = case_when(memb_mo==1 | volun_mo==1 ~ 1,
                                     memb_mo==0 & volun_mo==0 ~ 0)) %>%
  var_labels(memb_mo = "Whether one is member of one's main org",
             volun_mo = "Whether one is volunteer for one's main org",
             memvol_mo = "Whether one is member & volunteer for one's main org",
             memvol1_mo = "Whether one is member and/or volunteer for one's main org")
#There are 2/1 missings more in my coding because I treat all those have missings on the memb/volun-variable as missings (regardless of whether they indicate a main organization or not).
PercTable(d17$memb_mo, margins = c(1,2))
PercTable(d17$volun_mo, margins = c(1,2))
PercTable(d17$memvol_mo, margins = c(1,2))
# 53% are members of their main organization (n = 1,647); 21% volunteers (n = 639); 10% members & volunteers (n = 297). 
# Conditional on being involved, shares are 83%, 32% and 15%. 
# These percentages overlap: n = 1,350 are ONLY member of main organization, n = 342 ONLY volunteers, n = 297 are both.

## Composition and contact variables: ####

PercTable(d17$ox17a154, d17$memvol, useNA = "a", margins = c(1,2)) #educational composition
PercTable(d17$ox17a155, d17$memvol, useNA = "a", margins = c(1,2)) #educational contact
PercTable(d17$ox17a156, d17$memvol, useNA = "a", margins = c(1,2)) #ethnic composition
PercTable(d17$ox17a157, d17$memvol, useNA = "a", margins = c(1,2)) #ethnic contact


d17 <- d17 %>%
  mutate(edu_comp = factor(case_when(ox17a154==1 ~ "Almost none", ox17a154==2 ~ "Less than 5%", ox17a154==3 ~ "5-10%", ox17a154==4 ~ "10-25%", ox17a154==5 ~ "25-50%", ox17a154==6 ~ "50-75%", ox17a154==7 ~ "75-100%"), 
                           levels = c("Almost none", "Less than 5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-100%")) ,
         edu_comp_3cat = factor(case_when(ox17a154>=1 & ox17a154<=3 ~ "Less than 10%", ox17a154==4 | ox17a154==5 ~ "10-50%", ox17a154==6 | ox17a154==7 ~ "50-100%"),
                                levels = c("Less than 10%", "10-50%", "50-100%")),
         eth_comp = factor(case_when(ox17a156==1 ~ "Almost none", ox17a156==2 ~ "Less than 5%", ox17a156==3 ~ "5-10%", ox17a156==4 ~ "10-25%", ox17a156==5 ~ "25-50%", ox17a156==6 ~ "50-75%", ox17a156==7 ~ "75-100%"),
                           levels = c("Almost none", "Less than 5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-100%")),
         eth_comp_3cat = factor(case_when(ox17a156==1 ~ "Almost none", ox17a156==2 | ox17a156==3 ~ "0-10%", ox17a156>=4 & ox17a156<=7 ~ "More than 10%"),
                           levels = c("Almost none", "0-10%", "More than 10%")),
         edu_cont = factor(case_when(ox17a155==1 ~ "Every Week", ox17a155==2 ~ "Every Month", ox17a155==3 ~ "Once a year", ox17a155==4 ~ "Never"),
                           levels = c("Never", "Once a year", "Every Month", "Every Week")),
         eth_cont = factor(case_when(ox17a157==1 ~ "Every Week", ox17a157==2 ~ "Every Month", ox17a157==3 ~ "Once a year", ox17a157==4 ~ "Never"),
                           levels = c("Never", "Once a year", "Every Month", "Every Week"))) %>%
  var_labels(edu_comp = "% of members with higher education",
             edu_comp_3cat = "% members with higher education - 3 cats",
             eth_comp = "% of members with TMSA origins",
             eth_comp_3cat = "% members with TMSA origins - 3 cats",
             edu_cont = "Contact frequency with members with higher education",
             eth_cont = "Contact frequency with members with TMSA origins")

PercTable(d17$edu_comp, d17$edu_comp_3cat, margins = c(1,2))
PercTable(d17$eth_comp, d17$eth_comp_3cat, margins = c(1,2))
PercTable(d17$edu_cont, margins = c(1,2))
PercTable(d17$eth_cont, margins = c(1,2))


## Education and Ethnicity variables ####
#Educational attainment variable:

PercTable(d17$ox17a079, useNA = "a", margins = c(1,2))
PercTable(d17$ox17a080, margins = c(1,2))
d17 <- d17 %>%
  mutate(edu_4cat = factor(case_when(ox17a079>=1 & ox17a079<=10 ~ "Up to lower secondary",
                                     ox17a079>=11 & ox17a079<=19 ~ "Higher secondary or lower tertiary",
                              ox17a079>=20 & ox17a079<=22 ~ "Medium tertiary",
                              ox17a079>=23 & ox17a079<=27 ~ "University",
                              ox17a079==28 & (ox17a080==1 |ox17a080==2 | ox17a080==3) ~ "Up to lower secondary",
                              ox17a079==28 & (ox17a080==4 | ox17a080==5) ~ "Higher secondary or lower tertiary",
                              ox17a079==28 & ox17a080==6 ~ "Medium tertiary",
                              ox17a079==28 & ox17a080==7 ~ "University"),
                           levels = c("Up to lower secondary", "Higher secondary or lower tertiary", "Medium tertiary", "University")))
PercTable(d17$edu_4cat, margins = c(1,2))
# 24% up to lower secondary; 36% higher secondary or lower tertiary; 24% medium tertiary; 17% university.

#Ethnicity variable:
PercTable(d17$ox17a063, margins = c(1,2))
PercTable(d17$ox17a065, margins = c(1,2))
PercTable(d17$ox17a068, margins = c(1,2))
PercTable(d17$ox17a066, d17$ox17a068, margins = c(1,2))

d17 <- d17 %>%
  mutate(eth_4catB = factor(case_when(ox17a063==1 ~ "Dutch", 
                                      ox17a063==5 | ox17a063==6 ~ "Suriname/Antilles",
                                      ox17a063==3 | ox17a063==4 ~ "Turkey/Morocco",
                                      ox17a063>6 | ox17a063==2 ~ "Other"), 
                            levels = c("Dutch", "Suriname/Antilles", "Turkey/Morocco", "Other")))
d17$eth_4catB[(d17$ox17a066==5 | d17$ox17a066==6) | (d17$ox17a068==5 | d17$ox17a068==6)] <- "Suriname/Antilles"
d17$eth_4catB[(d17$ox17a066==3 | d17$ox17a066==4) | (d17$ox17a068==3 | d17$ox17a068==4)] <- "Turkey/Morocco"
d17$eth_4catB[(d17$ox17a066==2 | d17$ox17a066==7 | d17$ox17a066==8 | d17$ox17a066==9 | d17$ox17a066==10) | 
                (d17$ox17a068==2 | d17$ox17a068==7 | d17$ox17a068==8 | d17$ox17a068==9 | d17$ox17a068==10)] <- "Other"

d17 <- d17 %>%
  mutate(eth_3catB = factor(case_when(eth_4catB== "Dutch" ~ "Dutch origin", eth_4catB=="Suriname/Antilles" | eth_4catB=="Turkey/Morocco" ~ "TMSA origin", eth_4catB=="Other" ~ "Other origin"), 
                            levels = c("Dutch origin", "TMSA origin", "Other origin")))
PercTable(table(d17$eth_4catB))
# We count people with one Dutch-born parent are counted as minority: 
# 80% of sample has Dutch origins (n = 2,682); 4% of sample has Surinamese / Antillean origins (n = 126); 
# 2% of sample has Turkish / Moroccan origins (n = 51); 15% of sample has "other" origins (n = 458).

## Add information on urbanicity from background variables ####
bckgrdvars17 <- read_dta("01_Data/02_raw/2017_2018/avars_201710_EN_1.0p.dta")
d17 <- merge(d17, bckgrdvars17[, c("nomem_encr", "sted")], by = "nomem_encr", all.x = T, all.y = F)

## Subsetting the data that we want to merge with the 2018 data: ####
d17 <- d17 %>%
  dplyr::select(c("nomem_encr", "ox17a_m", "female", "birthyear", "age", "edu_4cat",
             "eth_4catB", "eth_3catB" ,
             "memb_pol", "memb_rel", "memb_sch", "memb_cul", "memb_spo", "memb_nbh",
             "memb_wor", "memb_env", "memb_aid", "memb_dev", "memb_hum", "memb_fai", 
             "memb_soc", "volun_pol", "volun_rel", "volun_sch", "volun_cul", "volun_spo", 
             "volun_nbh", "volun_wor", "volun_env", "volun_aid", "volun_dev", 
             "volun_hum", "volun_fai", "volun_soc", 
             "memvol_pol", "memvol_rel", "memvol_sch", "memvol_cul", "memvol_spo", 
             "memvol_nbh", "memvol_wor", "memvol_env", "memvol_aid", "memvol_dev", 
             "memvol_hum", "memvol_fai", "memvol_soc",
             "memb", "volun", "memvol", "norg_memb", "norg_volun", "norg_memvol",
             "mo", "memb_mo", "volun_mo", "memvol_mo", "memvol1_mo",
             "edu_comp", "edu_comp_3cat", "eth_comp", "eth_comp_3cat", 
             "edu_cont", "eth_cont", "sted"))

d17 <- d17 %>% 
  rename_with( ~ paste0("w1_", .x)) 
colnames(d17)[1] <- "nomem_encr" #keep ID variable as it was

d17 <- d17 %>% filter(!is.na(w1_memb))


#  2018 data ####
## Inspecting and Recoding basic variables (civic trajectories): ####
PercTable(d18$oz18a001, margins = c(1,2))
# We have 2,610 reinterviewees and 555 new interviewees, for a total of 3,165 interviews and a re-interview rate of 82%.

PercTable(d18$oz18a001, d18$oz18a003, rfrq = "110", margins = c(1,2))
# Of the reinterviewees, 64% reported to be a member/volunteer in wave 1 (compared to a membership/volunteering rate of 64% 
# in wave 1); n = 1,664. This is good news: seemingly no selective attrition by civic involvement status.

PercTable(d18$oz18a001, d18$oz18a_m, rfrq = "110", margins = c(1,2))
# 92% of those who took part in the 1st wave were interviewed in May 2018 (7 months after 1st interview); rest in June.

# Exploring civic transitions vis-a-vis Wave 1:

PercTable(d18$oz18a007, d18$oz18a155, rfrq = "110", margins = c(1,2))
# Of those involved in an organization in wave 1, 79% (n = 1,313) are still involved in their main organization: 
# 50% as member, 12% as volunteer, 17% as member & volunteer. We have 351 people (21%) leaving their main organization. 
# Among the most popular organizations, the exit rate varies from 6% for religious orgs to 25% for sports associations.

#General involvement classification in wave 2:

d18 <- d18 %>%
  mutate(auxoz18a155 = replace(oz18a155, is.na(oz18a155), 0),
         auxoz18a165 = replace(oz18a165, is.na(oz18a165), 0),
         auxoz18a174 = replace(oz18a174, is.na(oz18a174), 0),
         civtraj = auxoz18a155)

d18$civtraj[d18$auxoz18a155==4] <- 3 + d18$auxoz18a165[d18$auxoz18a155==4]
d18$civtraj[d18$auxoz18a174!=0 & d18$oz18a001==1] <- 7 + d18$auxoz18a174[d18$auxoz18a174!=0 & d18$oz18a001==1]
d18$civtraj[d18$civtraj==0] <- NA

d18 <- d18 %>%
  mutate(civtraj_5cat = case_when(civtraj>=1 & civtraj<=3 ~ "Still involved in main org",
                                  civtraj>=4 & civtraj<=6 ~ "Switched to different org",
                                  civtraj==7 ~ "Leaving without switching",
                                  civtraj>=8 & civtraj<=10 ~ "Getting involved",
                                  civtraj==11 ~ "Still not involved"))

d18$civtraj <- as.factor(d18$civtraj)
levels(d18$civtraj) <- c("Stay - Member",  "Stay - Volunteer",  "Stay - Member & volunteer",  "Switch - Member", 
                         "Switch - Volunteer",  "Switch - Member & volunteer",  "Stop - No new organization", 
                         "Start - Member", "Start - Volunteer", "Start - Member & volunteer",  "Still not involved" )
PercTable(d18$civtraj, margins = c(1))
PercTable(d18$civtraj_5cat, margins = c(1))

d18 <- d18%>%
  var_labels(civtraj = "Characterization of civic involvement in wave 2",
             civtraj_5cat = "Characterization of involvement in wave 2 (5 cats)")

## Additional information for different trajectories (composition and contact) ####
#Additional information for "stayers":
d18$stay_edu_comp <- d18$oz18a156
d18$stay_edu_comp[d18$oz18a156>7] <- NA

d18$stay_eth_comp <- d18$oz18a159
d18$stay_eth_comp[d18$oz18a159>7] <- NA

PercTable(d18$stay_edu_comp, margins = c(1))
PercTable(d18$stay_eth_comp, margins = c(1))

d18 <- d18 %>%
  mutate(stay_edu_comp_3cat = factor(case_when(stay_edu_comp>=1 & stay_edu_comp<=3 ~ "Less than 10%",
                                        stay_edu_comp==4 | stay_edu_comp==5 ~ "10-50%",
                                        stay_edu_comp==6 | stay_edu_comp==7 ~ "50-100%"),
                                     levels = c("Less than 10%", "10-50%", "50-100%")),
         stay_eth_comp_3cat = factor(case_when(stay_eth_comp==1 ~ "Almost none",
                                        stay_eth_comp==2 | stay_eth_comp==3 ~ "0-10%",
                                        stay_eth_comp>=4 & stay_eth_comp<=7 ~ "More than 10%"),
                                     levels = c("Almost none", "0-10%", "More than 10%")),
         stay_edu_cont = factor(case_when(oz18a157==1 ~ "Every Week", oz18a157==2 ~ "Every Month",
                                          oz18a157==3 ~ "Once a year", oz18a157==4 ~ "Never"),
                                levels = c("Never", "Once a year", "Every Month", "Every Week")),
         stay_eth_cont = factor(case_when(oz18a160==1 ~ "Every Week", oz18a160==2 ~ "Every Month",
                                          oz18a160==3 ~ "Once a year", oz18a160==4 ~ "Never"),
                                levels = c("Never", "Once a year", "Every Month", "Every Week")),
         stay_edu_rcont = factor(case_when(oz18a158==1 ~ "Much more often",
                                           oz18a158==2 ~ "Slightly more often",
                                           oz18a158==3 ~ "About as often",
                                           oz18a158==4 ~ "Slightly less often",
                                           oz18a158==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", 
                                 "About as often", "Slightly more often", "Much more often")),
         stay_eth_rcont = factor(case_when(oz18a161==1 ~ "Much more often",
                                           oz18a161==2 ~ "Slightly more often",
                                           oz18a161==3 ~ "About as often",
                                           oz18a161==4 ~ "Slightly less often",
                                           oz18a161==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", 
                                 "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(stay_edu_comp = "% of members with higher education (stayers)",
             stay_eth_comp = "% of members with TMSA origins (stayers)",
             stay_edu_comp_3cat = "% of members with higher education - 3 cats (stayers)",
             stay_eth_comp_3cat = "% of members with TMSA origins - 3 cats (stayers)",
             stay_edu_cont = "Contact freq with members with higher education (stayers)",
             stay_eth_cont = "Contact freq with members with TMSA origins (stayers)",
             stay_edu_rcont = "Relative contact freq with members with higher educ (stayers)",
             stay_eth_rcont = "Relative contact freq with members of TMSA origins (stayers)")
  
# Additional information for "stoppers": 
d18 <- d18 %>%
  mutate(stop_reas = factor(case_when(oz18a162==1 ~ "Lack of resources",
                                      oz18a162==2 ~ "Lack of interest",
                                      oz18a162==3 ~ "Lack of gratifying interaction",
                                      oz18a162==4 ~ "Organization no longer exists"),
                            levels = c("Lack of resources", "Lack of interest", "Lack of gratifying interaction", "Organization no longer exists")),
         stop_edu_rcont = factor(case_when(oz18a163==1 ~ "Much more often",
                                           oz18a163==2 ~ "Slightly more often",
                                           oz18a163==3 ~ "About as often",
                                           oz18a163==4 ~ "Slightly less often",
                                           oz18a163==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often")),
         stop_eth_rcont = factor(case_when(oz18a164==1 ~ "Much more often",
                                           oz18a164==2 ~ "Slightly more often",
                                           oz18a164==3 ~ "About as often",
                                           oz18a164==4 ~ "Slightly less often",
                                           oz18a164==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(stop_reas = "Reason for stopping involvement",
             stop_edu_rcont = "Relative contact freq with members with higher educ (stoppers)",
             stop_eth_rcont = "Relative contact freq with members of TMSA origins (stoppers)")

PercTable(d18$stop_edu_rcont)
PercTable(d18$stop_eth_rcont)

# Additional information for "switchers":
d18 <- d18 %>%
  mutate(switch_neworg = case_when(oz18a166==1 ~ "Political association",
                                   oz18a166==2 ~ "Religious association",
                                   oz18a166==3 ~ "School / youth association",
                                   oz18a166==4 ~ "Music / drama / hobby association",
                                   oz18a166==5 ~ "Sports association",
                                   oz18a166==6 ~ "Neighbourhood association",
                                   oz18a166==7 ~ "Work / interest association",
                                   oz18a166==8 ~ "Environmental association",
                                   oz18a166==9 ~ "Emergency association",
                                   oz18a166==10 ~ "Development aid association",
                                   oz18a166==11 ~ "Human rights association",
                                   oz18a166==12 ~ "Fair trade association",
                                   oz18a166==13 ~ "Societal association"),
         switch_path = case_when(oz18a167==1 ~ "Asked by a friend / family member",
                                 oz18a167==2 ~ "Asked by someone else",
                                 oz18a167==3 ~ "Reached out on own initiative")) %>%
  var_labels(switch_neworg = "Type of organization joined (switchers)",
             switch_path = "Pathway into new organization (switchers)")

d18$switch_edu_comp <- d18$oz18a168
d18$switch_edu_comp[d18$oz18a168>7] <- NA
d18$switch_eth_comp <- d18$oz18a171
d18$switch_eth_comp[d18$oz18a171>7] <- NA

d18 <- d18 %>%
  var_labels(switch_edu_comp = "% of members with higher education (switchers)",
             switch_eth_comp = "% of members with TMSA origins (switchers)")


d18 <- d18 %>%
  mutate(switch_edu_comp_3cat = factor(case_when(switch_edu_comp>=1 & switch_edu_comp<=3 ~ "Less than 10%",
                                          switch_edu_comp==4 | switch_edu_comp==5 ~ "10-50%",
                                          switch_edu_comp==6 | switch_edu_comp==7 ~ "50-100%"),
                                       levels = c("Less than 10%", "10-50%", "50-100%")),
         switch_eth_comp_3cat = factor(case_when(switch_eth_comp==1 ~ "Almost none",
                                          switch_eth_comp==2 | switch_eth_comp==3 ~ "0-10%",
                                          switch_eth_comp>=4 & switch_eth_comp<=7 ~ "More than 10%"),
                                       levels = c("Almost none", "0-10%", "More than 10%")),
         switch_edu_cont = factor(case_when(oz18a169==1 ~ "Every Week", oz18a169==2 ~ "Every Month",
                                            oz18a169==3 ~ "Once a year", oz18a169==4 ~ "Never"),
                                  levels = c("Never", "Once a year", "Every Month", "Every Week")),
         switch_eth_cont = factor(case_when(oz18a172==1 ~ "Every Week", oz18a172==2 ~ "Every Month",
                                            oz18a172==3 ~ "Once a year", oz18a172==4 ~ "Never"),
                                  levels = c("Never", "Once a year", "Every Month", "Every Week")),
         switch_edu_rcont = factor(case_when(oz18a170==1 ~ "Much more often",
                                             oz18a170==2 ~ "Slightly more often",
                                             oz18a170==3 ~ "About as often",
                                             oz18a170==4 ~ "Slightly less often",
                                             oz18a170==5 ~ "Much less often"),
                                   levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often")),
         switch_eth_rcont = factor(case_when(oz18a173==1 ~ "Much more often",
                                             oz18a173==2 ~ "Slightly more often",
                                             oz18a173==3 ~ "About as often",
                                             oz18a173==4 ~ "Slightly less often",
                                             oz18a173==5 ~ "Much less often"),
                                   levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(switch_edu_comp = "% of members with higher education (switchers)",
             switch_eth_comp = "% of members with TMSA origins (switchers)",
             switch_edu_comp_3cat = "% of members with higher education - 3 cats (switchers)",
             switch_eth_comp_3cat = "% of members with TMSA origins - 3 cats (switchers)",
             switch_edu_cont = "Contact freq with members with higher education (switchers)",
             switch_eth_cont = "Contact freq with members with TMSA origins (switchers)",
             switch_edu_rcont = "Relative contact freq with members with higher educ (switchers)",
             switch_eth_rcont = "Relative contact freq with members of TMSA origins (switchers)")


# Additional information for "starters":
d18 <- d18 %>%
  mutate(start_neworg = case_when(oz18a175==1 ~ "Political association",
                                  oz18a175==2 ~ "Religious association",
                                  oz18a175==3 ~ "School / youth association",
                                  oz18a175==4 ~ "Music / drama / hobby association",
                                  oz18a175==5 ~ "Sports association",
                                  oz18a175==6 ~ "Neighbourhood association",
                                  oz18a175==7 ~ "Work / interest association",
                                  oz18a175==8 ~ "Environmental association",
                                  oz18a175==9 ~ "Emergency association",
                                  oz18a175==10 ~ "Development aid association",
                                  oz18a175==11 ~ "Human rights association",
                                  oz18a175==12 ~ "Fair trade association",
                                  oz18a175==13 ~ "Societal association"),
         start_path = case_when(oz18a176==1 ~ "Asked by a friend / family member",
                                oz18a176==2 ~ "Asked by someone else",
                                oz18a176==3 ~ "Reached out on own initiative")) %>%
  var_labels(start_neworg = "Type of organization joined (starters)",
             start_path = "Pathway into new organization (starters)")

PercTable(d18$start_neworg)
PercTable(d18$start_path)

d18$start_edu_comp <- d18$oz18a177
d18$start_edu_comp[d18$oz18a177>7] <- NA
d18$start_eth_comp <- d18$oz18a180
d18$start_eth_comp[d18$oz18a180>7] <- NA

d18 <- d18 %>%
  var_labels(start_edu_comp = "% of members with higher education (starters)",
             start_eth_comp = "% of members with TMSA origins (starters)")

d18 <- d18 %>%
  mutate(start_edu_comp_3cat = factor(case_when(start_edu_comp>=1 & start_edu_comp<=3 ~ "Less than 10%",
                                         start_edu_comp==4 | start_edu_comp==5 ~ "10-50%",
                                         start_edu_comp==6 | start_edu_comp==7 ~ "50-100%"),
                                      levels = c("Less than 10%", "10-50%", "50-100%")),
         start_eth_comp_3cat = factor(case_when(start_eth_comp==1 ~ "Almost none",
                                          start_eth_comp==2 | start_eth_comp==3 ~ "0-10%",
                                          start_eth_comp>=4 & start_eth_comp<=7 ~ "More than 10%"),
                                      levels = c("Almost none", "0-10%", "More than 10%")),
         start_edu_cont = factor(case_when(oz18a178==1 ~ "Every Week", oz18a178==2 ~ "Every Month",
                                           oz18a178==3 ~ "Once a year", oz18a178==4 ~ "Never"),
                                 levels = c("Never", "Once a year", "Every Month", "Every Week")),
         start_eth_cont = factor(case_when(oz18a181==1 ~ "Every Week", oz18a181==2 ~ "Every Month",
                                           oz18a181==3 ~ "Once a year", oz18a181==4 ~ "Never"),
                                 levels = c("Never", "Once a year", "Every Month", "Every Week")),
         start_edu_rcont = factor(case_when(oz18a179==1 ~ "Much more often",
                                            oz18a179==2 ~ "Slightly more often",
                                            oz18a179==3 ~ "About as often",
                                            oz18a179==4 ~ "Slightly less often",
                                            oz18a179==5 ~ "Much less often"),
                                  levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often")),
         start_eth_rcont = factor(case_when(oz18a182==1 ~ "Much more often",
                                            oz18a182==2 ~ "Slightly more often",
                                            oz18a182==3 ~ "About as often",
                                            oz18a182==4 ~ "Slightly less often",
                                            oz18a182==5 ~ "Much less often"),
                                  levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(start_edu_comp = "% of members with higher education (starters)",
             start_eth_comp = "% of members with TMSA origins (starters)",
             start_edu_comp_3cat = "% of members with higher education - 3 cats (starters)",
             start_eth_comp_3cat = "% of members with TMSA origins - 3 cats (starters)",
             start_edu_cont = "Contact freq with members with higher education (starters)",
             start_eth_cont = "Contact freq with members with TMSA origins (starters)",
             start_edu_rcont = "Relative contact freq with members with higher educ (starters)",
             start_eth_rcont = "Relative contact freq with members of TMSA origins (starters)")

## Add information on urbanicity from background variables ####
bckgrdvars18 <- read_dta("01_Data/02_raw/2017_2018/avars_201805_EN_1.0p.dta")
d18 <- merge(d18, bckgrdvars18[, c("nomem_encr", "sted")], by = "nomem_encr", all.x = T, all.y = F)

## Subsetting the data that we want to merge with the 2018 data: ####
d18 <- d18 %>%
  dplyr::select(c("nomem_encr", "oz18a_m", "oz18a001", "oz18a003", "oz18a007",
             "civtraj", "civtraj_5cat", "stay_edu_comp", "stay_edu_comp_3cat",
             "stay_eth_comp", "stay_eth_comp_3cat", "stay_edu_cont", "stay_edu_rcont",
             "stay_eth_cont", "stay_eth_rcont", "stop_reas", "stop_edu_rcont", "stop_eth_rcont",
             "switch_path", "switch_neworg", "switch_edu_comp", "switch_edu_comp_3cat", 
             "switch_eth_comp", "switch_eth_comp_3cat", "switch_edu_cont", "switch_edu_rcont", 
             "switch_eth_cont", "switch_eth_rcont", "start_path", "start_neworg", 
             "start_edu_comp", "start_edu_comp_3cat", "start_eth_comp", "start_eth_comp_3cat",
             "start_edu_cont", "start_edu_rcont", "start_eth_cont", "start_eth_rcont", "sted"))

d18 <- d18 %>% 
  rename_with( ~ paste0("w2_", .x)) 
colnames(d18)[1] <- "nomem_encr" #keep ID variable as it was
colnames(d18)[8:39] <- c("stay_edu_comp", "stay_edu_comp_3cat",
                      "stay_eth_comp", "stay_eth_comp_3cat", "stay_edu_cont", "stay_edu_rcont",
                      "stay_eth_cont", "stay_eth_rcont", "stop_reas", "stop_edu_rcont", "stop_eth_rcont",
                      "switch_path", "switch_neworg", "switch_edu_comp", "switch_edu_comp_3cat", 
                      "switch_eth_comp", "switch_eth_comp_3cat", "switch_edu_cont", "switch_edu_rcont", 
                      "switch_eth_cont", "switch_eth_rcont", "start_path", "start_neworg", 
                      "start_edu_comp", "start_edu_comp_3cat", "start_eth_comp", "start_eth_comp_3cat",
                      "start_edu_cont", "start_edu_rcont", "start_eth_cont", "start_eth_rcont", "sted")

d18 <- d18 %>% filter(!is.na(w2_civtraj))

#  Merging the 2017 and 2018 data + save: ####

d <- merge(d17, d18, by = c("nomem_encr"), all.x = T, all.y = F) # merge, keep only observations that are available in 2017 
PercTable(d$w2_oz18a001, useNA = "a", margins = c(1,2)) 
#2594 individuals who participated in w1 and w2, 496 individuals who participated only in w1

d <- d %>%
  var_labels(w2_oz18a001 = "Participated in wave 1 and 2")

#  A few descriptives based on 2017 data: ####
## Educational segregation 
# Adding the contact benchmarks from wave 2:
d <- d %>%
  mutate(w1_edu_rcont = case_when(!is.na(stay_edu_rcont) ~ stay_edu_rcont,
                                  !is.na(stop_edu_rcont) ~ stop_edu_rcont)) %>%
  var_labels(w1_edu_rcont = "Relative contact frequency with members with higher ed")

## Ethnic segregation 
# Contact segregation: 
PercTable(d$w1_eth_cont, d$w1_eth_3catB, rfrq="001", margins = c(1,2))

# Adding the contact benchmarks from wave 2:
d <- d %>%
  mutate(w1_eth_rcont = case_when(!is.na(stay_eth_rcont) ~ stay_eth_rcont,
                                  !is.na(stop_eth_rcont) ~ stop_eth_rcont)) %>%
  var_labels(w1_edu_rcont = "Relative contact frequency with TMSA minority members")
PercTable(d$w1_eth_rcont, d$w1_eth_3catB, rfrq="001", margins = c(1,2))


# Preparing variables for Fig. 1 (Membership and quitting differentials) ####
#Prepare variables:
d <- d %>%
  mutate(w1_mo_edu_comp_memb = factor(case_when(w1_memb_mo==0 ~ "Not a member",
                                                w1_memb_mo==1 & w1_edu_comp_3cat=="Less than 10%" ~ "Less than 10%",
                                                w1_memb_mo==1 & w1_edu_comp_3cat=="10-50%" ~ "10-50%",
                                                w1_memb_mo==1 & w1_edu_comp_3cat=="50-100%" ~ "50-100%"), 
                                      levels = c( "Not a member", "Less than 10%", "10-50%", "50-100%")),
         w1_mo_eth_comp_memb = factor(case_when(w1_memb_mo==0 ~ "Not a member",
                                                w1_memb_mo==1 & w1_eth_comp_3cat=="Almost none" ~ "Almost none",
                                                w1_memb_mo==1 & w1_eth_comp_3cat=="0-10%" ~ "0-10%",
                                                w1_memb_mo==1 & w1_eth_comp_3cat=="More than 10%" ~ "More than 10%"), 
                                      levels = c( "Not a member", "Almost none", "0-10%", "More than 10%")),
         w1_mo_edu_comp_volun = factor(case_when(w1_volun_mo==0 ~ "Not a volunteer",
                                                 w1_volun_mo==1 & w1_edu_comp_3cat=="Less than 10%" ~ "Less than 10%",
                                                 w1_volun_mo==1 & w1_edu_comp_3cat=="10-50%" ~ "10-50%",
                                                 w1_volun_mo==1 & w1_edu_comp_3cat=="50-100%" ~ "50-100%"), 
                                       levels = c("Not a volunteer", "Less than 10%", "10-50%", "50-100%")),
         w1_mo_eth_comp_volun = factor(case_when(w1_volun_mo==0 ~ "Not a volunteer",
                                                 w1_volun_mo==1 & w1_eth_comp_3cat=="Almost none" ~ "Almost none",
                                                 w1_volun_mo==1 & w1_eth_comp_3cat=="0-10%" ~ "0-10%",
                                                 w1_volun_mo==1 & w1_eth_comp_3cat=="More than 10%" ~ "More than 10%"), 
                                       levels = c("Not a volunteer", "Almost none", "0-10%", "More than 10%")),
         w1_mo_edu_comp_memvol1 = factor(case_when(w1_memvol1_mo==0 ~ "Not a member/volunteer",
                                                   w1_memvol1_mo==1 & w1_edu_comp_3cat=="Less than 10%" ~ "Less than 10%",
                                                   w1_memvol1_mo==1 & w1_edu_comp_3cat=="10-50%" ~ "10-50%",
                                                   w1_memvol1_mo==1 & w1_edu_comp_3cat=="50-100%" ~ "50-100%"), 
                                         levels = c("Not a member/volunteer", "Less than 10%", "10-50%", "50-100%")),
         w1_mo_eth_comp_memvol1 = factor(case_when(w1_memvol1_mo==0 ~ "Not a member/volunteer",
                                                   w1_memvol1_mo==1 & w1_eth_comp_3cat=="Almost none" ~ "Almost none",
                                                   w1_memvol1_mo==1 & w1_eth_comp_3cat=="0-10%" ~ "0-10%",
                                                   w1_memvol1_mo==1 & w1_eth_comp_3cat=="More than 10%" ~ "More than 10%"), 
                                         levels = c("Not a member/volunteer", "Almost none", "0-10%", "More than 10%")))

#define levels for composition variables:
edu_comp_vector_memb <- rep(c("Not a member", "Less than 10%", "10-50%", "50-100%"), each = 4)
edu_comp_levels_memb <- c("Not a member", "Less than 10%", "10-50%", "50-100%")
eth_comp_vector_memb <- rep(c("Not a member", "Almost none", "0-10%", "More than 10%"), each = 3)
eth_comp_levels_memb <- c("Not a member", "Almost none", "0-10%", "More than 10%")
edu_comp_vector_volun <- rep(c("Not a volunteer", "Less than 10%", "10-50%", "50-100%"), each = 4)
edu_comp_levels_volun <- c("Not a volunteer", "Less than 10%", "10-50%", "50-100%")
eth_comp_vector_volun <- rep(c("Not a volunteer", "Almost none", "0-10%", "More than 10%"), each = 3)
eth_comp_levels_volun <- c("Not a volunteer", "Almost none", "0-10%", "More than 10%")
edu_comp_vector_memvol1 <- rep(c("Not a member/volunteer", "Less than 10%", "10-50%", "50-100%"), each = 4)
edu_comp_levels_memvol1 <- c("Not a member/volunteer", "Less than 10%", "10-50%", "50-100%")
eth_comp_vector_memvol1 <- rep(c("Not a member/volunteer", "Almost none", "0-10%", "More than 10%"), each = 3)
eth_comp_levels_memvol1 <- c("Not a member/volunteer", "Almost none", "0-10%", "More than 10%")

edu_4cat_levels <- c("Up to lower secondary", "Higher secondary or lower tertiary", "Medium tertiary", "University")
eth_3catB_levels <- c("Dutch origin", "TMSA origin", "Other origin")

## quitting 
#I only look at membership and/or volunteering combined (i.e., the variable memvol1), as the N is already quite low.
table(d$w1_eth_3catB, d$w1_mo_eth_comp_memvol1) 
# too few TMSA respondents in organizations with different ethnic compositions at risk of quitting. 

#create indicator variables for people of different ethnic groups in organizations with different ethnic compositions
d <- d %>%
  mutate(w2_quit_mo = case_when(w2_civtraj_5cat=="Leaving without switching" | w2_civtraj_5cat=="Switched to different org" ~ 1,
                                w2_civtraj_5cat=="Still involved in main org" ~ 0),
         w1_mo_eth_comp_memvol1_an = case_when(w1_mo_eth_comp_memvol1=="Almost none" ~ 1,
                                               w1_mo_eth_comp_memvol1 %in% c("0-10%", "More than 10%") ~ 0),
         w1_mo_eth_comp_memvol1_0_10 = case_when(w1_mo_eth_comp_memvol1=="0-10%" ~ 1,
                                                 w1_mo_eth_comp_memvol1 %in% c("Almost none", "More than 10%") ~ 0),
         w1_mo_eth_comp_memvol1_more_10 = case_when(w1_mo_eth_comp_memvol1=="More than 10%" ~ 1,
                                                    w1_mo_eth_comp_memvol1 %in% c("Almost none", "0-10%") ~ 0),
         w1_eth_3catB_num = as.numeric(w1_eth_3catB),
         w1_eth_3catB_an = as.factor(w1_eth_3catB_num * w1_mo_eth_comp_memvol1_an),
         w1_eth_3catB_0_10 = as.factor(w1_eth_3catB_num * w1_mo_eth_comp_memvol1_0_10),
         w1_eth_3catB_more_10 = as.factor(w1_eth_3catB_num * w1_mo_eth_comp_memvol1_more_10))

table(d$w1_eth_3catB_num, d$w1_eth_3catB_0_10, useNA = "a")

#binary education variable:
d <- d %>%
  mutate(w1_edu_2cat = factor(case_when(w1_edu_4cat=="Up to lower secondary" | w1_edu_4cat=="Higher secondary or lower tertiary" ~ "Less than HBO", 
                                        w1_edu_4cat=="Medium tertiary" | w1_edu_4cat== "University" ~ "HBO or higher"), 
                              levels = c("Less than HBO", "HBO or higher")))

# Export/Import data to stata to conduct analyses ####
write.dta(d, file = "01_Data/03_edit/2017_2018/LISS2017_for_stata_import.dta")

# Analysis of differential recruitment pathways (as robustness check) ####

d_dutch <- d %>% filter(w1_eth_3catB=="Dutch origin") %>%
  mutate(start_eth_comp_2cat = case_when(start_eth_comp<=2 ~ "Joins a Dutch organization", start_eth_comp>=3 ~ "Joins a diverse organization"))
d_dutch$start_eth_comp_2cat <- fct_relevel(d_dutch$start_eth_comp_2cat,"Joins a Dutch organization", "Joins a diverse organization")
PercTable(d_dutch$start_path, d_dutch$start_eth_comp_2cat, rfrq = "001", margins = c(1,2))
d_dutch <- d_dutch %>%
  mutate(start_2cat = case_when(start_path == "Asked by a friend / family member" | start_path == "Asked by someone else" ~ "Recruited through social network",
                                start_path == "Reached out on own initiative" ~ "Reached out on own initiative"),
         start_2cat_n = case_when(start_path == "Asked by a friend / family member" | start_path == "Asked by someone else" ~ 1,
                                  start_path == "Reached out on own initiative" ~ 0))

#Table A6 in Online supplement:
PercTable(d_dutch$start_2cat, d_dutch$start_eth_comp_2cat, rfrq = "001", margins = c(1,2))

