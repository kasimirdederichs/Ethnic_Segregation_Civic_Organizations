# In this do file we first prepare the 2017 and 2018 LISS data modules and then merge them together.

#  load datasets ####
d17 <- read_dta("01_Data/02_raw/2017_2018/BNB 2017.dta")
d18 <- read_dta("01_Data/02_raw/2017_2018/BNB 2018.dta")

#  2017 data ####
## Recoding basic variables (demographics, involvement in organizations) ####
d17 <- d17 %>%
  mutate(female = case_when(geslacht==1 ~ 0, geslacht==2 ~ 1),
         birthyear = gebjaar,
         age = leeftijd) %>%
  dplyr::select(-c("geslacht", "gebjaar", "leeftijd"))

PercTable(d17$maandnr, margins = c(1,2))
#99.8% interviewed in October 2017

#Apply table command to different variables:
vars_3_ <- c(paste0("vn1_t", 1:13, "_3_"))
vars_4_ <- c(paste0("vn1_t", 1:13, "_4_"))

sapply(d17[vars_3_], table, useNA="always")
sapply(d17[vars_4_], table, useNA="always")
# All looks good; all of these variables have 3,090 respondents (i.e. 9 NA's).

PercTable(d17$vn1_t1_3_, d17$vn1_t1_4_, rfrq = "110", margins = c(1,2)) # the same for all other combinations!
# Interesting to note that many volunteers are no members. 

# Creating involvement indicators by organization type:
setnames(d17, 
         old = c("vn1_t1_3_", "vn1_t2_3_", "vn1_t3_3_", "vn1_t4_3_", "vn1_t5_3_", "vn1_t6_3_", "vn1_t7_3_", "vn1_t8_3_", "vn1_t9_3_", "vn1_t10_3_", "vn1_t11_3_", "vn1_t12_3_", "vn1_t13_3_"), 
         new = c("memb_pol", "memb_rel", "memb_sch", "memb_cul", "memb_spo", "memb_nbh", "memb_wor", "memb_env", "memb_aid", "memb_dev", "memb_hum", "memb_fai", "memb_soc"))
setnames(d17, 
         old = c("vn1_t1_4_", "vn1_t2_4_", "vn1_t3_4_", "vn1_t4_4_", "vn1_t5_4_", "vn1_t6_4_", "vn1_t7_4_", "vn1_t8_4_", "vn1_t9_4_", "vn1_t10_4_", "vn1_t11_4_", "vn1_t12_4_", "vn1_t13_4_"), 
         new = c("volun_pol", "volun_rel", "volun_sch", "volun_cul", "volun_spo", "volun_nbh", "volun_wor", "volun_env", "volun_aid", "volun_dev", "volun_hum", "volun_fai", "volun_soc"))

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
PercTable(d17$vn2, margins = c(1,2))
PercTable(d17$norg_memvol, d17$vn2, useNA = "a")
#There are 2 meaningful missings on this variable.

#label types of organization
PercTable(d17$vn2, margins=c(1,2))

d17 <- d17 %>%
  mutate(mo_1_org = as.numeric(ifelse(norg_memvol==1, case_when(memvol_pol==1 ~ 1, memvol_rel==1 ~ 2,
                                                      memvol_sch==1 ~ 3, memvol_cul==1 ~ 4,
                                                      memvol_spo==1 ~ 5, memvol_nbh==1 ~ 6,
                                                      memvol_wor==1 ~ 7, memvol_env==1 ~ 8,
                                                      memvol_aid==1 ~ 9, memvol_dev==1 ~ 10,
                                                      memvol_hum==1 ~ 11, memvol_fai==1 ~ 12,
                                                      memvol_soc==1 ~ 13), NA)),
         mo = as.factor(case_when(norg_memvol==1 ~ mo_1_org,
                                  norg_memvol!=1 ~ as.numeric(vn2))))
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

PercTable(d17$vn3, d17$memvol, useNA = "a", margins = c(1,2)) #educational composition
PercTable(d17$vn4, d17$memvol, useNA = "a", margins = c(1,2)) #educational contact
PercTable(d17$vn6, d17$memvol, useNA = "a", margins = c(1,2)) #ethnic composition
PercTable(d17$vn7, d17$memvol, useNA = "a", margins = c(1,2)) #ethnic contact


d17 <- d17 %>%
  mutate(edu_comp = factor(case_when(vn3==1 ~ "Almost none", vn3==2 ~ "Less than 5%", vn3==3 ~ "5-10%", vn3==4 ~ "10-25%", vn3==5 ~ "25-50%", vn3==6 ~ "50-75%", vn3==7 ~ "75-100%"), 
                           levels = c("Almost none", "Less than 5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-100%")) ,
         edu_comp_3cat = factor(case_when(vn3>=1 & vn3<=3 ~ "Less than 10%", vn3==4 | vn3==5 ~ "10-50%", vn3==6 | vn3==7 ~ "50-100%"),
                                levels = c("Less than 10%", "10-50%", "50-100%")),
         eth_comp = factor(case_when(vn6==1 ~ "Almost none", vn6==2 ~ "Less than 5%", vn6==3 ~ "5-10%", vn6==4 ~ "10-25%", vn6==5 ~ "25-50%", vn6==6 ~ "50-75%", vn6==7 ~ "75-100%"),
                           levels = c("Almost none", "Less than 5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-100%")),
         eth_comp_3cat = factor(case_when(vn6==1 ~ "Almost none", vn6==2 | vn6==3 ~ "0-10%", vn6>=4 & vn6<=7 ~ "More than 10%"),
                           levels = c("Almost none", "0-10%", "More than 10%")),
         edu_cont = factor(case_when(vn4==1 ~ "Every Week", vn4==2 ~ "Every Month", vn4==3 ~ "Once a year", vn4==4 ~ "Never"),
                           levels = c("Never", "Once a year", "Every Month", "Every Week")),
         eth_cont = factor(case_when(vn7==1 ~ "Every Week", vn7==2 ~ "Every Month", vn7==3 ~ "Once a year", vn7==4 ~ "Never"),
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

PercTable(d17$ve1, useNA = "a", margins = c(1,2))
PercTable(d17$ve2, margins = c(1,2))
PercTable(d17$ve1, d17$oplmet, useNA = "a", margins = c(1,2))
d17 <- d17 %>%
  mutate(edu_4cat = factor(case_when(ve1>=1 & ve1<=10 ~ "Up to lower secondary",
                              ve1>=11 & ve1<=19 ~ "Higher secondary or lower tertiary",
                              ve1>=20 & ve1<=22 ~ "Medium tertiary",
                              ve1>=23 & ve1<=27 ~ "University",
                              ve1==28 & (ve2==1 |ve2==2 | ve2==3) ~ "Up to lower secondary",
                              ve1==28 & (ve2==4 | ve2==5) ~ "Higher secondary or lower tertiary",
                              ve1==28 & ve2==6 ~ "Medium tertiary",
                              ve1==28 & ve2==7 ~ "University"),
                           levels = c("Up to lower secondary", "Higher secondary or lower tertiary", "Medium tertiary", "University")))
PercTable(d17$edu_4cat, margins = c(1,2))
# 24% up to lower secondary; 36% higher secondary or lower tertiary; 24% medium tertiary; 17% university.

#Ethnicity variable:
PercTable(d17$va3, margins = c(1,2))
PercTable(d17$va4, margins = c(1,2))
PercTable(d17$va5, margins = c(1,2))
PercTable(d17$va4, d17$va5, margins = c(1,2))

d17$immig <- 0
d17$immig[is.na(d17$va3)] <- NA
d17$immig[d17$va3>1 & !is.na(d17$va3) & d17$va4!=1 & d17$va5!=1] <- 1
d17$immig[d17$va3==1 & ((d17$va4!=1 & !is.na(d17$va4)) | (d17$va5!=1 & !is.na(d17$va5)))] <- 2

d17$immig <- as.factor(d17$immig)
levels(d17$immig) <- c("Dutch origin", "1st gen immigrant", "2nd gen immigrant")

# 81% of sample has two Dutch-born parents (n = 2,464); 9% of sample is a 1st generation immigrant (n = 286); 
# 10% of sample is a 2nd generation immigrant (n = 297). So, that is 583 immigrants in total (19%). 

d17 <- d17 %>%
  mutate(eth_4catB = factor(case_when(va3==1 ~ "Dutch", 
                               va3==5 | va3==6 ~ "Suriname/Antilles",
                               va3==3 | va3==4 ~ "Turkey/Morocco",
                               va3>6 | va3==2 ~ "Other"), 
                            levels = c("Dutch", "Suriname/Antilles", "Turkey/Morocco", "Other")))
d17$eth_4catB[(d17$va4==5 | d17$va4==6) | (d17$va5==5 | d17$va5==6)] <- "Suriname/Antilles"
d17$eth_4catB[(d17$va4==3 | d17$va4==4) | (d17$va5==3 | d17$va5==4)] <- "Turkey/Morocco"
d17$eth_4catB[(d17$va4==2 | d17$va4==7 | d17$va4==8 | d17$va4==9 | d17$va4==10) | 
                (d17$va5==2 | d17$va5==7 | d17$va5==8 | d17$va5==9 | d17$va5==10)] <- "Other"

d17 <- d17 %>%
  mutate(eth_3catB = factor(case_when(eth_4catB== "Dutch" ~ "Dutch origin", eth_4catB=="Suriname/Antilles" | eth_4catB=="Turkey/Morocco" ~ "TMSA origin", eth_4catB=="Other" ~ "Other origin"), 
                            levels = c("Dutch origin", "TMSA origin", "Other origin")))

# We count people with one Dutch-born parent are counted as minority: 
# 80% of sample has Dutch origins (n = 2,682); 4% of sample has Surinamese / Antillean origins (n = 126); 
# 2% of sample has Turkish / Moroccan origins (n = 51); 15% of sample has "other" origins (n = 458).

## Subsetting the data that we want to merge with the 2018 data: ####

d17 <- d17 %>%
  dplyr::select(c("nomem_encr", "maandnr", "female", "birthyear", "age", "edu_4cat",
             "immig", "eth_4catB", "eth_3catB" ,
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
PercTable(d18$PL0, margins = c(1,2))
# We have 2,610 reinterviewees and 555 new interviewees, for a total of 3,165 interviews and a re-interview rate of 82%.

PercTable(d18$PL0, d18$PL10, rfrq = "110", margins = c(1,2))
# Of the reinterviewees, 64% reported to be a member/volunteer in wave 1 (compared to a membership/volunteering rate of 64% 
# in wave 1); n = 1,664. This is good news: seemingly no selective attrition by civic involvement status.

PercTable(d18$PL0, d18$maandnr, rfrq = "110", margins = c(1,2))
# 92% of those who took part in the 1st wave were interviewed in May 2018 (7 months after 1st interview); rest in June.

# Exploring civic transitions vis-a-vis Wave 1:

PercTable(d18$p_organisatie, d18$DW1, rfrq = "110", margins = c(1,2))
# Of those involved in an organization in wave 1, 79% (n = 1,313) are still involved in their main organization: 
# 50% as member, 12% as volunteer, 17% as member & volunteer. We have 351 people (21%) leaving their main organization. 
# Among the most popular organizations, the exit rate varies from 6% for religious orgs to 25% for sports associations.

PercTable(d18$DW11, rfrq = "111", margins = c(1,2))
PercTable(d18$p_organisatie, d18$DW12, rfrq = "111", margins = c(1,2))
PercTable(d18$DW13, rfrq = "111", margins = c(1,2))
# Of the 351 leavers, 15% (n = 52) has joined a new org. Somewhat surprisingly, as much as one third of these 
# "switchers" have joined a "societal" organization. 62% of the switchers joined the new organization on their 
# own initiative; 38% after being asked to join.

PercTable(d18$DW20, d18$PL0, rfrq = "101", margins = c(1,2))
# Of the 930 people who were not civically involved in wave 1, 82% is still not involved, but 18% joined (n = 163). 
# The 546 people who were not interviewed in wave 1 are less useful (we do not know when they joined), but from them 40% 
# have been involved as member/volunteer. Interestingly, this is much less than the involvement rate of 64% in wave 1, 
# suggesting that the new entrants constitute a substantively different sample than the sample of initial respondents.
# Or that there are question wording/ordering effects or something like that. 

PercTable(d18$DW22, d18$PL0, rfrq = "101", margins = c(1,2))
# Of the 163 recent joiners, 50% joined on their own initiative and 50% after being asked (so "own initiative" channel is 
# slightly less common than among "switchers").

#General involvement classification in wave 2:

d18 <- d18 %>%
  mutate(auxDW1 = replace(DW1, is.na(DW1), 0),
         auxDW11 = replace(DW11, is.na(DW11), 0),
         auxDW20 = replace(DW20, is.na(DW20), 0),
         civtraj = auxDW1)

d18$civtraj[d18$auxDW1==4] <- 3 + d18$auxDW11[d18$auxDW1==4]
d18$civtraj[d18$auxDW20!=0 & d18$PL0==1] <- 7 + d18$auxDW20[d18$auxDW20!=0 & d18$PL0==1]
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

## Additional informatoin for different trajectories (composition and contact) ####
#Additional information for "stayers":
d18$stay_edu_comp <- d18$DW2
d18$stay_edu_comp[d18$DW2>7] <- NA

d18$stay_eth_comp <- d18$DW5
d18$stay_eth_comp[d18$DW5>7] <- NA

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
         stay_edu_cont = factor(case_when(DW3==1 ~ "Every Week", DW3==2 ~ "Every Month",
                                   DW3==3 ~ "Once a year", DW3==4 ~ "Never"),
                                levels = c("Never", "Once a year", "Every Month", "Every Week")),
         stay_eth_cont = factor(case_when(DW6==1 ~ "Every Week", DW6==2 ~ "Every Month",
                                   DW6==3 ~ "Once a year", DW6==4 ~ "Never"),
                                levels = c("Never", "Once a year", "Every Month", "Every Week")),
         stay_edu_rcont = factor(case_when(DW4==1 ~ "Much more often",
                                    DW4==2 ~ "Slightly more often",
                                    DW4==3 ~ "About as often",
                                    DW4==4 ~ "Slightly less often",
                                    DW4==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", 
                                 "About as often", "Slightly more often", "Much more often")),
         stay_eth_rcont = factor(case_when(DW7==1 ~ "Much more often",
                                    DW7==2 ~ "Slightly more often",
                                    DW7==3 ~ "About as often",
                                    DW7==4 ~ "Slightly less often",
                                    DW7==5 ~ "Much less often"),
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
  mutate(stop_reas = factor(case_when(DW8==1 ~ "Lack of resources",
                               DW8==2 ~ "Lack of interest",
                               DW8==3 ~ "Lack of gratifying interaction",
                               DW8==4 ~ "Organization no longer exists"),
                            levels = c("Lack of resources", "Lack of interest", "Lack of gratifying interaction", "Organization no longer exists")),
         stop_edu_rcont = factor(case_when(DW9==1 ~ "Much more often",
                                    DW9==2 ~ "Slightly more often",
                                    DW9==3 ~ "About as often",
                                    DW9==4 ~ "Slightly less often",
                                    DW9==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often")),
         stop_eth_rcont = factor(case_when(DW10==1 ~ "Much more often",
                                    DW10==2 ~ "Slightly more often",
                                    DW10==3 ~ "About as often",
                                    DW10==4 ~ "Slightly less often",
                                    DW10==5 ~ "Much less often"),
                                 levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(stop_reas = "Reason for stopping involvement",
             stop_edu_rcont = "Relative contact freq with members with higher educ (stoppers)",
             stop_eth_rcont = "Relative contact freq with members of TMSA origins (stoppers)")

PercTable(d18$stop_edu_rcont)
PercTable(d18$stop_eth_rcont)

# Additional information for "switchers":

d18 <- d18 %>%
  mutate(switch_neworg = case_when(DW12==1 ~ "Political association",
                                   DW12==2 ~ "Religious association",
                                   DW12==3 ~ "School / youth association",
                                   DW12==4 ~ "Music / drama / hobby association",
                                   DW12==5 ~ "Sports association",
                                   DW12==6 ~ "Neighbourhood association",
                                   DW12==7 ~ "Work / interest association",
                                   DW12==8 ~ "Environmental association",
                                   DW12==9 ~ "Emergency association",
                                   DW12==10 ~ "Development aid association",
                                   DW12==11 ~ "Human rights association",
                                   DW12==12 ~ "Fair trade association",
                                   DW12==13 ~ "Societal association"),
         switch_path = case_when(DW13==1 ~ "Asked by a friend / family member",
                                 DW13==2 ~ "Asked by someone else",
                                 DW13==3 ~ "Reached out on own initiative")) %>%
  var_labels(switch_neworg = "Type of organization joined (switchers)",
             switch_path = "Pathway into new organization (switchers)")

PercTable(d18$switch_neworg)
PercTable(d18$switch_path)

d18$switch_edu_comp <- d18$DW14
d18$switch_edu_comp[d18$DW14>7] <- NA
d18$switch_eth_comp <- d18$DW17
d18$switch_eth_comp[d18$DW17>7] <- NA

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
         switch_edu_cont = factor(case_when(DW15==1 ~ "Every Week", DW15==2 ~ "Every Month",
                                   DW15==3 ~ "Once a year", DW15==4 ~ "Never"),
                                  levels = c("Never", "Once a year", "Every Month", "Every Week")),
         switch_eth_cont = factor(case_when(DW18==1 ~ "Every Week", DW18==2 ~ "Every Month",
                                   DW18==3 ~ "Once a year", DW18==4 ~ "Never"),
                                  levels = c("Never", "Once a year", "Every Month", "Every Week")),
         switch_edu_rcont = factor(case_when(DW16==1 ~ "Much more often",
                                    DW16==2 ~ "Slightly more often",
                                    DW16==3 ~ "About as often",
                                    DW16==4 ~ "Slightly less often",
                                    DW16==5 ~ "Much less often"),
                                   levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often")),
         switch_eth_rcont = factor(case_when(DW19==1 ~ "Much more often",
                                    DW19==2 ~ "Slightly more often",
                                    DW19==3 ~ "About as often",
                                    DW19==4 ~ "Slightly less often",
                                    DW19==5 ~ "Much less often"),
                                   levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(switch_edu_comp = "% of members with higher education (switchers)",
             switch_eth_comp = "% of members with TMSA origins (switchers)",
             switch_edu_comp_3cat = "% of members with higher education - 3 cats (switchers)",
             switch_eth_comp_3cat = "% of members with TMSA origins - 3 cats (switchers)",
             switch_edu_cont = "Contact freq with members with higher education (switchers)",
             switch_eth_cont = "Contact freq with members with TMSA origins (switchers)",
             switch_edu_rcont = "Relative contact freq with members with higher educ (switchers)",
             switch_eth_rcont = "Relative contact freq with members of TMSA origins (switchers)")

PercTable(d18$switch_edu_rcont)
PercTable(d18$switch_eth_rcont)

# Additional information for "starters":

d18 <- d18 %>%
  mutate(start_neworg = case_when(DW21==1 ~ "Political association",
                                   DW21==2 ~ "Religious association",
                                   DW21==3 ~ "School / youth association",
                                   DW21==4 ~ "Music / drama / hobby association",
                                   DW21==5 ~ "Sports association",
                                   DW21==6 ~ "Neighbourhood association",
                                   DW21==7 ~ "Work / interest association",
                                   DW21==8 ~ "Environmental association",
                                   DW21==9 ~ "Emergency association",
                                   DW21==10 ~ "Development aid association",
                                   DW21==11 ~ "Human rights association",
                                   DW21==12 ~ "Fair trade association",
                                   DW21==13 ~ "Societal association"),
         start_path = case_when(DW22==1 ~ "Asked by a friend / family member",
                                 DW22==2 ~ "Asked by someone else",
                                 DW22==3 ~ "Reached out on own initiative")) %>%
  var_labels(start_neworg = "Type of organization joined (starters)",
             start_path = "Pathway into new organization (starters)")

PercTable(d18$start_neworg)
PercTable(d18$start_path)

d18$start_edu_comp <- d18$DW23
d18$start_edu_comp[d18$DW23>7] <- NA
d18$start_eth_comp <- d18$DW26
d18$start_eth_comp[d18$DW26>7] <- NA

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
         start_edu_cont = factor(case_when(DW24==1 ~ "Every Week", DW24==2 ~ "Every Month",
                                     DW24==3 ~ "Once a year", DW24==4 ~ "Never"),
                                 levels = c("Never", "Once a year", "Every Month", "Every Week")),
         start_eth_cont = factor(case_when(DW27==1 ~ "Every Week", DW27==2 ~ "Every Month",
                                     DW27==3 ~ "Once a year", DW27==4 ~ "Never"),
                                 levels = c("Never", "Once a year", "Every Month", "Every Week")),
         start_edu_rcont = factor(case_when(DW25==1 ~ "Much more often",
                                      DW25==2 ~ "Slightly more often",
                                      DW25==3 ~ "About as often",
                                      DW25==4 ~ "Slightly less often",
                                      DW25==5 ~ "Much less often"),
                                  levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often")),
         start_eth_rcont = factor(case_when(DW28==1 ~ "Much more often",
                                      DW28==2 ~ "Slightly more often",
                                      DW28==3 ~ "About as often",
                                      DW28==4 ~ "Slightly less often",
                                      DW28==5 ~ "Much less often"),
                                  levels = c("Much less often", "Slightly less often", "About as often", "Slightly more often", "Much more often"))) %>%
  var_labels(start_edu_comp = "% of members with higher education (starters)",
             start_eth_comp = "% of members with TMSA origins (starters)",
             start_edu_comp_3cat = "% of members with higher education - 3 cats (starters)",
             start_eth_comp_3cat = "% of members with TMSA origins - 3 cats (starters)",
             start_edu_cont = "Contact freq with members with higher education (starters)",
             start_eth_cont = "Contact freq with members with TMSA origins (starters)",
             start_edu_rcont = "Relative contact freq with members with higher educ (starters)",
             start_eth_rcont = "Relative contact freq with members of TMSA origins (starters)")

## Subsetting the data that we want to merge with the 2017 data: ####

d18 <- d18 %>%
  dplyr::select(c("nomem_encr", "maandnr", "PL0", "PL10", "p_organisatie",
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
colnames(d18)[8:38] <- c("stay_edu_comp", "stay_edu_comp_3cat",
                      "stay_eth_comp", "stay_eth_comp_3cat", "stay_edu_cont", "stay_edu_rcont",
                      "stay_eth_cont", "stay_eth_rcont", "stop_reas", "stop_edu_rcont", "stop_eth_rcont",
                      "switch_path", "switch_neworg", "switch_edu_comp", "switch_edu_comp_3cat", 
                      "switch_eth_comp", "switch_eth_comp_3cat", "switch_edu_cont", "switch_edu_rcont", 
                      "switch_eth_cont", "switch_eth_rcont", "start_path", "start_neworg", 
                      "start_edu_comp", "start_edu_comp_3cat", "start_eth_comp", "start_eth_comp_3cat",
                      "start_edu_cont", "start_edu_rcont", "start_eth_cont", "start_eth_rcont")

d18 <- d18 %>% filter(!is.na(w2_civtraj))

#  Merging the 2017 and 2018 data + save: ####

d <- merge(d17, d18, by = c("nomem_encr"), all.x = T, all.y = F) # merge, keep only observations that are available in 2017 
PercTable(d$w2_PL0, useNA = "a", margins = c(1,2)) 
#2594 individuals who participated in w1 and w2, 496 individuals who participated only in w1

d <- d %>%
  var_labels(w2_PL0 = "Participated in wave 1 and 2")

#  Static descriptives based on 2017 data: ####

## Educational segregation ####
# Adding the contact benchmarks from wave 2:
d <- d %>%
  mutate(w1_edu_rcont = case_when(!is.na(stay_edu_rcont) ~ stay_edu_rcont,
                                  !is.na(stop_edu_rcont) ~ stop_edu_rcont)) %>%
  var_labels(w1_edu_rcont = "Relative contact frequency with members with higher ed")

## Ethnic segregation ####
# Contact segregation: 
PercTable(d$w1_eth_cont, d$w1_eth_3catB, rfrq="001", margins = c(1,2))

# Adding the contact benchmarks from wave 2:
d <- d %>%
  mutate(w1_eth_rcont = case_when(!is.na(stay_eth_rcont) ~ stay_eth_rcont,
                                  !is.na(stop_eth_rcont) ~ stop_eth_rcont)) %>%
  var_labels(w1_edu_rcont = "Relative contact frequency with TMSA minority members")
PercTable(d$w1_eth_rcont, d$w1_eth_3catB, rfrq="001", margins = c(1,2))

# Composition segregation:
PercTable(d$w1_eth_comp_3cat, d$w1_eth_3catB, rfrq="001", margins = c(1,2))
#Contact segregation conditional on composition segregation:
etc1 <- d %>% filter(w1_eth_comp_3cat=="Almost none")
PercTable(etc1$w1_eth_cont, etc1$w1_eth_3catB, rfrq="001", margins = c(1,2))
etc3 <- d %>% filter(w1_eth_comp_3cat=="More than 10%")
PercTable(etc3$w1_eth_cont, etc3$w1_eth_3catB, rfrq="001", margins = c(1,2))
PercTable(etc1$w1_eth_rcont, etc1$w1_eth_3catB, rfrq="001", margins = c(1,2))
PercTable(etc3$w1_eth_rcont, etc3$w1_eth_3catB, rfrq="001", margins = c(1,2))

# Dynamic descriptives based on 2018 data: ####
## Quitting: ####
# Relative frequency of trajectories by education:
mv <- d %>% filter(w1_volun==1 | w1_memb==1 | w1_memvol==1)
PercTable(mv$w2_civtraj_5cat, mv$w1_edu_4cat, rfrq="001", margins = c(1,2))
# There are no strong educational gradients in quitting rates, but higher-educated participants are somewhat less likely to 
# quit and when they do so, they are somewhat more likely to join another organization instead. These patterns are roughly 
# the same whether we consider people who were initially involved as volunteer, as member, or as both.

# Relative frequency of trajectories by ethnicity:
PercTable(mv$w2_civtraj_5cat, mv$w1_eth_3catB, rfrq="001", margins = c(1,2))
# The sample size for TMSA minorities is fairly small but the results are striking nonetheless: only about half of 
# the TMSA participants from wave 1 are still involved in the same org in wave 2 (7 months later). For ethnically 
# Dutch participants the "staying" rate is 81%. The high quitting rate among TMSA participants is all the more 
# striking because this concerns people's "most important" organizations (self-selected). 

# Relative frequency of trajectories by organizational composition: 
# Education:
PercTable(mv$w2_civtraj_5cat, mv$w1_edu_comp_3cat, rfrq="001", margins = c(1,2))
mv1 <- mv %>% filter(w1_edu_comp_3cat=="Less than 10%")
PercTable(mv1$w2_civtraj_5cat, mv1$w1_edu_4cat, rfrq="001", margins = c(1,2))
mv3 <- mv %>% filter(w1_edu_comp_3cat=="50-100%")
PercTable(mv3$w2_civtraj_5cat, mv3$w1_edu_4cat, rfrq="001", margins = c(1,2))

# Propensity of quitting among high/low educated depending on educational composition of vol. org.
# lower educated:
mvle <- d %>% filter((w1_edu_4cat=="Up to lower secondary" | w1_edu_4cat=="Higher secondary or lower tertiary" ) & (w1_volun==1 | w1_memb==1 | w1_memvol==1))
PercTable(mvle$w2_civtraj_5cat, mvle$w1_edu_comp_3cat, rfrq="001", margins = c(1,2))
mvhe <- d %>% filter((w1_edu_4cat=="Medium tertiary" | w1_edu_4cat=="University" ) & (w1_volun==1 | w1_memb==1 | w1_memvol==1))
PercTable(mvhe$w2_civtraj_5cat, mvhe$w1_edu_comp_3cat, rfrq="001", margins = c(1,2))
# Both lower and higher educated individuals quit at a similar rate,
# regardless of the educational composition of the vol. org. (no clear direction of quitting rates).


# Ethnicity:
PercTable(mv$w2_civtraj_5cat, mv$w1_eth_comp_3cat, rfrq="001", margins = c(1,2))
mv1et <- mv %>% filter(w1_eth_comp_3cat=="Almost none")
PercTable(mv1et$w2_civtraj_5cat, mv1et$w1_eth_3catB, rfrq="001", margins = c(1,2))
mv3et <- mv %>% filter(w1_eth_comp_3cat=="More than 10%")
PercTable(mv3et$w2_civtraj_5cat, mv3et$w1_eth_3catB, rfrq="001", margins = c(1,2))


# Propensity of quitting among Dutch and TMSA members depending on ethnic composition of vol. org.
mvdet <- d %>% filter((w1_eth_3catB=="Dutch origin") & (w1_volun==1 | w1_memb==1 | w1_memvol==1))
PercTable(mvdet$w2_civtraj_5cat, mvdet$w1_eth_comp_3cat, rfrq="001", margins = c(1,2))
mvnet <- d %>% filter((w1_eth_3catB=="TMSA origin") & (w1_volun==1 | w1_memb==1 | w1_memvol==1))
PercTable(mvnet$w2_civtraj_5cat, mvnet$w1_eth_comp_3cat, rfrq="001", margins = c(1,2))
# Dutch and TMSA individuals, are more likely to quit their involvement in vol. orgs. with more TMSA members. 

# Reasons for quitting among Dutch
PercTable(mvdet$stop_reas, mvdet$w1_edu_comp_3cat, rfrq="001", margins = c(1,2))
PercTable(mvdet$stop_reas, mvdet$w1_eth_comp_3cat, rfrq="001", margins = c(1,2))
# The differences across compositions of vol. orgs. are fairly small and there are too few cases to see whether they are systematic.

## Joining ####
# Selective joining with respect to education?
d <- d %>%
  mutate(start_edu_comp_3cat_all = case_when(!is.na(switch_edu_comp_3cat) ~ switch_edu_comp_3cat,
                                             !is.na(start_edu_comp_3cat) ~ start_edu_comp_3cat)) %>%
  var_labels(start_edu_comp_3cat_all = "% members with higher education - 3 cats (all starters/switchers)")
PercTable(d$start_edu_comp_3cat_all, rfrq="001", margins = c(1,2)) #142 joiners
PercTable(d$w1_edu_4cat, d$start_edu_comp_3cat_all, rfrq="010", margins = c(1,2)) 
PercTable(d$w2_civtraj_5cat, d$w1_edu_4cat, rfrq="000", margins = c(1,2)) 

# Selective joining with respect to ethnicity? (starting and switching)
d <- d %>%
  mutate(start_eth_comp_3cat_all = case_when(!is.na(switch_eth_comp_3cat) ~ switch_eth_comp_3cat,
                                             !is.na(start_eth_comp_3cat) ~ start_eth_comp_3cat)) %>%
  var_labels(start_eth_comp_3cat_all = "% members with TMSA origins - 3 cats (all starters/switchers)")
PercTable(d$start_eth_comp_3cat_all, rfrq="001", margins = c(1,2)) #189 joiners
PercTable(d$w1_eth_3catB, d$start_eth_comp_3cat_all, rfrq="010", margins = c(1,2)) 
PercTable(d$w2_civtraj_5cat, d$w1_eth_3catB, rfrq="000", margins = c(1,2)) 

# Mechanisms/Pathways into new organization
d <- d %>%
  mutate(start_path_all = case_when(!is.na(start_path) ~ start_path,
                                    !is.na(switch_path) ~ switch_path)) %>%
  var_labels(start_path_all = "Starting path (all starters/switchers)")
rd <- d %>% filter(w1_eth_3catB=="Dutch origin")
PercTable(rd$start_path_all, rd$start_eth_comp_3cat_all, rfrq="001", margins = c(1,2))
# Looking only at the joining transitions of Dutch respondents here, 
# we see no systematic differences in their recruitment-mechanisms for vol. orgs.
# with different ethnic compositions.

# The opportunity structures
# Share of respondents with TMSA origins/higher education
urband <- d %>%filter(w1_sted<=3)
rurald <- d %>%filter(w1_sted>3)
PercTable(urband$w1_edu_4cat, margins = c(1,2))
PercTable(rurald$w1_edu_4cat, margins = c(1,2))
PercTable(urband$w1_eth_3catB, margins = c(1,2))
PercTable(rurald$w1_eth_3catB, margins = c(1,2))

# Affiliations in organizations with different compositions.
PercTable(urband$w1_edu_comp_3cat, urband$w1_eth_comp_3cat, rfrq="111", margins = c(1,2))
PercTable(rurald$w1_edu_comp_3cat, rurald$w1_eth_comp_3cat, rfrq="111", margins = c(1,2))

PercTable(urband$w1_edu_comp_3cat, urband$w1_edu_4cat, rfrq="001", margins = c(1,2))
PercTable(rurald$w1_edu_comp_3cat, rurald$w1_edu_4cat, rfrq="001", margins = c(1,2))
PercTable(urband$w1_eth_comp_3cat, urband$w1_eth_3catB, rfrq="001", margins = c(1,2))
PercTable(rurald$w1_eth_comp_3cat, rurald$w1_eth_3catB, rfrq="001", margins = c(1,2))

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
write.dta(d, file = "01_Data/03_edit/2017_2018/LISS2017_for_stata_import_old.dta")

d_old <- d
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

