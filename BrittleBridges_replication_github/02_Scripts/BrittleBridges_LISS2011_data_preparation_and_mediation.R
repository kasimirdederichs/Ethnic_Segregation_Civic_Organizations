#2011 data preparation and mediation analysis

#load datasets and rename variables ####
df <- read_dta("01_Data/02_raw/2011/eu11a_EN_1.0p.dta")
table(df$eu11a_m) #all interviews of 2011 data were conducted in May 2011. 

backgr_info <- read_dta("01_Data/02_raw/2011/avars_201111_EN_2.0p.dta")
backgr_info <- backgr_info %>% dplyr::select(c("nomem_encr", "herkomstgroep", "geslacht", "leeftijd", "oplmet", "sted", "partner", "aantalki", "aantalhh", "nettohh_f"))
df <- merge(df, backgr_info, by = "nomem_encr", all.x = T, all.y = F) #add background information (ethnicity, gender, age, and highest educational degree)

#Add religious attendance and praying from a LISS survey fielded in April 2011:
relig_data <- read_dta("01_Data/02_raw/2011/cr11d_EN_1.0p.dta")
relig_data <- relig_data %>% 
  mutate(rel_attend = case_when(cr11d041<99~7-cr11d041, cr11d041==99~NA_real_), #religious attendance and praying, 0=never, 7=every day
         rel_pray = case_when(cr11d042<99~7-cr11d042, cr11d042==99~NA_real_)) %>%
    dplyr::select(c("nomem_encr", "rel_attend", "rel_pray"))
df <- merge(df, relig_data, by = "nomem_encr", all.x = T, all.y = F)

#Add network size:
netw <- read_dta("01_Data/02_raw/2011/cs11d_EN_3.0p.dta")
netw <- netw %>%
  mutate(netwsize = cs11d294 + cs11d295 + cs11d296 + cs11d297 + cs11d298) %>%
  dplyr::select(c("nomem_encr", "netwsize"))
df <- merge(df, netw, by = "nomem_encr", all.x = T, all.y = F)

#Add personality traits:
pers <- read_dta("01_Data/02_raw/2011/cp11d_1.0p_EN.dta")
pers <- pers %>%
  mutate(b5_extra = cp11d020 + 6-cp11d025 + cp11d030 + 6-cp11d035 + cp11d040 + 6-cp11d045 + cp11d050 + 6-cp11d055 + cp11d060 + 6-cp11d065,
         b5_agree = 6-cp11d021 + cp11d026 + 6-cp11d031 + cp11d036 + 6-cp11d041 + cp11d046 + 6-cp11d051 + cp11d056 + cp11d061 + cp11d066,
         b5_consc = cp11d022 + 6-cp11d027 + cp11d032 + 6-cp11d037 + cp11d042 + 6-cp11d047 + cp11d052 + 6-cp11d057 + cp11d062 + cp11d067,
         b5_stabi = 6-cp11d023 + cp11d028 + 6-cp11d033 + cp11d038 + 6-cp11d043 + 6-cp11d048 + 6-cp11d053 + 6-cp11d058 + 6-cp11d063 + 6-cp11d068,
         b5_intel = cp11d024 + 6-cp11d029 + cp11d034 + 6-cp11d039 + cp11d044 + 6-cp11d049 + cp11d054 + cp11d059 + cp11d064 + cp11d069) %>%
  dplyr::select(c("nomem_encr", "b5_extra", "b5_agree", "b5_consc", "b5_stabi", "b5_intel"))
df <- merge(df, pers, by = "nomem_encr", all.x = T, all.y = F)
df <- df %>%
  mutate(b5_extra = ifelse(is.na(b5_extra), mean(b5_extra, na.rm=T), b5_extra),
         b5_agree = ifelse(is.na(b5_agree), mean(b5_agree, na.rm=T), b5_agree),
         b5_consc = ifelse(is.na(b5_consc), mean(b5_consc, na.rm=T), b5_consc),
         b5_stabi = ifelse(is.na(b5_stabi), mean(b5_stabi, na.rm=T), b5_stabi),
         b5_intel = ifelse(is.na(b5_intel), mean(b5_intel, na.rm=T), b5_intel))

#add 2012 data:
d2012 <- read_dta("01_Data/02_raw/2011/cs12e_1.0p_EN.dta")
PercTable(d2012$cs12e_m) #96.2% of respondents filled out the survey in February 2011, 3.8% filled out the survey in March 2011
#actively involved if respondent either participates and/or volunteers for that organization
d2012 <- d2012 %>%
  mutate(part_vol2012_sports = ifelse(cs12e005==1 | cs12e007==1, 1, 0),
         part_vol2012_culturehobby = ifelse(cs12e010==1 | cs12e012==1, 1, 0),
         part_vol2012_union = ifelse(cs12e015==1 | cs12e017==1, 1, 0),
         part_vol2012_professional = ifelse(cs12e020==1 | cs12e022==1, 1, 0),
         part_vol2012_consumer = ifelse(cs12e025==1 | cs12e027==1, 1, 0), #these are only people who participate in activities/volunteers -> no checkbook members of consumer organizations/unions, NA's remain NA's
         part_vol2012_humanitarian = ifelse(cs12e030==1 | cs12e032==1, 1, 0),
         part_vol2012_environmental = ifelse(cs12e035==1 | cs12e037==1, 1, 0),
         part_vol2012_religious = ifelse(cs12e040==1 | cs12e042==1, 1, 0),
         part_vol2012_political = ifelse(cs12e045==1 | cs12e047==1, 1, 0),
         part_vol2012_education = ifelse(cs12e050==1 | cs12e052==1, 1, 0),
         part_vol2012_socialyouth = ifelse(cs12e055==1 | cs12e057==1, 1, 0),
         part_vol2012_other = ifelse(cs12e060==1 | cs12e062==1, 1, 0)) %>%
  dplyr::select(starts_with("nomem_encr"), starts_with("part_"))

df <- merge(df, d2012, by = "nomem_encr", all.x = T, all.y = F)

df <- dplyr::rename(df, volorg.1 = eu11a026, volorg.2 = eu11a027, volorg.3 = eu11a028, #dplyr:: necessary here because rename in plyr works differently. 
             nr_members.1 = eu11a029, nr_members.2 = eu11a063, nr_members.3 = eu11a073,
             kn_by_name.1 = eu11a030, kn_by_name.2 = eu11a064, kn_by_name.3 = eu11a074,
             sh_dif_eth.1 = eu11a031, sh_dif_eth.2 = eu11a065, sh_dif_eth.3 = eu11a075,
             sh_dif_rel.1 = eu11a032, sh_dif_rel.2 = eu11a066, sh_dif_rel.3 = eu11a076,
             sh_sam_edu.1 = eu11a033, sh_sam_edu.2 = eu11a067, sh_sam_edu.3 = eu11a077,
             sh_mig.1 = eu11a034, sh_mig.2 = eu11a068, sh_mig.3 = eu11a078,
             sh_sam_inc.1 = eu11a035, sh_sam_inc.2 = eu11a069, sh_sam_inc.3 = eu11a079,
             sh_sam_age.1 = eu11a036, sh_sam_age.2 = eu11a070, sh_sam_age.3 = eu11a080,
             sh_sam_sex.1 = eu11a037, sh_sam_sex.2 = eu11a071, sh_sam_sex.3 = eu11a081,
             di_imp_mat.1 = eu11a038, di_imp_mat.2 = eu11a072, di_imp_mat.3 = eu11a082,
             nr_orgs = eu11a001)

#recode background variables in merged dataset:
df <- df %>%
  mutate(ethnicity = case_when(herkomstgroep==0 ~ "Dutch", 
                               herkomstgroep==101 | herkomstgroep==201 ~ "Western, non-Dutch",
                               herkomstgroep==102 | herkomstgroep==202 ~ "Non-Western"),
         woman = case_when(geslacht==1 ~ 0,
                            geslacht==2 ~ 1),
         age_cont = leeftijd,
         age_2cat = case_when(leeftijd<=49 ~ 0, leeftijd>=50 ~ 1),
         hbo = case_when((oplmet>=1 & oplmet<=4) | (oplmet>=7 & oplmet>=9) ~ 0,
                          oplmet==5 | oplmet==6 ~ 1),
         inc_pp = nettohh_f/aantalhh)

#did the respondent continue their involvement in the three most important organizations?
df <- df %>%
  mutate(still_inv_volorg.1 = case_when(volorg.1==1 ~ part_vol2012_sports,
                                        volorg.1==2 ~ part_vol2012_culturehobby,
                                        volorg.1==3 ~ part_vol2012_union,
                                        volorg.1==4 ~ part_vol2012_professional,
                                        volorg.1==5 ~ part_vol2012_consumer,
                                        volorg.1==6 ~ part_vol2012_humanitarian,
                                        volorg.1==7 ~ part_vol2012_environmental,
                                        volorg.1==8 ~ part_vol2012_religious,
                                        volorg.1==9 ~ part_vol2012_political,
                                        volorg.1==10 ~ part_vol2012_education,
                                        volorg.1==11 ~ part_vol2012_socialyouth,
                                        volorg.1==12 ~ part_vol2012_other),
         still_inv_volorg.2 = case_when(volorg.2==1 ~ part_vol2012_sports,
                                        volorg.2==2 ~ part_vol2012_culturehobby,
                                        volorg.2==3 ~ part_vol2012_union,
                                        volorg.2==4 ~ part_vol2012_professional,
                                        volorg.2==5 ~ part_vol2012_consumer,
                                        volorg.2==6 ~ part_vol2012_humanitarian,
                                        volorg.2==7 ~ part_vol2012_environmental,
                                        volorg.2==8 ~ part_vol2012_religious,
                                        volorg.2==9 ~ part_vol2012_political,
                                        volorg.2==10 ~ part_vol2012_education,
                                        volorg.2==11 ~ part_vol2012_socialyouth,
                                        volorg.2==12 ~ part_vol2012_other),
         still_inv_volorg.3 = case_when(volorg.3==1 ~ part_vol2012_sports,
                                        volorg.3==2 ~ part_vol2012_culturehobby,
                                        volorg.3==3 ~ part_vol2012_union,
                                        volorg.3==4 ~ part_vol2012_professional,
                                        volorg.3==5 ~ part_vol2012_consumer,
                                        volorg.3==6 ~ part_vol2012_humanitarian,
                                        volorg.3==7 ~ part_vol2012_environmental,
                                        volorg.3==8 ~ part_vol2012_religious,
                                        volorg.3==9 ~ part_vol2012_political,
                                        volorg.3==10 ~ part_vol2012_education,
                                        volorg.3==11 ~ part_vol2012_socialyouth,
                                        volorg.3==12 ~ part_vol2012_other),
         quit_volorg.1 = 1-still_inv_volorg.1,
         quit_volorg.2 = 1-still_inv_volorg.2,
         quit_volorg.3 = 1-still_inv_volorg.3)

#share of involved individuals who are involved in at least 2 organizations:
table(df$nr_orgs)
prop.table(table(df$nr_orgs))
1-0.5589
#44.11% of all involved individuals are involved in 2 or more organizations. 

#select relevant variables
fedf <- df %>% 
  dplyr::select(starts_with("no"), starts_with("vol"), starts_with("nr"), starts_with("sh_"), starts_with("kn_"), starts_with("di_"), starts_with("ethnicity"),
                starts_with("woman"), starts_with("age"), starts_with("hbo"), starts_with("part"), starts_with("quit"), starts_with("aantalki"),
                starts_with("inc_pp"), starts_with("nettohh_f"), starts_with("aantalhh"), starts_with("sted"), starts_with("rel_"), starts_with("netw"), starts_with("b5_"))

#reshape in long-format and recode variables ####
fedf <- reshape(fedf, direction = "long",
                 varying = c("volorg.1", "nr_members.1", "kn_by_name.1", "sh_dif_eth.1", "sh_dif_rel.1", "sh_sam_edu.1", "sh_mig.1", "sh_sam_inc.1", "sh_sam_age.1", "sh_sam_sex.1", "di_imp_mat.1", "quit_volorg.1",
                             "volorg.2", "nr_members.2", "kn_by_name.2", "sh_dif_eth.2", "sh_dif_rel.2", "sh_sam_edu.2", "sh_mig.2", "sh_sam_inc.2", "sh_sam_age.2", "sh_sam_sex.2", "di_imp_mat.2", "quit_volorg.2",
                             "volorg.3", "nr_members.3", "kn_by_name.3", "sh_dif_eth.3", "sh_dif_rel.3", "sh_sam_edu.3", "sh_mig.3", "sh_sam_inc.3", "sh_sam_age.3", "sh_sam_sex.3", "di_imp_mat.3", "quit_volorg.3"),
                 sep = ".",
                 idvar = c("nomem_encr", "nohouse_encr"),
                 timevar = "volcount",
                 times = c(1, 2, 3)) %>%#
  filter(!is.na(volorg)) #remove all rows without any organization in it


#generate truncated variables and proportionate variables for strong and weak ties:
fedf <- fedf %>%
  mutate(nr_members_t = case_when(nr_members<=1000 ~ nr_members, nr_members>1000 ~ 1000), #94% of resp. retain orig. value
         di_imp_mat_t = case_when(di_imp_mat<=10 ~ di_imp_mat, di_imp_mat>10 ~ 10), #91% of resp. retain orig. value
         di_imp_mat_t5 = case_when(di_imp_mat<=5 ~ di_imp_mat, di_imp_mat>5 ~ 5), #xx% of resp. retain orig. value -> this used for classifying orgs as high/medium/low interaction contexts, change to regular truncated var.
         kn_by_name_t = case_when(kn_by_name<=100 ~ kn_by_name, kn_by_name>100 ~ 100), #95% of resp. retain orig. value
         sh_kn_by_name = kn_by_name/nr_members, #the share of co-members one knows by name (weak ties)
         sh_di_imp_mat = di_imp_mat/nr_members, #the share of co-members one discusses imp. matters with (strong ties)
         most_imp_org = case_when(volcount==1 ~ 1, volcount==2 | volcount==3 ~ 0), #dummy variable whether vol. org. is labeled as "most important" vol. org.
         type_of_org = as.factor(volorg))

levels(fedf$type_of_org) <- c("Sports", "CultureHobby", "Union", "Professional", "Consumer", "Humanitarian", "Environmental", "Religious", "Political", "Education", "SocialYouth", "Other")
table(fedf$type_of_org)
table(fedf$type_of_org, fedf$kn_by_name)
fedf %>% 
  filter(!is.na(type_of_org)) %>%
  group_by(type_of_org) %>%
  summarise_at(vars(kn_by_name_t, di_imp_mat_t5, sh_kn_by_name, sh_di_imp_mat), list(median = median, mean = mean), na.rm=T) %>%
  arrange(desc(kn_by_name_t_median))
# in religious, sports and professional organizations, people know the most people by name. The pattern for strong ties looks a bit different, but we should focus on weak ties here
# since coming in contact with someone (as in the 2017 item) is rather a weak tie. 

#function that recodes the variables indicating the socio-demographic make-up of the vol. orgs.:
table(fedf$sh_dif_eth, useNA = "a")
recode_shares <- function(x){
  x_new <- case_when(x==1 ~ 0, x==2 ~ 0.05, x==3 ~ 0.10, x==4 ~ 0.25, x==5 ~ 0.50,
                     x==6 ~ 0.75, x==7 ~ 0.90, x==8 ~ 0.95, x==9 ~ 1)
  return(x_new)
}
diversity_variables <- c("sh_dif_eth", "sh_dif_rel", "sh_sam_edu", "sh_mig", "sh_sam_inc", "sh_sam_age", "sh_sam_sex")
fedf <- fedf %>%
  mutate(across(all_of(diversity_variables), ~recode_shares(.))) #mutate across to apply function to multiple variables. 

#generate variables for share of same ethnicity within org. and share of sex/education/age outgroups:
fedf <- fedf %>%
  mutate(sh_sam_eth = 1-sh_dif_eth,
         sh_dif_sex = 1-sh_sam_sex,
         sh_dif_edu = 1-sh_sam_edu,
         sh_dif_age = 1-sh_sam_age)  

#The missingness of these composition variables is quite low (3%) across all observed dimensions.
round(prop.table(table(fedf$sh_dif_eth, useNA = "a")), digits = 3)
round(prop.table(table(fedf$sh_dif_rel, useNA = "a")), digits = 3)
round(prop.table(table(fedf$sh_sam_edu, useNA = "a")), digits = 3)
round(prop.table(table(fedf$sh_mig, useNA = "a")), digits = 3)
round(prop.table(table(fedf$sh_sam_inc, useNA = "a")), digits = 3)
round(prop.table(table(fedf$sh_sam_age, useNA = "a")), digits = 3)
round(prop.table(table(fedf$sh_sam_sex, useNA = "a")), digits = 3)

#construct weights (being involved in at least 2 orgs, i.e., entering the FE-analyses):
fedf <- fedf %>%
  mutate(pfe = case_when(nr_orgs<=1 & !is.na(sh_dif_eth) & !is.na(sh_sam_edu) & !is.na(sh_sam_age) & !is.na(sh_sam_sex) & !is.na(type_of_org) &
                         !is.na(nr_members_t) & !is.na(kn_by_name_t) & !is.na(di_imp_mat_t) & !is.na(quit_volorg) &
                         ethnicity=="Dutch" ~ 0, 
                         nr_orgs>=2 & !is.na(sh_dif_eth) & !is.na(sh_sam_edu) & !is.na(sh_sam_age) & !is.na(sh_sam_sex) & !is.na(type_of_org) &
                         !is.na(nr_members_t) & !is.na(kn_by_name_t) & !is.na(di_imp_mat_t) & !is.na(quit_volorg) &
                         ethnicity=="Dutch" ~ 1)) #assign value 1 to all who are part of the FE-sample, excluding cases that are missing for other reasons (e.g., missingness on other co-variate.)

#impute reasonable values for missing values of sted, hbo, partner (only for observations which would end up in the FE sample (i.e., respondents involved in at least 2 orgs)). 
table(fedf$sted, useNA = "a")
mean(fedf$sted, na.rm=T)
fedf$sted[is.na(fedf$sted)] <- 3 # 3 is close the the mean (mid-urban/rural)
table(fedf$partner, useNA = "a")
fedf$partner[is.na(fedf$partner)] <- 1 # most people in the sample live with a partner. 
table(fedf$hbo, useNA = "a")
fedf$hbo[is.na(fedf$hbo)] <- 0 # most people in the sample do not have a hbo degree 
fedf$aantalki[is.na(fedf$aantalki)] <- 0 #most people do not have any children living with them. 
mean_inc_pp <- mean(fedf$inc_pp, na.rm = T)
fedf$inc_pp[is.na(fedf$inc_pp)] <- mean_inc_pp
fedf$rel_attend[is.na(fedf$rel_attend)] <- 0 #most people never attend any religious service
fedf$rel_pray[is.na(fedf$rel_pray)] <- 0 #most people never pray

#weights excluding religion variables:
weightsmodel_no_relig <- glm(pfe ~ woman + age_cont + hbo + partner + as.factor(aantalki) + inc_pp + as.factor(sted) + netwsize + 
                               b5_extra + b5_agree + b5_consc + b5_stabi + b5_intel,
                    data = subset(fedf, !is.na(sh_dif_eth) & !is.na(sh_sam_edu) & 
                                  !is.na(sh_sam_age) & !is.na(sh_sam_sex) & !is.na(type_of_org) &
                                  !is.na(nr_members_t) & !is.na(kn_by_name_t) & !is.na(di_imp_mat_t) &
                                  !is.na(quit_volorg) & ethnicity=="Dutch"), family = binomial)

summary(weightsmodel_no_relig)
fedf$pred_pfe1_no_relig <- predict(weightsmodel_no_relig, fedf, type = "response") #assign predicted probabilities of being involved in at least 2 orgs (pfe==1), i.e., ending up in the FE-sample.
fedf <- fedf %>%
  mutate(invprobwgt_no_relig = case_when(pfe==0 ~ NA_real_, pfe==1 ~ 1/pred_pfe1_no_relig))

#weights including religion variables:
weightsmodel_incl_relig <- glm(pfe ~ woman + age_cont + hbo + partner + as.factor(aantalki) + inc_pp + as.factor(sted) + as.factor(rel_attend) + as.factor(rel_pray) + netwsize + 
                                 b5_extra + b5_agree + b5_consc + b5_stabi + b5_intel,
                             data = subset(fedf, !is.na(sh_dif_eth) & !is.na(sh_sam_edu) & 
                                             !is.na(sh_sam_age) & !is.na(sh_sam_sex) & !is.na(type_of_org) &
                                             !is.na(nr_members_t) & !is.na(kn_by_name_t) & !is.na(di_imp_mat_t) &
                                             !is.na(quit_volorg) & ethnicity=="Dutch"), family = binomial)

summary(weightsmodel_incl_relig)
fedf$pred_pfe1_incl_relig <- predict(weightsmodel_incl_relig, fedf, type = "response") #assign predicted probabilities of being involved in at least 2 orgs (pfe==1), i.e., ending up in the FE-sample.
fedf <- fedf %>%
  mutate(invprobwgt_incl_relig = case_when(pfe==0 ~ NA_real_, pfe==1 ~ 1/pred_pfe1_incl_relig))

sum(!is.na(fedf$invprobwgt_no_relig)) # 1545 valid values -> equals the initial number of observations before subtracting the 13 cases that did not provide additional information on the organization. 
sum(!is.na(fedf$invprobwgt_incl_relig))

# CREATE FINAL SAMPLE OF THOSE WHO ARE INVOLVED IN AT LEAST 2 ORGANIZATIONS ####
fedf2 <- fedf %>% filter(nr_orgs>=2)

#number of members by ethnicity in vol. org:
fedf2 <- fedf2 %>%
  mutate(memb_sam_eth = nr_members*(1-sh_dif_eth),
         memb_sam_eth_t = case_when(memb_sam_eth<=100 ~memb_sam_eth, memb_sam_eth>100 ~ 100),
         memb_dif_eth = nr_members*(sh_dif_eth),
         memb_dif_eth_t = case_when(memb_dif_eth<=100 ~memb_dif_eth, memb_dif_eth>100 ~ 100),
         memb_dif_edu = nr_members*(1-sh_sam_edu),
         memb_dif_edu_t = case_when(memb_dif_edu<=100 ~memb_dif_edu, memb_dif_edu>100 ~ 100),
         memb_sam_edu = nr_members*(sh_sam_edu),
         memb_sam_edu_t = case_when(memb_sam_edu<=100 ~memb_sam_edu, memb_sam_edu>100 ~ 100),
         memb_dif_age = nr_members*(1-sh_sam_age),
         memb_dif_age_t = case_when(memb_dif_age<=100 ~memb_dif_age, memb_dif_age>100 ~ 100),
         memb_sam_age = nr_members*(sh_sam_age),
         memb_sam_age_t = case_when(memb_sam_age<=100 ~memb_sam_age, memb_sam_age>100 ~ 100),
         memb_dif_sex = nr_members*(1-sh_sam_sex),
         memb_dif_sex_t = case_when(memb_dif_sex<=100 ~memb_dif_sex, memb_dif_sex>100 ~ 100),
         memb_sam_sex = nr_members*(sh_sam_sex),
         memb_sam_sex_t = case_when(memb_sam_sex<=100 ~memb_sam_sex, memb_sam_sex>100 ~ 100))

#log-transformed variables:
fedf2 <- fedf2 %>%
  mutate(di_imp_mat_log = log(di_imp_mat+1),
         di_imp_mat_t_log = log(di_imp_mat_t+1),
         kn_by_name_log = log(kn_by_name+1),
         kn_by_name_t_log = log(kn_by_name_t+1),
         nr_members_log = log(nr_members+1),
         nr_members_t_log = log(nr_members_t+1),
         memb_dif_eth_log = log(memb_dif_eth+1),
         memb_sam_eth_log = log(memb_sam_eth+1),
         memb_dif_edu_log = log(memb_dif_edu+1),
         memb_sam_edu_log = log(memb_sam_edu+1),
         memb_dif_age_log = log(memb_dif_age+1),
         memb_sam_age_log = log(memb_sam_age+1),
         memb_dif_sex_log = log(memb_dif_sex+1),
         memb_sam_sex_log = log(memb_sam_sex+1))

#create dummy variables for type of organization:
fedf2 <- fedf2 %>%
  mutate(typ_Sports = ifelse(type_of_org=="Sports", 1, 0),
         typ_CultureHobby = ifelse(type_of_org=="CultureHobby", 1, 0),
         typ_Union = ifelse(type_of_org=="Union", 1, 0),
         typ_Professional = ifelse(type_of_org=="Professional", 1, 0),
         typ_Consumer = ifelse(type_of_org=="Consumer", 1, 0),
         typ_Humanitarian = ifelse(type_of_org=="Humanitarian", 1, 0),
         typ_Environmental = ifelse(type_of_org=="Environmental", 1, 0),
         typ_Religious = ifelse(type_of_org=="Religious", 1, 0),
         typ_Political = ifelse(type_of_org=="Political", 1, 0),
         typ_Education = ifelse(type_of_org=="Education", 1, 0),
         typ_SocialYouth = ifelse(type_of_org=="SocialYouth", 1, 0),
         typ_Other = ifelse(type_of_org=="Other", 1, 0))

#Show number of respondents with at least 2 organizations by ethnicity:
fedf2 %>%
  group_by(ethnicity) %>%
  summarise(count = n_distinct(nomem_encr))

#restrict to final sample: (only Dutch respondents!!)
finsam_dutch <- fedf2 %>%
  filter(!is.na(sh_sam_eth), !is.na(sh_sam_edu), !is.na(sh_sam_age), !is.na(sh_sam_sex), !is.na(type_of_org),
         !is.na(nr_members_t_log), !is.na(kn_by_name_t_log), !is.na(di_imp_mat_t_log), !is.na(quit_volorg),
         ethnicity=="Dutch")
nrow(finsam_dutch)
#center continuous variables with respect to the entire sample:
finsam_dutch <- finsam_dutch %>%
  mutate(sh_sam_eth_c = sh_sam_eth-mean(sh_sam_eth, na.rm = T),
         sh_sam_edu_c = sh_sam_edu-mean(sh_sam_edu, na.rm = T),
         sh_sam_age_c = sh_sam_age-mean(sh_sam_age, na.rm = T),
         sh_sam_sex_c = sh_sam_sex-mean(sh_sam_sex, na.rm = T),
         sh_dif_eth_c = sh_dif_eth-mean(sh_dif_eth, na.rm = T),
         sh_dif_edu_c = sh_dif_edu-mean(sh_dif_edu, na.rm = T),
         sh_dif_age_c = sh_dif_age-mean(sh_dif_age, na.rm = T),
         sh_dif_sex_c = sh_dif_sex-mean(sh_dif_sex, na.rm = T),
         nr_members_log_c = nr_members_log-mean(nr_members_log, na.rm = T),         
         nr_members_t_log_c = nr_members_t_log-mean(nr_members_t_log, na.rm = T),
         kn_by_name_log_c = kn_by_name_log-mean(kn_by_name_log, na.rm = T),
         kn_by_name_t_log_c = kn_by_name_t_log-mean(kn_by_name_t_log, na.rm = T),
         di_imp_mat_log_c = di_imp_mat_log-mean(di_imp_mat_log, na.rm = T),
         di_imp_mat_t_log_c = di_imp_mat_t_log-mean(di_imp_mat_t_log, na.rm = T),
         sh_kn_by_name_c = sh_kn_by_name-mean(sh_kn_by_name, na.rm = T),
         sh_di_imp_mat_c = sh_di_imp_mat-mean(sh_di_imp_mat, na.rm = T))

#mean-center variables for mediation analysis: (prefix c_)
finsam_dutch <- finsam_dutch %>%
  group_by(nomem_encr) %>%
  mutate(c_sh_sam_eth_c = sh_sam_eth_c - mean(sh_sam_eth_c, na.rm=TRUE),
         c_sh_sam_age_c = sh_sam_age_c - mean(sh_sam_age_c, na.rm=TRUE),
         c_sh_sam_edu_c = sh_sam_edu_c - mean(sh_sam_edu_c, na.rm=TRUE),
         c_sh_sam_sex_c = sh_sam_sex_c - mean(sh_sam_sex_c, na.rm=TRUE),
         c_sh_dif_eth_c = sh_dif_eth_c - mean(sh_dif_eth_c, na.rm=TRUE),
         c_sh_dif_age_c = sh_dif_age_c - mean(sh_dif_age_c, na.rm=TRUE),
         c_sh_dif_edu_c = sh_dif_edu_c - mean(sh_dif_edu_c, na.rm=TRUE),
         c_sh_dif_sex_c = sh_dif_sex_c - mean(sh_dif_sex_c, na.rm=TRUE),
         c_nr_members_t_log_c = nr_members_t_log_c - mean(nr_members_t_log_c, na.rm=TRUE),
         c_quit_volorg = quit_volorg - mean(quit_volorg, na.rm=TRUE),
         c_kn_by_name_t_log_c = kn_by_name_t_log_c - mean(kn_by_name_t_log_c, na.rm=TRUE),
         c_sh_kn_by_name_c = sh_kn_by_name_c - mean(sh_kn_by_name_c, na.rm=TRUE),
         c_di_imp_mat_t_log_c = di_imp_mat_t_log_c - mean(di_imp_mat_t_log_c, na.rm=TRUE),
         c_typ_Sports = typ_Sports - mean(typ_Sports, na.rm=TRUE),
         c_typ_CultureHobby = typ_CultureHobby - mean(typ_CultureHobby, na.rm=TRUE),
         c_typ_Union = typ_Union - mean(typ_Union, na.rm=TRUE),
         c_typ_Professional = typ_Professional - mean(typ_Professional, na.rm=TRUE),
         c_typ_Consumer = typ_Consumer - mean(typ_Consumer, na.rm=TRUE),
         c_typ_Humanitarian = typ_Humanitarian - mean(typ_Humanitarian, na.rm=TRUE),
         c_typ_Environmental = typ_Environmental - mean(typ_Environmental, na.rm=TRUE),
         c_typ_Religious = typ_Religious - mean(typ_Religious, na.rm=TRUE),
         c_typ_Political = typ_Political - mean(typ_Political, na.rm=TRUE),
         c_typ_Education = typ_Education - mean(typ_Education, na.rm=TRUE),
         c_typ_SocialYouth = typ_SocialYouth - mean(typ_SocialYouth, na.rm=TRUE),
         c_typ_Other = typ_Other - mean(typ_Other, na.rm=TRUE)) %>%
  ungroup()

#restrict to respondents with at least two organizational affiliations: (This is the actually observed
#nr of orgs, not the number they give when they are asked about it in the beginning). There are small 
#differences between the two. 
id_nr_orgs <- data.frame(table(finsam_dutch$nomem_encr)) %>%
  mutate(obs_orgs = Freq) %>%
  arrange(obs_orgs)
finsam_dutch <- merge(finsam_dutch, id_nr_orgs, by.x = c("nomem_encr"), by.y = c("Var1"))
finsam_dutch <- finsam_dutch %>%
  filter(obs_orgs>=2) #now, N=1532. There are 13 cases in which respondents indicated that they were part of X organizations but then only provided information about X-1. 

write.dta(finsam_dutch, file = "01_Data/03_edit/2011_2012/civic_participation_for_analyses_dutch.dta")

# mediation analysis ####
#Run the script that generates the process function for the mediation analysis:
source("02_Scripts/process.R")

process (data = finsam_dutch, 
         y = "c_quit_volorg", x = "c_sh_dif_eth_c", #dependent and independent variable
         m =c("c_di_imp_mat_t_log_c", "c_sh_kn_by_name_c"), #multiple mediator in parallel, nr of strong ties, share of weak ties
         cov=c("c_sh_dif_edu_c", "c_sh_dif_sex_c", "c_nr_members_t_log_c", 
               "c_typ_CultureHobby", "c_typ_Union", "c_typ_Professional", "c_typ_Consumer", "c_typ_Humanitarian", 
               "c_typ_Environmental", "c_typ_Religious", "c_typ_Political", "c_typ_Education", "c_typ_SocialYouth", "c_typ_Other")
         , model = 4, #model 4 is simple mediation analysis that allows for multiple mediators
         contrast = 1, #test whether mediators have stat. sign. different effects. 
         total = 1, #get total effect
         stand = 1, #get standardized effects
         boot = 10000, #nr of bootstrapping iterations
         modelbt = 1, #all CIs based on bootstrapping
         seed = 19960909 #set seed
) 



#Calculate percentages for final column of table 3:

0.1803/0.2304 #direct effect
0.0141/0.2304 #indirect effect via weak ties
0.0360/0.2304 #indirect effect via strong ties
0.0501/0.2304 #strong and weak ties combined (total indirect effect): 22% 

#when rounded to 3 digits:
0.180/0.230 #direct effect
0.014/0.230 #indirect effect via weak ties
0.036/0.230 #indirect effect via strong ties
0.050/0.230 #strong and weak ties combined (total indirect effect): 

#mediation model just for strong ties (gives exactly the same results as the stata respective stata model on which we run the sensitivity analysis)
process (data = finsam_dutch, 
         y = "c_quit_volorg", x = "c_sh_dif_eth_c", #dependent and independent variable
         m ="c_di_imp_mat_t_log_c", #just weak ties as moderator, strong ties not in model.
         cov=c("c_sh_dif_edu_c", "c_sh_dif_sex_c", "c_nr_members_t_log_c", 
               "c_typ_CultureHobby", "c_typ_Union", "c_typ_Professional", "c_typ_Consumer", "c_typ_Humanitarian", 
               "c_typ_Environmental", "c_typ_Religious", "c_typ_Political", "c_typ_Education", "c_typ_SocialYouth", "c_typ_Other")
         , model = 4, #model 4 is simple mediation analysis that allows for multiple mediators
         total = 1, #get total effect
         stand = 1, #get standardized effects
         boot = 10000, #nr of bootstrapping iterations
         modelbt = 1, #all CIs based on bootstrapping
         seed = 19960909 #set seed
) 

#mediation model just for weak ties (gives exactly the same results as the stata respective stata model on which we run the sensitivity analysis)
process (data = finsam_dutch, 
         y = "c_quit_volorg", x = "c_sh_dif_eth_c", #dependent and independent variable
         m ="c_kn_by_name_t_log_c", #just weak ties as moderator, strong ties not in model.
         cov=c("c_sh_dif_edu_c", "c_sh_dif_sex_c", "c_nr_members_t_log_c",
               "c_typ_CultureHobby", "c_typ_Union", "c_typ_Professional", "c_typ_Consumer", "c_typ_Humanitarian", 
               "c_typ_Environmental", "c_typ_Religious", "c_typ_Political", "c_typ_Education", "c_typ_SocialYouth", "c_typ_Other")
         , model = 4, #model 4 is simple mediation analysis that allows for multiple mediators
         total = 1, #get total effect
         stand = 1, #get standardized effects
         boot = 10000, #nr of bootstrapping iterations
         modelbt = 1, #all CIs based on bootstrapping
         seed = 19960909 #set seed
) 


