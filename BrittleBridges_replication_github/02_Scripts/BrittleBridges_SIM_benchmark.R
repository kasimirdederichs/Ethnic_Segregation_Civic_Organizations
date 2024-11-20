
sim <- read_sav("01_Data/02_raw/SIM/SIM2020 CCF.sav")

sim <- sim %>%
  mutate(orig = case_when(etngba==1 ~ "Moroccan origin",
                          etngba==2 ~ "Turkish origin",
                          etngba==3 ~ "Antillean origin",
                          etngba==4 ~ "Surinamese origin",
                          etngba==5 ~ "Polish origin",
                          etngba==6 ~ "Somali origin",
                          etngba==7 ~ "Dutch origin",
                          etngba==8 ~ "Iranian origin"),
         intmode = case_when(kanaal==1 ~ "CAWI", kanaal==2 ~ "CAPI"),
         volpart = case_when(organ==1 ~ 1, organ==2 ~ 0, organ==92 ~ NA_real_),
         volcomp_dutch = case_when(organ_autnl==1 ~ 1, 
                                   organ_autnl==2 ~ 2, 
                                   organ_autnl==3 ~ 3, 
                                   organ_autnl==4 ~ 4,
                                   organ_autnl==5 ~ 5,
                                   organ_autnl==-6 | organ_autnl==-5 ~ NA_real_),
         worcomp_dutch = case_when(contactcol_autnl==1 ~ 1, 
                                   contactcol_autnl==2 ~ 2, 
                                   contactcol_autnl==3 ~ 3, 
                                   contactcol_autnl==4 ~ 4,
                                   contactcol_autnl==5 ~ 5,
                                   contactcol_autnl==-6 | contactcol_autnl==-5 ~ NA_real_),
         nbhcomp_dutch = case_when(contactbuurt_autnl==1 ~ 1, 
                                   contactbuurt_autnl==2 ~ 2, 
                                   contactbuurt_autnl==3 ~ 3, 
                                   contactbuurt_autnl==4 ~ 4,
                                   contactbuurt_autnl==5 ~ 5,
                                   contactbuurt_autnl==-6 | contactbuurt_autnl==-5 ~ NA_real_),
         educomp_dutch = case_when(contactopl_autnl==1 ~ 1, 
                                   contactopl_autnl==2 ~ 2, 
                                   contactopl_autnl==3 ~ 3, 
                                   contactopl_autnl==4 ~ 4,
                                   contactopl_autnl==5 ~ 5,
                                   contactopl_autnl==-6 | contactopl_autnl==-5 ~ NA_real_))

sim <- sim %>%
  filter(intmode == "CAWI" & orig %in% c("Dutch origin", "Moroccan origin", "Turkish origin", "Antillean origin", "Surinamese origin")) %>%
  dplyr::select(c("orig", "volpart", "volcomp_dutch", "worcomp_dutch", "nbhcomp_dutch", "educomp_dutch"))

sim_group <- sim %>%
  group_by(orig) %>%
  dplyr::summarize(
    vol_none = sum(volcomp_dutch == 1, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_some = sum(volcomp_dutch == 2, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_half = sum(volcomp_dutch == 3, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_many = sum(volcomp_dutch == 4, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_all = sum(volcomp_dutch == 5, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    nbh_none = sum(nbhcomp_dutch == 1, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_some = sum(nbhcomp_dutch == 2, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_half = sum(nbhcomp_dutch == 3, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_many = sum(nbhcomp_dutch == 4, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_all = sum(nbhcomp_dutch == 5, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    wor_none = sum(worcomp_dutch == 1, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_some = sum(worcomp_dutch == 2, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_half = sum(worcomp_dutch == 3, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_many = sum(worcomp_dutch == 4, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_all = sum(worcomp_dutch == 5, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    edu_none = sum(educomp_dutch == 1, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_some = sum(educomp_dutch == 2, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_half = sum(educomp_dutch == 3, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_many = sum(educomp_dutch == 4, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_all = sum(educomp_dutch == 5, na.rm = TRUE) / sum(!is.na(educomp_dutch))
  )


sim_group <- sim_group %>%
  pivot_longer(cols = -orig, names_to = "composition", values_to = "percentage") %>%
  mutate(setting = substr(composition, 1, 3),
         comp_string = as.factor(substr(composition, 5, nchar(composition))))
sim_group$comp_all <- factor(sim_group$comp_string, levels = c("all", "many", "half", "some", "none"))
sim_group$setting <- factor(sim_group$setting, levels = c("edu", "wor", "nbh", "vol"))


#Without civically inactive people:
sim_civact <- sim %>%
  filter(volpart==1)

sim_group_civact <- sim_civact %>%
  group_by(orig) %>%
  dplyr::summarize(
    vol_none = sum(volcomp_dutch == 1, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_some = sum(volcomp_dutch == 2, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_half = sum(volcomp_dutch == 3, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_many = sum(volcomp_dutch == 4, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    vol_all = sum(volcomp_dutch == 5, na.rm = TRUE) / sum(!is.na(volcomp_dutch)),
    nbh_none = sum(nbhcomp_dutch == 1, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_some = sum(nbhcomp_dutch == 2, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_half = sum(nbhcomp_dutch == 3, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_many = sum(nbhcomp_dutch == 4, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    nbh_all = sum(nbhcomp_dutch == 5, na.rm = TRUE) / sum(!is.na(nbhcomp_dutch)),
    wor_none = sum(worcomp_dutch == 1, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_some = sum(worcomp_dutch == 2, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_half = sum(worcomp_dutch == 3, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_many = sum(worcomp_dutch == 4, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    wor_all = sum(worcomp_dutch == 5, na.rm = TRUE) / sum(!is.na(worcomp_dutch)),
    edu_none = sum(educomp_dutch == 1, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_some = sum(educomp_dutch == 2, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_half = sum(educomp_dutch == 3, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_many = sum(educomp_dutch == 4, na.rm = TRUE) / sum(!is.na(educomp_dutch)),
    edu_all = sum(educomp_dutch == 5, na.rm = TRUE) / sum(!is.na(educomp_dutch))
  )


sim_group_civact <- sim_group_civact %>%
  pivot_longer(cols = -orig, names_to = "composition", values_to = "percentage") %>%
  mutate(setting = substr(composition, 1, 3),
         comp_string = as.factor(substr(composition, 5, nchar(composition))))
sim_group_civact$comp_all <- factor(sim_group_civact$comp_string, levels = c("all", "many", "half", "some", "none"))
sim_group_civact$setting <- factor(sim_group_civact$setting, levels = c("edu", "wor", "nbh", "vol"))


#Minority groups
sim_group_civact %>% filter(orig != "Dutch origin") %>%
  ggplot(aes(fill=comp_all, y=percentage, x=setting)) + 
  facet_wrap(~orig) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("all" = "grey10", "many" = "grey30", "half" = "grey50", "some" = "grey70", "none" = "grey90"), 
                    breaks = c("none", "some", "half", "many", "all")) +
  coord_flip() +
  xlab("Setting") +
  ylab("Percentage") +
  ggtitle("B - Ethnic Minorities") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(title = "How many co-attendees are of Dutch origin?", title.position = "top")) +
  scale_x_discrete(labels = c("Educational Institutions", "Workplaces", "Neighborhoods", "Civic Organization")) +
  scale_y_continuous(labels = c(0, 25, 50, 75, 100)) -> sim_benchmark_1bw

#Majority group:
sim_group_civact %>% filter(orig == "Dutch origin") %>%
  ggplot(aes(fill=comp_all, y=percentage, x=setting)) + 
  facet_wrap(~orig) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("all" = "grey90", "many" = "grey70", "half" = "grey50", "some" = "grey30", "none" = "grey10"), 
                    breaks = c("none", "some", "half", "many", "all")) +
  coord_flip() +
  xlab("Setting") +
  ylab("Percentage") +
  ggtitle("A - Ethnic Majority") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(title = "How many co-attendees are of Dutch origin?", title.position = "top")) +
  scale_x_discrete(labels = c("Educational Institutions", "Workplaces", "Neighborhoods", "Civic Organizations")) +
  scale_y_continuous(labels = c(0, 25, 50, 75, 100)) -> sim_benchmark_2bw

sim_benchmark_bw <- wrap_plots(sim_benchmark_2bw, sim_benchmark_1bw, ncol = 2)
sim_benchmark_bw
ggsave("03_Outputs/SIM_benchmark_bw.png", height = 5, width = 10)
