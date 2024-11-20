# This script produces the figures associated with the 2017 data.

# Re-import excel tables with results from stata-models: ####
#results:
stata_results <- list.files("01_Data/03_edit/2017_2018/predicted_probabilities", pattern='res_', full.names=TRUE)
predprobs <- lapply(stata_results, read_excel)

#restrict the confidence intervals in all plots to 0
predprobs <- predprobs %>%
  map(~mutate(., L_CI = ifelse(L_CI>=0, L_CI, 0)))
names(predprobs) <- paste0("predprobs_", sub(".*res", "", stata_results))
list2env(predprobs, .GlobalEnv)

# Figures in color ####
## Figure 1 for main text ####
### Panel 1: all organizations, by ethnicity (based on model mlogit w1_eth_comp_2cat i.w1_eth_3catB) 
t1_eth1 <- predprobs__across__eth_comp__eth.xlsx %>%
  filter(Outcome_Level %in% c(1, 2, 3, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attribute` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==0 ~ "Not involved", Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==2 ~ ">=5% TMSA origin"), 
                              levels = c("Not involved", "<5% TMSA origin", ">=5% TMSA origin")))

fig1_p1 <- ggplot(t1_eth1, aes(fill=`Individual attribute`,
                                 y=Pred_Probability,
                                 x=Composition,
                                 ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylab("Predicted probability") + 
  ylim(0, 0.4) +
  xlab("Ethnic composition of civic organizations") +
  scale_fill_manual(values = c("orange", "lightblue")) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
fig1_p1

### Combined outcome by education:
# Panel 2: lower-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
t1_lowereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia.xlsx %>%
  filter(Outcome_Level %in% c(1, 3) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attributes` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==3 ~ ">=5% TMSA origin")))

fig1_p2 <- ggplot(data = t1_lowereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual attributes`, pattern = `Individual attributes`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  scale_fill_manual(values = c("orange", "lightblue", "orange", "lightblue")) +
  labs(x = "Ethnic composition of lower-educated civic organizations", y = "Predicted probability", pattern = "Individual attributes") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
fig1_p2

# Panel 3: higher-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
t1_highereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia.xlsx %>%
  filter(Outcome_Level %in% c(2, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual ethnicity` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==2 ~ "<5% TMSA origin",
                                        Outcome_Level==4 ~ ">=5% TMSA origin")))

fig1_p3 <- ggplot(data = t1_highereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual ethnicity`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") + 
  scale_fill_manual(values = c("orange", "lightblue", "orange", "lightblue")) +
  labs(x = "Ethnic composition of higher-educated civic organizations", y = "Predicted probability") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'bottom', 
        panel.background = element_blank(),
        #legend.title = element_blank(),
        axis.line = element_line(color = "black"))
fig1_p3

###compile Figure 1 
design_fig1 <- "A
                B
                C"

figure_1<- wrap_plots(list(A=fig1_p1, 
                           B=fig1_p2,
                           C=fig1_p3), design = design_fig1)
figure_1 + plot_annotation(tag_levels = "A") #add plot annotations (Panel A,B,C)
ggsave("03_Outputs/figure_1.jpg", dpi = 600, width = 15, height = 20, units = c("cm"))
ggsave("03_Outputs/figure_1.eps", dpi = 600, width = 15, height = 20, units = c("cm"))

## Figure 1A - Online Supplement (Urban Sample) ####
### Panel 1: all organizations, by ethnicity (based on model mlogit w1_eth_comp_2cat i.w1_eth_3catB) 
tu1_eth1 <- predprobs__across__eth_comp__eth_urb_1.xlsx %>%
  filter(Outcome_Level %in% c(1, 2, 3, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attribute` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==0 ~ "Not involved", Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==2 ~ ">=5% TMSA origin"), 
                              levels = c("Not involved", "<5% TMSA origin", ">=5% TMSA origin")))

sfig1u_p1 <- ggplot(tu1_eth1, aes(fill=`Individual attribute`,
                               y=Pred_Probability,
                               x=Composition,
                               ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylab("Predicted probability") + 
  ylim(0, 0.45) +
  xlab("Ethnic composition of civic organizations") +
  ggtitle("Urban Sample") +
  scale_fill_manual(values = c("orange", "lightblue")) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1u_p1

### Combined outcome by education:
# Panel 2: lower-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tu1_lowereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_1.xlsx %>%
  filter(Outcome_Level %in% c(1, 3) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attributes` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==3 ~ ">=5% TMSA origin")))

sfig1u_p2 <- ggplot(data = tu1_lowereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual attributes`, pattern = `Individual attributes`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  scale_fill_manual(values = c("orange", "lightblue", "orange", "lightblue")) +
  labs(x = "Ethnic composition of lower-educated civic organizations", y = "Predicted probability", pattern = "Individual attributes") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1u_p2

# Panel 3: higher-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tu1_highereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_1.xlsx %>%
  filter(Outcome_Level %in% c(2, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual ethnicity` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==2 ~ "<5% TMSA origin",
                                        Outcome_Level==4 ~ ">=5% TMSA origin")))

sfig1u_p3 <- ggplot(data = tu1_highereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual ethnicity`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") + 
  scale_fill_manual(values = c("orange", "lightblue", "orange", "lightblue")) +
  labs(x = "Ethnic composition of higher-educated civic organizations", y = "Predicted probability") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'bottom', 
        panel.background = element_blank(),
        #legend.title = element_blank(),
        axis.line = element_line(color = "black"))
sfig1u_p3

## Figure 1B - Online Supplement (Rural Sample) ####
### Panel 1: all organizations, by ethnicity (based on model mlogit w1_eth_comp_2cat i.w1_eth_3catB) 
tr1_eth1 <- predprobs__across__eth_comp__eth_urb_0.xlsx %>%
  filter(Outcome_Level %in% c(1, 2, 3, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attribute` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==0 ~ "Not involved", Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==2 ~ ">=5% TMSA origin"), 
                              levels = c("Not involved", "<5% TMSA origin", ">=5% TMSA origin")))

sfig1r_p1 <- ggplot(tr1_eth1, aes(fill=`Individual attribute`,
                                  y=Pred_Probability,
                                  x=Composition,
                                  ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylab("Predicted probability") + 
  ylim(0, 0.45) +
  xlab("Ethnic composition of civic organizations") +
  ggtitle("Rural Sample") +
  scale_fill_manual(values = c("orange", "lightblue")) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1r_p1

### Combined outcome by education:
# Panel 2: lower-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tr1_lowereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_0.xlsx %>%
  filter(Outcome_Level %in% c(1, 3) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attributes` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==3 ~ ">=5% TMSA origin")))

sfig1r_p2 <- ggplot(data = tr1_lowereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual attributes`, pattern = `Individual attributes`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  scale_fill_manual(values = c("orange", "lightblue", "orange", "lightblue")) +
  labs(x = "Ethnic composition of lower-educated civic organizations", y = "Predicted probability", pattern = "Individual attributes") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1r_p2

# Panel 3: higher-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tr1_highereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_0.xlsx %>%
  filter(Outcome_Level %in% c(2, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual ethnicity` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==2 ~ "<5% TMSA origin",
                                        Outcome_Level==4 ~ ">=5% TMSA origin")))

sfig1r_p3 <- ggplot(data = tr1_highereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual ethnicity`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") + 
  scale_fill_manual(values = c("orange", "lightblue", "orange", "lightblue")) +
  labs(x = "Ethnic composition of higher-educated civic organizations", y = "Predicted probability") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'bottom', 
        panel.background = element_blank(),
        #legend.title = element_blank(),
        axis.line = element_line(color = "black"))
sfig1r_p3

#one single figure for urban and rural together. 
design_figS4 <- "AD
                 BE
                 CG"

sfigure_4 <- wrap_plots(list(A=sfig1u_p1, B=sfig1u_p2, C=sfig1u_p3,
                                 D=sfig1r_p1, E=sfig1r_p2, G=sfig1r_p3), design = design_figS4)
sfigure_4 + plot_annotation(tag_levels = "A") #add plot annotations (Panel A,B,C)
ggsave("03_Outputs/figure_1_urbanrural.jpg", dpi = 600, width = 30, height = 20, units = c("cm"))
ggsave("03_Outputs/figure_1_urbanrural.eps", dpi = 600, width = 30, height = 20, units = c("cm"))


# Figures in black and white ####
## Figure 1 for main text ####
### Panel 1: all organizations, by ethnicity (based on model mlogit w1_eth_comp_2cat i.w1_eth_3catB) 
t1_eth1 <- predprobs__across__eth_comp__eth.xlsx %>%
  filter(Outcome_Level %in% c(1, 2, 3, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attribute` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==0 ~ "Not involved", Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==2 ~ ">=5% TMSA origin"), 
                              levels = c("Not involved", "<5% TMSA origin", ">=5% TMSA origin")))

fig1_p1 <- ggplot(t1_eth1, aes(fill=`Individual attribute`,
                               y=Pred_Probability,
                               x=Composition,
                               ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylab("Predicted probability") + 
  ylim(0, 0.4) +
  xlab("Ethnic composition of civic organizations") +
  scale_fill_manual(values = c("grey60", "grey80")) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
fig1_p1

### Combined outcome by education:
# Panel 2: lower-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
t1_lowereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia.xlsx %>%
  filter(Outcome_Level %in% c(1, 3) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attributes` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==3 ~ ">=5% TMSA origin")))

fig1_p2 <- ggplot(data = t1_lowereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual attributes`, pattern = `Individual attributes`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) +
  labs(x = "Ethnic composition of lower-educated civic organizations", y = "Predicted probability", pattern = "Individual attributes") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
fig1_p2

# Panel 3: higher-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
t1_highereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia.xlsx %>%
  filter(Outcome_Level %in% c(2, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual ethnicity` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==2 ~ "<5% TMSA origin",
                                        Outcome_Level==4 ~ ">=5% TMSA origin")))

fig1_p3 <- ggplot(data = t1_highereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual ethnicity`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) +
  labs(x = "Ethnic composition of higher-educated civic organizations", y = "Predicted probability") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'bottom', 
        panel.background = element_blank(),
        #legend.title = element_blank(),
        axis.line = element_line(color = "black"))
fig1_p3

###compile Figure 1 
design_fig1 <- "A
                B
                C"

figure_1<- wrap_plots(list(A=fig1_p1, 
                           B=fig1_p2,
                           C=fig1_p3), design = design_fig1)
figure_1 + plot_annotation(tag_levels = "A") #add plot annotations (Panel A,B,C)
ggsave("03_Outputs/figure_1_bw.jpg", dpi = 600, width = 15, height = 20, units = c("cm"))
ggsave("03_Outputs/figure_1_bw.eps", dpi = 600, width = 15, height = 20, units = c("cm"))

## Figure 1A - Online Supplement (Urban Sample) ####
### Panel 1: all organizations, by ethnicity (based on model mlogit w1_eth_comp_2cat i.w1_eth_3catB) 
tu1_eth1 <- predprobs__across__eth_comp__eth_urb_1.xlsx %>%
  filter(Outcome_Level %in% c(1, 2, 3, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attribute` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==0 ~ "Not involved", Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==2 ~ ">=5% TMSA origin"), 
                              levels = c("Not involved", "<5% TMSA origin", ">=5% TMSA origin")))

sfig1u_p1 <- ggplot(tu1_eth1, aes(fill=`Individual attribute`,
                                  y=Pred_Probability,
                                  x=Composition,
                                  ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylab("Predicted probability") + 
  ylim(0, 0.45) +
  xlab("Ethnic composition of civic organizations") +
  ggtitle("Urban Sample") +
  scale_fill_manual(values = c("grey60", "grey80")) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1u_p1

### Combined outcome by education:
# Panel 2: lower-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tu1_lowereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_1.xlsx %>%
  filter(Outcome_Level %in% c(1, 3) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attributes` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==3 ~ ">=5% TMSA origin")))

sfig1u_p2 <- ggplot(data = tu1_lowereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual attributes`, pattern = `Individual attributes`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) +
  labs(x = "Ethnic composition of lower-educated civic organizations", y = "Predicted probability", pattern = "Individual attributes") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1u_p2

# Panel 3: higher-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tu1_highereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_1.xlsx %>%
  filter(Outcome_Level %in% c(2, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual ethnicity` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==2 ~ "<5% TMSA origin",
                                        Outcome_Level==4 ~ ">=5% TMSA origin")))

sfig1u_p3 <- ggplot(data = tu1_highereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual ethnicity`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) +
  labs(x = "Ethnic composition of higher-educated civic organizations", y = "Predicted probability") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'bottom', 
        panel.background = element_blank(),
        #legend.title = element_blank(),
        axis.line = element_line(color = "black"))
sfig1u_p3

## Figure 1B - Online Supplement (Rural Sample) ####
### Panel 1: all organizations, by ethnicity (based on model mlogit w1_eth_comp_2cat i.w1_eth_3catB) 
tr1_eth1 <- predprobs__across__eth_comp__eth_urb_0.xlsx %>%
  filter(Outcome_Level %in% c(1, 2, 3, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attribute` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==0 ~ "Not involved", Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==2 ~ ">=5% TMSA origin"), 
                              levels = c("Not involved", "<5% TMSA origin", ">=5% TMSA origin")))

sfig1r_p1 <- ggplot(tr1_eth1, aes(fill=`Individual attribute`,
                                  y=Pred_Probability,
                                  x=Composition,
                                  ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylab("Predicted probability") + 
  ylim(0, 0.45) +
  xlab("Ethnic composition of civic organizations") +
  ggtitle("Rural Sample") +
  scale_fill_manual(values = c("grey60", "grey80")) +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1r_p1

### Combined outcome by education:
# Panel 2: lower-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tr1_lowereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_0.xlsx %>%
  filter(Outcome_Level %in% c(1, 3) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual attributes` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==1 ~ "<5% TMSA origin",
                                        Outcome_Level==3 ~ ">=5% TMSA origin")))

sfig1r_p2 <- ggplot(data = tr1_lowereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual attributes`, pattern = `Individual attributes`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") +
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) +
  labs(x = "Ethnic composition of lower-educated civic organizations", y = "Predicted probability", pattern = "Individual attributes") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'none', 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
sfig1r_p2

# Panel 3: higher-educated orgs only (based on model: mlogit w1_ee_comp_5cat i.w1_eth_3catB)
tr1_highereducated_orgs <- predprobs__across__ee_comp__eth_edu_ia_urb_0.xlsx %>%
  filter(Outcome_Level %in% c(2, 4) & Group %in% c("Dutch origin", "TMSA origin")) %>%
  mutate(`Individual ethnicity` = factor(Group, levels = c("Dutch origin", "TMSA origin")),
         Composition = factor(case_when(Outcome_Level==2 ~ "<5% TMSA origin",
                                        Outcome_Level==4 ~ ">=5% TMSA origin")))

sfig1r_p3 <- ggplot(data = tr1_highereducated_orgs, aes(x = Composition, y= Pred_Probability, fill = `Individual ethnicity`, ymin=L_CI, ymax=U_CI)) +
  geom_bar(position=position_dodge(), aes(y=Pred_Probability), color="black", stat="identity") + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) +
  labs(x = "Ethnic composition of higher-educated civic organizations", y = "Predicted probability") +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.9)) +
  ylim(0, 0.3) +
  theme(legend.position = 'bottom', 
        panel.background = element_blank(),
        #legend.title = element_blank(),
        axis.line = element_line(color = "black"))
sfig1r_p3

#one single figure for urban and rural together. 
design_figS4 <- "AD
                 BE
                 CG"

sfigure_4 <- wrap_plots(list(A=sfig1u_p1, B=sfig1u_p2, C=sfig1u_p3,
                             D=sfig1r_p1, E=sfig1r_p2, G=sfig1r_p3), design = design_figS4)
sfigure_4 + plot_annotation(tag_levels = "A") #add plot annotations (Panel A,B,C)
ggsave("03_Outputs/figure_1_urbanrural_bw.jpg", dpi = 600, width = 30, height = 20, units = c("cm"))
ggsave("03_Outputs/figure_1_urbanrural_bw.eps", dpi = 600, width = 30, height = 20, units = c("cm"))


# Predicted probabilities of individual outcomes of ologit regression by ethnicity ####
pred_prob_within <- readxl::read_excel("01_Data/03_edit/2017_2018/predicted_probabilities/h2_individual_outcome_categories.xlsx")
pred_prob_within <- pred_prob_within %>%
  mutate(frequency_f = factor(frequency, levels = c("never", "yearly", "monthly", "weekly")))
ggplot(pred_prob_within) +
  facet_wrap(~ethnicity) +
  geom_bar(position=position_dodge(), aes(x = frequency_f, y = mean, fill = ethnicity), color = "black", stat = "identity",) +
  geom_errorbar(aes(x = frequency_f, ymin =lci, ymax=uci), width = 0.3) +
  xlab("Frequency of contact with co-members of TMSA origin") +
  ylab("Predicted Probability") +
  scale_fill_manual(values = c("Dutch origin" = "orange", "TMSA origin" = "lightblue")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none")
ggsave("03_Outputs/within_pred_probs.jpg", height = 5, width = 7.5)
ggsave("03_Outputs/within_pred_probs.eps", height = 5, width = 7.5)

ggplot(pred_prob_within) +
  facet_wrap(~ethnicity) +
  geom_bar(position=position_dodge(), aes(x = frequency_f, y = mean, fill = ethnicity), color = "black", stat = "identity",) +
  geom_errorbar(aes(x = frequency_f, ymin =lci, ymax=uci), width = 0.3) +
  xlab("Frequency of contact with co-members of TMSA origin") +
  ylab("Predicted Probability") +
  scale_fill_manual(values = c("Dutch origin" = "grey60", "TMSA origin" = "grey80")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none")
ggsave("03_Outputs/within_pred_probs_bw.jpg", height = 5, width = 7.5)
ggsave("03_Outputs/within_pred_probs_bw.eps", height = 5, width = 7.5)


