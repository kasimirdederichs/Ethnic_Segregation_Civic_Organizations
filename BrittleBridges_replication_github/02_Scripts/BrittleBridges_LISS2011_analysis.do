
version 16.1
clear all
set showbaselevels on
set more off
set logtype text
capture log close
set seed 19960909

*Set global working directory
global workdir "/yourpath/BrittleBridges_replication" 

global data "${workdir}/01_Data/03_edit/2011_2012"
global outputs "${workdir}/03_Outputs"

use "$data/civic_participation_for_analyses_dutch.dta", clear


*Preparation for the Fixed effects models:
xtset nomem_encr

*sample variable
gen fe_sample = .
replace fe_sample = 1 if kn_by_name_t_log_c!=. & sh_di_imp_mat_c!=. & c.sh_sam_eth_c!=. & c.nr_members_t_log_c!=. & sh_sam_edu_c!=. & sh_sam_sex_c!=. & type_of_org!=. & quit_volorg!=.
gen fe_sample_first_org = .
replace fe_sample_first_org = 1 if fe_sample ==1 & volcount==1

*Dummy variables for descriptive overview:
tab sted, gen(sted_d)
tab aantalki, gen(aantalki_d)

*Correlation number of members in organization and Share of members known by name
cor nr_members sh_kn_by_name if fe_sample==1

********************************************
*** COMPOSITION->TIES WITHIN ORGANIZATION***
********************************************

*range of original nr_members_log_c variable:
summarize nr_members_log_c
*ranges from -3.16 to 5.36
*range of truncated (at t=1000 members) variable:
summarize nr_members_t_log_c
*ranges from -3.16 to 3.06

***Outcome: Weak ties***
xtreg sh_kn_by_name c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_wties_sh_ols_1_fe
*The negative effect persists. 
xtreg sh_kn_by_name c.sh_dif_eth_c##c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_wties_sh_ols_ia_fe
margins, dydx(c.sh_dif_eth_c) at(c.nr_members_t_log_c = (-3(1)3))
*Negative effect especially among smaller organizations. 

esttab comp_wties_sh_ols_1_fe comp_wties_sh_ols_ia_fe using $outputs/orgsize_robustness.rtf, b(3) se(3) wide replace


***Outcome: Strong ties***

xtreg di_imp_mat_t_log_c c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_sties_nr_ols_1_fe

*There is a negative effect of the share of ethnic minority members on the number of people one discusses important matters with: With each percentage point more minority members, the log of people known by name decreases by .42, while controlling for the size of the organization. Thus, in two organizations of the same size and type (e.g., sports/religious etc.), individuals tend to know fewer people by name if the share of ethnic minorities is higher among the co-members. 
test c.sh_dif_eth_c = sh_dif_edu_c
test c.sh_dif_eth_c = sh_dif_sex_c
*The ethnicity composition coefficient is not sign. larger than the sex and educ coefficients. 

xtreg di_imp_mat_t_log_c c.sh_dif_eth_c##c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_sties_nr_ols_ia_fe
margins, dydx(c.sh_dif_eth_c) at(c.nr_members_t_log_c = (-3(1)3))
*But does this hold across all organization sizes? We find that the negative effect persists across almost all levels of orgnaization size. Only in very large organizations, the effect turns insignificant. 

esttab comp_sties_nr_ols_1_fe comp_sties_nr_ols_ia_fe, b(3) se(3) wide


**********************************************************
*** COMPOSITION-> TIES WITHIN ORGANIZATION->QUITTING *****
**********************************************************

*OLS:
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_1_fe
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org sh_kn_by_name_c di_imp_mat_t_log_c if ethnicity == "Dutch", fe vce(robust) //sh_kn_by_name_c kn_by_name_t_log_c
est sto comp_bties_quit_ols_2_fe

esttab comp_bties_quit_ols_1_fe comp_bties_quit_ols_2_fe, b(3) se(3) wide


***********************************
***TABLE 2: Testing Hypothesis 3/4***
***********************************

*retiterating the final models:
xtreg sh_kn_by_name c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_wties_sh_ols_1_fe
xtreg di_imp_mat_t_log_c c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_sties_nr_ols_1_fe
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_1_fe
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org sh_kn_by_name_c di_imp_mat_t_log_c if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_2_fe

esttab comp_wties_sh_ols_1_fe comp_sties_nr_ols_1_fe comp_bties_quit_ols_1_fe comp_bties_quit_ols_2_fe, b(3) se(3) wide
esttab comp_wties_sh_ols_1_fe comp_sties_nr_ols_1_fe comp_bties_quit_ols_1_fe comp_bties_quit_ols_2_fe using $outputs/table2.rtf, replace b(3) se(3) wide nogaps ///
	mtitle("Weak ties" "Strong ties" "Quitting" "Quitting") nonumbers ///
	title("Fixed Effects Regression: Weak and strong ties within organizations and Quitting") ///
	order(sh_dif_eth_c sh_dif_edu_c sh_dif_sex_c sh_kn_by_name_c di_imp_mat_t_log_c nr_members_t_log_c type_of_org) ///
	coeflabels(sh_dif_eth_c "Share of co-members with different ethnicity" ///
			   sh_dif_edu_c "Share of co-members with different education" ///
			   sh_dif_sex_c "Share of co-members with different sex" ///
			   sh_kn_by_name_c "Weak ties" ///
			   di_imp_mat_t_log_c "Strong ties" ///
			   nr_members_t_log_c "Log(Nr of members)" ///
			   1.type_of_org "Sports (ref.)" 2.type_of_org "Culture/Hobby" 3.type_of_org "Union" ///
			   4.type_of_org "Professional" 5.type_of_org "Consumer" 6.type_of_org "Humanitarian aid" ///
			   7.type_of_org "Environmental" 8.type_of_org "Religious" 9.type_of_org "Political" ///
			   10.type_of_org "Education" 11.type_of_org "Social/Youth" 12.type_of_org "Other" ///
			   _cons "Constant") ///
	addnote("Note: Only Dutch individuals")

	
*H3: cumulative effect of selective leaving:
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity == "Dutch", fe vce(robust)
margins, at(sh_dif_eth=(0(0.1)1))

*Model 4 without strong ties (for footnote)
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org sh_kn_by_name_c if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_foot

*** ON HYPOTHESIS 3: EXAMPLES FOR TEXT
*Calculate substantive effect for strong ties:
xtreg kn_by_name_t_log_c c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
//di exp(_b[sh_sam_eth_c]*1)
*Increase in ingroup size by 100%:
di exp(abs(_b[sh_dif_eth_c])*1)
di exp(abs(_b[sh_dif_eth_c])*1)*4 //e.g. a person with 4 strong ties in 0% ingroup org would have 6.36 strong ties in 100% ingroup org. 
*Increase in ingroup size by 10%:
di exp(abs(_b[sh_dif_eth_c])*0.1)
di exp(abs(_b[sh_dif_eth_c])*0.1)*4 // e.g. a person with 4 strong ties in 70% ingroup org would have 4.19 strong ties in 80% ingroup org. 

*********************************************
***DESCRIPTIVE TABLE FOR ONLINE SUPPLEMENT***
*********************************************

*variables on the organization level:
estpost tabstat sh_dif_eth sh_dif_edu sh_dif_sex sh_kn_by_name di_imp_mat_t di_imp_mat_t_log nr_members_t nr_members_t_log quit_volorg typ_Sports typ_CultureHobby typ_Union typ_Professional typ_Consumer typ_Humanitarian typ_Environmental typ_Religious typ_Political typ_Education typ_SocialYouth typ_Other if fe_sample==1, statistics(mean sd) columns(statistics)
esttab . using $outputs/des2011_org_level.rtf, cells("mean(fmt(%9.3f)) sd") mtitle("Organization Level") ///
			coeflabels(sh_dif_eth "Share of co-members with different ethnicity" ///
					   sh_dif_edu "Share of co-members with different education" ///
					   sh_dif_sex "Share of co-members with different sex" ///
					   sh_kn_by_name "Weak ties" ///
					   di_imp_mat "Strong ties" ///
					   di_imp_mat_t_log "Log(Strong ties)" ///
					   nr_members_t "Nr of members" ///
					   nr_members_t_log "Log(Nr of members)" ///
					   typ_Sports "Sports (ref.)" ///
					   typ_CultureHobby "Culture/Hobby" ///
					   typ_Union "Union" ///
					   typ_Professional "Professional" ///
					   typ_Consumer "Consumer" ///
					   typ_Humanitarian "Humanitarian aid" ///
					   typ_Environmental "Environmental" ///
					   typ_Religious "Religious" ///
					   typ_Political "Political" ///
					   typ_Education "Education" ///
					   typ_SocialYouth "Social/Youth" ///
					   typ_Other "Other" ///
					   ) replace


*variables on the respondent level (only take one observation per respondent by using fe_sample_first_org==1):
estpost tabstat obs_orgs woman age_cont hbo partner aantalki_d1 aantalki_d2 aantalki_d3 aantalki_d4 aantalki_d5 aantalki_d6 aantalki_d7 inc_pp sted_d1 sted_d2 sted_d3 sted_d4 sted_d5 if fe_sample_first_org==1, statistics(mean sd) columns(statistics)
esttab . using $outputs/des2011_resp_level.rtf, cells("mean(fmt(%9.3f)) sd") mtitle("Respondent Level") ///
			coeflabels(obs_orgs "Number of observed organizations" ///
					   woman "Woman" ///
					   age_cont "Age (in years)" ///
					   hbo "Higher educated" ///
					   partner "Partner in HH" ///
					   sted_d1 "Extremely urban" ///
					   sted_d2 "Very urban" ///
					   sted_d3 "Moderately urban" ///
					   sted_d4 "Slightly urban" ///
					   sted_d5 "Not urban") replace 

***********************************************************
***ROBOUSTNESS CHECK: TABLE 2 INCLUDING AGE AS CONTROL***
***********************************************************

xtreg sh_kn_by_name c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c sh_dif_age_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_wties_sh_ols_1_fe_a
xtreg di_imp_mat_t_log_c c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c sh_dif_age_c i.type_of_org if ethnicity=="Dutch", fe vce(robust)
est sto comp_sties_nr_ols_1_fe_a
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c sh_dif_age_c i.type_of_org if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_1_fe_a
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c sh_dif_age_c i.type_of_org sh_kn_by_name_c di_imp_mat_t_log_c if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_2_fe_a

esttab comp_wties_sh_ols_1_fe_a comp_sties_nr_ols_1_fe_a comp_bties_quit_ols_1_fe_a comp_bties_quit_ols_2_fe_a, b(2) se(2) wide
esttab comp_wties_sh_ols_1_fe_a comp_sties_nr_ols_1_fe_a comp_bties_quit_ols_1_fe_a comp_bties_quit_ols_2_fe_a using $outputs/table2_robust_age.rtf, replace b(2) se(2) wide nogaps ///
	mtitle("Weak ties" "Strong ties" "Quitting" "Quitting") nonumbers ///
	title("Fixed Effects Regression: Weak and strong ties within organizations and Quitting (controlling for age composition)") ///
	order(sh_dif_eth_c sh_dif_edu_c sh_dif_sex_c sh_dif_age_c sh_kn_by_name_c di_imp_mat_t_log_c nr_members_t_log_c type_of_org) ///
	coeflabels(sh_dif_eth_c "Share of co-members with different ethnicity" ///
			   sh_dif_edu_c "Share of co-members with different education" ///
			   sh_dif_sex_c "Share of co-members with different sex" ///
			   sh_dif_age_c "Share of co-members with different age" ///
			   sh_kn_by_name_c "Weak ties" ///
			   di_imp_mat_t_log_c "Strong ties" ///
			   nr_members_t_log_c "Log(Nr of members)" ///
			   1.type_of_org "Sports (ref.)" 2.type_of_org "Culture/Hobby" 3.type_of_org "Union" ///
			   4.type_of_org "Professional" 5.type_of_org "Consumer" 6.type_of_org "Humanitarian aid" ///
			   7.type_of_org "Environmental" 8.type_of_org "Religious" 9.type_of_org "Political" ///
			   10.type_of_org "Education" 11.type_of_org "Social/Youth" 12.type_of_org "Other" ///
			   _cons "Constant") ///
	addnote("Note: Only Dutch individuals")



**********************************************
***ROBOUSTNESS CHECK: TABLE 2 RE-WEIGHTED***
**********************************************

xtreg sh_kn_by_name c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org [pw=invprobwgt_no_relig] if ethnicity=="Dutch", fe vce(robust)
est sto comp_wties_sh_ols_1_fe_w
xtreg di_imp_mat_t_log_c c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org [pw=invprobwgt_no_relig] if ethnicity=="Dutch", fe vce(robust)
est sto comp_sties_nr_ols_1_fe_w
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org [pw=invprobwgt_no_relig] if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_1_fe_w
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org sh_kn_by_name_c di_imp_mat_t_log_c [pw=invprobwgt_no_relig] if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_2_fe_w

esttab comp_wties_sh_ols_1_fe_w comp_sties_nr_ols_1_fe_w comp_bties_quit_ols_1_fe_w comp_bties_quit_ols_2_fe_w, b(2) se(2) wide
esttab comp_wties_sh_ols_1_fe_w comp_sties_nr_ols_1_fe_w comp_bties_quit_ols_1_fe_w comp_bties_quit_ols_2_fe_w using $outputs/table2_robust_weighted.rtf, replace b(2) se(2) wide nogaps ///
	mtitle("Weak ties" "Strong ties" "Quitting" "Quitting") nonumbers ///
	title("Fixed Effects Regression: Weak and strong ties within organizations and Quitting (re-weighted)") ///
	order(sh_dif_eth_c sh_dif_edu_c sh_dif_sex_c sh_kn_by_name_c di_imp_mat_t_log_c nr_members_t_log_c type_of_org) ///
	coeflabels(sh_dif_eth_c "Share of co-members with different ethnicity" ///
			   sh_dif_edu_c "Share of co-members with different education" ///
			   sh_dif_sex_c "Share of co-members with different sex" ///
			   sh_kn_by_name_c "Weak ties" ///
			   di_imp_mat_t_log_c "Strong ties" ///
			   nr_members_t_log_c "Log(Nr of members)" ///
			   1.type_of_org "Sports (ref.)" 2.type_of_org "Culture/Hobby" 3.type_of_org "Union" ///
			   4.type_of_org "Professional" 5.type_of_org "Consumer" 6.type_of_org "Humanitarian aid" ///
			   7.type_of_org "Environmental" 8.type_of_org "Religious" 9.type_of_org "Political" ///
			   10.type_of_org "Education" 11.type_of_org "Social/Youth" 12.type_of_org "Other" ///
			   _cons "Constant") ///
	addnote("Note: Only Dutch individuals")

*and the same including religiosity variables in the weights
xtreg sh_kn_by_name c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org [pw=invprobwgt_incl_relig] if ethnicity=="Dutch", fe vce(robust)
est sto comp_wties_sh_ols_1_fe_wr
xtreg di_imp_mat_t_log_c c.sh_dif_eth_c c.nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org [pw=invprobwgt_incl_relig] if ethnicity=="Dutch", fe vce(robust)
est sto comp_sties_nr_ols_1_fe_wr
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org [pw=invprobwgt_incl_relig] if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_1_fe_wr
xtreg quit_volorg sh_dif_eth_c nr_members_t_log_c sh_dif_edu_c sh_dif_sex_c i.type_of_org sh_kn_by_name_c di_imp_mat_t_log_c [pw=invprobwgt_incl_relig] if ethnicity == "Dutch", fe vce(robust)
est sto comp_bties_quit_ols_2_fe_wr

esttab comp_wties_sh_ols_1_fe_wr comp_sties_nr_ols_1_fe_wr comp_bties_quit_ols_1_fe_wr comp_bties_quit_ols_2_fe_wr, b(2) se(2) wide
	

