*Analysis of the 2017 LISS data:

version 16.1
clear all
set showbaselevels on
set more off
set logtype text
capture log close
set seed 19960909
set scrollbufsize 800000 //allows to scroll back further in the outputs. 
// ssc install asdoc

*Set global working directory
global workdir "/yourpath/BrittleBridges_replication" 

global data "${workdir}/01_Data/03_edit/2017_2018"
global predprob "${workdir}/01_Data/03_edit/2017_2018/predicted_probabilities"
global outputs "${workdir}/03_Outputs"

use "$data/LISS2017_for_stata_import.dta", clear

***Generate variables***

*Generate combined composition variable: Diverse if at least 5% TMSA, Higher Educated if at least 50% with HBO degree. 
tab w1_eth_comp w1_edu_comp, nol
generate w1_ee_comp_4cat = .
replace w1_ee_comp_4cat = 1 if (w1_eth_comp==1 | w1_eth_comp==2) & (w1_edu_comp==1 | w1_edu_comp==2 | w1_edu_comp==3 | w1_edu_comp==4)
replace w1_ee_comp_4cat = 2 if (w1_eth_comp==1 | w1_eth_comp==2) & (w1_edu_comp==5 | w1_edu_comp==6 | w1_edu_comp==7)
replace w1_ee_comp_4cat = 3 if (w1_eth_comp==3 | w1_eth_comp==4 | w1_eth_comp==5 | w1_eth_comp==6 | w1_eth_comp==7) & (w1_edu_comp==1 | w1_edu_comp==2 | w1_edu_comp==3 | w1_edu_comp==4)
replace w1_ee_comp_4cat = 4 if (w1_eth_comp==3 | w1_eth_comp==4 | w1_eth_comp==5 | w1_eth_comp==6 | w1_eth_comp==7) & (w1_edu_comp==5 | w1_edu_comp==6 | w1_edu_comp==7)
label define w1_ee_comp_4cat 1 "Dutch, Lower educated" 2 "Dutch, Higher educated" 3 "Diverse, Lower educated" 4 "Diverse, Higher educated"
label values w1_ee_comp_4cat w1_ee_comp_4cat
label variable w1_ee_comp_4cat "Composition in terms of ethnicity and education"
tab w1_ee_comp_4cat, m

*Adding those who are uninvolved as a category. 
generate w1_ee_comp_5cat = w1_ee_comp_4cat
replace w1_ee_comp_5cat = 0 if w1_memvol1==0 & w1_ee_comp_4cat==.
label define w1_ee_comp_5cat 0 "Not involved" 1 "Dutch, Lower educated" 2 "Dutch, Higher educated" 3 "Diverse, Lower educated" 4 "Diverse, Higher educated"
label values w1_ee_comp_5cat w1_ee_comp_5cat
label variable w1_ee_comp_5cat "Composition in terms of ethnicity and education"
tab w1_ee_comp_5cat, m
*There remain 578 involved individuals with missing values for whom we are missing the ethnic and/or educational composition.

*Based on this sample, we generate compositional variables for the single dimensions (education and ethnicity): We only take individuals into account who have non-missing values on the w1_ee_comp_5cat variable. 
generate w1_edu_comp_2cat = .
replace w1_edu_comp_2cat = 1 if (w1_edu_comp==1 | w1_edu_comp==2 | w1_edu_comp==3 | w1_edu_comp==4) & w1_ee_comp_5cat!=.
replace w1_edu_comp_2cat = 2 if (w1_edu_comp==5 | w1_edu_comp==6 | w1_edu_comp==7) & w1_ee_comp_5cat!=.
replace w1_edu_comp_2cat = 0 if w1_edu_comp==. & w1_memvol1==0 & w1_ee_comp_5cat!=. 
label define w1_edu_comp_2cat 0 "Not involved" 1 "Lower educated" 2 "Higher educated"
label values w1_edu_comp_2cat w1_edu_comp_2cat
label var w1_edu_comp_2cat "Educational composition of organization"


generate w1_eth_comp_2cat = .
replace w1_eth_comp_2cat = 1 if (w1_eth_comp==1 | w1_eth_comp==2) & w1_ee_comp_5cat!=.
replace w1_eth_comp_2cat = 2 if (w1_eth_comp==3 | w1_eth_comp==4 | w1_eth_comp==5 | w1_eth_comp==6 | w1_eth_comp==7) & w1_ee_comp_5cat!=.
replace w1_eth_comp_2cat = 0 if w1_eth_comp==. & w1_memvol1==0 & w1_ee_comp_5cat!=.
label define w1_eth_comp_2cat 0 "Not involved" 1 "Dutch" 2 "Diverse"
label values w1_eth_comp_2cat w1_eth_comp_2cat
label var w1_eth_comp_2cat "Educational composition of organization"

tab w1_ee_comp_5cat w1_eth_comp_2cat, m
tab w1_ee_comp_5cat w1_edu_comp_2cat, m


***Create urbanicity variable:
generate urban = .
replace urban = 1 if w1_sted==1 | w1_sted==2
replace urban = 0 if w1_sted==3 | w1_sted==4 | w1_sted==5
generate rural = 1-urban

***Create type of org. variable with value for missings: (I used this variable for looking, whether one can run a model predicting the composition while controlling for the type of org while including those who are not involved, but it does not converge)
generate w1_mo_all = w1_mo 
replace w1_mo_all = 0 if w1_mo_all==. & w1_memvol1!=.

**************************
***DESCRIPTIVE OVERVIEW***
**************************

*generate dummy variables for descriptive overview:
tab w1_eth_comp_2cat, gen(w1_eth_comp_2cat_d)
tab w1_edu_comp_2cat, gen(w1_edu_comp_2cat_d)
tab w1_ee_comp_5cat, gen(w1_ee_comp_5cat_d)
tab w1_edu_2cat, gen(w1_edu_2cat_d) 
gen w1_eth_cont_des = w1_eth_cont //generate variable with appropriate missing coding for descriptive overview
replace w1_eth_cont_des = 5 if w1_ee_comp_5cat!=. & w1_ee_comp_5cat!=0 & w1_eth_3catB!=. & w1_edu_2cat!=. & w1_eth_cont ==.
tab w1_eth_cont_des, gen(w1_eth_cont_des_d) 
//tab w1_edu_cont, m gen(w1_edu_cont_d) //including missing as category 
gen urban_des = 0
replace urban_des = 1 if urban==1
gen rural_des = 0
replace rural_des = 1 if urban==0
gen urbanruralmiss_des = 0
replace urbanruralmiss_des = 1 if urban==.

gen allsample = 1 if w1_ee_comp_5cat!=.

bysort w1_eth_3catB: tabstat w1_memvol w1_eth_comp_2cat_d1 w1_eth_comp_2cat_d2 w1_eth_comp_2cat_d3, statistics(sum mean) columns(s)

bysort w1_eth_3catB: asdoc tabstat ///
		w1_memvol w1_ee_comp_5cat_d1 ///
		w1_eth_comp_2cat_d2 w1_eth_comp_2cat_d3 ///
		w1_edu_comp_2cat_d2 w1_edu_comp_2cat_d3 ///
		w1_ee_comp_5cat_d2 w1_ee_comp_5cat_d3 w1_ee_comp_5cat_d4 w1_ee_comp_5cat_d5 ///
		w1_eth_cont_des_d1 w1_eth_cont_des_d2 w1_eth_cont_des_d3 w1_eth_cont_des_d4 w1_eth_cont_des_d5 ///
		urban_des rural_des urbanruralmiss_des ///
		w1_edu_2cat_d1 w1_edu_2cat_d2 ///
		allsample ///
		if w1_ee_comp_5cat!=. & w1_eth_3catB!=. & w1_edu_2cat!=., statistics(sum mean) columns(s) format(%9.3f) save($outputs/des2017.doc) replace

tabstat ///
		w1_memvol w1_ee_comp_5cat_d1 ///
		w1_eth_comp_2cat_d2 w1_eth_comp_2cat_d3 ///
		w1_edu_comp_2cat_d2 w1_edu_comp_2cat_d3 ///
		w1_ee_comp_5cat_d2 w1_ee_comp_5cat_d3 w1_ee_comp_5cat_d4 w1_ee_comp_5cat_d5 ///
		w1_eth_cont_des_d1 w1_eth_cont_des_d2 w1_eth_cont_des_d3 w1_eth_cont_des_d4 w1_eth_cont_des_d5 ///
		urban_des rural_des urbanruralmiss_des ///
		w1_edu_2cat_d1 w1_edu_2cat_d2 ///
		allsample ///
		if w1_ee_comp_5cat!=. & w1_eth_3catB!=. & w1_edu_2cat!=., statistics(sum mean) columns(s) format(%9.3f)
		

tab w1_eth_3catB urban, col
tab w1_edu_2cat urban, col

*Get share of eth minorities and higher educated among civic participants:
tab w1_eth_3catB w1_memvol if allsample==1 & w1_ee_comp_5cat!=. & w1_eth_3catB!=. & w1_edu_2cat!=., col //3.92%
tab w1_edu_2cat w1_memvol if allsample==1 & w1_ee_comp_5cat!=. & w1_eth_3catB!=. & w1_edu_2cat!=., col //49.75%

*****************************************************
***MEMBERSHIP/VOLUNTEERING AT W1 (CROSS-SECTOINAL)***
*****************************************************

***Ethnicity***
***************
*Initial model:
	putexcel set $predprob/res_across__eth_comp__eth, replace
	putexcel A1 = "Group"
	putexcel B1 = "Outcome_Level"
	putexcel C1 = "Pred_Probability"
	putexcel D1 = "L_CI"
	putexcel E1 = "U_CI"
	putexcel A2 = "Dutch origin"
	putexcel A3 = "TMSA origin"
	putexcel A4 = "Other origin"
	putexcel A5 = "Dutch origin"
	putexcel A6 = "TMSA origin"
	putexcel A7 = "Other origin"
	putexcel A8 = "Dutch origin"
	putexcel A9 = "TMSA origin"
	putexcel A10 = "Other origin"

	mlogit w1_eth_comp_2cat i.w1_eth_3catB if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=., baseoutcome(0) vce(robust)
	margins w1_eth_3catB,pwcompare
	margins w1_eth_3catB
	matrix mr = r(table)' //transposed matrix
	matrix outcome_level = [0,0,0,1,1,1,2,2,2]'
	matrix b = mr[1..., 1]
	matrix ll = mr[1..., 5]
	matrix ul = mr[1..., 6]
	putexcel B2 = matrix(outcome_level)
	putexcel C2 = matrix(b) 
	putexcel D2 = matrix(ll) 
	putexcel E2 = matrix(ul) 
	
mlogit w1_eth_comp_2cat i.w1_eth_3catB if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=., baseoutcome(0) vce(robust) rrr
est sto f1_eth
margins, dydx(*) predict(outcome(1))
margins, dydx(*) predict(outcome(2))
margins w1_eth_3catB, predict(outcome(1))
margins w1_eth_3catB, predict(outcome(2))
margins w1_eth_3catB, pwcompare(effects)



*stratified by degree of urbanicity:
forvalues s = 0(1)1 {
	
	putexcel set $predprob/res_across__eth_comp__eth_urb_`s', replace
	putexcel A1 = "Group"
	putexcel B1 = "Outcome_Level"
	putexcel C1 = "Pred_Probability"
	putexcel D1 = "L_CI"
	putexcel E1 = "U_CI"
	putexcel A2 = "Dutch origin"
	putexcel A3 = "TMSA origin"
	putexcel A4 = "Other origin"
	putexcel A5 = "Dutch origin"
	putexcel A6 = "TMSA origin"
	putexcel A7 = "Other origin"
	putexcel A8 = "Dutch origin"
	putexcel A9 = "TMSA origin"
	putexcel A10 = "Other origin"
	
mlogit w1_eth_comp_2cat i.w1_eth_3catB if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==`s', baseoutcome(0) vce(robust)
margins w1_eth_3catB, pwcompare
margins w1_eth_3catB
matrix mree = r(table)'
matrix outcome_level = [0,0,0,1,1,1,2,2,2]'
matrix b = mree[1..., 1]
matrix ll = mree[1..., 5]
matrix ul = mree[1..., 6]
putexcel B2 = matrix(outcome_level)	
putexcel C2 = matrix(b)
putexcel D2 = matrix(ll)
putexcel E2 = matrix(ul)
}
mlogit w1_eth_comp_2cat i.w1_eth_3catB if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==1, baseoutcome(0) vce(robust) rrr
est sto f1_eth_urb1
mlogit w1_eth_comp_2cat i.w1_eth_3catB if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==0, baseoutcome(0) vce(robust) rrr
est sto f1_eth_urb0

* Ethnicity, 3-category outcome (i.e., only involved individuals)
	putexcel set $predprob/res_across__eth_comp3cat__eth_type, replace
	putexcel A1 = "Group"
	putexcel B1 = "Outcome_Level"
	putexcel C1 = "Pred_Probability"
	putexcel D1 = "L_CI"
	putexcel E1 = "U_CI"
	putexcel A2 = "Dutch origin"
	putexcel A3 = "TMSA origin"
	putexcel A4 = "Other origin"
	putexcel A5 = "Dutch origin"
	putexcel A6 = "TMSA origin"
	putexcel A7 = "Other origin"

		mlogit w1_eth_comp_2cat i.w1_eth_3catB i.w1_mo if w1_eth_comp_2cat>=0, baseoutcome(1) vce(robust)
		margins w1_eth_3catB, pwcompare
		margins w1_eth_3catB
		matrix mr = r(table)' //transposed matrix
		matrix list mr
		matrix outcome_level = [0,0,1,1,2,2]'
		matrix b = mr[1..., 1]
		matrix ll = mr[1..., 5]
		matrix ul = mr[1..., 6]
		putexcel B2 = matrix(outcome_level)
		putexcel C2 = matrix(b) 
		putexcel D2 = matrix(ll) 
		putexcel E2 = matrix(ul)
	

	
**** Multidimensional analysis: Interaction effect***
*****************************************************
putexcel set $predprob/res_across__ee_comp__eth_edu_ia, replace

putexcel A1 = "Group"
	putexcel B1 = "Outcome_Level"
	putexcel C1 = "Pred_Probability"
	putexcel D1 = "L_CI"
	putexcel E1 = "U_CI"
	putexcel A2 = "Dutch origin"
	putexcel A3 = "TMSA origin"
	putexcel A4 = "Other origin"
	putexcel A5 = "Dutch origin"
	putexcel A6 = "TMSA origin"
	putexcel A7 = "Other origin"
	putexcel A8 = "Dutch origin"
	putexcel A9 = "TMSA origin"
	putexcel A10 = "Other origin"
	putexcel A11 = "Dutch origin"
	putexcel A12 = "TMSA origin"
	putexcel A13 = "Other origin"
	putexcel A14 = "Dutch origin"
	putexcel A15 = "TMSA origin"
	putexcel A16 = "Other origin"
	
*Run multinomial models:
mlogit w1_ee_comp_5cat i.w1_eth_3catB##i.w1_edu_2cat if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=., baseoutcome(0) vce(robust) //check further digit of coefs and p values: est tab, p(%12.10g)#
margins w1_eth_3catB#i.w1_edu_2cat, pwcompare(effects)
margins w1_eth_3catB#i.w1_edu_2cat

margins w1_eth_3catB, pwcompare(effects)
margins w1_eth_3catB
matrix mree = r(table)'
matrix outcome_level = [0,0,0,1,1,1,2,2,2,3,3,3,4,4,4]'
matrix b = mree[1..., 1]
matrix ll = mree[1..., 5]
matrix ul = mree[1..., 6]
putexcel B2 = matrix(outcome_level)	
putexcel C2 = matrix(b)
putexcel D2 = matrix(ll)
putexcel E2 = matrix(ul)

mlogit w1_ee_comp_5cat i.w1_eth_3catB##i.w1_edu_2cat if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=., baseoutcome(0) vce(robust) rrr
est sto f1_ethedu
margins w1_eth_3catB, pwcompare(effects)
margins w1_eth_3catB, predict(outcome(1)) //Dutch, lower educated
margins w1_eth_3catB, predict(outcome(3)) //diverse, lower educated
di r(_b)
margins w1_eth_3catB, predict(outcome(2)) //Dutch, higher educated org. 
margins w1_eth_3catB, predict(outcome(4)) //diverse, higher educated org.
margins w1_eth_3catB, pwcompare(effect)



**** Interaction effects - urban sample:
putexcel set $predprob/res_across__ee_comp__eth_edu_ia_urb_1, replace

putexcel A1 = "Group"
	putexcel B1 = "Outcome_Level"
	putexcel C1 = "Pred_Probability"
	putexcel D1 = "L_CI"
	putexcel E1 = "U_CI"
	putexcel A2 = "Dutch origin"
	putexcel A3 = "TMSA origin"
	putexcel A4 = "Other origin"
	putexcel A5 = "Dutch origin"
	putexcel A6 = "TMSA origin"
	putexcel A7 = "Other origin"
	putexcel A8 = "Dutch origin"
	putexcel A9 = "TMSA origin"
	putexcel A10 = "Other origin"
	putexcel A11 = "Dutch origin"
	putexcel A12 = "TMSA origin"
	putexcel A13 = "Other origin"
	putexcel A14 = "Dutch origin"
	putexcel A15 = "TMSA origin"
	putexcel A16 = "Other origin"
	
*Run multinomial models:
mlogit w1_ee_comp_5cat i.w1_eth_3catB##i.w1_edu_2cat if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==1, baseoutcome(0) vce(robust)
margins w1_eth_3catB, pwcompare
margins w1_eth_3catB
matrix mree = r(table)'
matrix outcome_level = [0,0,0,1,1,1,2,2,2,3,3,3,4,4,4]'
matrix b = mree[1..., 1]
matrix ll = mree[1..., 5]
matrix ul = mree[1..., 6]
putexcel B2 = matrix(outcome_level)	
putexcel C2 = matrix(b)
putexcel D2 = matrix(ll)
putexcel E2 = matrix(ul)

mlogit w1_ee_comp_5cat i.w1_eth_3catB##i.w1_edu_2cat if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==1, baseoutcome(0) vce(robust) rrr
est sto f1_ethedu_urb1

**** Interaction effects - rural sample:
putexcel set $predprob/res_across__ee_comp__eth_edu_ia_urb_0, replace

putexcel A1 = "Group"
	putexcel B1 = "Outcome_Level"
	putexcel C1 = "Pred_Probability"
	putexcel D1 = "L_CI"
	putexcel E1 = "U_CI"
	putexcel A2 = "Dutch origin"
	putexcel A3 = "TMSA origin"
	putexcel A4 = "Other origin"
	putexcel A5 = "Dutch origin"
	putexcel A6 = "TMSA origin"
	putexcel A7 = "Other origin"
	putexcel A8 = "Dutch origin"
	putexcel A9 = "TMSA origin"
	putexcel A10 = "Other origin"
	putexcel A11 = "Dutch origin"
	putexcel A12 = "TMSA origin"
	putexcel A13 = "Other origin"
	putexcel A14 = "Dutch origin"
	putexcel A15 = "TMSA origin"
	putexcel A16 = "Other origin"
	
*Run multinomial models:
mlogit w1_ee_comp_5cat i.w1_eth_3catB##i.w1_edu_2cat if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==0, baseoutcome(0) vce(robust)
margins w1_eth_3catB, pwcompare
margins w1_eth_3catB
matrix mree = r(table)'
matrix outcome_level = [0,0,0,1,1,1,2,2,2,3,3,3,4,4,4]'
matrix b = mree[1..., 1]
matrix ll = mree[1..., 5]
matrix ul = mree[1..., 6]
putexcel B2 = matrix(outcome_level)	
putexcel C2 = matrix(b)
putexcel D2 = matrix(ll)
putexcel E2 = matrix(ul)

mlogit w1_ee_comp_5cat i.w1_eth_3catB##i.w1_edu_2cat if w1_ee_comp_5cat!=. & w1_edu_2cat!=. & w1_eth_3catB!=. & urban==0, baseoutcome(0) vce(robust) rrr
est sto f1_ethedu_urb0
margins, predict(outcome(1))
margins, predict(outcome(2))
margins, predict(outcome(3))

gen w1_eth_edu_6cat = .
replace w1_eth_edu_6cat = 1 if w1_eth_3catB==1 & w1_edu_2cat==1
replace w1_eth_edu_6cat = 2 if w1_eth_3catB==1 & w1_edu_2cat==2
replace w1_eth_edu_6cat = 3 if w1_eth_3catB==2 & w1_edu_2cat==1
replace w1_eth_edu_6cat = 4 if w1_eth_3catB==2 & w1_edu_2cat==2
replace w1_eth_edu_6cat = 5 if w1_eth_3catB==3 & w1_edu_2cat==1
replace w1_eth_edu_6cat = 6 if w1_eth_3catB==3 & w1_edu_2cat==2
label define w1_eth_edu_6cat 1 "Dutch origin, No HBO" 2 "Dutch origin, At least HBO" 3 "TMSA origin, No HBO" ///
							 4 "TMSA origin, At least HBO" 5 "Other origin, No HBO" 6 "Other origin, At least HBO"
label values w1_eth_edu_6cat w1_eth_edu_6cat



****************************************************
***Intergroup contact WITHIN civic organizations:***
****************************************************

*Generate variable that distinguishes high, medium, and low-contact types of organizations:
gen w1_mo_cont_3type = .
replace w1_mo_cont_3type = 1 if w1_mo==8 | w1_mo==10 | w1_mo==11 | w1_mo==12 
replace w1_mo_cont_3type = 2 if w1_mo==4 | w1_mo==3 | w1_mo==1 | w1_mo==6 | w1_mo==9 | w1_mo==13
replace w1_mo_cont_3type = 3 if w1_mo==5 | w1_mo==7 | w1_mo==2
label def w1_mo_cont_3type 1 "little contact" 2 "medium contact" 3 "much contact"
label values w1_mo_cont_3type w1_mo_cont_3type
tab w1_mo_cont_3type

***ETHNICITY
*COMPARING DUTCH AND TMSA INDIVIDUALS (while adjusting for ethnic composition of organization):

*Generate dummy variables for at least weekly/monthly contact:
gen w1_eth_cont_at_least_weekly = .
replace w1_eth_cont_at_least_weekly = 0 if w1_eth_cont==1 | w1_eth_cont ==2 | w1_eth_cont==3
replace w1_eth_cont_at_least_weekly = 1 if w1_eth_cont ==4
tab w1_eth_cont_at_least_weekly

gen w1_eth_cont_at_least_monthly = .
replace w1_eth_cont_at_least_monthly = 0 if w1_eth_cont==1 | w1_eth_cont ==2
replace w1_eth_cont_at_least_monthly = 1 if w1_eth_cont==3 | w1_eth_cont ==4
tab w1_eth_cont_at_least_monthly

tab w1_eth_3catB if w1_eth_3catB!=. & w1_eth_comp_3cat!=. & w1_eth_cont!=., m
*The following analyses rely on 1321 Dutch and 66 TMSA respondents (and 212 other)

*without controls:
ologit w1_eth_cont i.w1_eth_3catB if w1_eth_comp!=. & w1_mo_cont_3type!=., vce(robust) or
est sto within_eth_seg_nocontr
eststo: margins, dydx(w1_eth_3catB) post //this gives marginal effects for each level of the dependent variable separately which makes it more difficult to read. I would just stick to the odds ratios. 
*with controls - ologit:
ologit w1_eth_cont i.w1_eth_3catB i.w1_eth_comp i.w1_mo_cont_3type, vce(robust) or
est sto within_eth_seg_ologit

*export to word - eform shows exponentiated coefficients, i.e., odds ratios. 
esttab within_eth_seg_nocontr within_eth_seg_ologit, wide b(3) se(3) eform
esttab within_eth_seg_nocontr within_eth_seg_ologit using $outputs/table1.rtf, replace b(3) se(3) wide nogaps eform ///
	mtitle("Contact with TMSA co-members" "Contact with TMSA co-members") nonumbers ///
	title("Ordered Logit Regression: Contact with TMSA co-members") ///
	order(w1_eth_3catB w1_eth_comp w1_mo_cont_3type) ///
	coeflabels(1.w1_eth_3catB "Dutch origin (ref.)" ///
			   2.w1_eth_3catB "TMSA origin" ///
			   3.w1_eth_3catB "Other origin" ///
			   1.w1_eth_comp "None (ref.)" ///
			   2.w1_eth_comp "Less than 5%" 3.w1_eth_comp "5-10%" 4.w1_eth_comp "10-25%" ///
			   5.w1_eth_comp "25-50%" 6.w1_eth_comp "50-75%" 7.w1_eth_comp "75-100%" ///
			   1.w1_mo_cont_3type "Low contact frequency org. (ref.)" ///
			   2.w1_mo_cont_3type "Medium frequency org." ///
			   3.w1_mo_cont_3type "High frequency org." ///
			   _cons "Constant") ///
	addnote("Note: ...")

*Redo the analysis using OLS regression: 
reg w1_eth_cont i.w1_eth_3catB, vce(robust)
est sto within_eth_seg_ols1 //treating contact as continuous yields substantively the same results
reg w1_eth_cont i.w1_eth_3catB i.w1_eth_comp i.w1_mo_cont_3type, vce(robust)
est sto within_eth_seg_ols2 //treating contact as continuous yields substantively the same results
esttab within_eth_seg_ols1 within_eth_seg_ols2, wide b(3) se(3)
esttab within_eth_seg_ols1 within_eth_seg_ols2 using $outputs/table1_ols_robust.rtf, replace b(3) se(3) wide nogaps eform ///
	mtitle("Contact with TMSA co-members" "Contact with TMSA co-members") nonumbers ///
	title("Ordinary Least Squares Regression: Contact with TMSA co-members") ///
	order(w1_eth_3catB w1_eth_comp w1_mo_cont_3type) ///
	coeflabels(1.w1_eth_3catB "Dutch origin (ref.)" ///
			   2.w1_eth_3catB "TMSA origin" ///
			   3.w1_eth_3catB "Other origin" ///
			   1.w1_eth_comp "None (ref.)" ///
			   2.w1_eth_comp "Less than 5%" 3.w1_eth_comp "5-10%" 4.w1_eth_comp "10-25%" ///
			   5.w1_eth_comp "25-50%" 6.w1_eth_comp "50-75%" 7.w1_eth_comp "75-100%" ///
			   1.w1_mo_cont_3type "Low contact frequency org. (ref.)" ///
			   2.w1_mo_cont_3type "Medium frequency org." ///
			   3.w1_mo_cont_3type "High frequency org." ///
			   _cons "Constant") ///
	addnote("Note: ...")


*Redo the analysis using "at least monthly contact" as dependent variable:

logit w1_eth_cont_at_least_monthly i.w1_eth_3catB i.w1_eth_comp i.w1_mo_cont_3type, vce(robust) or
est sto within_eth_seg_1_monthly 
margins, dydx(w1_eth_3catB)
esttab within_eth_seg_ologit within_eth_seg_ols1 within_eth_seg_ols2 within_eth_seg_1_monthly, wide b(3) se(3)
*All ethnicity effects remain significant except in the weekly model (p=0.051). 




*Predicted probabilities of original model:
ologit w1_eth_cont i.w1_eth_3catB i.w1_eth_comp i.w1_mo_cont_3type, vce(robust)
predict pr_never pr_yearly pr_monthly pr_weekly, pr
label var pr_never "Never"
label var pr_yearly "Yearly"
label var pr_monthly "Monthly"
label var pr_weekly "Weekly"

putexcel set $predprob/h2_individual_outcome_categories, replace
putexcel A1 = "frequency" B1 = "mean" C1 = "SE" D1 = "DF" E1 = "X" F1 = "lci" G1 = "uci" H1 = "N" I1 = "Y" J1 = "Z" K1 = "ethnicity"
putexcel A2 = "never" A3 = "yearly" A4 = "monthly" A5 = "weekly"
putexcel A6 = "never" A7 = "yearly" A8 = "monthly" A9 = "weekly"
putexcel K2 = "Dutch origin" K3 = "Dutch origin" K4 = "Dutch origin" K5 = "Dutch origin"
putexcel K6 = "TMSA origin" K7 = "TMSA origin" K8 = "TMSA origin" K9 = "TMSA origin"
mean pr_never pr_yearly pr_monthly pr_weekly if w1_eth_3catB==1
matrix list = r(table)
putexcel B2 = matrix(r(table)')
mean pr_never pr_yearly pr_monthly pr_weekly if w1_eth_3catB==2
matrix list = r(table)
putexcel B6 = matrix(r(table)')

*Marginal effects:
ologit w1_eth_cont i.w1_eth_3catB i.w1_eth_comp i.w1_mo_cont_3type, vce(robust)
mchange w1_eth_3catB, brief
mchange, brief
mtable, at(w1_eth_3catB=(1 2 3)) atmeans stat(ci)
