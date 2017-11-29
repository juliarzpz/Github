* Extended Essay - Final version
* 1June2017 Julia Ruiz Pozuelo


********************************************************************************
********************************************************************************
********************************************************************************
**********                                                             *********
**********              Subjective poverty: Results                    *********
**********                                                             *********
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* SETTING UP THE DO-FILE
********************************************************************************

clear all 
version 14
set more off, perm

capture log close 
log using statatest, replace text 

********************************************************************************
* 1. FIGURES AND GRAPHS
********************************************************************************

gen long hhidfull=.
replace hhidfull=10000*q1c+hhid
sort hhidfull

gen year=. 
replace year=1993 if round==1
replace year=1994 if round==2
replace year=1995 if round==3
replace year=1997 if round==4
replace year=1999 if round==5
replace year=2004 if round==6

*Make sure there are no duplicates: 
sort hhidfull
quietly by hhidfull: gen dup = cond(_N==1, 0, _n) 
tab dup


xtset hhidfull year
xtdescribe 

*****************************************
*****           Figure 1-3         *****
***************************************** 

histogram subpov if year==1995, title("Subjective poverty, 1995") color(red)
histogram subpov if year==2004, title("Subjective poverty, 2004") color(blue)
histogram subpov if year==2009, title("Subjective poverty, 2009") color(green)

*****************************************
*****          Subpov concerns      *****
***************************************** 

** 1. Consumption adequacy

* Comparing subpov with adequacy of consumption, housing, etc. 

* Consumption: 
tab subpov q8p1s7a if year==2009

*Housing
tab subpov q9p1s7a if year==2009

** 2. Reliability

* In separate files: 
* Testing reliability_subpov.dta"
* Testingreliability.do 

** 3. Unitary Model 

* Separate file "testing unitary model.do" 
 

*****************************************
*****         Consumption data      *****
***************************************** 

by year: sum rconspc

by hhidfull: gen lrconspc=log(rconspc)
by hhidfull: gen dlrconspc=lrconspc-lrconspc[_n-1] 

sort hhidfull year
by hhidfull: gen drconspc=rconspc - rconspc[_n-1]
sum hhsize if drconspc<0


gen negative=. 
replace negative=0 if drconspc!=. 
replace negative=1 if drconspc<0

by hhidfull: egen neg2=max(negative)
sum neg2
tab neg2


*****************************************
*****            Shocks data        *****
*****************************************

tab death
tab ill
tab ill2
tab rind
tab cropshock 
tab anrain
* Severe livestock shock (gendi + water) 
tab liv_d
tab liv_w
gen liv_severe=(liv_d+liv_w )/2
tab liv_severe
* Moderate livestock shock 
tab liv_dd
tab liv_ww
gen liv_moderate=(liv_dd+liv_ww )/2
tab liv_moderate

*Rainfall 

drop lanrain
gen lanrain=log(anrain)

by hhidfull: egen average_rain= mean(anrain)

gen devanrain=anrain-average_rain

gen devanrainsq=devanrain^2
* Annual rainfall
sum anrain 
* Log (anrrain)
sum lanrain 
* Historical average rainfall
sum average_rain 
* Deviation
sum devanrain
* Deviation squared
sum devanrainsq


*****************************************
*****       Table Main Stats        *****
*****************************************

sum subpov bpl HHsatisf cons rconspc rind anrain hhsize age sexhead catholic ///
 muslim tigray oromo

********************************************************************************
** 2. Consumption decomposition: Reduced-form consumption equation
********************************************************************************

* create some interaction 
gen rainlsu=anrain*lsu

* Create trend
gen year2=year-1992


*** create other changes variables

sort hhidfull round

foreach var of varlist cons-lrconsae {
by hhidfull: gen d`var'=`var'-`var'[_n-1]
label variable d`var' "change in `var'"
}




********************************************************************************
** 3. MODEL (FIXED EFFECTS) 
********************************************************************************


*************************
* 1. CONSUMPTION
*************************


xtset hhidfull year
xtdescribe 

xtreg rconspc anrain hhsize rind, fe
outreg2 subpov using resultscons.xls , nolabel title(Model) ctitle (consumption) tstat  bracket bdec(5) tdec(1)   addstat(Adjusted R-squared, e(r2_a), F test, e(F), Observations, e(N)) rdec(2) alpha(0.01, 0.05, 0.10) symbol(***, **, *)  replace


* Retrieve Fe
drop u_fe
predict u_fe, u
* Test joint significance
test  _b[anrain]==_b[hhsize]==_b[rind]==0

** A. Generating consumption permanent
drop mean_anrain
drop mean_rin
drop mean_hhsize

sort hhidfull
by hhidfull: egen mean_anrain=mean(anrain)
by hhidfull: egen mean_rind=mean(rind)
by hhidfull: egen mean_hhsize=mean(hhsize)
 

drop cons_perm_fe
gen cons_perm_fe = u_fe + _b[_cons] + _b[hhsize]*hhsize + _b[anrain]*mean_anrain + _b[rind]*mean_rind 

** B. Generating consumption transitory 

drop cons_trans_fe
gen cons_trans_fe= _b[anrain]*anrain + _b[rind]*rind - _b[anrain]*mean_anrain - _b[rind]*mean_rind 


** C. Generating incunexplained
* Calculate the unexplained component = 
drop cons_unexp_fe
gen cons_unexp_fe= rconspc - cons_perm_fe -cons_trans_fe


sum cons_perm_fe cons_trans_fe cons_unexp_fe
corr cons_perm_fe cons_trans_fe cons_unexp_fe

* For footnote: RE vs. FE
** Random effects

xtreg rconspc anrain hhsize rind, fe
estimates store fixed
xtreg rconspc anrain hhsize rind, re
estimates store random
hausman fixed random


*************************
* 2. SUB POVERTY 
*************************


ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo
outreg2 using mymod.xls, replace ctitle(Logit coeff)


* Marginal effects
quietly: ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe i.sexhead catholic muslim age tigray oromo
margins, dydx (*) predict(outcome(6)) atmeans post
outreg2 using mymod.xls, replace ctitle(Margins)


quietly: ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe i.sexhead catholic muslim age tigray oromo
margins, dydx (*) predict(outcome(2)) atmeans post
outreg2 using mymod.xls, append ctitle(Margins)


* Checking the puzzle: 

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe i.sexhead catholic muslim age tigray oromo if cons_trans_fe>0
outreg2 using mymod.xls, replace ctitle(Logit coeff)


ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe i.sexhead catholic muslim age tigray oromo if cons_trans_fe<0
outreg2 using mymod.xls, append ctitle(Logit coeff)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe
outreg2 using mymod.xls, append ctitle(Logit coeff)


*Appendix 
* Odds ratio
ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo, or
outreg2 using mymod.xls, append ctitle(Odds ratio with demo) eform


* Predicted probabilities 
margins, atmeans post
outreg2 using mymod.xls, replace ctitle(Logit coeff)


*************************
* 3. SOCIAL COMPARISONS
*************************

egen mean_consperm=mean(cons_perm_fe), by(village year)
egen mean_constrans=mean(cons_trans_fe), by(village year)
egen mean_consunexp=mean(cons_unexp_fe), by(village year)


drop refperm
drop refttrans
drop redunexp

gen refperm=cons_perm_fe-mean_consperm
gen reftrans=cons_trans_fe-mean_constrans
gen refunexp=cons_unexp_fe-mean_consunexp


ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo
outreg2 using mymodref.xls, replace ctitle(Logit coeff)

margins, dydx (*) predict(outcome(6)) atmeans post
outreg2 using mymod.xls, replace ctitle(Margins)


ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo, or
outreg2 using mymod.xls, replace ctitle(Odds ratio) eform


test _b[mean_consperm]==_b[refperm]
test _b[mean_constrans]==_b[reftrans]
test _b[mean_consunexp]==_b[refunexp]


margins, dydx(refunexp) atmeans post 
outreg2 using mymod.xls, replace ctitle(Margins)


margins, dydx(refunexp) atmeans predict(outcome(6))



** Test role village
ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo
outreg2 using mymodref.xls, replace ctitle(Logit coeff Stefan definition)


test _b[mean_consperm]==_b[cons_perm_fe]
test _b[mean_constrans]==_b[cons_trans_fe]
test _b[mean_consunexp]==_b[cons_unexp_fe]


********************************************************************************
** 4. ROBUSTNESS TESTS
********************************************************************************


* ALTERNATIVE MEASURES SUB POV 

corr subpov bpl

ologit bpl refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo
outreg2 using mymodref.xls, replace ctitle(Happiness)
margins, dydx(refunexp) atmeans 

ologit satisfied refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo
outreg2 using mymodref.xls, append ctitle(Life satisfaction)
margins, dydx(refunexp) atmeans 

* SUBSAMPLES

gen rcons2=rconspc*hhsize 

drop quant_cons
xtile quant_cons = rcons2, nq(2)

ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if quant_cons==1
outreg2 using subsamples.xls, replace ctitle(Below median)

ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if quant_cons==2
outreg2 using subsamples.xls, append ctitle(Above median)

** GOLOGIT 

gologit2 subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo, npl(age refperm reftrans refunexp )
outreg2 using mymodref.xls, replace ctitle(gologit)



drop quant_cons
xtile quant_cons = rconspc, nq(5)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo  if quant_cons==1
outreg2 using subsamples.xls, replace ctitle(Below median)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo  if quant_cons==5
outreg2 using subsamples.xls, append ctitle(Above median)

** Try povHHcomp

gen richer=.
replace richer=0 if povHHcomp!=.  
replace richer=1 if povHHcomp==1
replace richer=1 if povHHcomp==2
replace richer=1 if povHHcomp==3

gen  poorer=.
replace poorer=0 if povHHcomp!=. 
replace poorer=1 if povHHcomp==5
replace poorer=1 if povHHcomp==6
replace poorer=1 if povHHcomp==7


tab richer
tab poorer

gen richertrans=richer*cons_trans_fe
gen poorertrans=poorer*cons_trans_fe

ologit subpov cons_perm_fe richertrans poorertrans cons_unexp_fe sexhead catholic muslim age tigray oromo 
outreg2 using subsamples.xls, replace ctitle(Below median)


** Try power of village 

gen 
replace villagesmall=0
replace villagesmall=1 if village<=67

gen 
replace villagebig=0
replace villagebig=1 if village>67

ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if villagesmall==1
outreg2 using subsamples.xls, replace ctitle(Small village)

ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if villagebig==1
outreg2 using subsamples.xls, append ctitle(Big village)


xtile quant_village=village, nq(3)

ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if quant_village==1
outreg2 using subsamples.xls, replace ctitle(Below median)

ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if quant_village==3
outreg2 using subsamples.xls, append ctitle(Above median)


* Testing if poorer have bigger effects with lsu

xtile quant_lsu=lsu, nq(5)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo  if quant_lsu==1
outreg2 using subsamples.xls, replace ctitle(Below median)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo  if quant_lsu==5
outreg2 using subsamples.xls, append ctitle(Above median)


drop quant_land
xtile quant_land=land, nq(5)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo  if quant_land==1 & year==2004
outreg2 using subsamples.xls, replace ctitle(Below median)

ologit subpov cons_perm_fe cons_trans_fe cons_unexp_fe sexhead catholic muslim age tigray oromo  if quant_land==5 & year==2004
outreg2 using subsamples.xls, append ctitle(Above median)



forv i=1/5{
	ologit subpov refperm reftrans refunexp mean_consperm mean_constrans mean_consunexp sexhead catholic muslim age tigray oromo  if quant_cons==`i'
	estimates sto quantile`i'
}

estimates table quantile1 quantile2  /* 
*/ quantile3 quantile4 quantile5,  /* 
*/ style(oneline)


drop quant_cons2
xtile quant_cons2 = rconspc, nq(2)

forv i=1/5{
	reg subpov cons_perm_fe cons_trans_fe cons_unexp_fe mean_consperm mean_constrans mean_consunexp  if quant_cons2==`i', robust
	outreg2 subpov2 using robust2.xls , nolabel title(Model) ctitle (ologit) tstat  bracket bdec(5) tdec(1)   addstat(Adjusted R-squared, e(r2_a), F test, e(F), Observations, e(N)) rdec(2) alpha(0.01, 0.05, 0.10) symbol(***, **, *) append
	estimates sto quantile`i'
}




drop quant_cons_noref
xtile quant_cons_noref = rconspc, nq(5)

forv i=1/5{
	reg subpov cons_perm_fe cons_trans_fe cons_unexp_fe mean_consperm mean_constrans mean_consunexp  if quant_cons_noref==`i', robust
	estimates sto quantile`i'
}

estimates table quantile1 quantile2  /* 
*/ quantile3 quantile4 quantile5,  /* 
*/ style(oneline)



log close
