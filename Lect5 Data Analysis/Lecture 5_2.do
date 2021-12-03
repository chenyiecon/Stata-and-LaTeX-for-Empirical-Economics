clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 5 Data Analysis\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture5_$sysdate.txt", text replace
cd "$path1"

****************************
* Example of IV Estimation *
****************************
use cfps10_rtedu, clear

global clist "male minority age age2 i.age_group feduca meduca fbirtha mbirtha fparty mparty"

*pay attention to the: weak identification test, overidentification test
ivreg2 logwage $clist (yedu=edu_p_s edu_p_j) if (edu==5|edu==6), liml robust cluster(countyid)

*why overidentification test does not tell us that much...
ivreg2 logwage $clist (yedu=edu_policy) if (edu==5|edu==6), liml robust cluster(countyid)

gen edu_policy2 = edu_policy + 0.01*runiform()
ivreg2 logwage $clist (yedu=edu_policy edu_policy2) if (edu==5|edu==6), liml robust cluster(countyid)




********************************************************************************
*                    Section 3: Data Analysis                                  *
********************************************************************************
**************************
* Mastering Interactions *
**************************
use MEPS_merged, clear

renvars AGE TOTEXP PERWT RTHLTH31 / age med_exp pweight health
gen college = [inrange(EDU,4,6)]
keep if inrange(age,40,84)

gen age_health = age*health

regress med_exp age health age_health [pw = pweight], vce(robust)
regress med_exp age health c.age#c.health [pw = pweight], vce(robust)
regress med_exp c.age##c.health [pw = pweight], vce(robust)

regress med_exp age i.health c.age#i.health [pw = pweight], vce(robust)
regress med_exp c.age##i.health [pw = pweight], vce(robust)

regress med_exp i.age i.health i.age#i.health [pw = pweight], vce(robust)
regress med_exp i.age##i.health [pw = pweight], vce(robust)

regress med_exp c.age##i.(health college) [pw = pweight], vce(robust)
regress med_exp c.age##i.health##i.college [pw = pweight], vce(robust)

display _b[1.college#age#3.health]

*******************************************************
* No Variation in a Subcategory in a Non-linear Model *
*******************************************************
use CFPS2010_havechild, clear

gen mother_retire = [mother_age>=50&mother_age<.]

/*pay attention to the number of observations in OLS and probit*/
reghdfe havechild mother_retire, vce(robust) absorb(provcd age)

probit havechild mother_retire i.age i.provcd, vce(robust)

/*why observations in provcd=12 are dropped?*/
list havechild if provcd == 12

**********************************
* How to obtain the Fixed Effect?*
**********************************
use MEPS_merged, clear

renvars AGE TOTEXP PERWT RTHLTH31 PANEL / age med_exp pweight health panel
destring panel, force replace
gen college = [inrange(EDU,4,6)]
keep if inrange(age,40,84)

*the FE we wish to get
regress med_exp college i.health i.panel i.age [pw = pweight] // -1260 for age 40; 3348.202 for age 84
display _b[_cons]
display _b[_cons] + _b[84.age]

*if directly save FE after -reghdfe-, mean of FEs will be adjusted to zero
reghdfe med_exp college i.health i.panel [pw = pweight], absorb(age_FE=age)
scalar s_cons = _b[_cons]

mean age_FE  [pw = pweight]
replace age_FE = age_FE + s_cons // add back the constant term

summ age_FE if age == 40
summ age_FE if age == 84

*************************************************
*Application of -predict-: Two Sample Prediction*
*************************************************
use MEPS_merged, clear

renvars AGE TOTEXP PERWT RTHLTH31 / age med_exp pweight health
keep if inrange(age,40,84)
drop if health == .

gen agesq = age*age/100

estimates describe using mortality_NHIS.ster
estimates use mortality_NHIS.ster 

predict mortality

************************
*Decomposition in Stata*
************************
use cfps10_rtedu, clear

keep logwage age age2 male
drop if logwage == .

bysort male: summ logwage age

reg logwage age age2 if male == 0
predict wage_pseudo if male == 1

*B-O decomposition
summ logwage if male == 0
scalar r1 = r(mean)

summ logwage if male == 1
scalar r2 = r(mean)

summ wage_pseudo if male == 1
scalar r3 = r(mean)

display "share explained = " (r3 - r1)/(r2 - r1)

*compare to Stata command
oaxaca logwage age age2, by(male) weight(1)
display .27442395


****************************************
* Example of Counterfactual Simulation *
****************************************
use tfr_prov.dta, clear
xtset prov year

gen after = year-leadingteam
replace after = 0 if after<0

reghdfe totaltfr after gdp_pc nonagri_share gdp1st_share gdp2nd_share if inrange(year,1969,1978), absorb(prov year) vce(robust)

*Move Back 5 Years*
gen after_placebo = F5.after
reghdfe totaltfr after_placebo gdp_pc nonagri_share gdp1st_share gdp2nd_share if inrange(year,1964,1973), absorb(prov year) vce(robust)

*Move Forward 5 Years*
replace after_placebo = L5.after
reghdfe totaltfr after_placebo gdp_pc nonagri_share gdp1st_share gdp2nd_share if inrange(year,1974,1982), absorb(prov year) vce(robust)

*placebo simulation, group randomly formed from 1970 to 1975*
use tfr_prov, clear 
keep prov
duplicates drop
save prov_base, replace

set seed 1

*第一步：产生一组随机的改革年份，记录下每次“虚拟”改革对应的系数
forvalues i = 1/100 {
	use prov_base,clear
	gen leadingteam = 1970+int((1975-1970+1)*runiform())
	save temp_leadingteam,replace

	use tfr_prov, clear 
	merge m:1 prov using temp_leadingteam.dta,nogenerate update replace

	gen after = year-leadingteam
	replace after = 0 if after<0

	quietly reghdfe totaltfr after gdp_pc nonagri_share gdp1st_share gdp2nd_share, absorb(prov year) vce(robust)
	local c`i' = _b[after]
}

*第二步：将“虚拟”改革对应的系数描绘出来
clear
set obs 100
gen sim1 = .

forvalues i = 1/100 {
	replace sim1 = `c`i'' if _n == `i'
}

kdensity sim1 ///
, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
       title("Linear Specification",size(medium) margin(medium)) ///
       ylabel(0(1)5 , angle(0) format(%12.0f)) ytitle("Density", margin(medium) size(*1.1)) ///
	   xlabel(-0.3(0.1)0.3,format(%12.1f)) xtick(-0.3(0.1)0.3) xtitle("Coefficient")  ///
       legend(off) note("") xline(-0.245,lpattern(dash) lwidth(thin) lcolor(black))

erase temp_leadingteam.dta
erase prov_base.dta

************************
* Example of Bootstrap *
************************
use CFPS2010, clear
gen log_inc = log(income)

areg log_inc yedu, vce(cluster countyid) absorb(age)

*method 1, vce(bootstrap)
areg log_inc yedu, vce(bootstrap, reps(100) cluster(countyid) seed(1)) absorb(age)

*method 2, bootstrap prefix
bootstrap coef = _b[yedu], reps(100) cluster(countyid) seed(1): areg log_inc yedu, absorb(age)

*method 3, self-defined program
correlate income yedu

	*step 1: define the return scalar from the program
	program drop _all
	program define var_corr, rclass

	correlate income yedu
	matrix X = r(C)
	scalar corr = X[1,2]

	return scalar corr = corr
	end

	*step 2: bootstrap in a standard fashion
	bootstrap corr = r(corr), rep(100) seed(1): var_corr
	


*****************************************************************************************
* Exercise: A Simple and Partial Replication of "How Much Should We Trust DinD Estimates*
*****************************************************************************************
set seed 1
local count0 = 0
local count1 = 0
local count2 = 0
local count3 = 0

forvalues i = 1/100 {
use sim_wage_0.8.dta, clear

preserve
	clear
	set obs 50
	gen state = _n

	gen rank = runiform()
	sort rank
	gen treat = [_n<=25]
	
	drop rank
	tempfile treat
	save `treat', replace
restore

quietly merge m:1 state using `treat', nogenerate

local reform = runiformint(1985,1995)
quietly gen after = [year >= `reform']

quietly reghdfe log_wage edu c.treat#c.after, absorb(state year) vce(robust)
if (abs(_b[c.treat#c.after]/_se[c.treat#c.after])>=1.96) local count0 = `count0' + 1

quietly reghdfe log_wage edu c.treat#c.after, absorb(state year) vce(cluster state)
if (abs(_b[c.treat#c.after]/_se[c.treat#c.after])>=1.96) local count1 = `count1' + 1

quietly bootstrap coef = _b[c.treat#c.after], reps(100): reghdfe log_wage edu c.treat#c.after, absorb(state year)
if (abs(_b[coef]/_se[coef])>=1.96) local count2 = `count2' + 1

quietly bootstrap coef = _b[c.treat#c.after], reps(100) cluster(state): reghdfe log_wage edu c.treat#c.after, absorb(state year)
if (abs(_b[coef]/_se[coef])>=1.96) local count3 = `count3' + 1
}
display "`count0',`count1',`count2',`count3'"

*end of the do file
log close
clear
