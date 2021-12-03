clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 5 Data Analysis\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture5_$sysdate.txt", text replace
cd "$path1"


*extra: S.D. v.s. S.E.

use CFPS2010, clear
drop if missing(yedu)

reg yedu // S.E. = .0447506
mean yedu // equivalent

summ yedu // S.D. = 4.243296
display r(sd)/sqrt(r(N)) // .04475064

********************************************************************************
*                 Section 2: Sampling Weight and Clustering                    *
********************************************************************************
**********************************
*What Sampling Weight Represents?*
**********************************
use SEER2004, clear // pop of each age group in U.S.

use MEPS2004, clear // PERWT04F - sampling weight

rename AGE31X age
collapse (sum) pop_meps = PERWT04F, by(age)

merge 1:1 age using SEER2004

keep if _merge == 3
drop if age >= 85 // value 85 means >=85
drop _merge

replace pop_meps = pop_meps/10^6 // change units to million
replace pop_seer = pop_seer/10^6

twoway line pop_meps age, lwidth(medthin) || line pop_seer age, lpattern(dash) lwidth(medthin) ///
||,  graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
	   ylabel(0(1)5, nogrid angle(0) format(%12.0f) labgap(*3)) xlabel(0(10)85,angle(0) labgap(*3)) ///
	   xtick(0(5)85) ytick(0(0.5)5) ///
       ytitle("Population (Millions)")  xtitle("Age")  ///
	   legend(order(1 2)label(1 "Population - MEPS") label(2 "Population Structure - SEER") col(2) size(*0.9) rowgap(*0.2) bmargin(medsmall)) 


*******************************
*How does Sampling Weight Work*
*******************************
set type double

use MEPS2004, clear

renvars AGE31X SEX TOTEXP04 PERWT04F / age sex med_exp pweight
generate female = sex - 1

/*descripitive analysis*/
*very clost to frequency weight
mean med_exp age sex [pw = pweight]
mean med_exp age sex [fw = floor(pweight)]

/*regression analysis*/
regress med_exp age sex, robust

regress med_exp age sex [pw = pweight]
regress med_exp age sex [pw = pweight], vce(robust) // robust is implicitly incorporated once pweight is used

/*how does sampling weight work?*/
foreach var in med_exp age sex {
	gen `var'_s = `var'*sqrt(pweight) // keep in mind pweight is usually a large number
}
gen cons_s = 1*sqrt(pweight)

*effectively, regression with sample weight = weighted least square with robust standard error
regress med_exp_s age_s sex_s cons_s, nocons robust

*******************************************
*Why Sampling Weight is not Always Better?*
*******************************************
use CFPS2010, clear

table prov, content(mean rswt_nat)

gen agesq = age*age/2
gen log_inc = log(income)

reghdfe log_inc yedu, vce(robust) absorb(age)
reghdfe log_inc yedu [pw = rswt_nat], absorb(age)

********************************************************
*Cluster -- Higher Cluster Level, Larger Standard Error*
********************************************************
reghdfe log_inc yedu [pw = rswt_nat], absorb(age)
reghdfe log_inc yedu [pw = rswt_nat], cluster(hhid) absorb(age)
reghdfe log_inc yedu [pw = rswt_nat], cluster(countyid) absorb(age)

*rule of thumb, # clusters >= 50
reghdfe log_inc yedu [pw = rswt_nat], cluster(prov) absorb(age)

*how to know number of clusters?
unique countyid


*end of the do file
log close
clear
