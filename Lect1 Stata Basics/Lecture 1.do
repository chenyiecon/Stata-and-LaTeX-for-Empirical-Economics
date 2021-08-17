clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\IESR Teaching\计量软件应用\Lecture 1 Stata Basics\Stata Exercise" 
log using "$path1\lecture1_1.txt", text replace
cd "$path1"

********************************************************************************
*                      1.2 Introduction to Stata                               *
********************************************************************************

use "TFR CHINA.dta", clear
********************************
* Three Ways of Using Comments *
********************************
* 1st way of comment

/* 2nd way 
of comment */

gen x = 5   // 3rd way of comment

***************************************
* Three Ways of Writing Long Commands *
***************************************

*default output
twoway line tfr year, yaxis(1) || connected sexratio year, yaxis(2)

* 1st way
twoway line tfr year,lcolor(black) lpattern(dash) yaxis(1) || connected sexratio year, msymbol(plus)yaxis(2) ///
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
       ylabel(0(2)8 , angle(0) format(%12.0f) axis(1)) ytitle("Total Fertility Rate", margin(medium) axis(1) ) ///
       ylabel(100(3)115 , angle(0) format(%12.0f) axis(2)) ytitle("Sex Ratio at Birth", margin(medium) axis(2) ) ///
	   xlabel(1950(10)2000) xtick(1949(1)2002) xtitle("Year", margin(vlarge)) ///
	   title(" ",size(medium) margin(medium)) ///
	   legend(label(1 "Total Fertility Rate") label(2 "Sex Ratio at Birth") col(2) size(medsmall)) ///
	   xline(1970 1978,lpattern(dash) lwidth(thin) lcolor(black))

* 2nd way
twoway line tfr year,lcolor(black) lpattern(dash) yaxis(1) || connected sexratio year, msymbol(plus)yaxis(2) /*
*/ ||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) /*
*/       ylabel(0(2)8 , angle(0) format(%12.0f) axis(1)) ytitle("Total Fertility Rate", margin(medium) axis(1) ) /*
*/       ylabel(100(3)115 , angle(0) format(%12.0f) axis(2)) ytitle("Sex Ratio at Birth", margin(medium) axis(2) ) /*
*/	   xlabel(1950(10)2000) xtick(1949(1)2002) xtitle("Year") /*
*/	   title(" ",size(medium) margin(medium)) /*
*/	   legend(label(1 "Total Fertility Rate") label(2 "Sex Ratio at Birth") col(2) size(medsmall)) /*
*/	   xline(1970 1978,lpattern(dash) lwidth(thin) lcolor(black))

* 3rd way
#delimit ;
twoway line tfr year,lcolor(black) lpattern(dash) yaxis(1) || connected sexratio year, msymbol(plus)yaxis(2) 
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) 
       ylabel(0(2)8 , angle(0) format(%12.0f) axis(1)) ytitle("Total Fertility Rate", margin(medium) axis(1) ) 
       ylabel(100(3)115 , angle(0) format(%12.0f) axis(2)) ytitle("Sex Ratio at Birth", margin(medium) axis(2) )
	   xlabel(1950(10)2000) xtick(1949(1)2002) xtitle("Year") 
	   title(" ",size(medium) margin(medium)) 
	   legend(label(1 "Total Fertility Rate") label(2 "Sex Ratio at Birth") col(2) size(medsmall)) 
	   xline(1970 1978,lpattern(dash) lwidth(thin) lcolor(black));
#delimit cr


********************************************************************************
*                      1.3 Describing Your Data                                *
********************************************************************************
use cps4_small.dta,clear // load data
describe
summarize

*tabulate command
tabulate female
tabulate female married
tabulate female married, column
tabulate female married, row

tabulate wage // don't use the command for continuous variable

*summarize command
summarize wage
summarize wage, detail
summarize wage educ exper hrswk

*if condition
summarize wage if female==1
tabulate married if female==0

*distinguish = and ==
generate married_women = [married == 1] if female == 1

*combining condition
count if female==1&married==1
count if (female==1|married==1)&wage<=20
count if female==1|(married==1&wage<=20)

*******************************************
* Exercise 1.1 - Old Age Support in China *
*******************************************
use support.dta, clear

tabulate whom_to_rely

tabulate whom_to_rely urban
tabulate whom_to_rely urban, column

tabulate whom_to_rely survey_year if urban==0, column
tabulate whom_to_rely survey_year if urban==1, column

*******************
* Plotting Graphs *
*******************
use cps4_small.dta,clear

*histogram
histogram wage
graph export hist_wage.wmf,replace

*twoway graphs
twoway scatter wage educ
twoway lfit wage educ

twoway (scatter wage educ) (lfit wage educ)
twoway scatter wage educ || lfit wage educ
graph export lfit.wmf,replace



********************************************************************************
*                      1.4 Data Manipulation                                   *
********************************************************************************
*****************
*gen versus egen*
*****************
sysuse sp500, clear
gen  sum_close0 = sum(close)  // running sum
egen sum_close1 = sum(close)  // sum of total

list close sum_close0 sum_close1 in 1/10
list sum_close0 sum_close1 in -1

*treat missing values in a different way
clear
input v1  v2
      1   5
      2   .
      .   3
      2   4
      4   .
      .   6
end
gen mean       = (v1+v2)/2

egen mean_egen1 = mean(v1)
egen mean_egen2 = rowmean(v1 v2)
list

***************************************
*Example - Computer deflator from CPI?*
***************************************
use cpi.dta, clear
sort year

*method 1
replace cpi = cpi/100
gen log_cpi = log(cpi)

gen log_deflator = sum(log_cpi) // think, why here we should use -gen- instead of -egen-?
gen deflator = exp(log_deflator)

summarize deflator if year == 2000
replace deflator = deflator/r(mean)

*method 2 (simulate what we do in Excel, not recommended)
drop deflator

gen deflator = 100 if _n == 1
local N = _N
forvalues i = 2/`N' {
	replace deflator = deflator[`i'-1]*cpi if _n == `i'
}
summarize deflator if year == 2000
replace deflator = deflator/r(mean)

***************************
*bysort: generate by group*
***************************
use chip2002, clear

generate senior = [age>=60] if age!=. 
bysort hhid: egen n_senior = sum(senior)

drop senior n_senior

****************************************
*Exercise 1.2 - keep nuclear households*
****************************************
*keep nuclear family
tab relation

gen couple = [relation==1|relation==2]
bysort hhid:egen N_couple = total(couple)

drop if N_couple>2 // this part is optional

gen to_drop = [relation>3|(relation==3&age>=18)]
bysort hhid: egen drop = total(to_drop)
gen not_nuclear = [drop > 0] // as long as one hh member does not satisfy the condition, the hh should be excluded

*****************************
*Using -reshape- and -merge-*
*****************************
/*The standard Stata data structure is observation(id X year)-by-variable
Excel data often have this structure: id-by-year in one sheet for one variable
*/
use population_state_by_year.dta, clear

reshape long y,i(state) j(year)
rename y pop

tempfile temp
save `temp', replace


use disposable_income_state_by_year.dta, clear

reshape long y,i(state) j(year)
rename y inc

merge 1:1 state year using `temp'

********************************************************************************
*                      1.5 Programming Basics                                  *
********************************************************************************
***********************************
*Generate Wage Deviation from Mean*
***********************************
use cps4_small.dta,clear

*method 1
summarize wage // find that the mean is 20.61566
generate wage_d1 = wage - 20.61566

*method 2 - automation and use return values
summarize wage
return list
display r(mean)

generate wage_d2 = wage - r(mean)

*use scalars to record numbers
summarize educ
scalar m_educ = r(mean)

summarize exper
display r(mean)
display m_educ

********************************************
*Exercise 1.3 - compute standard deviations*
********************************************
use cps4_small.dta,clear

summarize wage
scalar define mean_wage = r(mean)
scalar define N_wage = r(N)

generate wage_d_sq = (wage - mean_wage)^2

summarize wage_d_sq
scalar sum_square = r(mean)*r(N) // There are many ways to compute the summation. How many can you think of?

display sqrt(sum_square/(N_wage-1))

summarize wage
display r(sd) // compare to r(sd)

**************************
*Examples of Local Global*
**************************
use cps4_small.dta,clear

local varlist1 wage female educ exper
global varlist2 female educ exper

summarize `varlist1' // the way of calling local
regress wage $varlist2 // the way of calling global

**********************************
*Using forvalues/foreach - Basics*
**********************************
* forvalues: number only
forvalues i = 0/1 {
	regress wage $varlist2 if married == `i'
}

* foreach: number or string
foreach i in 0 1 {
	regress wage $varlist2 if married == `i'
}

foreach var in $varlist2 {
	summarize `var', detail
}

*********************************
*Example - Using levelsof and if*
*********************************
use census1990_1cent.dta, clear
levelsof province

*Example: aging process from 1990 to 2000
foreach y in 1990 2000 {
	use census`y'_1cent.dta, clear

	if (`y'==2000) {
		replace province = "Sichuan" if province == "Chongqing"	
	}

	generate elderly = [age>=60] if age!=.

	if (`y'==1990) quietly levelsof province, local(levels)

	foreach p in `levels' {
		quietly summarize elderly if province == "`p'"	
		local r`p'`y' = r(mean)
	}
}

foreach p in `levels' {
	display "For province `p', the share of elderly is `r`p'1990' in 1990 and `r`p'2000' in 2000."
}

*Note that above codes are designed purposely for using -levelsof- and -if-
*To achieve the same output, you can do it in a "much" simpler way

use census1990_1cent.dta, clear
append using census2000_1cent.dta

replace province = "Sichuan" if province == "Chongqing"

generate elderly = [age>=60] if age!=.
collapse (mean) elderly, by(province year)
reshape wide elderly, i(province) j(year)

*We will see more examples of -collapse- in the future

********************************************************************************
*                     1.6 Running Regressions                                  *
********************************************************************************
use cfps2010, clear

regress loginc yedu male age age2 
ereturn list

display _b[yedu]
display _se[yedu]

**********************************************
* Pay attention to the number of observation *
**********************************************
regress loginc yedu male age age2 
regress loginc yedu male age age2 meduca feduca

regress loginc yedu male age age2 if missing(feduca,meduca) == 0

*****************
*Hypothesis Test*
*****************
regress loginc yedu male age age2 meduca feduca

*Hypothesis 1: return to equation equals 10%
test yedu = 0.1 // note that varname here actually stands for _b[varname]
test _b[yedu] = 0.1

*Hypothesis 2: parental education does not matter
test meduca feduca
test (meduca=0)(feduca=0) // testing joint hypothesis

*Hypothesis 3: 1+2
test (yedu=0.1)(meduca=0)(feduca=0)

*Q: how to test the hypothesis "separately" instead of "jointly"?

*********************************
*Marginal Effect of Probit Model*
*********************************
use nhanes2f, clear
keep if missing(diabetes,black,female,age)==0

probit diabetes i.black i.female age, nolog // nolog simply suppress the MLE process

*marginal effects at the means
margins, dydx(black female age) atmeans 

foreach var in black female age{
	quietly summarize `var'
	scalar m_`var' = r(mean)
}

*where does the MEM of age come from?
display normalden(_b[_cons]+_b[1.black]*m_black+_b[1.female]*m_female+_b[age]*m_age)*_b[age]

*average marginal effects
margins, dydx(black female age)

*where does the AME of female come from?
generate ME = normalden(_b[_cons]+_b[1.black]*black+_b[1.female]*female+_b[age]*age)*_b[age]
summarize ME

**************************************************
*Optional: in non-linear models, factor variables*
*and continuous variables are treated differently*
**************************************************
quietly probit diabetes i.black i.female age, nolog
margins, dydx(black female age)

quietly probit diabetes black female age, nolog
margins, dydx(black female age)

*Reading Material 1.3 gives the example how AME and MEM of factor variables are computed

*for linear models, they are the same
quietly regress diabetes i.black i.female age
margins, dydx(black female age)

quietly regress diabetes black female age
margins, dydx(black female age)




*end of the do file
log close
clear
