clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 4 Data Cleaning\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture4_$sysdate.txt", text replace
cd "$path1"


********************************************************************************
*                     Section 1: Data Input                                    *
********************************************************************************
***************
*Mannual Input*
***************
clear
input year cpi
2010	103.3
2009	99.3
2008	105.9
2007	104.8
2006	101.5
2005	101.8
2004	103.9
2003	101.2
2002	99.2
2001	100.7
2000	100.4
1999	98.6
1998	99.2
1997	102.8
1996	108.3
1995	117.1
1994	124.1
1993	114.7
1992	106.4
1991	103.4
1990	103.1
1989	118
1988	118.8
1987	107.3
1986	106.5
1985	109.3
1984	102.7
1983	102
1982	102
1981	102.5
1980	107.5
end

********************************
*Example of Insheet and Pitfall*
********************************
insheet using "lhfa_boys_b.txt",clear
tempfile b
save `b',replace

insheet using "lhfa_boys_a.txt",clear
append using `b', force

****************************
*Safer Alternative - Infile*
****************************
infile int (month l) float (m) str8 (s) float (sd sd3neg sd2neg sd1neg sd0 sd1 sd2 sd3) using "lhfa_boys_b.txt",clear
drop if _n == 1

**********************
*Import Excel & Xpose*
**********************
import excel using Section2All_xls.xls, clear sheet("20100 Ann") cellrange(A8:AT54)

keep if inlist(A,"Line","27","29")
xpose,clear
rename v1 year
rename v2 disp_inc
rename v3 expenditure
drop if year == .

*************************
*Exercise 1 --- Solution*
*************************
*第一步：用Stata给Excel的第一行全部加上y
import excel using disposable_income.xls, clear cellrange(A6:BP66)

foreach var of varlist _all {
	replace `var' = "y" + `var' if _n == 1
}

*第二步：利用outsheet/insheet中option [no]names的转换，将数据第一列变成变量名
outsheet using disposable_income.txt, replace nonames

insheet using disposable_income.txt, clear names 
renvars ygeofips ygeoname, predrop(1)

*第三部：reshape转换成常用的Stata形式
destring y*, force replace
reshape long y, i(geofips) j(year)
rename y disp_inc

**************************
*Preparing the Dictionary*
**************************
*大多数情况下数据都会准备好dictionary，不需要自己写
file open dictfile using "$path1\1980_2010hh.dct", write replace

file write dictfile "dictionary {" _n
file write dictfile "_lrecl(1478)" _n
file write dictfile "_column(1) NEWID %7f" _n
file write dictfile "_column(8) BLSURBN %1f" _n
file write dictfile "_column(9) REGION %1f" _n
file write dictfile "_column(10) CUTENUR %1f" _n
file write dictfile "_column(13) REPSTAT %1f" _n
file write dictfile "_column(19) TOTWT %11.3f" _n
file write dictfile "_column(30) ADJWT %11.3f" _n
file write dictfile "_column(41) FULLYEAR %1f" _n
file write dictfile "_column(54) FAMSIZE %4.1f" _n
forvalues i = 1/109 {
local pos = 60 + (`i'-1)*10
file write dictfile "_column(`pos') VAR`i' %10.2f" _n
}
file write dictfile "}" _n
file close dictfile

************************************
*Example of Using a Dictionary File*
************************************
infile using "1980_2010hh.dct", using("ffile001.") clear

clear
forvalues y = 2000/2001 {
forvalues q = 1/4 {
local yy = substr("`y'",3,2)
infile using "1980_2010hh.dct", using("ffile`yy'`q'.") clear
generate QUARTER = `q'
generate YEAR = `y'
save "`y'_`q'.dta",replace
}

use "`y'_1.dta",replace
forvalues q = 2/4 {
append using "`y'_`q'.dta"
erase "`y'_`q'.dta"
}
erase "`y'_1.dta"

tempfile year`y'
save `year`y'',replace
}

use `year2000',clear
append using `year2001'

***********************************
*Storage Type versus Output Format*
***********************************
use census_2005, clear
recast int income 
save census_2005_new, replace // check the size of the new file

use census_2005, clear
list income in 1/5

format income %6.2f
list income in 1/5

format income %4.0f
list income in 1/5
save census_2005_new2, replace // check the size of the new file


********************************************************************************
*                     Section 2: Work on One Data Set                          *
********************************************************************************
*******************************
*Regression and Missing Values*
*******************************
use CFPS2010, clear
drop if missing(loginc)

replace meduca = . in -100/-1 // manually generate some missing values

egen meduca_mean = mean(meduca)
gen meduca2 = meduca
replace meduca2 = meduca_mean if meduca>=.

/*for simple ols, replacing with group mean does not affect the coefficient, but 
changes the s.e.*/
reg loginc meduca 
reg loginc meduca2 

*for multiple regression, it changes both coefficient and s.e.
reg loginc yedu meduca 
reg loginc yedu meduca2 


******************************************
*Several Useful Commands in Data Cleaning*
******************************************
use 2010adult_072016, clear

*search for a variable
lookfor 教育
lookfor 学历 // that's why the command could be tricky, one concept can be in different words
lookfor 保险

*understand the variable
codebook gender

codebook income
summ income, detail

codebook qj3_s_1
tab qj3_s_1
tab qj3_s_1, nolabel

*difference between -clonevar- and -gen-
gen edu1 = qc1
tab edu1

clonevar edu2 = qc1
tab edu2

*example of renvars
keep tb4_a_c*

renvars, upper 
renvars, lower 

renvars, predrop(6)
renvars, prefix(tb4_a_)

renvars, subst(_a_ _b_)



log close
clear
