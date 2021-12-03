clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 4 Data Cleaning\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture4_$sysdate.txt", text replace
cd "$path1"

**************************************************
*Exercise 2: Compute Total Transfer from Children*
**************************************************
use family_transfer.dta, clear

describe
tab1 ce009_1_1_a ce009_1_1_b ce009_1_1_every, m

keep householdID ce009*

*# variables: 10*(4+4+3+3) --> 4+4+3+3
reshape long ce009_@_1 ce009_@_1_a ce009_@_1_b ce009_@_1_every ce009_@_2 ce009_@_2_a ce009_@_2_b ce009_@_2_every ce009_@_3 ce009_@_3_a ce009_@_3_b ce009_@_4 ce009_@_4_a ce009_@_4_b, i(householdID) j(child)

*# variables: 4+4+3+3 --> 4
reshape long ce009__@ ce009__@_a ce009__@_b ce009__@_every, i(householdID child) j(type)

* hh x child x type of transfer as one observation
renvars ce009*, presub(ce009__ transfer)

* compute transfer in each hh x child x type cell
replace transfer_every = . if !inrange(transfer_every,1,4)
replace transfer = . if (transfer < 0)|(transfer_every >= .)

recode transfer_every (1 = 12)(2 = 4)(3 = 2)(4 = 1), gen(freq)
replace transfer = transfer*freq if inlist(type,1,2)

*use range inference if missing
list if (transfer >=.) & (transfer_a < . | transfer_b < .)
replace transfer =(transfer_a + transfer_b)/2 if  (transfer >=.) & (transfer_a < . & transfer_b < 10000)

*aggregate the transfer
collapse (sum) transfer, by(householdID)

summarize transfer if transfer>0, detail


*****************************************
*Exercise 3: Assign Province to Counties*
*****************************************
import excel using "county_prov_match.xls", clear sheet("1997") cellrange(A6)

renvars A - F / county fis_pop fis_nat fis_col fis_ret fis_other
gen line = _n

generate prov=0
local j=1
foreach province in "北京" "天津" "河北" "山西" "内蒙古" "辽宁" "吉林"/*
*/ "黑龙江" "上海" "江苏" "浙江" "安徽" "福建" "江西" "山东" "河南" "湖北"/*
*/ "湖南" "广东" "广西" "海南" "四川" "贵州" "云南" "西藏" "陕西" "甘肃" "青海" "宁夏" "新疆" {
	local p`j' "`province'"

	* find out the first line where the province name show up
	replace prov = 1 if  strpos(county,"`province'") & strpos(county,"计")
	quietly summ line if strpos(county,"`province'") & strpos(county,"计")
	replace prov = 0 if line>r(min)
	local j = `j' + 1
}

gen province = sum(prov)
tostring province,force replace
forvalues i=1/`j' {
replace province="`p`i''" if province=="`i'"
}


********************************************************************************
*                     Section 3: Append and Merge                              *
********************************************************************************
*********************
*Example of -Append-*
*********************
sysuse citytemp, clear
keep if region == 4
save west, replace

sysuse citytemp, clear
keep if region == 3
save south, replace

sysuse citytemp, clear
keep if region == 1

append using west south, generate(filenum) nolabel

erase west.dta
erase south.dta

********************
*Example of -xtset-*
********************
clear
input year prov person income
2000 11 1 1000
2000 11 2 1200
2000 31 1 1100
2000 31 2 1300
end
save data2000, replace

clear
input year prov person income
2002 11 1 1500
2002 31 1 900
2002 31 2 1600
end
save data2002, replace

*why we should use -group- after -append-
use data2000, clear
egen id1 = group(prov person)
gen id2 = prov*10 + person
tempfile d2000
save `d2000', replace

use data2002, clear
egen id1 = group(prov person)
gen id2 = prov*10 + person
append using `d2000'
list

*the correct way of generating id
*often you need to set the storage type to double
egen id3 = group(prov person)
list

*Lag operate actually means previous "year", not previous wave!
xtset id2 year
gen lag_inc = L.income
list

egen year_id = group(year)
list

xtset id2 year_id
replace lag_inc = L.income
list

erase data2000.dta
erase data2002.dta 

************************************************
*Use Panel Information to Update Missing Values*
************************************************
use mother_age, clear

reshape wide mother_edu mother_age, i(pid) j(survey_year)
order pid *edu* *age* // open the data to have a general idea how to update missing values

foreach var in edu {
*用2014的nonmissing替换2010/2012的missing
if (!missing(mother_`var'2014)) {
	replace mother_`var'2010 = mother_`var'2014 if missing(mother_`var'2010)
	replace mother_`var'2012 = mother_`var'2014 if missing(mother_`var'2012)
}

*用2012的nonmissing替换2010/2014的missing
if (!missing(mother_`var'2012)) {
	replace mother_`var'2010 = mother_`var'2012 if missing(mother_`var'2010)
	replace mother_`var'2014 = mother_`var'2012 if missing(mother_`var'2014)
}

*用2010的nonmissing替换2012/2014的missing
if (!missing(mother_`var'2010)) {
	replace mother_`var'2012 = mother_`var'2010 if missing(mother_`var'2012)
	replace mother_`var'2014 = mother_`var'2010 if missing(mother_`var'2014)
}
}

*类似的过程更新年龄的missing
foreach var in age {
if (!missing(mother_`var'2014)) {
	replace mother_`var'2010 = mother_`var'2014 - 4 if missing(mother_`var'2010)
	replace mother_`var'2012 = mother_`var'2014 - 2 if missing(mother_`var'2012)
}

if (!missing(mother_`var'2012)) {
	replace mother_`var'2010 = mother_`var'2012 - 2 if missing(mother_`var'2010)
	replace mother_`var'2014 = mother_`var'2012 + 2 if missing(mother_`var'2014)
}

if (!missing(mother_`var'2010)) {
	replace mother_`var'2012 = mother_`var'2010 + 2 if missing(mother_`var'2012)
	replace mother_`var'2014 = mother_`var'2010 + 4 if missing(mother_`var'2014)
}
}

reshape long mother_edu mother_age, i(pid) j(survey_year)


*************************************************
*Example of -Merge-: Obtaining Initial Condition*
*************************************************
*method 1 - standard -merge-
use tfr_prov, clear

*准备基期的信息
preserve
keep if year == 1978
rename totaltfr totaltfr_1978
tempfile base
save `base', replace
restore

*把基期的信息merge到原数据
merge m:1 prov using `base', nogenerate

*method 2 - use -egen-
use tfr_prov, clear
tempvar temp
gen `temp' = totaltfr if year == 1978 // 该变量除了1978年以外，均为missing
bysort prov: egen totaltfr_1978 = sum(`temp')


*pay attention to the name!
use tfr_prov, clear

preserve
drop totaltfr
save base, replace
restore

keep if year == 1978 // separate the information into two data
rename totaltfr totaltfr_1978
preserve
keep if prov<30 
save d1, replace
restore

keep if prov>=30
save d2, replace

*what's wrong?
use base, clear
merge m:1 prov using d1, nogenerate
merge m:1 prov using d2, nogenerate

*correct method 1
use base, clear
merge m:1 prov using d1, nogenerate
merge m:1 prov using d2, nogenerate update

*correct method 2 (more recommended)
*why? overriding information is generally dangerous
forvalues i = 1/2 {
	use d`i', replace
	rename totaltfr_1978 totaltfr_1978_`i'
	tempfile d`i'
	save `d`i'', replace
}

use base, clear
merge m:1 prov using `d1', nogenerate
merge m:1 prov using `d2', nogenerate
egen totaltfr_1978 = rowtotal(totaltfr_1978_*)

erase base.dta
erase d1.dta
erase d2.dta

**************************************************
*Why Missing Values can be Problematic in -merge-*
**************************************************
*missing value not uniquely identified
use tfr_prov, clear

preserve
keep if year == 1978
rename totaltfr totaltfr_1978
replace prov = . if inlist(prov,64,65)
tempfile base
save `base', replace
restore

replace prov = . if inlist(prov,64,65)
merge m:1 prov using `base', nogenerate

*missing value uniquely identified
use tfr_prov, clear

preserve
keep if year == 1978
rename totaltfr totaltfr_1978
replace prov = . if inlist(prov,65)
tempfile base
save `base', replace
restore

replace prov = . if inlist(prov,65)
merge m:1 prov using `base', nogenerate

*********************
*Stability of -sort-*
*********************
use tfr_prov, clear
keep year province

expand 5
set seed 1
gen line = 1+floor(runiform()*5)

*sort year province
sort year province, stable
list in 1/20


*************************
*Duplicated Observations*
*************************
use m12rst, clear
gen obs_line = _n

duplicates report // duplicates in all variables
duplicates report hhid line wave // duplicates in the listed variables

duplicates list hhid line wave
duplicates tag hhid line wave, generate(dup_tag)
keep if dup_tag == 1
sort hhid line wave, stable // manually check duplicated observation, keep observation number



*********************
*Example of -joinby-*
*********************
clear
input hid cid x1 x2
1025 3 11 320
1025 1 12 300
1025 4 10 275
1026 2 13 280
1027 5 15 210
end
save child, replace

clear
input hid pid x1 x3
1030 10 39 600
1025 11 20 643
1025 12 27 721
1026 13 30 760
1026 14 26 668
1030 15 32 684
end
save parent, replace

joinby hid using child
list

erase child.dta
erase parent.dta

*The x1 variable is in both datasets.
*In default, variable in master file will be kept

********************************************************************************
*                          Section 4: Wrap Up                                  *
********************************************************************************
***************************
*Examples of Drop Outliers*
***************************
*method 1
use CFPS2010, clear

count if labor_inc <. & income <. // 3,264

winsor2 labor_inc, trim cuts(0 99) replace
winsor2 income, trim cuts(0 99) replace

count if labor_inc <. & income <. // 3,229

*method 2
use CFPS2010, clear

gen drop = 0
foreach var in labor_inc income {
	summ `var' if `var'<., d
	local high = r(p99)
	replace drop = 1 if `var'>`high'
}
drop if drop == 1

count if labor_inc <. & income <. // 3,229

*method 3 (problematic)
use CFPS2010, clear

gen drop = 0
foreach var in labor_inc income {
	summ `var' if `var'<., d
	local high = r(p99)
	drop if `var'>`high'
}

count if labor_inc <. & income <. // 3,205

*****************************************
*Transition between Numerical and String*
*****************************************
clear
input double income0 str8 income1 str8 income2
123456789 "$100"	"100"
123456789 "$2000"	"2,000"
123456789 "$1500"	"1,500"
123456789 "$250"	"250"
end

encode income1, generate(new_inc1) label(inc1)
encode income2, generate(new_inc2) label(inc2)
list, nolabel
drop new_*

gen new_inc1 = string(income0)
tostring income0, generate(new_inc2)
list
drop new_*
* -tostring- slightly smarter than -string()-

gen new_inc1 = real(income1)
gen new_inc2 = real(income2)
list
drop new_*

destring income1 income2, generate(new_inc1 new_inc2) force
list
drop new_*
* but not so much...

* the correct approach
gen new_inc1 = subinstr(income1,"$","",.) // recall that the missing value "." can be interpreted as infinitely large
gen new_inc2 = subinstr(income2,",","",.)
list

************************
*A quick view of labels*
************************
use 2010adult_072016, clear

label dir
label list qa2 // attention! here is the label name, not the variable name


********************************
*Exercise 4: Automatic Labeling*
********************************
infix standard 1-3 age 4-6 pop 7-14 using "stdpop.18ages.txt", clear

capture label drop L_age

forvalues i = 1/18 {
	local down = 5*(`i'-1)
	local up = 5*`i'-1

	label define L_age `i' "`down'-`up' years", add
}

label list L_age

label define L_age 18 "85+ years", modify
label values age L_age

tab age

*Transforming between values and labels using -decode- and -encode-
decode age, generate(str_age)

encode str_age, generate(num1_age) label(L2_age)
label list L_age L2_age // if label name does not exist, create as the way it generates

*encode in a desired way we want (specified by a label)
encode str_age, generate(num2_age) label(L_age) // the appearances of num1_age and num2_age are the same, 
                                                // but their values are different.

*end of the do file
log close
clear
