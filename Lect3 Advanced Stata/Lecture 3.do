clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 3 Re-introducing Stata\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture3_$sysdate.txt", text replace
cd "$path1"

********************************************************************************
*               Section 1: Introduction of Stata                               *
********************************************************************************
****************
* Stata Set-up *
****************
sysdir
*now move profile.do to the Stata directory

sysdir
*now delete profile.do and restart Stata

**************************************
* Why Double Precision is Important? *
**************************************
*hhid & line uniquely identifies individual
set type float

use chip2002_ind,clear
duplicates report hhid line
tab line 

gen id = hhid*10 + line
duplicates report id
*why id do not uniquely identify the individual?

drop id
set type double
gen id = hhid*10 + line
duplicates report id

clear

*****************
* Variable List *
*****************
sysuse nlsw88,clear

sum age race married never_married grade
sum age-grade
sum s*
sum ?a?e


********************************************************************************
*               Section 3: More Programming Details                            *
********************************************************************************
*********************************
*      scalar/local/global      *
*********************************
********
*scalar*
********
clear

scalar a = 3
scalar b = ln(a) + (3^4.2)/exp(2)
display a
display b

/*scalar can also be used for strings*/
scalar s1 = "hello, world"
scalar s2 = substr(s1,1,5)   
display s1
display s2

*******
*local*
*******
local a 2 + 3
local b = 2 + 3
display "`a'"
display `a'
display "`b'"

***********************************
*why should we avoid using scalar?*
***********************************
sysuse auto, clear
scalar p = 0.7
display p
display price

**********************************
*Using Global Together with Local*
**********************************
use cfps2010, clear

global varlist1 yedu male age age2
global varlist2 yedu male age age2 meduca feduca

*wrong approach
forvalues i = 1/2 {
	reg loginc $varlist`i'
}

*correct approach 1
forvalues i = 1/2 {
	reg loginc ${varlist`i'}
}

*correct approach 2
local varlist1 yedu male age age2
local varlist2 yedu male age age2 meduca feduca

forvalues i = 1/2 {
	reg loginc `varlist`i''
}

**************************
*random number generating*
**************************
help statistical function
help random number

*usage of seed
forvalues i = 1/2 {
	clear
	set obs 100

	set seed 2019 // compare with commenting this line
	gen v1 = runiform()

	list in 1/10
}

*many distributions can be transformed from uniform distribution
display 1+floor(runiform()*8)

***************
*matrix basics*
***************
*matrix: store information
use "$path1\mental_health.dta", clear

drop if missing(P_SR_satisified,P_SR_health)==1

collapse (mean) CESD [pw = INDV_weight_ad2], by(P_SR_satisified P_SR_health)

reshape wide CESD, i(P_SR_satisified) j(P_SR_health)

renvars CESD* / Excellent Very_Good Good Fair Poor
decode P_SR_satisified, generate(name)
replace name = substr(name,3,.)
order name

mkmat Excellent Very_Good Good Fair Poor, matrix(X) rownames(name)

plotmatrix, m(X) c(red) ylabel(,angle(45)) xlabel(,angle(45)) ///
	s(2(2)20) ytitle("Self-Rated Life Satisfaction", margin(medium) size(*1.1)) xtitle("Self-Rated Health")


*matrix: extract information
use "$path1\hhinc_child.dta",clear

gen loghhinc = log(1+hhincpc)

global varlist1 "loghhinc age male urban"
regress z_height $varlist1, robust
help regress

matrix list e(V)

matrix V = e(V)
display V[2,3]

*********************************
* Generate Tables Automatically *
*********************************
use "$path1\hhinc_child.dta",clear

global var1 "age male urban height weight"
global var2 "z_height z_weight"
global var3 "hhincpc tapwater toilet m_age m_height m_edu m_smoking f_age f_height f_edu f_smoking"
global var4 "insurance prevservice"
global var5 "protn fat kcal carbo"

keep hhid line wave $var1 $var2 $var3 $var4 $var5

*第一步：计算所有需要的数字
forvalues j=1/5 { /*第一层循环：列*/
if (`j'==1) local command " "
if (`j'==2) local command "if urban==1"
if (`j'==3) local command "if urban==0"
if (`j'==4) local command "if male==1"
if (`j'==5) local command "if male==0"
quietly count `command'
local num`j'=r(N)

forvalues k=1/5 {
	foreach var of global var`k' { /*第二层循环：行*/
		quietly reg `var' `command'  
		local c`var'`j'=_b[_cons] 
*summarize + r(mean) 也是类似的效果，该方法的优势是以后结合sample weight会
*容易些（之后会提到）
			local si`var'`j'=" " // 养成好习惯，赋值要“完整”
			if (`j'==3) {
				quietly reg `var' urban  
				quietly test urban
				if (r(p)<=0.01) local si`var'`j'="***"
				if (r(p)<=0.05&r(p)>0.01) local si`var'`j'="**"
				if (r(p)<=0.1&r(p)>0.05) local si`var'`j'="*"
			}
			if (`j'==5) {
				quietly reg `var' male  
				quietly test male
				if (r(p)<=0.01) local si`var'`j'="°°°"
				if (r(p)<=0.05&r(p)>0.01) local si`var'`j'="°°"
				if (r(p)<=0.1&r(p)>0.05) local si`var'`j'="°"
			} // 根据T-test的结果加星号
	}
}
}

*第二步：将所得到的数字填到表格中去
*这里采取的先填一行，再挪到下一行
*（作为练习，可以自己尝试先填一列，再填下一列）

clear
generate name=" "
forvalues i=1/5 {
generate v`i'=" "
}

set obs 100
local i=1 
forvalues k=1/5 {
	foreach var of global var`k' { /*第一层循环：行*/
		replace name="`var'" if  _n==`i'
		forvalues j=1/5 { /*第一层循环：列*/
			replace v`j'=string(`c`var'`j'', "%12.2f")+"`si`var'`j''" if _n==`i'
		}
	local i=`i'+1
}
local i=`i'+1
}

*以下都属于细节微调
local i=`i'-1
forvalues j=1/5 {
replace v`j'=string(`num`j'', "%12.0f")  if _n==`i'
}
drop if _n>`i'

replace v2=" " if _n==3
replace v3=" " if _n==3
replace v4=" " if _n==2
replace v5=" " if _n==2

log close
clear

