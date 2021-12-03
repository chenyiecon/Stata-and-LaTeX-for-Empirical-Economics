clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 6 Tables and Graphs\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture6_$sysdate.txt", text replace
cd "$path1"

********************************************************************************
*                    Section 2: Making Tables                                  *
********************************************************************************
use "hhinc_child.dta",clear
replace age = floor(age)

gen loghhinc = log(1+hhincpc)

global varlist1 "loghhinc age male urban m_age m_edu m_height m_smoking f_age f_edu f_height f_smoking"

recode age (0/4 = 1)(5/9 = 2)(10/17 = 3)(nonmissing = .), gen(age_group1)
recode age (0/4 = 1)(5/9 = 2)(nonmissing = .), gen(age_group2)

*Panel A: boys
eststo clear
bysort age_group1: eststo: regress z_height $varlist1 if male == 1, vce(robust)
bysort age_group2: eststo: regress z_weight $varlist1 if male == 1, vce(robust)
outreg2 [*] using "gradient_boy.txt", se nonotes nocons nolabel bdec(3) text replace keep(loghhinc)

*Panel B: girls
eststo clear
bysort age_group1: eststo: regress z_height $varlist1 if male == 0, vce(robust)
bysort age_group2: eststo: regress z_weight $varlist1 if male == 0, vce(robust)
outreg2 [*] using "gradient_girl.txt", se nonotes nocons nolabel bdec(3) text replace keep(loghhinc)

*combine multiple panels, treat "tables" as data
insheet using "gradient_girl.txt", clear names
drop if _n <= 2
tempfile append
save `append', replace

insheet using "gradient_boy.txt", clear names
append using `append'

gen space = " "
order v1 - v4 space
outsheet using "gradient_all.txt", replace nonames
*if you wish to put the mean in the data, you can also do it here


********************************************************************************
*                  Section 3: Plotting Graphs                                  *
********************************************************************************
*************************
*Example 1:             *
*1. multiple lines      *
*2. adjust detail       *
*3. two y-axis          *
*4. external information*
*************************
use "TFR CHINA.dta", clear
merge 1:1 year using "Sexratio China new.dta", nogenerate

*w/o any adjustment
twoway line tfr year || connected sexratio year

*make the axis right
twoway line tfr year, yaxis(1) || connected sexratio year, yaxis(2)

*make the graph looks better
twoway line tfr year,lcolor(black) lpattern(dash) yaxis(1) || connected sexratio year, msymbol(plus) yaxis(2) ///
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
       ylabel(0(2)8, angle(0) format(%12.0f) axis(1)) ytitle("Total Fertility Rate", margin(medium) axis(1) ) ///
       ylabel(100(5)120, angle(0) format(%12.0f) axis(2)) ytitle("Sex Ratio at Birth", margin(medium) axis(2) ) ///
	   xlabel(1950(10)2000) xtick(1949(1)2002) xtitle("Year") ///
	   title(" ",size(medium) margin(medium)) ///
	   legend(label(1 "Total Fertility Rate") label(2 "Sex Ratio at Birth") col(2) size(medsmall)) ///
	   xline(1970 1978, lpattern(dash) lwidth(thin) lcolor(black))

*plot in a specific range (下面两张图有什么区别？)
twoway line sexratio year
twoway line sexratio year if inrange(year,1960,2000) /*of course, you can drop the missing observations*/


******************************************
*Example 2:                              *
*5. pitfall! -sort- in -twoway line-     *
******************************************
use "NIPA_to_match.dta", clear

set seed 2018
gen random = uniform()
sort random

twoway line rate_nipa year /*by default, Stata connects data according to order in data*/
twoway line rate_nipa year, sort /*you can also sort in advance*/


******************************************
*Example 3:                              *
*6. Combine Multiple Graphs              *
******************************************
use us_sav_experiment, clear
merge n:1 year using "NIPA_to_match.dta", nogenerate keep(3)
gen y = .

*********************
*use -graph combine-*
*********************
twoway line sav_rate year if experiment == 0,yaxis(1) xaxis(1) lcolor(blue) lwidth(medthin) lwidth(medthin) || connected sav_rate year if experiment == 1, yaxis(1) xaxis(1) lcolor(red) mcolor(red) msymbo(smcircle) msize(small) lwidth(medthin) mlwidth(medthin) ///
	|| connected sav_rate year if experiment == 2, yaxis(1) xaxis(1) lcolor(black) mcolor(black) msymbo(plus) msize(small)  lwidth(medthin) mlwidth(medthin) 	///
	|| connected sav_rate year if experiment == 3, yaxis(1) xaxis(1) lcolor(pink) mcolor(pink) msymbo(circle_hollow)  lwidth(medthin) mlwidth(medthin) ///
	|| connected sav_rate year if experiment == 4, yaxis(1) xaxis(1) lcolor(emerald) mcolor(emerald) msymbo(square)  lwidth(medthin) mlwidth(medthin) ///
	|| line y year,yaxis(2) xaxis(2) 		///		
||,  graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
	   ylabel(0(2)12, nogrid angle(0) format(%12.0f) axis(1) noticks labgap(*3)) xlabel(1980(5)2010,angle(0) axis(1) noticks labgap(*3)) ///
	   ylabel(0(2)12, axis(2) angle(0) noticks) xlabel(,axis(2) nolabel noticks) ///
	   xtick(1980(5)2010,axis(1) tposition(i)) xtick(1980(5)2010,axis(2) tposition(i))  ///
	   ytick(0(2)12,axis(1) tposition(i)) ytick(0(2)12,axis(2) tposition(i))  ///
       ytitle("Percent", axis(1))  xtitle("Year",axis(1))  ///
       ytitle("",axis(2))  xtitle("",axis(2)) ///
	   title("Savings Rate") ///
	   legend(order(1 2 3 4 5) label(1 "Simulated savings rate - baseline") label(2 "Experiment 1 - Fix Costs of Treatment")  label(3 "Experiment 2 - Experiment 1 + Fix Health Technology") label(4 "Experiment 3 - Experiment 2 + Fix Copayment Rate") label(5 "Experiment 4 - Experiment 3 + Fix Income Level") col(1) size(*0.9) rowgap(*0.2) bmargin(medsmall)) 
graph save saving, replace

twoway line sav_rate year if experiment == 0,yaxis(1) xaxis(1) lcolor(blue) lwidth(medthin) lwidth(medthin) || connected sav_rate year if experiment == 1, yaxis(1) xaxis(1) lcolor(red) mcolor(red) msymbo(smcircle) msize(small) lwidth(medthin) mlwidth(medthin) ///
	|| connected sav_rate year if experiment == 2, yaxis(1) xaxis(1) lcolor(black) mcolor(black) msymbo(plus) msize(small)  lwidth(medthin) mlwidth(medthin) 	///
	|| connected sav_rate year if experiment == 3, yaxis(1) xaxis(1) lcolor(pink) mcolor(pink) msymbo(circle_hollow)  lwidth(medthin) mlwidth(medthin) ///
	|| connected sav_rate year if experiment == 4, yaxis(1) xaxis(1) lcolor(emerald) mcolor(emerald) msymbo(square)  lwidth(medthin) mlwidth(medthin) ///
	|| line y year,yaxis(2) xaxis(2) 		///		
||,  graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
	   ylabel(0(2)12, nogrid angle(0) format(%12.0f) axis(1) noticks labgap(*3)) xlabel(1980(5)2010,angle(0) axis(1) noticks labgap(*3)) ///
	   ylabel(0(2)12, axis(2) angle(0) noticks) xlabel(,axis(2) nolabel noticks) ///
	   xtick(1980(5)2010,axis(1) tposition(i)) xtick(1980(5)2010,axis(2) tposition(i))  ///
	   ytick(0(2)12,axis(1) tposition(i)) ytick(0(2)12,axis(2) tposition(i))  ///
       ytitle("Percent", axis(1))  xtitle("Year",axis(1))  ///
       ytitle("",axis(2))  xtitle("",axis(2)) ///
	   title("Savings Rate") ///
	   legend(off) 
graph save saving_no_legend, replace

twoway line health_share year if experiment == 0,yaxis(1) xaxis(1) lcolor(blue) lwidth(medthin) lwidth(medthin) || connected health_share year if experiment == 1, yaxis(1) xaxis(1) lcolor(red) mcolor(red) msymbo(smcircle) msize(small) lwidth(medthin) mlwidth(medthin) ///
	|| connected health_share year if experiment == 2, yaxis(1) xaxis(1) lcolor(black) mcolor(black) msymbo(plus) msize(small)  lwidth(medthin) mlwidth(medthin) 	///
	|| connected health_share year if experiment == 3, yaxis(1) xaxis(1) lcolor(pink) mcolor(pink) msymbo(circle_hollow)  lwidth(medthin) mlwidth(medthin) ///
	|| connected health_share year if experiment == 4, yaxis(1) xaxis(1) lcolor(emerald) mcolor(emerald) msymbo(square)  lwidth(medthin) mlwidth(medthin) ///
	|| line y year,yaxis(2) xaxis(2) 	 ///		
||,  graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
	   ylabel(10(2)22, nogrid angle(0) format(%12.0f) axis(1) noticks labgap(*3)) xlabel(1980(5)2010,angle(0) axis(1) noticks labgap(*3)) ///
	   ylabel(10(2)22,axis(2) angle(0) noticks) xlabel(,axis(2) nolabel noticks) ///
	   xtick(1980(5)2010,axis(1) tposition(i)) xtick(1980(5)2010,axis(2) tposition(i))  ///
	   ytick(10(2)20,axis(1) tposition(i)) ytick(10(2)20,axis(2) tposition(i))  ///
       ytitle("Percent", axis(1))  xtitle("Year",axis(1))  ///
       ytitle("",axis(2))  xtitle("",axis(2)) ///
	   title("Medical Expenses") ///
	   legend(order(1 2 3 4 5)label(1 "Simulated savings rate - baseline") label(2 "Experiment 1 - Fix Costs of Treatment")  label(3 "Experiment 2 - Experiment 1 + Fix Health Technology") label(4 "Experiment 3 - Experiment 2 + Fix Copayment Rate") label(5 "Experiment 4 - Experiment 3 + Fix Income Level") col(1) size(*0.9) rowgap(*0.2) bmargin(medsmall)) 
graph save health, replace

graph combine saving.gph health.gph
graph combine saving.gph health.gph, graphregion(fcolor(gs16) lcolor(gs16)) imargin(0) xsize(20) ysize(10)	   
graph combine saving_no_legend.gph health.gph, graphregion(fcolor(gs16) lcolor(gs16)) cols(1) imargin(0) xsize(12) ysize(20)	   

****************
*graph by group*
****************
rename sav_rate var1
rename health_share var2
reshape long var, i(year experiment) j(type)

twoway line var year if experiment == 0, lcolor(blue) lwidth(medthin) lwidth(medthin) || connected var year if experiment == 1, lcolor(red) mcolor(red) msymbo(smcircle) msize(small) lwidth(medthin) mlwidth(medthin) ///
	|| connected var year if experiment == 2, lcolor(black) mcolor(black) msymbo(plus) msize(small)  lwidth(medthin) mlwidth(medthin) 	///
	|| connected var year if experiment == 3, lcolor(pink) mcolor(pink) msymbo(circle_hollow)  lwidth(medthin) mlwidth(medthin) ///
	|| connected var year if experiment == 4, lcolor(emerald) mcolor(emerald) msymbo(square)  lwidth(medthin) mlwidth(medthin) ///
||, by(type) 

*adjust the details
label define var 1 "Savings Rate" 2 "Medical Expenses"
label values type var

twoway line var year if experiment == 0, lcolor(blue) lwidth(medthin) lwidth(medthin) || connected var year if experiment == 1, lcolor(red) mcolor(red) msymbo(smcircle) msize(small) lwidth(medthin) mlwidth(medthin) ///
	|| connected var year if experiment == 2, lcolor(black) mcolor(black) msymbo(plus) msize(small)  lwidth(medthin) mlwidth(medthin) 	///
	|| connected var year if experiment == 3, lcolor(pink) mcolor(pink) msymbo(circle_hollow)  lwidth(medthin) mlwidth(medthin) ///
	|| connected var year if experiment == 4, lcolor(emerald) mcolor(emerald) msymbo(square)  lwidth(medthin) mlwidth(medthin) ///
||, by(type, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) yrescale  note("")) ///
    ytitle("Percent")  xtitle("Year") ///
	legend(order(1 2 3 4 5)label(1 "Simulated savings rate - baseline") label(2 "Experiment 1 - Fix Costs of Treatment")  label(3 "Experiment 2 - Experiment 1 + Fix Health Technology") label(4 "Experiment 3 - Experiment 2 + Fix Copayment Rate") label(5 "Experiment 4 - Experiment 3 + Fix Income Level") col(1) size(*0.9) rowgap(*0.2) bmargin(medsmall)) 

/*两种方法各有千秋                                       
*graph combine对单张图的控制更好（毕竟先生成单单张图）   
*by()整体感更强，当子图很多时更方便                      */

	
******************************************
*Example 4:                              *
*7. Plot the CI                          *
*8. the order matters                    *
******************************************
use  CI_example.dta, clear

gen up = coef + 1.96*se_coef
gen down = coef - 1.96*se_coef

twoway rarea down up year,color(gs12) || line coef year, lwidth(medthick) lcolor(black)  ///
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
       title("",size(medium) margin(medium)) ///
       ylabel(-0.1(0.05)0.05 , angle(0) format(%12.2f) ) ytitle("Coefficient of the Elderly", margin(medium) size(*1.1) axis(1)) ///
	   xlabel(1990(5)2010) xtick(1990(5)2010) xtitle("Year")  ///
	   legend(order(2 1)label(2 "Coefficient") label(1 "95% CI") col(2) size(medsmall))

*why order matters?
twoway line coef year, lwidth(medthick) lcolor(black) || rarea down up year,color(gs12) ///
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
       title("",size(medium) margin(medium)) ///
       ylabel(-0.1(0.05)0.05 , angle(0) format(%12.2f) ) ytitle("Coefficient of the Elderly", margin(medium) size(*1.1) axis(1)) ///
	   xlabel(1990(5)2010) xtick(1990(5)2010) xtitle("Year")  ///
	   legend(order(2 1)label(2 "Coefficient") label(1 "95% CI") col(2) size(medsmall))


********************************************************************************
*         Several Graph Example Other than Line Graphs                         *
********************************************************************************
******************************************
*Example 5: Bar Graph                    *
*Number of Sent-down-youth Each Year     *
******************************************
clear
input str9 year total chadui collective_farm state_farm
1962-1966	129.28	87.06	0	42.22
1967-1968	199.68	165.96	0	33.72
1969	267.38	220.44	0	46.94
1970	106.4	74.99	0	31.41
1971	74.83	50.21	0	24.62
1972	67.39	50.26	0	17.13
1973	89.61	80.64	0	8.97
1974	172.48	119.19	34.63	18.66
1975	236.86	163.45	49.68	23.73
1976	188.03	122.86	41.51	23.66
1977	171.68	113.79	41.9	15.99
1978	48.09	26.04	18.92	3.13
1979	24.77	7.32	16.44	1.01
end

foreach var in total chadui collective_farm state_farm {
replace `var'= `var'/100
}

gen v_temp = chadui + collective_farm
encode year, generate(period)

twoway bar chadui period, barw(0.6) base(0) ///
|| rbar v_temp chadui period, barw(0.6) ///
|| rbar total v_temp period, barw(0.6) ///
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
     ylabel(0(0.5)3, angle(0) format(%12.1f)) ytitle("Number of SDYs (Million)", margin(medium)) ///
	 xlabel(1(1)13, noticks valuelabel angle(90)) xtitle("Year") ///
	 legend(label(1 "Rural Villages") label(2 "Collective Farms") label(3 "State Farms") ring(0) pos(2) colgap(*0.5)  ) 
graph export "SDY_number.pdf",replace

******************************************
*Example 6: Matrix Plot                  *
*Distribution of Depression              *
*requires external command -plotmatrix-  *
******************************************
use "mental_health.dta", clear

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

graph export "Mental_Life_Quality.pdf",replace

******************************************
*Example 7: Regression Discontinuity     *
*Female Retirement Age at 50             *
*requires external command -rdplot-      *
******************************************
use "female_retirement", clear

rdplot retirement age if inrange(age,40,60),c(50) graph_options( ///
graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
       title("",size(medium) margin(medium)) ///
       ylabel(0(0.1)0.7 , angle(0) format(%12.1f)) ytitle("Retirement Probabilities", size(*1.0)) ///
	   xlabel(40(5)60) xtick(40(5)60) xtitle("Age")  ///
       legend(off) )
graph export "ret_rd.pdf",replace

******************************************
*Example 8: Bubble Plot                  *
*GDP p.c. and Life Expectancy            *
******************************************
use "LE_GDP_2015", clear

drop if GDP_pc > 100000
gen log_GDP = log(GDP_pc)
drop GDP_pc

expand 2

gen group = .
replace group = 1 if _n <= _N/2
replace group = 2 if _n > _N/2

replace life_expectancy = . if !inlist(countryname,"China","United States","India") & group == 2

twoway scatter life_expectancy log_GDP [aw = population] if group == 1, msymbol(circle_hollow) mcolor(black) ///
     ||scatter life_expectancy log_GDP [aw = population] if group == 2, msymbol(circle_hollow) mcolor(red) ///
     || qfit life_expectancy log_GDP [aw = population], lcolor(black) lpattern(dash) lwidth(medthick) ///
||, graphregion(fcolor(gs16) lcolor(gs16)) plotregion(lcolor(gs16) margin(zero)) ///
     ylabel(50(10)90, angle(0) format(%12.0f)) ytitle("Life Expectancy (Years)", margin(medium)) ///
	 xlabel(5(1)12) xtitle("log GDP per capita ($)") legend(off) ///
	 text(80 8.5 "China", size(large) color(red)) text(76 11 "United States", size(large) color(red)) text(71.5 6.8 "India", size(large) color(red)) 
graph export "GDP_LE.pdf",replace


*end of the do file
log close
clear
	
