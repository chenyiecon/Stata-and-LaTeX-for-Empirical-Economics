clear all
set more off, permanent
capture log close

global path1  "E:\OneDrive\Teaching\SoftwareEmpiricalEconomics\Lecture 7 LaTeX and Writing\Stata Exercise" 
global sysdate=c(current_date)
log using "$path1\lecture7_$sysdate.txt", text replace
cd "$path1"



********************************************************************************
*               Making Tables by -estout- Package                              *
********************************************************************************
use "$path1\hhinc_child.dta",clear
replace age = floor(age)

gen loghhinc = log(1+hhincpc)

global varlist1 "loghhinc age urban m_age m_edu m_height m_smoking f_age f_edu f_height f_smoking"

recode age (0/4 = 1)(5/9 = 2)(10/17 = 3)(nonmissing = .), gen(age_group_height)
recode age (0/4 = 1)(5/9 = 2)(nonmissing = .), gen(age_group_weight)

*在estout中，你希望显示变量的label，而不是变量名
label variable loghhinc "log(HH Income p.c.)"

*Panel A: boys
eststo clear
foreach y in height weight {
	levelsof age_group_`y', local(levels)

foreach i of local levels {
	eststo: regress z_`y' $varlist1 if male == 1 & age_group_`y' == `i', vce(robust)
	summ z_`y' if e(sample)==1
	estadd scalar ymean = r(mean) // 这种方法更直观，有更方便的方法，如estadd summ
}
}

esttab, se nogaps label // 先用最简单的命令，看一下回归结果。等确定表格设计后，再写完整的命令。

local titles "& 0--4 & 5--9 & 10--17 & 0--4 & 5--9 \\ \cline{2-6} "
local numbers "& (1) & (2) & (3) & (4) & (5) \\ \hline" // 自定义表头，需要时可以借助于Excel2LaTeX插件

esttab using gradient_all.tex, replace style(tex) label fragment nonotes booktabs nolines ///
	b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
	keep(loghhinc) stats(ymean r2 N, fmt(3 3 0) labels("\hline Sample Mean" "R-Squared" "Observations")) ///
	mgroups("Height-for-Age z Score" "Weight-for-Age z Score", pattern(1 0 0 1 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	mlabels(none) nonumbers nomtitle posthead("`titles'" "`numbers'") ///
	refcat(loghhinc "\textbf{Panel A: Boys}", nolabel)

/*
上述option一共7行
第1-2行控制表格整体风格，很少改动（除了replace会改成append）
第3行控制model specific的一些东西，报告哪些变量的系数，最后报告哪些统计量？
第4-6行控制表头，其中第4、5行代表分组，如不需要可以去掉
第7行表示在loghhinc新增一行用以说明，涉及multiple panel时需要
*/
	
*Panel B: girls
eststo clear
foreach y in height weight {
	levelsof age_group_`y', local(levels)

foreach i of local levels {
	eststo: regress z_`y' $varlist1 if male == 0 & age_group_`y' == `i', vce(robust)
	summ z_`y' if e(sample)==1
	estadd scalar ymean = r(mean) 
}
}

esttab using gradient_all.tex, append style(tex) label fragment nonotes booktabs nolines ///
	b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
	keep(loghhinc) stats(ymean r2 N, fmt(3 3 0) labels("\hline Sample Mean" "R-Squared" "Observations")) ///
	nomtitle nonumbers  ///
	refcat(loghhinc "\midrule \textbf{Panel B: Girls}", nolabel)
/*
我自己偏向于用nolines这一option，然后有需要时自己划线
*/	
	
********************************************************************************
*end of the do file
log close
clear
	
