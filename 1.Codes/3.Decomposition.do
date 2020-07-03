forvalues i = 1/4{
use "2.Data/dtafiles/Cont.dta",clear
keep if year == 1975 | year == 1990 | year == 2005 | year == 2019
keep if type == `i'
keep year major men women

recode major 1/4=1 5/8=2 9/14=3 15/28=4 29/37=5 38/42=6 43=7 44/48=8 49/59=9 60/63=10 64/68=11 ,gen(majord)

*Definition of STEM :  science, engineering, agriculture, and medicine & healthcare
*recode majord 3/6=1 1/2=0 6/11=0, gen(stem)
recode majord 3/5=1 1/2=0 6=2 7/11=0, gen(stem)
*工芸をnon-stemにする
replace stem = 0 if major == 27

gen total = men + women
recode year  2019=2020
reshape wide men women total, i(major) j(year)

rename men* Mj*
rename women* Fj*
rename total* Tj*

forvalues t = 1975(15)2020{
	egen F`t' = total(Fj`t')
	egen M`t' = total(Mj`t')
	egen T`t' = total(Tj`t')
	gen MjM`t' = Mj`t'/M`t' * 100
	gen FjF`t' = Fj`t'/F`t' * 100
	gen Dj`t' = Fj`t' / F`t' - Mj`t' / M`t'
	replace Dj`t' = 100 / 2 * abs(Dj`t')
	egen D`t' = total(Dj`t')
}

*** 非類似度指数の推移を計算。 
sum D1975 D1990 D2005 D2020, sep(0)

*** Decomposition

*keep if stem == 0

forvalues s = 1975(15)2020{
	forvalues t = 1990(15)2020{
	bysort stem: gen comp2j_`s'`t' = 100 / 2 * abs((Fj`t' / Tj`t' * Tj`s') / (F`t' / T`t' * T`s') - (Mj`t' / Tj`t' * Tj`s') / (M`t' / T`t' * T`s')) - Dj`s' if `s' < `t'
	bysort stem: gen comp1j_`s'`t' = Dj`t' - 100 /2 * abs((Fj`t' / Tj`t' * Tj`s') / (F`t' / T`t' * T`s') - (Mj`t' / Tj`t' * Tj`s') / (M`t' / T`t' * T`s')) if `s' < `t'		
	
	bysort stem: egen comp2_`s'`t' = total(comp2j_`s'`t') if `s' < `t'
	bysort stem: egen comp1_`s'`t' = total(comp1j_`s'`t') if `s' < `t'
	}
}

keep ///
D1975 D1990 D2005 D2020 ///
 comp1_19751990 comp1_19752020 comp1_19902005 comp1_20052020 ///
 comp2_19751990 comp2_19752020 comp2_19902005 comp2_20052020 stem

bysort stem: gen id = _n
drop if id > 1

lab def steml 1 "STEM" 0 "Non-STEM" 2"Health"
lab val stem steml

*** Rename for reshape

rename D1975 D1
rename D1990 D2
rename D2005 D3
rename D2020 D4

rename comp1_19751990 comp11
rename comp1_19902005 comp12
rename comp1_20052020 comp13
rename comp1_19752020 comp14

rename comp2_19751990 comp21
rename comp2_19902005 comp22
rename comp2_20052020 comp23
rename comp2_19752020 comp24

****Reshape long

reshape long D comp1 comp2, i(stem) j(year)
xtset stem year
replace year = year * 15 + 1960

gen Ds = D if year == 1975
replace Ds = Ds[_n-1] if Ds[_n-1] ~= .
gen Dr = D/Ds

recode year 2020=2019

* Labeling
lab def yearlab 1975 "1975–1990" 1990 "1990–2005" 2005 "2005–2019" 2019 "1975–2019"
lab val year yearlab

save "2.Data/dtafiles/Decomp.dta",replace

use "2.Data/dtafiles/Decomp.dta",clear
keep if stem == 1
rename comp2 comp2st
rename comp1 comp1st
save "2.Data/dtafiles/DecompStem.dta",replace

use "2.Data/dtafiles/Decomp.dta",clear
keep if stem == 0
rename comp2 comp2ns
rename comp1 comp1ns
save "2.Data/dtafiles/DecompNonStem.dta",replace

use "2.Data/dtafiles/Decomp.dta",clear
keep if stem == 2
rename comp2 comp2hl
rename comp1 comp1hl
save "2.Data/dtafiles/DecompNonHealth.dta",replace

merge 1:1 year using 2.Data/dtafiles/DecompStem.dta
drop _merge
merge 1:1 year using 2.Data/dtafiles/DecompNonStem.dta
* Figure
*comp2ns comp1ns comp2st comp1st
*専攻分布効果:Major mix effect (相対的なサイズ変化の寄与分)
*性別構成効果:Sex composition effect (専攻における専攻内の男女比率変化の寄与分)
graph bar comp1st comp1ns comp1hl comp2st comp2ns comp2hl , ///
over(year, label(angle(45))) stack ylabel(-35(5)10, grid glw(thin)) yline(0, lc(gs3)) ///
bar(4, col(gs4)) bar(5, col(ebblue)) bar(6, col(dkorange)) bar(1, col(gs11)) bar(2, col(eltblue)) bar(3, col(sandb)) ///
legend(label(1 "Major mix effect (STEM)") label(4 "Sex composition effect (STEM)") label(2 "Major mix effect (Non-STEM)") label(5 "Sex composition effect (Non-STEM)") label(3 "Major mix effect (Health)") label(6 "Sex composition effect (Health)")  symxsize(vlarge) size(small)) 
graph export 3.Results/Decomp`i'.pdf, replace
keep year comp2hl comp1hl comp2st comp1st comp2ns comp1ns
}


