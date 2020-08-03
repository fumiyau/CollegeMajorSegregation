******************************************
**  Supp analysis w/out others fields  ***
******************************************

******************************************
************  Selecting fields  **********
******************************************
 *To select fields, we first chose those 14 largest fields that, taken together, 
 *account for half of all degrees given across the period. (England and Li 2006)
 
use "2.Data/dtafiles/Cont_rist.dta",clear
 keep if type == 1
 sort year scorex /*Sort by score*/
bysort year: gen score_order=sum(scorex)
bysort type: egen scoremax_order=max(score_order)
gen score_rel_order = score_order/scoremax_order
keep if score_rel_order > 0.25
 *We chose 10 majors in 1975 that account for 75% of all degrees 
 *1文学,5法学・政治学,6商学・経済学,7社会学,15機械工学,16電気通信工学,17土木建築工学,40薬学,50小学校課程,62音楽
 *Plus 2 majors that also accounts for 75% of all degrees in 2019 but not included in 1988 (ta major if year == 2018)
 *看護学,food science 41, 45
 
******************************************
*************** Figures  *****************
******************************************

***Plot Overal trend***
use "2.Data/dtafiles/Cont_rist.dta",clear
gen score1975=score if year  ==1975
bysort type: egen scoremax=max(score1975)
gen scorerel=(score/scoremax)*100 - 100
replace MitMt = MitMt * 100
replace FitFt = FitFt * 100
local opts_line "ms(oh)  recast(connected) ytitle("") xtitle("") xtick(1975(5)2020, grid) xlabel(1975(15)2020)"

**Overall : MitMt FitFt
keep if major == 1 | major == 5 | major == 6 | major == 7 | major == 15| major == 16 | major == 17 | ///
	major == 40 | major == 50 | major == 62 | ///
	major == 41 | major == 45
*Total

local opt_title1 "Fig_MF_total_top12"
local opt_title2 "Fig_MF_national_top12"
local opt_title3 "Fig_MF_public_top12"
local opt_title4 "Fig_MF_private_top12"
local opt_line1 "lc(gs1) lp(solid)"
local opt_line2 "lc(gs8) lp(shortdash)"
local opt_scatter1 "mlabel(MitMt) mlabsize(medsmall) m(o) mc(gs1)"
local opt_scatter2 " mlabel(FitFt) mlabsize(medsmall) m(oh) mc(gs8)"
local opt_by "by(major, note(""))"

format FitFt %3.1f
format MitMt %3.1f

forvalues i = 1/4{
	twoway ///
	(line MitMt year , `opt_line1' `opt_by') ///
	(line FitFt year , `opt_line2' `opt_by') ///
	(scatter MitMt year if year == 1975 , mlabposition(1) mlabc(gs1) `opt_scatter1' `opt_by' ) ///
	(scatter FitFt year if year == 1975 , mlabposition(5) mlabc(gs8) `opt_scatter2' `opt_by' ) ///
	(scatter MitMt year if year == 2019 , mlabposition(11) mlabc(gs1) `opt_scatter1' `opt_by' ) ///
	(scatter FitFt year if year == 2019 , mlabposition(7) mlabc(gs8) `opt_scatter2' `opt_by' ) ///
	if type == `i',  ylabel(0(10)40, format(%2.0f) labsize(medsmall)) xtick(1975(5)2020, grid) xlabel(1975(15)2020, labsize(medsmall)) ///
	scheme(plotplain) xtitle("") legend(order(1 2) row(1) pos(6) label(1 "Mit / Mt") label(2 "Fit / Ft"))
	graph export 3.Results/sensitivity/excl_others/`opt_title`i''.pdf,replace 
}

**Overall : D index
*Major
use "2.Data/dtafiles/Cont_rist.dta",clear

recode major 1/4=1 5/8=2 9/14=3 15/28=4 29/37=5 38/42=6 43=7 44/48=8 49/59=9 60/63=10 64/68=11 ,gen(majord)
sort type year majord
by type year majord: gen id = _n
by type year majord: gen MitMtd=sum(MitMt)
by type year majord: egen MitMtdm=max(MitMtd)
by type year majord: gen FitFtd=sum(FitFt)
by type year majord: egen FitFtdm=max(FitFtd)
keep if id == 1
drop MitMt  MitMtd FitFt FitFtd
rename MitMtdm MitMt
rename FitFtdm FitFt
drop scorex score

bysort type year: gen scorex=100*(1/2)*(abs(MitMt-FitFt))
bysort type year: gen score=sum(scorex)
keep if majord == 11

keep type year score
rename score scorem
save "2.Data/dtafiles/sensitivity/excl_others/ContMajor.dta",replace

*Minor
use "2.Data/dtafiles/Cont_rist.dta",clear

gen score1975=score if year  ==1975
bysort type: egen scoremax=max(score1975)
gen scorerel=(score/scoremax)*100 - 100
replace MitMt = MitMt * 100
replace FitFt = FitFt * 100
keep if major == 67 /*change value because of omitting 68*/

keep type year score 

**Merge datasets
merge 1:1 year type using "2.Data/dtafiles/sensitivity/excl_others/ContMajor.dta"

label variable score "Detailed (59 fields)"
label variable scorem "Aggregated (11 fields)"
*Trends in sex segregation by college major, 1975-2019

local opt_line1 "lc(gs3) lp(solid)"
local opt_line2 "lc(gs3) lp(shortdash)"
local opt_scatter1 "mlabel(score) mlabsize(small) m(o) mc(gs3)"
local opt_scatter2 " mlabel(scorem) mlabsize(small) m(oh) mc(gs3)"
format score %3.1f
format scorem %3.1f
twoway ///
(line score year , `opt_line1' by(type, imargin(medsmall) note("")) ) ///
(scatter score year if year == 1975 , mlabposition(1) `opt_scatter1' by(type, note("")) ) ///
(scatter score year if year == 2019 , mlabposition(12) `opt_scatter1' by(type, note("")) ) ///
, xtick(1975(5)2020, labsize(medsmall) grid) xlabel(1975(15)2020, labsize(medsmall)) ytick(0(10)80, grid) ylabel(0(20)80, labsize(medsmall) format(%2.0f)) ///
legend(order(0)) scheme(plotplain) xtitle("Year") ytitle("D index") 
graph export "3.Results/sensitivity/excl_others/Fig1_1.pdf",replace 

*(line scorem year , `opt_line2' by(type, note("")) ) ///

*(scatter scorem year if year == 1975 , mlabposition(5) `opt_scatter2' by(type, note("")) ) ///
*(scatter scorem year if year == 2019 , mlabposition(6) `opt_scatter2' by(type, note("")) ) ///

*Create D index by STEM
use "2.Data/dtafiles/Cont_rist.dta",clear
recode major 1/4=1 5/8=2 9/14=3 15/28=4 29/37=5 38/42=6 43=7 44/48=8 49/59=9 60/63=10 64/68=11 ,gen(majord)
*Definition of STEM :  science, engineering, agriculture, and medicine & healthcare
*recode majord 3/6=1 1/2=0 7/11=0, gen(stem)
recode majord 3/5=1 1/2=0 6=2 7/11=0, gen(stem)
*工芸をnon-stemにする
replace stem = 0 if major == 27
drop scorex score
sort type year major stem
bysort type year stem: gen scorex=100*(1/2)*(abs(MitMt-FitFt))
bysort type year stem: gen score=sum(scorex)
bysort type year stem: egen scorem=max(score)
bysort type year stem: gen id=_n
keep if id == 1
drop score
rename scorem score
keep if stem == 1
rename score scorest
save "2.Data/dtafiles/sensitivity/excl_others/ContStem.dta",replace

use "2.Data/dtafiles/Cont_rist.dta",clear
recode major 1/4=1 5/8=2 9/14=3 15/28=4 29/37=5 38/42=6 43=7 44/48=8 49/59=9 60/63=10 64/68=11 ,gen(majord)
*Definition of STEM :  science, engineering, agriculture, and medicine & healthcare
*recode majord 3/6=1 1/2=0 7/11=0, gen(stem)
recode majord 3/5=1 1/2=0 6=2 7/11=0, gen(stem)
*工芸をnon-stemにする
replace stem = 0 if major == 27
drop scorex score
sort type year major stem
bysort type year stem: gen scorex=100*(1/2)*(abs(MitMt-FitFt))
bysort type year stem: gen score=sum(scorex)
bysort type year stem: egen scorem=max(score)
bysort type year stem: gen id=_n
keep if id == 1
drop score
rename scorem score
keep if stem == 0
rename score scorens
save "2.Data/dtafiles/sensitivity/excl_others/ContNonStem.dta",replace

use "2.Data/dtafiles/Cont_rist.dta",clear
recode major 1/4=1 5/8=2 9/14=3 15/28=4 29/37=5 38/42=6 43=7 44/48=8 49/59=9 60/63=10 64/68=11 ,gen(majord)
*Definition of STEM :  science, engineering, agriculture, and medicine & healthcare
*recode majord 3/6=1 1/2=0 7/11=0, gen(stem)
recode majord 3/5=1 1/2=0 6=2 7/11=0, gen(stem)
*工芸をnon-stemにする
replace stem = 0 if major == 27
drop scorex score
sort type year major stem
bysort type year stem: gen scorex=100*(1/2)*(abs(MitMt-FitFt))
bysort type year stem: gen score=sum(scorex)
bysort type year stem: egen scorem=max(score)
bysort type year stem: gen id=_n
keep if id == 1
drop score
rename scorem score
keep if stem == 2
rename score scorehl
save "2.Data/dtafiles/sensitivity/excl_others/ContHealth.dta",replace

merge 1:1 year type using "2.Data/dtafiles/sensitivity/excl_others/ContStem.dta"
drop _merge
merge 1:1 year type using "2.Data/dtafiles/sensitivity/excl_others/ContNonStem.dta"

label variable scorens "Non-STEM"
label variable scorest "STEM"
label variable scorehl "Health"
twoway ///
(line scorens year , lc(black) lp(dash) by(type, note("")) ) ///
(line scorest year ,  lc(gs8) lp(solid) by(type, note("")) ) ///
(line scorehl year ,  lc(gs4) lp(dash_dot) by(type, note("")) ) ///
, xtick(1975(5)2020, grid) xlabel(1975(15)2020) ylabel(0(10)100) ///
scheme(plotplain) xtitle("Year") legend(row(1) pos(6))
graph export "3.Results/sensitivity/excl_others/Fig1_1_stemx.pdf",replace 

keep if year == 1975 | year == 1990 | year == 2005 | year == 2019
keep year type scorehl scorest scorens
sort type year
order year type scorens scorest scorehl
gen score = scorens + scorest + scorehl

twoway ///
(line scorens year , lc(black) lp(dash) by(type, note("")) ) ///
(line scorest year ,  lc(gs8) lp(solid) by(type, note("")) ) ///
, xtick(1975(5)2020, grid) xlabel(1975(15)2020) ylabel(0(10)100) ///
scheme(plotplain) xtitle("Year") legend(row(1) pos(6))
graph export 3.Results/Fig1_1_stem.pdf,replace 

******
local opts_line " ytitle("") xtitle("") xtick(1975(5)2020, grid) xlabel(1975(15)2020)"
twoway line score year, lc(cranberry) `opts_line' ylabel(0(10)100) ///
by(type, note("") row(1) title("Trends in sex segregation by college major, 1975-2019") )
graph export 3.Results/Fig1_1a.pdf,replace 

*twoway scatter scorerel year, mc(navy) lc(navy) lp(shortdash) `opts_line' ylabel(-50(10)0) ///
*by(type,  note("") row(1) title("(B) % change from 1975") legend(off))
*graph save 3.Results/Fig1_1b.gph, replace
*graph export 3.Results/Fig1_1b.pdf,replace 

*graph combine 3.Results/Fig1_1a.gph 3.Results/Fig1_1b.gph ///
*,imargin(small) scale(2.0) scheme(plotplain) b1title("Year", size(small))

*graph export 3.Results/Fig1_1.pdf,replace 

***Plot Specific trend selective***
forvalues i=1/4 {
use "2.Data/dtafiles/Cont.dta",clear
keep if type == `i'
keep if major  == 1| major == 4 | major == 5 | major == 6 | major == 16 | major == 17 | major == 28 | major == 41 | major == 42 | major == 50 | major == 51 | major  == 60
local opts_line "ms(oh)  recast(connected) ytitle("") xtitle("") xtick(1975(5)2020, grid) xlabel(1975(15)2020) ylabel(0(5)15)"
twoway ///
(scatter scorex year, mc(black) lc(black) lp(solid)`opts_line' by(major, note("")) ) ///
,scheme(plotplain) legend(row(10) pos(6))
graph export 3.Results/Fig1_1sp_type`i'.pdf,replace 
}

******************************************
************  Decompostiion  *************
******************************************
forvalues i = 1/4{
use "2.Data/dtafiles/Cont_rist.dta",clear
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

save "2.Data/dtafiles/sensitivity/Decomp.dta",replace

use "2.Data/dtafiles/sensitivity/Decomp.dta",clear
keep if stem == 1
rename comp2 comp2st
rename comp1 comp1st
save "2.Data/dtafiles/sensitivity/DecompStem.dta",replace

use "2.Data/dtafiles/sensitivity/Decomp.dta",clear
keep if stem == 0
rename comp2 comp2ns
rename comp1 comp1ns
save "2.Data/dtafiles/sensitivity/DecompNonStem.dta",replace

use "2.Data/dtafiles/sensitivity/Decomp.dta",clear
keep if stem == 2
rename comp2 comp2hl
rename comp1 comp1hl
save "2.Data/dtafiles/sensitivity/DecompNonHealth.dta",replace

merge 1:1 year using 2.Data/dtafiles/sensitivity/DecompStem.dta
drop _merge
merge 1:1 year using 2.Data/dtafiles/sensitivity/DecompNonStem.dta
* Figure
*comp2ns comp1ns comp2st comp1st
*専攻分布効果:Major mix effect (相対的なサイズ変化の寄与分)
*性別構成効果:Sex composition effect (専攻における専攻内の男女比率変化の寄与分)
graph bar comp1st comp1ns comp1hl comp2st comp2ns comp2hl , ///
over(year, label(angle(45))) stack ylabel(-35(5)10, grid glw(thin)) yline(0, lc(gs3)) ///
bar(4, col(gs4)) bar(5, col(ebblue)) bar(6, col(dkorange)) bar(1, col(gs11)) bar(2, col(eltblue)) bar(3, col(sandb)) ///
legend(label(1 "Major mix effect (STEM)") label(4 "Sex composition effect (STEM)") label(2 "Major mix effect (Non-STEM)") label(5 "Sex composition effect (Non-STEM)") label(3 "Major mix effect (Health)") label(6 "Sex composition effect (Health)")  symxsize(vlarge) size(small)) saving(3.Results/Decomp`i'.gph, replace)
graph export 3.Results/sensitivity/Decomp`i'.pdf, replace
keep year comp2hl comp1hl comp2st comp1st comp2ns comp1ns
}



