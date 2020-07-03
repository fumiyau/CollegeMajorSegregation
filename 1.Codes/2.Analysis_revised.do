******************************************
************  Selecting fields  **********
******************************************
 *To select fields, we first chose those 14 largest fields that, taken together, 
 *account for half of all degrees given across the period. (England and Li 2006)
 
use "2.Data/dtafiles/Cont.dta",clear
 keep if type == 1
 sort year scorex /*Sort by score*/
bysort year: gen score_order=sum(scorex)
bysort type: egen scoremax_order=max(score_order)
gen score_rel_order = score_order/scoremax_order
keep if score_rel_order > 0.25
 *We chose 12 majors in 1975 that account for 75% of all degrees 
 *文学,法学・政治学,商学・経済学,社会学,機械工学,電気通信工学,土木建築工学,応用化学,薬学,教育学,小学校課程,音楽
 *labels: 1,5,6,7,15,16,17,18,40,49,50,62
 *Plus 4 majors that also accounts for 75% of all degrees in 2018 but not included in 1988 (ta major if year == 2018)
 *人文科学・その他, 工学・その他,看護学,教育学・その他 4, 28, 41, 59
 
******************************************
*************** Figures  *****************
******************************************

***Plot Overal trend***
use "2.Data/dtafiles/Cont.dta",clear
gen score1975=score if year  ==1975
bysort type: egen scoremax=max(score1975)
gen scorerel=(score/scoremax)*100 - 100
replace MitMt = MitMt * 100
replace FitFt = FitFt * 100
local opts_line "ms(oh)  recast(connected) ytitle("") xtitle("") xtick(1975(5)2020, grid) xlabel(1975(15)2020)"

**Overall : MitMt FitFt
keep if major == 1 | major == 4 | major == 5 | major == 6 | major == 7 | major == 15| major == 16 | major == 17 | ///
	major == 18 | major == 40 | major == 49 | major == 50 | major == 62 | ///
	major == 28 | major == 41 | major == 59
*Total

local opt_title1 "Fig_MF_total_top16"
local opt_title2 "Fig_MF_national_top16"
local opt_title3 "Fig_MF_public_top16"
local opt_title4 "Fig_MF_private_top16"
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
	graph export 3.Results/`opt_title`i''.pdf,replace 
}

**Overall : D index
*Major
use "2.Data/dtafiles/Cont.dta",clear

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
save "2.Data/dtafiles/ContMajor.dta",replace

*Minor
use "2.Data/dtafiles/Cont.dta",clear

gen score1975=score if year  ==1975
bysort type: egen scoremax=max(score1975)
gen scorerel=(score/scoremax)*100 - 100
replace MitMt = MitMt * 100
replace FitFt = FitFt * 100
keep if major == 68

keep type year score 

**Merge datasets
merge 1:1 year type using 2.Data/dtafiles/ContMajor.dta

label variable score "Detailed (68 fields)"
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
graph export 3.Results/Fig1_1.pdf,replace 

*(line scorem year , `opt_line2' by(type, note("")) ) ///

*(scatter scorem year if year == 1975 , mlabposition(5) `opt_scatter2' by(type, note("")) ) ///
*(scatter scorem year if year == 2019 , mlabposition(6) `opt_scatter2' by(type, note("")) ) ///

*Create D index by STEM
use "2.Data/dtafiles/Cont.dta",clear
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
save "2.Data/dtafiles/ContStem.dta",replace

use "2.Data/dtafiles/Cont.dta",clear
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
save "2.Data/dtafiles/ContNonStem.dta",replace

use "2.Data/dtafiles/Cont.dta",clear
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
save "2.Data/dtafiles/ContHealth.dta",replace

merge 1:1 year type using 2.Data/dtafiles/ContStem.dta
drop _merge
merge 1:1 year type using 2.Data/dtafiles/ContNonStem.dta

label variable scorens "Non-STEM"
label variable scorest "STEM"
label variable scorehl "Health"
twoway ///
(line scorens year , lc(black) lp(dash) by(type, note("")) ) ///
(line scorest year ,  lc(gs8) lp(solid) by(type, note("")) ) ///
(line scorehl year ,  lc(gs4) lp(dash_dot) by(type, note("")) ) ///
, xtick(1975(5)2020, grid) xlabel(1975(15)2020) ylabel(0(10)100) ///
scheme(plotplain) xtitle("Year") legend(row(1) pos(6))
graph export 3.Results/Fig1_1_stemx.pdf,replace 

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

***National detail***
use "2.Data/dtafiles/Cont.dta",clear
keep if type == 2
twoway ///
(line MitMt year , lc(black) lp(solid) by(major, note("")) ) ///
(line FitFt year ,  lc(gs8) lp(solid) by(major, note("")) ) ///
, xtick(1975(5)2020, grid) xlabel(1975(15)2020) ///
scheme(plotplain) xtitle("Year") legend(row(1) pos(6))
graph export 3.Results/Fig_MF_national.pdf,replace
***Public detail***
use "2.Data/dtafiles/Cont.dta",clear
keep if type == 3
twoway ///
(line MitMt year , lc(black) lp(solid) by(major, note("")) ) ///
(line FitFt year ,  lc(gs8) lp(solid) by(major, note("")) ) ///
, xtick(1975(5)2020, grid) xlabel(1975(15)2020) ///
scheme(plotplain) xtitle("Year") legend(row(1) pos(6))
graph export 3.Results/Fig_MF_public.pdf,replace
***Private detail***
use "2.Data/dtafiles/Cont.dta",clear
keep if type == 4
twoway ///
(line MitMt year , lc(black) lp(solid) by(major, note("")) ) ///
(line FitFt year ,  lc(gs8) lp(solid) by(major, note("")) ) ///
, xtick(1975(5)2020, grid) xlabel(1975(15)2020) ///
scheme(plotplain) xtitle("Year") legend(row(1) pos(6))
graph export 3.Results/Fig_MF_private.pdf,replace 
