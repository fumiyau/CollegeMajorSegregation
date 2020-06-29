******************************************
*******  For year 2008 to 2019  **********
******************************************

capture cd "/Users/fumiyau/Dropbox (Princeton)/09.CollegeMajor/CollegeMajorSegregation"
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 2008 & year < 2020
replace v1 = 1*(type-1) + v1 - (type-1)*3484
*save "2.Data/NameEdited/dfint2008-2019.dta",replace
forvalues i=0/11 {
drop if v1 == 1 + `i'*78 | v1 == 2 + `i'*78
forvalues j=3/78 {
replace v1 = `j' + `i'*78 - (`i'-1)*78 if v1 == `j' + `i'*78
}
}
replace v1 = v1 - 80
drop major
rename v1 major
*omit others in homeeconomics
drop if major == 49
replace major = major - 1 if major > 49
*merge 中等教育学校 and 特別支援教育課程 with others
recode major 61=62  56=62
replace major = major - 1 if major > 56
replace major = major - 1 if major > 59
sort type year major
save "2.Data/dtafiles/dfint2008-2019ed.dta",replace

******************************************
*******      For year 2007      **********
******************************************

import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year == 2007
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint2007.dta",replace
forvalues i=0/3 {
drop if v1 == 1 + `i'*77 | v1 == 2 + `i'*77
forvalues j=3/77 {
replace v1 = `j' + `i'*77 - (`i'-1)*77 if v1 == `j' + `i'*77
}
}
replace v1 = v1 - 79
drop major
rename v1 major
*merge 中等教育学校 and 特別支援教育課程 with others
recode major 61=62  56=62
sort year major
replace major = major - 1 if major > 56
replace major = major - 1 if major > 59
save "2.Data/dtafiles/dfint2007ed.dta",replace

******************************************
*******    For year 2003-2006   **********
******************************************
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 2003 & year < 2007
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint2003-2006.dta",replace
forvalues i=0/16 {
drop if v1 == 1 + `i'*76 | v1 == 2 + `i'*76
forvalues j=3/76 {
replace v1 = `j' + `i'*76 - (`i'-1)*76 if v1 == `j' + `i'*76
}
}
replace v1 = v1 - 78
drop major
rename v1 major
*merge 中等教育学校 with others
recode major 56=61
sort year major
replace major = major - 1 if major > 56
save "2.Data/dtafiles/dfint2003-2006ed.dta",replace

******************************************
*******    For year 1999-2002   **********
******************************************

*医学専門学群, 体育専門学群, 芸術専門学群をそれぞれその他に統合
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 1999 & year < 2003
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1999-2003.dta",replace
forvalues i=0/16 {
drop if v1 == 1 + `i'*79 | v1 == 2 + `i'*79
forvalues j=3/79 {
replace v1 = `j' + `i'*79 - (`i'-1)*79 if v1 == `j' + `i'*79
}
}
replace v1 = v1 - 81
drop major
rename v1 major
*merge 医学専門学群 with others
recode major 43=42
replace major = major - 1 if major > 43
*merge 体育専門学群 and 中等教育学校 with others
recode major 56=62 60=62
sort year major
replace major = major - 1 if major > 56
replace major = major - 1 if major > 59
*merge 芸術専門学群 with others
recode major 65=64
replace major = major - 1 if major > 64
save "2.Data/dtafiles/dfint1999-2002ed.dta",replace

******************************************
*******    For year 1992-1998   **********
******************************************

import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 1992 & year < 1999
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1992-1998.dta",replace
forvalues i=0/28 {
drop if v1 == 1 + `i'*78 | v1 == 2 + `i'*78
forvalues j=3/78 {
replace v1 = `j' + `i'*78 - (`i'-1)*78 if v1 == `j' + `i'*78
}
}
replace v1 = v1 - 80
drop major
rename v1 major
*merge 医学専門学群 with others
recode major 43=42
replace major = major - 1 if major > 43
*merge 体育専門学群 with others
recode major 59=61
sort year major
replace major = major - 1 if major > 59
*merge 芸術専門学群 with others
recode major 65=64
replace major = major - 1 if major > 64
save "2.Data/dtafiles/dfint1992-1998ed.dta",replace

******************************************
*******    For year 1988-1991   **********
******************************************

import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 1988 & year < 1992
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1988-1991.dta",replace
forvalues i=0/16 {
drop if v1 == 1 + `i'*77 | v1 == 2 + `i'*77
forvalues j=3/77 {
replace v1 = `j' + `i'*77 - (`i'-1)*77 if v1 == `j' + `i'*77
}
}
replace v1 = v1 - 79
drop major
rename v1 major
*merge 医学専門学群 with others
recode major 43=42
replace major = major - 1 if major > 43
*merge 体育専門学群 with others
recode major 59=61
sort year major
replace major = major - 1 if major > 59
*merge 芸術専門学群 with others
recode major 65=64
replace major = major - 1 if major > 64
save "2.Data/dtafiles/dfint1988-1991ed.dta",replace

******************************************
*******      For year 1987      **********
******************************************
*障害児教育課程はあとで対処

import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year == 1987

drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1987.dta",replace
forvalues i=0/4 {
drop if v1 == 1 + `i'*76 | v1 == 2 + `i'*76
forvalues j=3/76 {
replace v1 = `j' + `i'*76 - (`i'-1)*76 if v1 == `j' + `i'*76
}
}
replace v1 = v1 - 78
drop major
rename v1 major

save "2.Data/dtafiles/dfint1987ed.dta",replace

******************************************
*******    For year 1984-1986   **********
******************************************
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 1984 & year < 1987

drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1984-1986.dta",replace
forvalues i=0/12 {
drop if v1 == 1 + `i'*75 | v1 == 2 + `i'*75
forvalues j=3/75 {
replace v1 = `j' + `i'*75 - (`i'-1)*75 if v1 == `j' + `i'*75
}
}
replace v1 = v1 - 77
drop major
rename v1 major

save "2.Data/dtafiles/dfint1984-1986ed.dta",replace

******************************************
*******    For year 1979-1983   **********
******************************************
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 1979 & year < 1984

drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1979-1983.dta",replace
forvalues i=0/20 {
drop if v1 == 1 + `i'*78 | v1 == 2 + `i'*78
forvalues j=3/78 {
replace v1 = `j' + `i'*78 - (`i'-1)*78 if v1 == `j' + `i'*78
}
}
replace v1 = v1 - 80
drop major
rename v1 major

recode major 39=38 41=40 45=44
replace major = major-1 if major > 39
replace major = major-1 if major > 40
replace major = major-1 if major > 43
save "2.Data/dtafiles/dfint1979-1983ed.dta",replace

******************************************
*******      For year 1978      **********
******************************************
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year == 1978
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1978.dta",replace
forvalues i=0/4 {
drop if v1 == 1 + `i'*77 | v1 == 2 + `i'*77
forvalues j=3/77 {
replace v1 = `j' + `i'*77 - (`i'-1)*77 if v1 == `j' + `i'*77
}
}
replace v1 = v1 - 79
drop major
rename v1 major

recode major 39=38 41=40 45=44
replace major = major-1 if major > 39
replace major = major-1 if major > 40
replace major = major-1 if major > 43
save "2.Data/dtafiles/dfint1978ed.dta",replace

******************************************
*******    For year 1975-1977   **********
******************************************
import delimited "2.Data/NameEdited/df_all.csv", encoding(utf8) clear
keep if year >= 1975 & year < 1978
drop v1 
gen v1 = _n
*save "2.Data/NameEdited/dfint1975-1977.dta",replace
forvalues i=0/12 {
drop if v1 == 1 + `i'*76 | v1 == 2 + `i'*76
forvalues j=3/76 {
replace v1 = `j' + `i'*76 - (`i'-1)*76 if v1 == `j' + `i'*76
}
}
replace v1 = v1 - 78
drop major
rename v1 major

recode major 39=38 41=40 45=44
replace major = major-1 if major > 39
replace major = major-1 if major > 40
replace major = major-1 if major > 43
save "2.Data/dtafiles/dfint1975-1977ed.dta",replace

***data merge 1988-2019
use 2.Data/dtafiles/dfint1988-1991ed.dta,clear
append using 2.Data/dtafiles/dfint1992-1998ed.dta
append using 2.Data/dtafiles/dfint1999-2002ed.dta
append using 2.Data/dtafiles/dfint2003-2006ed.dta
append using 2.Data/dtafiles/dfint2007ed.dta
append using 2.Data/dtafiles/dfint2008-2019ed.dta

*drop 最後のその他
drop if major >=70
*障害児教育課程分を詰める (60 -> 59)
replace major = major - 1 if major > 59
save 2.Data/dtafiles/dfint1988-2019ed.dta,replace

***data merge 1975-1987
use 2.Data/dtafiles/dfint1987ed.dta, clear
append using 2.Data/dtafiles/dfint1984-1986ed.dta
append using 2.Data/dtafiles/dfint1979-1983ed.dta
append using 2.Data/dtafiles/dfint1978ed.dta
append using 2.Data/dtafiles/dfint1975-1977ed.dta

*merge 医学専門学群 with others
recode major 43=42
replace major = major - 1 if major > 43
*merge 体育専門学群 with others
recode major 59=61
sort year major
replace major = major - 1 if major > 59
*merge 芸術専門学群 with others
recode major 65=64
replace major = major - 1 if major > 64

*drop 最後のその他
drop if major >=69

save 2.Data/dtafiles/dfint1975-1987ed.dta,replace

******************************************
*************** Correction ***************
******************************************
use 2.Data/dtafiles/dfint1988-2019ed.dta,clear
append using 2.Data/dtafiles/dfint1975-1987ed.dta

****************************
replace men = 19707 if year == 1999 & type == 4 & major == 18

*Collapse
sort type year major
by type year major: gen summen=sum(men)
by type year major: gen sumwomen=sum(women)
by type year major: egen maxmen=max(summen)
by type year major: egen maxwomen=max(sumwomen)
by type year major: gen n=_n
keep if n==1
drop men women summen sumwomen n
rename maxmen men
rename maxwomen women

*Create Mit/Mt and  Fit/Ft
local sex "women men"
foreach x of local sex{
bysort type year: gen `x'sum=sum(`x')
bysort type year: egen `x'max=max(`x'sum)
}
drop mensum womensum
gen MitMt=men/menmax
gen FitFt=women/womenmax

*Create D index
sort type year major
bysort type year: gen scorex=100*(1/2)*(abs(MitMt-FitFt))
bysort type year: gen score=sum(scorex)

*Label define and save
gen majornl=major
label define typel 1"Total" 2"National" 3"Public" 4"Private"
label values type typel

*label define majorl 1"文学" 2"史学" 3"哲学" 4"人文科学・その他" 5"法学・政治学" 6"商学・経済学" 7"社会学" 8"社会科学・その他" 9"数学" 10"物理学" 11"化学" 12"生物学" 13"地学" 14"理学・その他" 15"機械工学" 16"電気通信工学" 17"土木建築工学" 18"応用化学" 19"応用理学" 20"原子力工学" 21"鉱山学" 22"金属工学" 23"繊維工学" 24"船舶工学" 25"航空工学" 26"経営工学" 27"工芸学" 28"工学・その他" 29"農学" 30"農芸化学" 31"農業工学" 32"農業経済学" 33"林学" 34"林産学" 35"獣医学畜産学" 36"水産学" 37"農学・その他" 38"医学" 39"歯学" 40"薬学" 41"看護学" 42"保険・その他" 43"商船学" 44"家政学" 45"食物学" 46"被服学" 47"住居学" 48"児童学" 49"教育学" 50"小学校課程" 51"中学校課程" 52"高等学校課程" 53"特別教科課程" 54"盲学校課程" 55"聾学校課程" 56"養護学校課程" 57"幼稚園課程" 58"体育学" 59"教育学・その他" 60"美術" 61"デザイン" 62"音楽" 63"芸術・その他" 64"教養学" 65"総合科学" 66"教養課程（文科）" 67"教養課程（理科）" 68"教養課程（その他）"
label define majorl 1"Literature" 2"History" 3"Philosophy" 4"Humanities: Others" 5"Law and politics" 6"Commerce and economics" 7"Sociology" 8"Social sciences: Others" 9"Mathematics" 10"Physics" 11"Chemistry" 12"Biology" 13"Geography" 14"Physical sciences: Others" 15"Mechanical engineering" 16"Telecommunications engineering" 17"Civil engineering" 18"Applied chemistry" 19"Applied science" 20"Nuclear engineering" 21"Mining engineering" 22"Metallurgical engineering" 23"Textile engineering" 24"Marine engineering" 25"Aeronautical engineering" 26"Engineering management" 27"Crafts" 28"Engineering: Others" 29"Agricultural sciences" 30"Agricultural chemistry" 31"Agricultural engineering" 32"Agricultural economics" 33"Forestry" 34"Forest products" 35"Veterinary medicine" 36"Fisheries science" 37"Agricultural sciences: Others" 38"Medicine" 39"Dentistry" 40"Pharmacy" 41"Nursing" 42"Health: Others" 43"Merchant marine" 44"Home economics" 45"Food science" 46"Clothing" 47"Housing" 48"Child" 49"Pedagogy" 50"Elementary school education" 51"Junior high school education" 52"High school education" 53"Specialized subjects education" 54"Visually impaired education" 55"Deaf education" 56"Special school education" 57"Kindergarten education" 58"Physical education" 59"Pedagogy: Others" 60"Fine arts" 61"Design" 62"Music" 63"Fine arts: Others" 64"Liberal arts" 65"General science" 66"Liberal arts (humanities and social sciene)" 67"Liberal arts (science)" 68"Others: Others"
label values major majorl
* 人文科学・その他：国際・地域系

save "2.Data/dtafiles/Cont.dta",replace
