/**********************************************************************************
Do file to take the WKB governments data and create a government partisanship dataset (based on MARPOR data).

Please edit:
	1) cd "path" ==> The path for the working directory (the folder that contains the WKB data set)
    2) "infile" ==> This is the name of the Seki-Williams Government data; only change this if you changed the name!
	3) "t_outfile" ==> The name you wish to call your new Stata data file that contains the government data at a new unit of analysis (the default is annual).
	4) "p_outfile" ==> The name you wish to call your new Stata data file that contains government partisanship data; the default time period is annual.
	5) Choose the temporal dimension by commenting out all but one dimension with the "*".	Depending on your machine, it might take a few moments for the data set to be created.
		
Please note:
	1) If you are working with an older version of Stata, make sure that you have set a high enough memory, especially for daily data sets (the daily data set is quite large).
	2) Run the entire do file at one time!  Since the do file relies on temporary data and variables, these are deleted as soon as Stata is done executing the command.
	3) If you want to include supporting parties, then comment out lines 392-394.
	
This produces the following files:
	1) Governments dataset with a different unit of analysis: e.g., government/year (if you select year): you name this file in -t_outfile-
	2) Government partisanship dataset (named via -p_outfile-); NOTE that the unit of analysis will be government-year (or other time period), so there will be multiple observations per time period!
	
**********************************************************************************/


**********************************************************************************/
**********************************************************************************/
*** 1: Working directory
cd ""

*** 2: Name the new data set with the transformed unit of analysis
local t_outfile "Seki-Williams Transformed Governments--Version 2.0"

*** 3: Name the new government partisanship data set
local p_outfile "Seki-Williams Government Partisanship"

*** : Confirm the name of the original data set
local infile "Seki-Williams Governments--Version 2.0"

*** 3: Select one of the following; comment out the rest
*global t "daily"
*global t "monthly"
*global t "quarterly"
global t "annual"
**********************************************************************************/
**********************************************************************************/


clear
version 11
set mem 750m
set more off

use "`infile'.dta", clear

*** Generate the start date in Stata -ts- format
gen start_ts = mdy(startmonth,startday,startyear)

*** Drop an errant observation for Macedonia
drop if ccode == 343 & start_ts == .

*** Drop the later elections for Sri Lanka
drop if mapp == 0 & ccode == 780

*** Generate the end date in Stata -ts- format
sort ccode govtseq
bys ccode: gen end_ts = (start_ts[_n+1])
replace end_ts = date("31dec2014","DMY") if end_ts == . & inlist(ccode, 2, 20, 200, 205, 210, 211, 212, 220, 225, 230, 235, 255, 290, 305, 310, 316, 317, 325, 338, 344, 349, 350, 352, 355, 360, 366, 367, 368, 375, 380, 385, 390, 395, 666, 740, 900, 920)   
format start_ts %td
format end_ts %td

sort ccode govtseq
tempfile wkb
save `wkb', replace

*** Create the variable measuring the last government's end date
preserve
	sort ccode govtseq
	
	tempvar ts_start
	gen `ts_start' = mdy(startmonth,startday,startyear)
	bys ccode: gen ts_final = `ts_start'[_n+1]
	replace ts_final = end_ts if ts_final == .
	format ts_final %td
	drop if end_ts == .

	keep ccode ts_final
	lab var ts_final "Final government end date in TS format"
	sort ccode
	tempfile l
	save `l', replace
restore

sort ccode
merge ccode using `l', keep(ts_final)
drop _merge
sort ccode govtseq

gen startquarter = startmonth
recode startquarter (1/3=1) (4/6=2) (7/9=3) (10/12=4)

tempvar s_ts e_ts

if "$t" == "daily" {
	gen `s_ts' = mdy(startmonth,startday,startyear)
	local t
}

if "$t" == "monthly" {
	gen `s_ts' = ym(startyear,startmonth)
	local t mofd
}

if "$t" == "quarterly" {
	gen `s_ts' = yq(startyear,startquarter)
	local t qofd
}

else if "$t" == "annual" {
	gen `s_ts' = startyear
	local t year
}

bys ccode: gen `e_ts' = (`s_ts'[_n+1])
replace `e_ts' = `t'(end_ts) if `e_ts' == .
cap drop duration
gen duration = `e_ts' - `s_ts'
	
drop if `e_ts' == .
drop if duration == .
keep ccode `s_ts' `e_ts' duration govtseq

local obs = _N
quietly foreach i of numlist 1(1)`obs' {
	preserve
	keep if _n == `i'
	quietly sum duration if _n==1
	local dur = r(mean)+1
	quietly sum `s_ts' if _n==1
	local start = r(mean)
	local end = `start'+`dur'+1
	expand `dur'
	egen ts = seq(), f(`start') t(`end')
	tempfile t_`i'
	save `t_`i'', replace
	restore
}

use `t_1', clear
quietly foreach i of numlist 2(1)`obs' {
	append using `t_`i''
}
lab var ts "Date in $t TS format"
sort ccode ts

sort ccode govtseq
merge ccode govtseq using `wkb'
drop if _merge==2
drop _merge

if "$t" == "daily" {
	format ts %td
}

if "$t" == "monthly" {
	format ts %tm
}

if "$t" == "quarterly" {
	format ts %tq
}

else if "$t" == "annual" {
	format ts %ty
}

lab var start_ts "Government start date in daily time series format"
lab var end_ts "Government end date in daily time series format"
lab var duration "Government duration in $t format"

cap drop __*
order ccode govtseq ts start_ts end_ts duration
sort ccode ts govtseq

save "`t_outfile'.dta", replace

**********************************************************************
*** Generate the government parties dataset
**********************************************************************
use `wkb', clear

* First, create a temporary variable that counts the number of government parties
tempvar no_parties
gen `no_parties' = 10 if py10seat !=.
replace `no_parties' = 9 if py10seat == . & py9seat !=.
replace `no_parties' = 8 if py9seat == . & py8seat !=.
replace `no_parties' = 7 if py8seat == . & py7seat !=.
replace `no_parties' = 6 if py7seat == . & py6seat !=.
replace `no_parties' = 5 if py6seat == . & py5seat !=.
replace `no_parties' = 4 if py5seat == . & py4seat !=.
replace `no_parties' = 3 if py4seat == . & py3seat !=.
replace `no_parties' = 2 if py3seat == . & py2seat !=.
replace `no_parties' = 1 if py2seat == . & py1seat !=.
replace `no_parties' = 0 if gparties == 0
replace `no_parties' = 5 if ccode == 325 & inlist(govtseq, 57, 58)
tab2 gparties `no_parties'

list ccode govtseq if gparties ~= `no_parties'
list ccode govtseq if `no_parties' == .

tab `no_parties', miss

gen seat = py1seat
gen name = py1name
gen cab_perc = py1cab_perc
gen mpp = mpppy1


* 2 government parties
preserve
	keep if `no_parties' == 2
	expand 2
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui replace seat = py2seat if `id' == 2
	qui replace name = py2name if `id' == 2
	qui replace cab_perc = py2cab_perc if `id' == 2
	qui replace mpp = mpppy2 if `id' == 2
	
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np2
	save `np2', replace
restore

* 3 government parties
preserve
	keep if `no_parties' == 3
	expand 3
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2 3 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np3
	save `np3', replace
restore

* 4 government parties
preserve
	keep if `no_parties' == 4
	expand 4
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)4 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np4
	save `np4', replace
restore

* 5 government parties
preserve
	keep if `no_parties' == 5
	expand 5
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)5 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np5
	save `np5', replace
restore

* 6 government parties
preserve
	keep if `no_parties' == 6
	expand 6
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)6 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np6
	save `np6', replace
restore

* 7 government parties
preserve
	keep if `no_parties' == 7
	expand 7
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)7 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np7
	save `np7', replace
restore

* 8 government parties
preserve
	keep if `no_parties' == 8
	expand 8
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)8 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np8
	save `np8', replace
restore

* 9 government parties
preserve
	keep if `no_parties' == 9
	expand 9
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)9 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np9
	save `np9', replace
restore

* 10 government parties
preserve
	keep if `no_parties' == 10
	expand 10
	sort ccode govtseq
	tempvar id
	bys ccode govtseq: gen `id' = _n
	qui foreach i of numlist 2(1)10 {
		replace seat = py`i'seat if `id' == `i'
		replace name = py`i'name if `id' == `i'
		replace cab_perc = py`i'cab_perc if `id' == `i'
		replace mpp = mpppy`i' if `id' == `i'
	}
	drop py1name - mpppy10
	order country ccode govtseq name seat mpp

	tempfile np10
	save `np10', replace
restore

keep if `no_parties' == 1

qui foreach i of numlist 2(1)10 {
	append using `np`i''
}

drop py1name - mpppy10

*** 2: Include supporting parties?  If so, comment out these lines:
tempvar supp
gen `supp' = strpos(name, "[")
drop if `supp' != 0

rename mpp party
sort ccode govtseq party
order country ccode govtseq party name seat cab_perc
cap drop __*

tempfile gp
save `gp', replace


**********************************************************************
*** Generate the government partisanship dataset
**********************************************************************
use `gp', clear

* Drop all those countries that are not in the MARPOR data:
drop if inlist(ccode, 51, 110, 315, 560, 565, 571, 750, 770, 771)  

*** Change some of the previous election dates so that they are consistent with the manifesto data
recode peday (5=7) if ccode == 2 & peyear == 1946
recode peyear (1946=1944) if ccode == 2
recode peday (7=2) if ccode == 2 & peyear == 1950
recode peyear (1950=1948) if ccode == 2
recode peday (2=4) if ccode == 2 & peyear == 1954
recode peyear (1954=1952) if ccode == 2
recode peday (4=6) if ccode == 2 & peyear == 1958
recode peyear (1958=1956) if ccode == 2
recode peday (6=8) if ccode == 2 & peyear == 1962
recode peyear (1962=1960) if ccode == 2
recode peday (8=3) if ccode == 2 & peyear == 1966
recode peyear (1966=1964) if ccode == 2
recode peday (3=5) if ccode == 2 & peyear == 1970
recode peyear (1970=1968) if ccode == 2
recode peday (4=7) if ccode == 2 & peyear == 1974
recode peyear (1974=1972) if ccode == 2
recode peday (7=2) if ccode == 2 & peyear == 1978
recode peyear (1978=1976) if ccode == 2
recode peday (2=4) if ccode == 2 & peyear == 1982
recode peyear (1982=1980) if ccode == 2
recode peday (4=6) if ccode == 2 & peyear == 1986
recode peyear (1986=1984) if ccode == 2
recode peday (6=8) if ccode == 2 & peyear == 1990
recode peyear (1990=1988) if ccode == 2
recode peday (8=3) if ccode == 2 & peyear == 1994
recode peyear (1994=1992) if ccode == 2
recode peday (3=5) if ccode == 2 & peyear == 1998
recode peyear (1998=1996) if ccode == 2
recode peday (5=7) if ccode == 2 & peyear == 2002
recode peyear (2002=2000) if ccode == 2
recode peday (7=2) if ccode == 2 & peyear == 2006
recode peyear (2006=2004) if ccode == 2
recode peday (2=4) if ccode == 2 & peyear == 2010
recode peyear (2010=2008) if ccode == 2

recode peday (21=23) if ccode == 20 & peyear == 2006
recode peday (19=18) if ccode == 211 & peyear == 2003
recode peday (19=10) if ccode == 395 & peyear == 2003
recode peday (9=8) if ccode == 385 & peyear == 1985

gen past_ts = mdy(pemonth,peday,peyear)
format past_ts %td

*** Merge in the MARPOR data (2016a update)
preserve
	use "MPDataset_MPDS2016a.dta", clear

	gen ccode = country 
	recode ccode (11 = 380) (12 = 385) (13 = 390) (14 = 375) (15 = 395) (21 = 211) (22 = 210) (23 = 212) (31 = 220) (32 = 325) (33 = 230) (34 = 350) (35 = 235) (41 = 255) (42 = 305) (43 = 225) (51 = 200) (53 = 205) (54 = 338) (55 = 352) (61 = 2) (62 = 20) (63 = 900) (64 = 920) (71 = 740) (72 = 666) (73 = 780) (74 = 640) (75 = 339) (76 = 371) (77 = 373) (78 = 370) (79 = 346) (80 = 355) (81 = 344) (82 = 316) (83 = 366) (84 = 372) (85 = 265) (86 = 310) (87 = 367) (88 = 368) (89 = 343) (90 = 359) (91 = 341) (92 = 290) (93 = 360) (94 = 365) (96 = 317) (97 = 349) (98 = 369) (113 = 730) (171 = 70)

	sort country edate party 
	order countryname country ccode edate party rile

	gen past_ts = edate
	format past_ts %td

	*** Correct some of the previous election dates so that they are consistent with the SW data.
	replace past_ts = date("18may1954","DMY") if past_ts == date("18apr1954","DMY") & ccode == 205
	replace past_ts = date("18jun1969","DMY") if past_ts == date("16jun1969","DMY") & ccode == 205
	replace past_ts = date("26jun1949","DMY") if past_ts == date("29jun1949","DMY") & ccode == 211 
	replace past_ts = date("24mar1990","DMY") if past_ts == date("25mar1990","DMY") & ccode == 310
	replace past_ts = date("05jun1992","DMY") if past_ts == date("06jun1992","DMY") & ccode == 316
	replace past_ts = date("02jun2006","DMY") if past_ts == date("03jun2006","DMY") & ccode == 316
	replace past_ts = date("28may2010","DMY") if past_ts == date("29may2010","DMY") & ccode == 316
	replace past_ts = date("25oct2013","DMY") if past_ts == date("26oct2013","DMY") & ccode == 316
	replace past_ts = date("05jun1992","DMY") if past_ts == date("06jun1992","DMY") & ccode == 317
	replace past_ts = date("25sep1998","DMY") if past_ts == date("26sep1998","DMY") & ccode == 317	
	replace past_ts = date("05apr1992","DMY") if past_ts == date("06apr1992","DMY") & ccode == 325
	replace past_ts = date("27mar1994","DMY") if past_ts == date("28mar1994","DMY") & ccode == 325	
	replace past_ts = date("09apr2006","DMY") if past_ts == date("10apr2006","DMY") & ccode == 325	
	replace past_ts = date("17jun2001","DMY") if past_ts == date("18jun2001","DMY") & ccode == 355
	replace past_ts = date("16sep1956","DMY") if past_ts == date("26sep1956","DMY") & ccode == 380 
	replace past_ts = date("20sep1998","DMY") if past_ts == date("21sep1998","DMY") & ccode == 380
	replace past_ts = date("13sep1981","DMY") if past_ts == date("14sep1981","DMY") & ccode == 385
	replace past_ts = date("10sep1989","DMY") if past_ts == date("11sep1989","DMY") & ccode == 385
	replace past_ts = date("12sep1993","DMY") if past_ts == date("13sep1993","DMY") & ccode == 385
	replace past_ts = date("15sep1997","DMY") if past_ts == date("16sep1997","DMY") & ccode == 385
	replace past_ts = date("03nov1959","DMY") if past_ts == date("03jul1959","DMY") & ccode == 666
	replace past_ts = date("01nov1965","DMY") if past_ts == date("02nov1965","DMY") & ccode == 666	
	
	sort ccode past_ts party
	tempfile marpor
	save `marpor', replace
restore

sort ccode past_ts party
merge ccode past_ts party using `marpor'
drop if _merge == 2
drop _merge

sort ccode govtseq party

tempvar nonmiss denom perc_gp_nonmiss
gen `nonmiss' = cond(!missing(rile), 1, 0)
bys ccode govtseq: egen `denom' = sum(seat * `nonmiss')
gen `perc_gp_nonmiss' = (seat * `nonmiss') / `denom'

tempvar nump denomp 
bys ccode govtseq: egen `nump' = total((cond(rile == .), 1, 0) * seat)
bys ccode govtseq: egen `denomp' = total(seat)
gen percpartmiss = 100 * (`nump' / `denomp')
recode percpartmiss (.=100)
lab var percpartmiss "% of government seats with missing MARPOR data"

*** These composite variables have been used in other studies to measure free-market economic position (ecopos: Tavits 2007 AJPS), total economic emphasis (econ4: Williams, Seki and Whitten 2016 PSRM) and hawk score (hawk: Whitten and Williams 2011 AJPS).
gen ecopos = (per401 + per402 + per407 + per414) - (per403 + per404 + per405 + per406 + per412)
egen econ4 = rowtotal(per401 - per416)
gen hawk = (per104 - per105 - per106)

local V rile planeco markeco welfare intpeace ecopos econ4 hawk per101 per102 per103 per104 per105 per106 per107 per108 per109 per110 per201 per202 per203 per204 per301 per302 per303 per304 per305 per401 per402 per403 per404 per405 per406 per407 per408 per409 per410 per411 per412 per413 per414 per415 per416 per501 per502 per503 per504 per505 per506 per507 per601 per602 per603 per604 per605 per606 per607 per608 per701 per702 per703 per704 per705 per706

qui foreach v of local V {
	tempvar `v'
	gen ``v'' = `v' if mpp_pm == party
	bys ccode govtseq: egen pm_`v' = sum(``v''), missing
	
	tempvar perc_`v'
	gen `perc_`v'' = `perc_gp_nonmiss' * `v'
	bys ccode govtseq: egen govt_`v' = sum(`perc_`v''), missing
	
	lab var pm_`v' "PM's `v' score for that government (govtseq)"
	lab var govt_`v' "Government's weighted `v' score (only includes available MARPOR data)"
}

*** Save this as a temporary file that we can eventually merge into the data sets with different time periods:
keep ccode govtseq govt_* pm_* percpartmiss
duplicates drop ccode govtseq, force

sort ccode govtseq
tempfile p
save `p', replace

******************************************************************************
*** Make sure that you have created the Governments data set in your preferred time dimension!
******************************************************************************
use "`t_outfile'.dta", clear

* Drop all those countries that are not in the MARPOR data:
drop if inlist(ccode, 51, 110, 315, 560, 565, 571, 750, 770, 771)  

sort ccode govtseq
merge ccode govtseq using `p'
drop if _merge == 2
drop _merge

*** Drop the "duration" values for the last government in each country.
replace duration = . if end_ts == date("31dec2014","DMY")

lab def ccode 2 "USA" 20 "Canada" 51 "Jamaica" 52 "Trinidad & Tobago" 70 "Mexico" 95 "Panama" 110 "Guyana" 200 "Great Britain" 205 "Ireland" 210 "Netherlands" 211 "Belgium" 212 "Luxembourg" 220 "France" 225 "Switzerland" 230 "Spain" 235 "Portugal" 255 "Germany" 265 "German Democratic Republic" 290 "Poland" 305 "Austria" 310 "Hungary" 315 "Czechoslovakia" 316 "Czech Republic" 317 "Slovakia" 325 "Italy" 338 "Malta" 339 "Albania" 341 "Montenegro" 343 "Macedonia" 344 "Croatia" 346 "Bosnia-Herzegovina" 349 "Slovenia" 350 "Greece" 352 "Cyprus" 355 "Bulgaria" 359 "Moldova" 360 "Romania" 365 "Russia" 366 "Estonia" 367 "Latvia" 368 "Lithuania" 369 "Ukraine" 370 "Belarus" 371 "Armenia" 372 "Georgia" 373 "Azerbaijan" 375 "Finland" 380 "Sweden" 385 "Norway" 390 "Denmark" 395 "Iceland" 560 "South Africa" 565 "Namibia" 571 "Botswana" 640 "Turkey" 666 "Israel" 730 "Korea" 740 "Japan" 750 "India" 770 "Pakistan" 771 "Bangladesh" 780 "Sri Lanka" 900 "Australia" 920 "New Zealand"
lab val ccode ccode

order ccode ts govtseq start_ts end_ts 
sort ccode ts govtseq
compress

save "`p_outfile'.dta", replace

