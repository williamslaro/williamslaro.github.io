/*******************************************************************************************
Do file to create the aggregate data sets from the master (individual) MIP data set, 1939-2015
Please cite: Heffington, Colton, Brandon Park and Laron K. Williams.  Forthcoming.  "The 'Most Important Problem' Dataset (MIPD): A New Dataset on American Issue Importance"  Conflict Management and Peace Science

Please edit:
	1) cd "path" ==> The path for the working directory (the folder that contains the individual-level data set)
    2) "outfile" ==> The name you wish to call your new Stata data file.
	3) Make decisions on the options that are described in Steps 3-8.
		
Please note:
	1) If you are working with an older version of Stata, make sure that you have set a high enough memory (-set mem 1500m-).
	2) Run the entire do file at one time!  Since the do file relies on temporary data and variables, these are deleted as soon as Stata is done executing the command.
	3) Creating a monthly (or even quarterly) dataset takes some time, so be patient.

*******************************************************************************************/

macro drop _all

*** 1: Working directory
*cd ""

*** 2: Name the new data set
local outfile "Transformed Aggregate MIPD"

*** 3: Temporal domain: select one of the following and comment out the rest (default is SURVEY)
global t "survey"
*global t "month"
*global t "quarter"
*global t "year"

*** 4: Coding scheme: select one of the following and comment out the rest
global c "cap"
*global c "marpor"
*global c "singer"
*global c "cap marpor"
*global c "marpor singer"
*global c "cap marpor singer"

*** 5: Subgroup percentages: if no subgroup desired, leave all commented out
*global sub "party_id"
*global sub "ideology" 
*global sub "income_quartile" 
*global sub "male" 
*global sub "white"
*global sub "college"
*global sub "approve" 

*** 6: Denominator: include "missing/don't know" in denominator (default is to exclude).
*global d "include"

*** 7: Response selection: exclude closed-ended and short (default is to not exclude any surveys).
*global r "open+short"
*global r "open+noshort"

*** 8: Question wording: exclude which values of mipid: MIP only includes all traditional "most important problem" variations; MIP- employs a strict definition where the question has to ask "most important problem" (default is to include all variations).
*global s "MIP"
*global s "MIP-"

use "MIPD--Release 1.0.dta", clear

*******************************************************************************************
*** Keep only those variables that we need for the aggregate data set
*******************************************************************************************
keep studyid - interviewmethod mip_marpor1 mip_cap1 mip_singer1 $sub

*******************************************************************************************
*** Drop those surveys with inappropriate question wordings?
***
*** 3 = "facing your community today"
*** 17 = "most urgent problem"
*** 23 = "most important problem that you and your family face today"
*** 30 = "main problem"
*** 31 = "will be the most important problem...in the 21st Century"
*******************************************************************************************
if "$s" == "MIP" {
	drop if inlist(mipid, 3, 17, 23, 30, 31)
}
if "$s" == "MIP-" {
	drop if inlist(mipid, 3, 17, 23, 30, 31)
	drop if inlist(mipid, 6, 12, 20, 24, 99) 
}

*******************************************************************************************
*** Determine whether to keep closed-ended and/or short response options
***
*** "short" = pre-aggregated broad response options (8-10)
*** "openended" = 1 if response options are open-ended, 0 otherwise
*******************************************************************************************
if "$r" == "open+short" {
	drop if openended == 0 | short == 1
}
if "$r" == "open+noshort" {
	drop if openended == 0
}

***********************************************************************************
*** Generate the shell dataset
***********************************************************************************
preserve
	clear
	set obs 77
	egen year = fill(1939(1)2015)

	expand 12
	sort year
	egen month = fill(1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12 1(1)12)

	gen quarter = month
	recode quarter (1/3=1) (4/6=2) (7/9=3) (10/12=4)

	gen tsm = ym(year,month)
	gen tsq = yq(year,quarter)
	gen tsy = year
	
	sort tsm
	tempfile shell
	save `shell'
restore


***********************************************************************************
*** Create the weighted percentages across surveys
***********************************************************************************

* For those surveys without an available weight, fill in -weight- with 1.
recode weight (.=1) if weightavail == 0

* Generate the survey-specific percentages: no subgroup
qui if "$sub" == "" {
	foreach c in $c {
		tempvar mip_obs mip_den
		if "$d" == "include" {
			gen `mip_obs' = 1
		}
		else {
			gen `mip_obs' = 1 if !missing(mip_`c'1)
		}
		bys studyid: egen `mip_den' = sum(weight * `mip_obs')	

		levelsof mip_`c'1, local(mip)
		foreach m of local mip {
			tempvar mip`m' mip_num`m'
			gen `mip`m'' = cond(mip_`c'1 == ., ., cond(mip_`c'1 == `m', 1, 0, .))

			local v: label `c' `m'
			bys studyid: egen `mip_num`m'' = sum(weight * `mip`m'')
			gen `c'`m' = 100 * (`mip_num`m'' / `mip_den') 
			lab var `c'`m' "`c': % identifying `v' as MIP"
		}
	}
}

* Generate the survey-specific percentages: subgroup
qui else {
	levelsof $sub, local(S)
	local vnum = 1
	foreach v of local S {
		tempvar sub`vnum'_den
		gen `sub`vnum'_den' = cond($sub == ., ., cond($sub == `v', 1, 0))
		local sublab: label $sub `v'
		
		qui foreach c in $c {
			tempvar mip_obs mip_den
			if "$d" == "include" {
				gen `mip_obs' = 1
			}
			else {
				gen `mip_obs' = 1 if !missing(mip_`c'1)
			}
			bys studyid: egen `mip_den' = sum(weight * `mip_obs' * `sub`vnum'_den')		
		
			levelsof mip_`c'1, local(mip)
			foreach m of local mip {
				tempvar mip`m' mip_num`m'_`vnum'
				gen `mip`m'' = cond(mip_`c'1 == ., ., cond(mip_`c'1 == `m', 1, 0, .))

				local vlab: label `c' `m'
				bys studyid: egen `mip_num`m'_`vnum'' = sum(weight * `mip`m'' * `sub`vnum'_den')
				gen `c'`m'_`vnum' = 100 * (`mip_num`m'_`vnum'' / `mip_den') 
				lab var `c'`m'_`vnum' "`c': % of `sublab' identifying `vlab' as MIP"
			}		
		}
		local vnum = `vnum' + 1
	}
}

qui duplicates drop studyid, force


*******************************************************************************************
*** Aggregation weighting procedure
*******************************************************************************************
qui if "$t" == "survey" | "$t" == "" {
	drop weight mip_*
	cap drop id
	cap drop __*
	order studyid *_ts fw_* surve* sponso* overs* sampl* question mipid short open* weighta* interviewmethod
	sort fw_start
	save "`outfile'.dta", replace
}

qui if "$t" == "year" | "$t" == "quarter" | "$t" == "month" {
	gen tsm = mofd(fw_start)
	gen tsy = yofd(fw_start)
	gen tsq = qofd(fw_start)
	
	if "$t" == "year" {
		bys tsy: egen numsurveys_y = count(id)	
		lab var numsurveys_y "Number of surveys in that year"
	}
	if "$t" == "quarter" {
		bys tsq: egen numsurveys_q = count(id)
		lab var numsurveys_q "Number of surveys in that quarter"
	}
	if "$t" == "month" {
		bys tsm: egen numsurveys_m = count(id)
		lab var numsurveys_m "Number of surveys in that month"
	}
	
	local num: word count $c
	foreach n of numlist 1(1)`num' {
		local mip: word `n' of $c
		local vlist = "`vlist' `mip'*"
	}
	
	foreach v of varlist `vlist' {
		local l`v': var label `v'	
		bys tsm: egen `v'_perc = mean(`v')
		label var `v'_perc "`l`v''"	
		
		drop `v'
		rename `v'_perc `v'
	}
	duplicates drop tsm, force
	if "$t" == "year" {
		tempvar numnonmiss 
		bys tsy: egen `numnonmiss' = count(id)
		gen nummiss_y = 12 - `numnonmiss'
		lab var nummiss_y "Number of months with missing data in that year"
		preserve
			keep tsy num*
			duplicates drop tsy, force
			sort tsy
			tempfile no_y
			save `no_y', replace 
		restore		
		keep ts* `vlist'		
	}
	if "$t" == "quarter" {
		tempvar numnonmiss 
		bys tsq: egen `numnonmiss' = count(id)
		gen nummiss_q = 4 - `numnonmiss'
		lab var nummiss_q "Number of months with missing data in that quarter"
		preserve
			keep tsq num*
			duplicates drop tsq, force
			sort tsq
			tempfile no_q
			save `no_q', replace 
		restore				
		keep ts* `vlist'		
	}
	if "$t" == "month" {
		keep ts* `vlist' num*
	}
	sort tsm
	tempfile m
	save `m', replace 
	
	if "$t" == "year" {
		use `shell', clear
		merge tsm using `m'
		tab _merge
		drop _merge
		
		sort tsy
		merge tsy using `no_y'
		tab _merge
		drop _merge
		
		foreach v of varlist `vlist' {
			local l`v': var label `v'	
			bys tsy: egen `v'_perc = mean(`v')
			label var `v'_perc "`l`v'': annual"		
		}		
		duplicates drop tsy, force
		recode numsurveys_y (.=0)
		recode nummiss_y (.=12)		
		keep tsy *_perc num*
		format tsy %ty
		sort tsy
		save "`outfile'.dta", replace 		
	}
	
	if "$t" == "quarter" {
		use `shell', clear
		merge tsm using `m'
		tab _merge
		drop _merge
		
		sort tsq
		merge tsq using `no_q'
		tab _merge
		drop _merge
		
		foreach v of varlist `vlist' {
			local l`v': var label `v'	
			bys tsq: egen `v'_perc = mean(`v')
			label var `v'_perc "`l`v'': quarterly"		
		}		
		duplicates drop tsq, force
		recode numsurveys_q (.=0)
		recode nummiss_q (.=4)
		keep tsq *_perc num*
		format tsq %tq
		sort tsq
		save "`outfile'.dta", replace 		
	}
	
	if "$t" == "month" {
		use `shell', clear
		merge tsm using `m'
		tab _merge
		drop _merge	
		
		foreach v of varlist `vlist' {
			local l`v': var label `v'	
			label var `v' "`l`v'': monthly"
			rename `v' `v'_perc
		}		
		keep tsm *_perc num*
		recode numsurveys_m (.=0)
		format tsm %tm
		sort tsm
		save "`outfile'.dta", replace 
	}
}
	
		