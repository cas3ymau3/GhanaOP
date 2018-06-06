* FILEPATHS 
	** wave 3 
	global wave3 "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\GLSS\raw_data\GLSS3-1991\data\stata"
	** wave 4 
	global wave4 "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\GLSS\raw_data\GLSS4-1998\data\stata"
	** wave 5 
	global wave5_1 "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\GLSS\raw_data\GLSS5-2006\GHA_2006_GLSS_v01_M_v01_A_SHIP_Stata8" 
	global wave5_2 "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\GLSS\raw_data\GLSS5-2006\GLSS5-Data\stata\aggregates"
	** wave 6 
	global wave6 "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\GLSS\raw_data\GLSS6-2013\GLSS6_2012-2013\DATA\STATA\STATA\AGGREGATES"
	** intermediates 
	global intermediates "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\GLSS\processed_data\intermediates"
	** scripts 
	global scripts "C:\Users\cmaue\Dropbox\E-IPER\2017-18\GhanaOP\scripts"
	
* WAVE 3
{
	** create filelist (see `AggregateOutcomes_DataProcessing_Notes.txt') 
	local filelist "AGG2.DTA" 
	forval i=3/12 { 
		local filelist `filelist' "AGG`i'.DTA"
	}
	*
	forval j=1/40 { 
		local filelist `filelist' "SUBAGG`j'.DTA" 
	}
	* 
	local Nfiles : word count `filelist'
	di "`Nfiles'" 
	forval k=1/`Nfiles' {
		local file : word `k' of `filelist'
		di "`file'" 
	}
	*
	
	** merge all relevant `AGG' and `SUBAGG' files 
	cd $wave3 
	use AGG1.dta 
	forval i=1/`Nfiles' {
		local file : word `i' of `filelist'
		merge 1:1 clust nh using `file' 
		drop _merge
		drop if (missing(clust) | missing(nh))
	}
	* 
	
	** merge to `POV_GHA.DTA'
	merge 1:1 clust nh using POV_GH.DTA 
	drop _merge 
	drop if (missing(clust) | missing(nh)) 
	
	** save as `outcomes3' in `processed_data/intermediates'
	cd $intermediates 
	save outcomes3.dta, replace 
	
	** list variables 
	foreach var of varlist *{
	di "`var'" _col(20) "`: var l `var''"
	}
	*
	clear
}
*	
	
* WAVE 4 
{
	** create filelist (see `AggregateOutcomes_DataProcessing_Notes.txt') 
	local filelist "AGG2.DTA" 
	forval i=3/12 { 
		local filelist `filelist' "AGG`i'.DTA"
	}
	*
	forval j=1/40 { 
		local filelist `filelist' "SUBAGG`j'.DTA" 
	}
	* 
	local Nfiles : word count `filelist'
	di "`Nfiles'" 
	forval k=1/`Nfiles' {
		local file : word `k' of `filelist'
		di "`file'" 
	}
	*
	
	** merge all relevant `AGG' and `SUBAGG' files 
	cd $wave4 
	use AGG1.dta 
	forval i=1/`Nfiles' {
		local file : word `i' of `filelist'
		merge 1:1 clust nh using `file' 
		drop _merge
		drop if (missing(clust) | missing(nh))
	}
	* 
	
	** merge to `POV_GHA.DTA'
	merge 1:1 clust nh using POV_GH.DTA 
	drop _merge 
	drop if (missing(clust) | missing(nh)) 
	
	** save as `outcomes4' in `processed_data/intermediates'
	cd $intermediates
	save outcomes4.dta, replace 
	
	** list variables 
	foreach var of varlist *{
	di "`var'" _col(20) "`: var l `var''"
	}
	*
	clear 
}
*

* WAVE 5 
{
	** start with data in `Stata8' files 
	cd $wave5_1 
	use GHA_2005_H 
	merge 1:1 HID using GHA_2005_E 
	drop _merge 
	cd $intermediates
	save outcomes5.dta, replace 
	clear
	
	** create filelist (see `AggregateOutcomes_DataProcessing_Notes.txt') 
	local filelist "agg1.dta" 
	forval i=2/12 { 
		local filelist `filelist' "agg`i'.dta"
	}
	*
	forval j=1/21 { 
		local filelist `filelist' "exp`j'.dta" 
	}
	* 
	forval k=1/16 { 
		local filelist `filelist' "inc`k'.dta" 
	}
	* 
	local Nfiles : word count `filelist'
	di "`Nfiles'" 
	forval l=1/`Nfiles' {
		local file : word `l' of `filelist'
		di "`file'" 
	}
	*
	
	** merge all relevant `agg', `exp', and `inc' files 
	forval i=1/`Nfiles' {
	
		* open file 
		cd $wave5_2 
		local file : word `i' of `filelist'
		use `file' 
		di "WORKING ON FILE: `file'"
		
		* drop obs missing `clust' or `nh' 
		drop if (missing(clust) | missing(nh)) 
		
		* generate HID 
		gen HID = string(clust)+"/"+string(nh) 
		
		* check if there are duplicates 
		duplicates tag HID, g(dupes) 
		egen sumdupes = sum(dupes) 
		local ind1 = sumdupes[1]
		
		* collapse if not already at hh-level 
		if `ind1'>0 {
			* replace numeric vars with zero if missing 
			foreach var of varlist * {
				capture confirm numeric variable `var' 
					if !_rc {
					replace `var' = 0 if missing(`var') 
					} 
				}
				*
			* keep only `HID', `clust', `nh' and outcome var(s)
			cd $scripts 
			do GLSS_wave5_varkeep 
			keep HID clust nh $keeplist
			
			* save variable labels and collapse to hh-level 
			foreach v of var * {
			local l`v' : variable label `v'
			if `"`l`v''"' == "" {
			local l`v' "`v'"
			}
			capture confirm numeric variable `v' 
			if !_rc {
			local vallbl`v' : value label `v'
			} 
			}
			collapse (first) clust nh /// 
					 (sum) $keeplist /// 
					 , by(HID) 
			foreach v of var * {
			label var `v' "`l`v''"
			capture confirm numeric variable `v' 
			if !_rc {
			label val `v' `vallbl`v''
			} 
			}
			* 	
		}
		else { 
			drop dupes sumdupes 
		} 
		* 
		
		* merge to `outcomes5.dta'
		cd $intermediates 
		sleep 500
		save temp.dta, replace 
		clear 
		use outcomes5.dta 
		merge 1:1 HID using temp
		drop _merge 
		drop if (missing(clust) | missing(nh))
		sleep 100
		save outcomes5.dta, replace 
		clear
	}
	* 
	

	** merge to `pov_gha5.DTA'
	cd $wave5_2
	use pov_gh5.dta
	gen HID = string(clust)+"/"+string(nh) 
	cd $intermediates 
	merge 1:1 HID using outcomes5
	drop _merge 
	drop if (missing(clust) | missing(nh)) 
	save outcomes5.dta, replace 
	
	** list variables 
	foreach var of varlist *{
	di "`var'" _col(20) "`: var l `var''"
	}
	*
	clear 
}
*

* WAVE 6 
	
	** merge all relevant files (see `AggregateOutcomes_DataProcessing_Notes.txt') 
	cd $wave6 
	use GHA_2013_H 
	merge 1:1 HID using GHA_2013_H 
	drop _merge 
	merge 1:1 HID using GHA_2013_E
	drop _merge 
	merge 1:1 HID using GHA_2013_INCOME 
	drop _merge 
	merge 1:1 HID using INC_AGRI 
	drop _merge 
	merge 1:1 HID using POV_GHA_2013_GLSS6-updated
	drop _merge 
	cd $intermediates 
	save outcomes6.dta, replace 
	
	** list variables 
	foreach var of varlist *{
	di "`var'" _col(20) "`: var l `var''"
	}
	*
	clear 
