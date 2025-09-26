********************************************************************************
/*
Country-level and regional prosperity gap measure for PG Dashboard.
June 6, 2025
*/
********************************************************************************


*********************************************************************************
*** Survey PG estimates ***
*********************************************************************************

// list of surveys in pip 
use "${bindatainput}pip_survey_${vintage}.dta", clear 
drop if reporting_level!="national" & inlist(country_code,"CHN")
keep country_code year welfare_time reporting_level welfare_type mean
duplicates drop 
decode welfare_type, gen(wt)
drop welfare_type
ren wt welfare_type
ren welfare_time survey_year 
ren reporting_level area 
tempfile allsurveys 
save 	`allsurveys'


// PG estiamtes submitted to Andres
use "${outputpath}pg_svy_${vintage}.dta", clear
replace welfare_type = "consumption" if welfare_type == "CON"
replace welfare_type = "income" 	 if welfare_type == "INC"

merge 1:1 country_code survey_year area welfare_type using `allsurveys', keep(3) nogen

gen i = pg * mean/$ps

ren area reporting_level

keep  country_code year reporting_level welfare_type pg i mean
order country_code year reporting_level welfare_type pg i mean

label var country_code 		"country/economy code"
label var year 				"year"
label var reporting_level 	"reporting data level: rural, urban, or national coverage"
label var welfare_type 		"income or consumption used in survey"
label var pg 				"Prosperity Gap, z=$ps/day"
label var i 				"Inequality, z=ybar"
label var mean 				"Mean, $/day $ppp PPPs"

export excel 	 	"${outputpath}PIPinput_PGdash_${vintage}.xlsx",  firstrow(varlabels) replace
import excel 	 	"${outputpath}PIPinput_PGdash_${vintage}.xlsx", clear
export delimited 	"${outputpath}PIPinput_survey_PGdash_${vintage}.csv", replace novarnames
cap erase 			"${outputpath}PIPinput_PGdash_${vintage}.xlsx"


*********************************************************************************
*** Lineup PG estimates ***
*********************************************************************************

// Country data used for 3PR
use "${outputpath}pg_lnp_${vintage}.dta", clear

keep  country_code year reporting_level welfare_type pg i mean
order country_code year reporting_level welfare_type pg i mean

label var country_code 		"country/economy code"
label var year 				"year"
label var reporting_level 	"reporting data level: rural, urban, or national coverage"
label var welfare_type 		"income or consumption used in survey"
label var pg 				"Prosperity Gap, z=$ps/day"
label var i 				"Inequality, z=ybar"
label var mean 				"Mean, $/day $ppp PPPs"

export excel 	 	"${outputpath}PIPinput_PGdash_${vintage}.xlsx",  firstrow(varlabels) replace
import excel 	 	"${outputpath}PIPinput_PGdash_${vintage}.xlsx", clear
export delimited 	"${outputpath}PIPinput_lineup_PGdash_${vintage}.csv", replace novarnames
cap erase 			"${outputpath}PIPinput_PGdash_${vintage}.xlsx"


*********************************************************************************
*** Aggregated PG estimates ***
*********************************************************************************

use "${outputpath}pg_lnp_${vintage}.dta", clear

preserve 																		// world pg
	collapse (mean) pg mean (rawsum) pop [w=pop], by(year)
	gen region_code = "WLD"
	tempfile world 
	save 	`world'
restore

collapse (mean) pg mean (rawsum) pop [w=pop], by(year region_code)

append using `world'
ren pop pop_reg

merge m:1 year using `world', keep(3) keepusing(pop pg)

gen pg_contribution = pg * pop_reg/pop

gen i = pg * mean/$ps

keep  region_code year pg pg_contribution i mean
order region_code year pg pg_contribution i mean

label var region_code 		"PIP region"
label var year 				"year"
label var pg 				"Prosperity Gap, z=$ps/day"
label var pg_contribution 	"Prosperity Gap regional contribution to global, z=$ps/day"
label var i 				"Inequality, z=ybar"
label var mean 				"Mean, $/day $ppp PPPs"

export excel 	 	"${outputpath}PIPinput_PGdash_${vintage}.xlsx",  firstrow(varlabels) replace
import excel 	 	"${outputpath}PIPinput_PGdash_${vintage}.xlsx", clear
export delimited 	"${outputpath}PIPinput_region_PGdash_${vintage}.csv", replace novarnames
cap erase 			"${outputpath}PIPinput_PGdash_${vintage}.xlsx"

*********************************************************************************
