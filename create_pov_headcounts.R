# SURVEY YEARS -----------------------------------------------------------------

## Povline = 2.15 ------------------------------
hc1 <- pipr::get_stats(
  povline = 2.15, 
  format = "rds" 
)
setDT(hc1)
hc1 <- 
  hc1[, 
         if (.N > 1) {
           .SD[!reporting_level == "rural"] 
         } else {
           .SD
         }, 
         by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
hc1 <- 
  hc1[, 
         if (.N > 1) {
           .SD[welfare_type == "income"] 
         } else {
           .SD
         }, 
         by = .(country_name, year, reporting_level)
  ]
hc1 <- hc1[
  , 
  .(
    Country_code = country_code, Country_name = country_name, Survey_comparability = survey_comparability, Year = year, Headcount215 = headcount
  )
]

## Povline = 3.65 ------------------------------
hc2 <- pipr::get_stats(
  povline = 3.65, 
  format = "rds" 
)
setDT(hc2)
hc2 <- 
  hc2[, 
      if (.N > 1) {
        .SD[!reporting_level == "rural"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
hc2 <- 
  hc2[, 
      if (.N > 1) {
        .SD[welfare_type == "income"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, reporting_level)
  ]
hc2 <- hc2[
  , 
  .(
    Country_code = country_code, Country_name = country_name, Survey_comparability = survey_comparability, Year = year, Headcount365 = headcount
  )
]

## Povline = 6.85 ------------------------------
hc3 <- pipr::get_stats(
  povline = 6.85, 
  format = "rds" 
)
setDT(hc3)
hc3 <- 
  hc3[, 
      if (.N > 1) {
        .SD[!reporting_level == "rural"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
hc3 <- 
  hc3[, 
      if (.N > 1) {
        .SD[welfare_type == "income"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, reporting_level)
  ]
hc3 <- hc3[
  , 
  .(
    Country_code = country_code, Country_name = country_name, Survey_comparability = survey_comparability, Year = year, Headcount685 = headcount
  )
]
## Joyn ----------------------------------------
hc <- joyn::merge(
  x          = hc1, 
  y          = hc2, 
  yvars      = T, 
  by         = c("Country_code", "Year"), 
  match_type = "1:1", 
  keep       = "left"
)
hc[
  , 
  report := NULL
]
hc <- joyn::merge(
  x          = hc, 
  y          = hc3, 
  yvars      = T, 
  by         = c("Country_code", "Year"), 
  match_type = "1:1", 
  keep       = "left"
)
hc[
  , 
  report := NULL
]

## Save ------------------------------


saveRDS(
  object = hc, 
  file   = here::here(
    "data", 
    "headcounts_survey.rds"
  )
)



# LINEUP YEARS -----------------------------------------------------------------




## Povline = 2.15 ------------------------------
hc1_lineup <- pipr::get_stats(
  povline   = 2.15, 
  format    = "rds", 
  fill_gaps = TRUE
)
setDT(hc1_lineup)
hc1_lineup <- 
  hc1_lineup[, 
      if (.N > 1) {
        .SD[!reporting_level == "rural"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
hc1_lineup <- 
  hc1_lineup[, 
      if (.N > 1) {
        .SD[welfare_type == "income"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, reporting_level)
  ]
hc1_lineup <- hc1_lineup[
  , 
  .(
    Country_code = country_code, Country_name = country_name, Survey_comparability = survey_comparability, Year = year, Headcount215 = headcount
  )
]

## Povline = 3.65 ------------------------------
hc2_lineup <- pipr::get_stats(
  povline   = 3.65, 
  format    = "rds" , 
  fill_gaps = TRUE
)
setDT(hc2_lineup)
hc2_lineup <- 
  hc2_lineup[, 
      if (.N > 1) {
        .SD[!reporting_level == "rural"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
hc2_lineup <- 
  hc2_lineup[, 
      if (.N > 1) {
        .SD[welfare_type == "income"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, reporting_level)
  ]
hc2_lineup <- hc2_lineup[
  , 
  .(
    Country_code = country_code, Country_name = country_name, Survey_comparability = survey_comparability, Year = year, Headcount365 = headcount
  )
]

## Povline = 6.85 ------------------------------
hc3_lineup <- pipr::get_stats(
  povline   = 6.85, 
  format    = "rds" , 
  fill_gaps = TRUE
)
setDT(hc3_lineup)
hc3_lineup <- 
  hc3_lineup[, 
      if (.N > 1) {
        .SD[!reporting_level == "rural"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, welfare_type)
  ][, 
    if (.N > 1) {
      .SD[reporting_level == "national"] 
    } else {
      .SD
    }, 
    by = .(country_name, year, welfare_type)
  ]
hc3_lineup <- 
  hc3_lineup[, 
      if (.N > 1) {
        .SD[welfare_type == "income"] 
      } else {
        .SD
      }, 
      by = .(country_name, year, reporting_level)
  ]
hc3_lineup <- hc3_lineup[
  , 
  .(
    Country_code = country_code, Country_name = country_name, Survey_comparability = survey_comparability, Year = year, Headcount685 = headcount
  )
]
## Joyn ----------------------------------------
hc_lineup <- joyn::merge(
  x          = hc1_lineup, 
  y          = hc2_lineup, 
  yvars      = T, 
  by         = c("Country_code", "Year"), 
  match_type = "1:1", 
  keep       = "left"
)
hc_lineup[
  , 
  report := NULL
]
hc_lineup <- joyn::merge(
  x          = hc_lineup, 
  y          = hc3_lineup, 
  yvars      = T, 
  by         = c("Country_code", "Year"), 
  match_type = "1:1", 
  keep       = "left"
)
hc_lineup[
  , 
  report := NULL
]

## Save ------------------------------


saveRDS(
  object = hc_lineup, 
  file   = here::here(
    "data", 
    "headcounts_lineup.rds"
  )
)















