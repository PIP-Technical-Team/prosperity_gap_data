# SURVEY YEARS -----------------------------------------------------------------

## Povline = 2.15 ------------------------------
hc1 <- pipr::get_stats(
  povline = 2.15
)
setDT(hc1)
# hc1 <- 
#   hc1[, 
#          if (.N > 1) {
#            .SD[!reporting_level == "rural"] 
#          } else {
#            .SD
#          }, 
#          by = .(country_name, year, welfare_type)
#   ][, 
#     if (.N > 1) {
#       .SD[reporting_level == "national"] 
#     } else {
#       .SD
#     }, 
#     by = .(country_name, year, welfare_type)
#   ]
# hc1 <- 
#   hc1[, 
#          if (.N > 1) {
#            .SD[welfare_type == "income"] 
#          } else {
#            .SD
#          }, 
#          by = .(country_name, year, reporting_level)
#   ]
hc1 <- hc1[
  , 
  .(
    Country_code         = country_code, 
    Country_name         = country_name, 
    Survey_comparability = survey_comparability, 
    Year                 = year, 
    Headcount215         = headcount, 
    Reporting_level      = reporting_level, 
    Welfare_type         = welfare_type)
]

## Povline = 3.65 ------------------------------
hc2 <- pipr::get_stats(
  povline = 3.65
)
setDT(hc2)
# hc2 <- 
#   hc2[, 
#       if (.N > 1) {
#         .SD[!reporting_level == "rural"] 
#       } else {
#         .SD
#       }, 
#       by = .(country_name, year, welfare_type)
#   ][, 
#     if (.N > 1) {
#       .SD[reporting_level == "national"] 
#     } else {
#       .SD
#     }, 
#     by = .(country_name, year, welfare_type)
#   ]
# hc2 <- 
#   hc2[, 
#       if (.N > 1) {
#         .SD[welfare_type == "income"] 
#       } else {
#         .SD
#       }, 
#       by = .(country_name, year, reporting_level)
#   ]
hc2 <- hc2[
  , 
  .(
    Country_code         = country_code, 
    Country_name         = country_name, 
    Survey_comparability = survey_comparability, 
    Year                 = year, 
    Headcount365         = headcount, 
    Reporting_level      = reporting_level, 
    Welfare_type         = welfare_type)
]

## Povline = 6.85 ------------------------------
hc3 <- pipr::get_stats(
  povline = 6.85
)
setDT(hc3)
# hc3 <- 
#   hc3[, 
#       if (.N > 1) {
#         .SD[!reporting_level == "rural"] 
#       } else {
#         .SD
#       }, 
#       by = .(country_name, year, welfare_type)
#   ][, 
#     if (.N > 1) {
#       .SD[reporting_level == "national"] 
#     } else {
#       .SD
#     }, 
#     by = .(country_name, year, welfare_type)
#   ]
# hc3 <- 
#   hc3[, 
#       if (.N > 1) {
#         .SD[welfare_type == "income"] 
#       } else {
#         .SD
#       }, 
#       by = .(country_name, year, reporting_level)
#   ]
hc3 <- hc3[
  , 
  .(
    Country_code         = country_code, 
    Country_name         = country_name, 
    Survey_comparability = survey_comparability, 
    Year                 = year, 
    Headcount685         = headcount, 
    Reporting_level      = reporting_level, 
    Welfare_type         = welfare_type)
]
## Joyn ----------------------------------------
# hc <- joyn::merge(
#   x          = hc1, 
#   y          = hc2, 
#   yvars      = TRUE, 
#   by         = c("Country_code", "Year"), 
#   match_type = "1:1", 
#   keep       = "left",
#   reportvar = FALSE
# )
# # hc[
# #   , 
# #   report := NULL
# # ]
# hc <- joyn::merge(
#   x          = hc, 
#   y          = hc3, 
#   yvars      = T, 
#   by         = c("Country_code", "Year"), 
#   match_type = "1:1", 
#   keep       = "left",
#   reportvar = FALSE
# )
# hc[
#   , 
#   report := NULL
# ]

## Joyn ------------------------------

hc <- joyn::left_join(
  x          = hc1, 
  y          = hc2, 
  yvars      = TRUE, 
  by         = c("Country_code", "Year", "Welfare_type", "Reporting_level"), 
  relationship = "one-to-one", 
  reportvar = FALSE
)

hc <- joyn::left_join(
  x          = hc, 
  y          = hc3, 
  yvars      = TRUE, 
  by         = c("Country_code", "Year", "Welfare_type", "Reporting_level"), 
  relationship = "one-to-one", 
  reportvar = FALSE
)


## Save ------------------------------


# saveRDS(
#   object = hc, 
#   file   = here::here(
#     "data", 
#     "headcounts_survey.rds"
#   )
# )

fst::write_fst(
  x = hc,
  path = here::here("headcounts_survey.fst")
)


# LINEUP YEARS -----------------------------------------------------------------




## Povline = 2.15 ------------------------------
hc1_lineup <- pipr::get_stats(
  povline   = 2.15, 
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
    Country_code         = country_code, 
    Country_name         = country_name, 
    Survey_comparability = survey_comparability, 
    Year                 = year, 
    Headcount215         = headcount, 
    Reporting_level      = reporting_level, 
    Welfare_type         = welfare_type
  )
]

## Povline = 3.65 ------------------------------
hc2_lineup <- pipr::get_stats(
  povline   = 3.65, 
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
    Country_code         = country_code, 
    Country_name         = country_name, 
    Survey_comparability = survey_comparability, 
    Year                 = year, 
    Headcount365         = headcount, 
    Reporting_level      = reporting_level, 
    Welfare_type         = welfare_type
  )
]

## Povline = 6.85 ------------------------------
hc3_lineup <- pipr::get_stats(
  povline   = 6.85, 
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
    Country_code         = country_code, 
    Country_name         = country_name, 
    Survey_comparability = survey_comparability, 
    Year                 = year, 
    Headcount685         = headcount, 
    Reporting_level      = reporting_level, 
    Welfare_type         = welfare_type
  )
]
## Joyn ----------------------------------------
hc_lineup <- joyn::merge(
  x          = hc1_lineup, 
  y          = hc2_lineup, 
  yvars      = T, 
  by         = c("Country_code", "Year"), 
  match_type = "1:1", 
  keep       = "left",
  reportvar = FALSE
)
# hc_lineup[
#   , 
#   report := NULL
# ]
hc_lineup <- joyn::merge(
  x          = hc_lineup, 
  y          = hc3_lineup, 
  yvars      = T, 
  by         = c("Country_code", "Year"), 
  match_type = "1:1", 
  keep       = "left",
  reportvar = FALSE
)
# hc_lineup[
#   , 
#   report := NULL
# ]

## Save ------------------------------


# saveRDS(
#   object = hc_lineup, 
#   file   = here::here(
#     "data", 
#     "headcounts_lineup.rds"
#   )
# )


fst::write_fst(
  x = hc_lineup,
  path = here::here("headcounts_lineup.fst")
)












