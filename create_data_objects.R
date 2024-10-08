# Country data -----------------------------------------------------------------
# dt_country <- fread(
#   here(
#     "data", 
#     "PIPinput_survey_20231016.csv"
#   )
# )
library(data.table)
library(collapse)
library(here)

dt_country <- fread(
  here("PIPinput_survey.csv"))

setnames(
  dt_country, 
  old = dt_country |> 
    colnames(), 
  new = c(
    "Country_code", 
    "Year", 
    "Reporting_level", 
    "Welfare_type", 
    #"Population", 
    "PG", 
    "Inequality", 
    "Mean"
  )
)
# dt_country <- dt_country |> qDT() 
# dt_country[, .N, by = c("Country_code", "Year")][N>1,]
# dt_country[Country_code == "ALB" & Year == 2016]
# Region data ------------------------------------------------------------------
dt_region <- fread(
  here::here(
    "PIPinput_region.csv"
  )
)
setnames(
  dt_region, 
  old = dt_region |> 
    colnames(), 
  new = c(
    "Region_code", 
    "Year", 
    "PG", 
    "PG_decomp",
    "Inequality",
    "Mean"
  )
)
## Country lineups -------------------------------------------------------------
dt_lineup <- fread(
  here::here(
    "PIPinput_lineup.csv"
  )
)
setnames(
  dt_lineup, 
  old = dt_lineup |> 
    colnames(), 
  new = c(
    "Country_code", 
    "Year", 
    "Reporting_level", 
    "Welfare_type", 
    #"Population",
    "PG", 
    "Inequality",
    "Mean"
    #"Mean_b40"
  )
)
## PIP data --------------------------------------------------------------------
dt_pip <- pipr::get_stats() |> 
  qDT()
dt_country <- dt_country |> 
  joyn::left_join(y = dt_pip |> 
                    fselect(Year = year, 
                            Country_code = country_code, 
                            Survey_comparability = survey_comparability, 
                            Welfare_type = welfare_type, 
                            Reporting_level = reporting_level), 
                  reportvar = FALSE, 
                  by = c("Country_code", 
                         "Year", 
                         "Welfare_type", 
                         "Reporting_level"), 
                  relationship = "one-to-one")
# Exclude double rows
# dt_pip <- 
#   dt_pip[, 
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
# dt_pip <- 
#   dt_pip[, 
#          if (.N > 1) {
#            .SD[welfare_type == "income"] 
#          } else {
#            .SD
#          }, 
#          by = .(country_name, year, reporting_level)
#   ]
# Data Objects -----------------------------------------------------------------
countries_lookup <- dt_pip[
  ,
  .(country_name, region_name, country_code, region_code),
  by = c("country_name", "country_code", "region_name", "region_code")
][, 1:4]
setnames(
  countries_lookup, 
  old = c("country_name", "country_code", "region_name", "region_code"), 
  new = c("Country_name", "Country_code", "Region_name", "Region_code")
)

survey_lookup <- dt_pip[
  , 
  .(Year = year, 
    Country_code = country_code, 
    Survey_comparability = survey_comparability, 
    Welfare_type = welfare_type, 
    Reporting_level = reporting_level)
]

# Add country and region names -------------------------------------------------
dt_country <- joyn::merge(
  x          = dt_country, 
  y          = countries_lookup, 
  by         = c("Country_code"),
  keep       = "left",
  yvars      = TRUE,
  reportvar  = FALSE
)
# 
# dt_country <- dt_country |> 
#   fmutate(Survey_comparability = paste0(Country_code, Reporting_level, Welfare_type))

#------------------------------------------------------
dt_lineup <- joyn::joyn(
  x          = dt_lineup, 
  y          = countries_lookup, 
  by         = c("Country_code"),
  keep       = "left",
  match_type = "m:1",
  yvars      = TRUE, 
  reportvar  = FALSE)
# dt_lineup[
#   , 
#   report := NULL
# ]
dt_lineup <- joyn::joyn(
  x          = dt_lineup, 
  y          = survey_lookup, 
  by         = c("Country_code", "Year", "Welfare_type", "Reporting_level"), 
  keep       = "left",
  match_type = "m:1",
  yvars      = TRUE, 
  reportvar  = FALSE)
# dt_lineup[
#   , 
#   report := NULL
# ]
# dt_lineup[
#   ,
#   Survey_comparability := 1L
# ]

# dt_region <- joyn::merge(
#   x          = dt_region, 
#   y          = unique(countries_lookup[, .(Region_name, Region_code)]), 
#   by         = c("Region_code"), 
#   keep       = "left",
#   yvars      = TRUE,
#   reportvar  = FALSE
# )

dt_region <- joyn::left_join(
  x          = dt_region,
  y          = unique(countries_lookup[, .(Region_name, Region_code)]),
  by         = c("Region_code"),
  relationship = "many-to-one",
  y_vars_to_keep      = TRUE,
  reportvar  = FALSE
)





# dt_region[
#   , 
#   report := NULL
# ]
wld <- dt_region[
  Region_code == "WLD"
]
dt_region <- dt_region[
  !Region_code == "WLD"
]
# Add global numbers -----------------------------------------------------------
global_summary <- dt_region[
  , 
  .(
    PG = sum(PG),
    PG_decom = sum(PG_decomp), 
    Inequality = sum(Inequality),
    Mean = sum(Mean)
  ), 
  by = Year
]
global_region <- data.table(
  Region_code = "Global",
  Year        = global_summary$Year,
  PG          = global_summary$PG,
  PG_decomp   = global_summary$PG_decom,
  Inequality  = global_summary$Inequality,
  Mean        = global_summary$Mean, 
  Region_name = "Global"
)

dt_region <- rbindlist(
  list(
    dt_region, 
    global_region
  )
)
dt_region <- dt_region[
  , .(
    Region_code, 
    Year, 
    PG, 
    PG_decomp, 
    Inequality, 
    Mean,
    Region_name
  )
]
setorder(
  dt_region, Year
)
dt_region[
  Region_name == "Global", 
  PG := PG_decomp
]
dt_region[
  Region_name == "Global", 
  Inequality := wld$Inequality
]
dt_region[
  Region_name == "Global", 
  Mean := wld$Mean
]
# Income groups ----------------------------------------------------------------
dt_class <- haven::read_dta(
  here::here("CLASS.dta")
)
dt_class <- dt_class |> 
  as.data.table() |> 
  unique(
    by = c("economy", "incgroup_current")
  )
## Clean country names
dt_class[
  38, 
  economy := "Cote d'Ivoire"
]
dt_class[
  179, 
  economy := "Sao Tome and Principe"
]
dt_class[
  198, 
  economy := "Turkiye"
]
# dt_country <- joyn::merge(
#   x = dt_country, 
#   y = dt_class, 
#   by = c("Country_name = economy"), 
#   keep = "left", 
#   yvars = "incgroup_current"
# )
dt_country <- joyn::left_join(
  x = dt_country, 
  y = dt_class, 
  by = c("Country_name = economy"), 
  reportvar = FALSE,
  relationship = "many-to-one",
  yvars = "incgroup_current"
)
# dt_country[
#   , 
#   report := NULL
# ]

# Growth -----------------------------------------------------------------------
## Logs for growth rates
dt_region[, `:=`(
  PG_log         = log(PG),              
  PG_decomp_log  = log(PG_decomp),
  Inequality_log = log(Inequality), 
  Mean_log       = log(Mean)        
)]
dt_country[, `:=`(
  PG_log         = log(PG),              
  Inequality_log = log(Inequality), 
  Mean_log       = log(Mean)
  #Mean_b40_log   = log(Mean_b40)
)]
dt_lineup[, `:=`(
  PG_log         = log(PG),              
  Inequality_log = log(Inequality), 
  Mean_log       = log(Mean)
  #Mean_b40_log   = log(Mean_b40)
)]
## Growth rates
dt_region[
  , `:=` (
    PG_growth          = PG_log - shift(PG_log, type = 'lag', n = 1), 
    PG_decomp_growth   = PG_decomp_log - shift(PG_decomp_log, type = "lag", n = 1), 
    Inequality_growth  = Inequality_log - shift(Inequality_log, type = "lag", n = 1), 
    Mean_growth        = Mean_log - shift(Mean_log, type = "lag", n = 1)
  )
  , by = Region_code
]
dt_country[
  , 
  `:=` (
    time_diff = Year - shift(Year, type = "lag", n = 1)
  ), 
  by          = Country_name
]
dt_country[
  , `:=` (
    PG_growth          = (PG_log         - shift(PG_log, type = 'lag', n = 1))/time_diff, 
    Inequality_growth  = (Inequality_log - shift(Inequality_log, type = "lag", n = 1))/time_diff, 
    Mean_growth        = (Mean_log       - shift(Mean_log, type = "lag", n = 1))/time_diff
    #Mean_b40_growth    = (Mean_b40_log   - shift(Mean_b40_log, type = "lag", n = 1))/time_diff
  )
  , by                 = Country_name
]
dt_lineup[
  , 
  `:=` (
    time_diff = Year - shift(Year, type = "lag", n = 1)
  ), 
  by          = Country_name
]
dt_lineup[
  , `:=` (
    PG_growth          = (PG_log         - shift(PG_log, type = 'lag', n = 1))/time_diff, 
    Inequality_growth  = (Inequality_log - shift(Inequality_log, type = "lag", n = 1))/time_diff, 
    Mean_growth        = (Mean_log       - shift(Mean_log, type = "lag", n = 1))/time_diff
    #Mean_b40_growth    = (Mean_b40_log   - shift(Mean_b40_log, type = "lag", n = 1))/time_diff
  )
  , by                 = Country_name
]

# Imputation data --------------------------------------------------------------

dt_imputed <- copy(dt_country)
# Exclude double rows
dt_imputed <-
  dt_imputed[,
             if (.N > 1) {
               .SD[!Reporting_level == "rural"]
             } else {
               .SD
             },
             by = .(Country_code, Year, Welfare_type)
  ][,
    if (.N > 1) {
      .SD[Reporting_level == "national"]
    } else {
      .SD
    },
    by = .(Country_code, Year, Welfare_type)
  ]
dt_imputed <-
  dt_imputed[,
             if (.N > 1) {
               .SD[Welfare_type == "income"]
             } else {
               .SD
             },
             by = .(Country_code, Year, Reporting_level)
  ]

country_combo <- unique(
  dt_imputed[
    ,
    .(Country_name, Country_code, Region_name, Region_code)
  ], 
  by = c("Country_name", "Country_code", "Region_name", "Region_code")
)

## Fill Missing ----
dt_imputed <- joyn::joyn(
  y              = CJ(
    Country_name = dt_imputed$Country_name |> unique(), 
    Year         = dt_imputed$Year         |> unique()
  ),
  x              = dt_imputed, 
  by             = c("Country_name", "Year"),
  reportvar = FALSE
)
# dt_imputed <- joyn::joyn(
#   x             = dt_imputed, 
#   y             = country_combo, 
#   by            = c("Country_name"),
#   update_values = T,
#   reportvar = FALSE
# )
#dt_imputed[, report := NULL]

## Create RowTrue giving row number, and Row giving the row numbers but with missing values ----
dt_imputed[
  , 
  RowTrue := rowidv(.SD, cols = "Country_name")
]
dt_imputed[
  , 
  Row := fifelse(
    is.na(PG), NA, RowTrue
  )
]
## Look backward and forward to impute the row numbers for Row ----
dt_imputed[
  , 
  Backward := collapse::na_locf(Row),
  by = Country_name
]
dt_imputed[
  , 
  Backward := fifelse(
    is.na(Backward), 1000, Backward
  )
]
dt_imputed[
  , 
  Forward := collapse::na_focb(Row),
  by = Country_name
]
dt_imputed[
  , 
  Forward := fifelse(
    is.na(Forward), 1000, Forward
  )
]
## Find whether Backward or Forward imputes the nearest row number ----
dt_imputed[
  , 
  `:=`(
    ImputeDirection = ifelse(
      abs(RowTrue - Backward) < abs(RowTrue - Forward), "Backward", "Forward"
    ), 
    ImputeDistance = ifelse(
      abs(RowTrue - Backward) < abs(RowTrue - Forward), abs(RowTrue - Backward) , abs(RowTrue - Forward)
    )
  )
]

## Replace X NAs with closest non-missing ----
dt_imputed[
  , 
  Reporting_level := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      collapse::na_focb(Reporting_level), 
      collapse::na_locf(Reporting_level)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Welfare_type := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      collapse::na_focb(Welfare_type), 
      collapse::na_locf(Welfare_type)
    )
    X_replace
  }
]
dt_imputed[
  , 
  PG := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      collapse::na_focb(PG), 
      collapse::na_locf(PG)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Inequality := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      collapse::na_focb(Inequality), 
      collapse::na_locf(Inequality)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Mean := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      collapse::na_focb(Mean), 
      collapse::na_locf(Mean)
    )
    X_replace
  }
]
# dt_imputed[
#   , 
#   Mean_b40 := {
#     X_replace <- ifelse(
#       ImputeDirection == "Forward" 
#       #collapse::na_focb(Mean_b40), 
#       #collapse::na_locf(Mean_b40F)
#     )
#     X_replace
#   }
# ]
dt_imputed[
  , 
  Survey_comparability := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      collapse::na_focb(Survey_comparability), 
      collapse::na_locf(Survey_comparability)
    )
    X_replace
  }
]

# Save objects as fst ------------------------------------
## country fst ####
fst::write_fst(
  x = dt_country,
  path = here::here("dt_country.fst")
)

## region fst ####
fst::write_fst(
  x = dt_region,
  path = here::here("dt_region.fst")
)

## lineup fst ####
fst::write_fst(
  x = dt_lineup,
  path = here::here("dt_lineup.fst")
)

## country lookup ####
fst::write_fst(
  x = countries_lookup,
  path = here::here("countries_lookup.fst")
)

## imputed ####
fst::write_fst(
  x = dt_imputed,
  path = here::here("dt_imputed.fst")
)
#lineup_old <- fst::read_fst(here::here("dt_lineup.fst"))
