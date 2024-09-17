# Country data -----------------------------------------------------------------
dt_country <- fread(
  here(
    "data", 
    "PIPinput_survey_20231016.csv"
  )
)
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
    "Mean", 
    "Mean_b40"
  )
)
## Exclude double rows
dt_country <- 
  dt_country[, 
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
dt_country <- 
  dt_country[, 
             if (.N > 1) {
               .SD[Welfare_type == "income"] 
             } else {
               .SD
             }, 
             by = .(Country_code, Year, Reporting_level)
  ]

# Region data ------------------------------------------------------------------
dt_region <- fread(
  here::here(
    "data", 
    "PIPinput_region_20231017.csv"
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
    "data", 
    "PIPinput_country_20231002.csv"
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
    "Population",
    "PG", 
    "Inequality",
    "Mean", 
    "Mean_b40"
  )
)
## PIP data --------------------------------------------------------------------
dt_pip <- pipr::get_stats(format = "rds") |> 
  as.data.table()
# Exclude double rows
dt_pip <- 
  dt_pip[, 
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
dt_pip <- 
  dt_pip[, 
         if (.N > 1) {
           .SD[welfare_type == "income"] 
         } else {
           .SD
         }, 
         by = .(country_name, year, reporting_level)
  ]
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
  .(Year = year, Country_code = country_code, Survey_comparability = survey_comparability)
]

# Add country and region names -------------------------------------------------
dt_country <- joyn::merge(
  x          = dt_country, 
  y          = countries_lookup, 
  by         = c("Country_code"),
  keep       = "left",
  yvars      = TRUE
)
dt_country[
  , 
  report := NULL
]
dt_country <- joyn::merge(
  x          = dt_country, 
  y          = survey_lookup, 
  by         = c("Country_code", "Year"), 
  keep       = "left",
  yvars      = TRUE, 
  match_type = "1:1"
)
dt_country[
  , 
  report := NULL
]

dt_lineup <- joyn::merge(
  x          = dt_lineup, 
  y          = countries_lookup, 
  by         = c("Country_code"),
  keep       = "left",
  yvars      = TRUE
)
dt_lineup[
  , 
  report := NULL
]
dt_lineup <- joyn::merge(
  x          = dt_lineup, 
  y          = survey_lookup, 
  by         = c("Country_code", "Year"), 
  keep       = "left",
  yvars      = TRUE, 
  match_type = "1:1"
)
dt_lineup[
  , 
  report := NULL
]
dt_lineup[
  ,
  Survey_comparability := 1L
]
dt_region <- joyn::merge(
  x          = dt_region, 
  y          = unique(countries_lookup[, .(Region_name, Region_code)]), 
  by         = c("Region_code"), 
  keep       = "left",
  yvars      = TRUE
)
dt_region[
  , 
  report := NULL
]
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
  here::here("data", "CLASS.dta")
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
dt_country <- joyn::merge(
  x = dt_country, 
  y = dt_class, 
  by = c("Country_name = economy"), 
  keep = "left", 
  yvars = "incgroup_current"
)
dt_country[
  , 
  report := NULL
]
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
  Mean_log       = log(Mean), 
  Mean_b40_log   = log(Mean_b40)
)]
dt_lineup[, `:=`(
  PG_log         = log(PG),              
  Inequality_log = log(Inequality), 
  Mean_log       = log(Mean), 
  Mean_b40_log   = log(Mean_b40)
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
    Mean_growth        = (Mean_log       - shift(Mean_log, type = "lag", n = 1))/time_diff, 
    Mean_b40_growth    = (Mean_b40_log   - shift(Mean_b40_log, type = "lag", n = 1))/time_diff
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
    Mean_growth        = (Mean_log       - shift(Mean_log, type = "lag", n = 1))/time_diff, 
    Mean_b40_growth    = (Mean_b40_log   - shift(Mean_b40_log, type = "lag", n = 1))/time_diff
  )
  , by                 = Country_name
]

# Imputation data --------------------------------------------------------------

dt_imputed <- copy(dt_country)
country_combo <- unique(
  dt_imputed[
    ,
    .(Country_name, Country_code, Region_name, Region_code)
  ], 
  by = c("Country_name", "Country_code", "Region_name", "Region_code")
)

## Fill Missing ----
dt_imputed <- joyn::merge(
  y              = CJ(
    Country_name = dt_imputed$Country_name |> unique(), 
    Year         = dt_imputed$Year         |> unique()
  ),
  x              = dt_imputed, 
  by             = c("Country_name", "Year") 
)
dt_imputed[, report := NULL]
dt_imputed <- joyn::merge(
  x             = dt_imputed, 
  y             = country_combo, 
  by            = c("Country_name"),
  update_values = T 
)
dt_imputed[, report := NULL]

## Create RowTrue giving row number, and Row giving the row numbers but with missing values ----
dt_imputed[
  , 
  RowTrue := rowidv(.SD, cols = "Country_name")
]
dt_imputed[
  , 
  Row := ifelse(
    is.na(PG), NA, RowTrue
  )
]
## Look backward and forward to impute the row numbers for Row ----
dt_imputed[
  , 
  Backward := na.locf(Row, na.rm = F),
  by = Country_name
]
dt_imputed[
  , 
  Backward := ifelse(
    is.na(Backward), 1000, Backward
  )
]
dt_imputed[
  , 
  Forward := na.locf(Row, fromLast =T, na.rm = F),
  by = Country_name
]
dt_imputed[
  , 
  Forward := ifelse(
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
      na.locf(Reporting_level, na.rm = F, fromLast = TRUE), 
      na.locf(Reporting_level)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Welfare_type := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Welfare_type, na.rm = F, fromLast = TRUE), 
      na.locf(Welfare_type)
    )
    X_replace
  }
]
dt_imputed[
  , 
  PG := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(PG, na.rm = F, fromLast = TRUE), 
      na.locf(PG, na.rm = F)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Inequality := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Inequality, na.rm = F, fromLast = TRUE), 
      na.locf(Inequality)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Mean := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Mean, na.rm = F, fromLast = TRUE), 
      na.locf(Mean, na.rm = F)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Mean_b40 := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Mean_b40, na.rm = F, fromLast = TRUE), 
      na.locf(Mean_b40, na.rm = F)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Survey_comparability := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Survey_comparability, na.rm = F, fromLast = TRUE), 
      na.locf(Survey_comparability, na.rm = F)
    )
    X_replace
  }
]

# Save objects ----------------------------------------------------------------
saveRDS(
  object = dt_country, 
  file   = here::here(
    "data", 
    "dt_country.rds"
  ) 
)
saveRDS(
  object = dt_region, 
  file   = here::here(
    "data", 
    "dt_region.rds"
  ) 
)
saveRDS(
  object = dt_lineup, 
  file   = here::here(
    "data", 
    "dt_lineup.rds"
  ) 
)
saveRDS(
  object = countries_lookup, 
  file   = here::here(
    "data", 
    "countries_lookup.rds"
  ) 
)
saveRDS(
  object = dt_imputed,
  file   = here::here(
    "data",
    "dt_imputed.rds"
  )
)


