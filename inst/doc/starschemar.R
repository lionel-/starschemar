## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(starschemar)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(mrs, 12), split.table = Inf)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(mrs_age[,-c(1:6)]), split.table = Inf)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(mrs_cause[,-c(1:6)], 18), split.table = Inf)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(mrs_age), split.table = Inf)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(mrs_cause), split.table = Inf)

## -----------------------------------------------------------------------------
dput(colnames(mrs_age))

## -----------------------------------------------------------------------------
library(tidyr)
library(starschemar)

dm_mrs_age <- dimensional_model() %>%
  define_fact(
    name = "mrs_age",
    measures = c(
      "Deaths"
    ),
    agg_functions = c(
      "SUM"
    ),
    nrow_agg = "nrow_agg"
  ) %>%
  define_dimension(
    name = "when",
    attributes = c(
      "Week Ending Date",
      "WEEK",
      "Year"
    )
  ) %>%
  define_dimension(
    name = "when_available",
    attributes = c(
      "Data Availability Date",
      "Data Availability Week",
      "Data Availability Year"
    )
  ) %>%
  define_dimension(
    name = "where",
    attributes = c(
      "REGION",
      "State",
      "City"
    )
  ) %>%
  define_dimension(
    name = "who",
    attributes = c(
      "Age Range"
    )
  )

## -----------------------------------------------------------------------------
dm_mrs_cause <- dimensional_model() %>%
  define_fact(
    name = "mrs_cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "Other Deaths"
    ),
  ) %>%
  define_dimension(
    name = "when",
    attributes = c(
      "Week Ending Date",
      "WEEK",
      "Year"
    )
  ) %>%
  define_dimension(
    name = "when_received",
    attributes = c(
      "Reception Date",
      "Reception Week",
      "Reception Year"
    )
  ) %>%
  define_dimension(
    name = "when_available",
    attributes = c(
      "Data Availability Date",
      "Data Availability Week",
      "Data Availability Year"
    )
  ) %>%
  define_dimension(
    name = "where",
    attributes = c(
      "REGION",
      "State",
      "City"
    )
  )

## -----------------------------------------------------------------------------
st_mrs_age <- star_schema(mrs_age, dm_mrs_age)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(st_mrs_age$dimension$when), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$when_available), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$where), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$who), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$fact$mrs_age), split.table = Inf)

## -----------------------------------------------------------------------------
st_mrs_age <- st_mrs_age %>%
  role_playing_dimension(
    dim_names = c("when", "when_available"),
    name = "When Common",
    attributes = c("date", "week", "year")
  ) %>%
  snake_case() %>%
  character_dimensions(NA_replacement_value = "Unknown",
                       length_integers = list(week = 2))

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(st_mrs_age$dimension$when), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$when_available), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$where), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$who), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$when_common), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$fact$mrs_age), split.table = Inf)

## -----------------------------------------------------------------------------
st_mrs_cause <- star_schema(mrs_cause, dm_mrs_cause) %>%
  snake_case() %>%
  character_dimensions(
    NA_replacement_value = "Unknown",
    length_integers = list(
      week = 2,
      data_availability_week = 2,
      reception_week = 2
    )
  ) %>%
  role_playing_dimension(
    dim_names = c("when", "when_received", "when_available"),
    name = "when_common",
    attributes = c("date", "week", "year")
  )

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(st_mrs_cause$dimension$when), split.table = Inf)
pander::pandoc.table(head(st_mrs_cause$dimension$when_received), split.table = Inf)
pander::pandoc.table(head(st_mrs_cause$dimension$when_available), split.table = Inf)
pander::pandoc.table(head(st_mrs_cause$dimension$where), split.table = Inf)
pander::pandoc.table(head(st_mrs_cause$dimension$when_common), split.table = Inf)
pander::pandoc.table(head(st_mrs_cause$fact$mrs_cause), split.table = Inf)

## -----------------------------------------------------------------------------
ct_mrs <- constellation(list(st_mrs_age, st_mrs_cause), name = "mrs")

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ct_mrs$dimension$when), split.table = Inf)
pander::pandoc.table(head(ct_mrs$dimension$when_available), split.table = Inf)
pander::pandoc.table(head(ct_mrs$dimension$where), split.table = Inf)

## -----------------------------------------------------------------------------
dim_names <- st_mrs_age %>%
    get_dimension_names()

where <- st_mrs_age %>%
  get_dimension("where")

# View(where)

when <- st_mrs_age %>%
  get_dimension("when")

# View(when)
# when[when$when_key %in% c(36, 37, 73), ]

who <- st_mrs_age %>%
  get_dimension("who")

# View(who)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(when[when$when_key %in% c(36, 37, 73), ], split.table = Inf)

## -----------------------------------------------------------------------------
updates_st_mrs_age <- record_update_set() %>%
  update_selection_general(
    dimension = where,
    columns_old = c("state", "city"),
    old_values = c("CT", "Bridgepor"),
    columns_new = c("city"),
    new_values = c("Bridgeport")
  ) %>%
  match_records(dimension = when,
                old = 37,
                new = 36) %>%
  update_record(
    dimension = when,
    old = 73,
    values = c("1962-02-17", "07", "1962")
  ) %>%
  update_selection(
    dimension = who,
    columns = c("age_range"),
    old_values = c("<1 year"),
    new_values = c("1: <1 year")
  ) %>%
  update_selection(
    dimension = who,
    columns = c("age_range"),
    old_values = c("1-24 years"),
    new_values = c("2: 1-24 years")
  ) %>%
  update_selection(
    dimension = who,
    columns = c("age_range"),
    old_values = c("25-44 years"),
    new_values = c("3: 25-44 years")
  ) %>%
  update_selection(
    dimension = who,
    columns = c("age_range"),
    old_values = c("45-64 years"),
    new_values = c("4: 45-64 years")
  ) %>%
  update_selection(
    dimension = who,
    columns = c("age_range"),
    old_values = c("65+ years"),
    new_values = c("5: 65+ years")
  )

## -----------------------------------------------------------------------------
st_mrs_age <- st_mrs_age %>%
  modify_dimension_records(updates_st_mrs_age)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(st_mrs_age$dimension$when), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$when_available), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$where), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$who), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$dimension$when_common), split.table = Inf)
pander::pandoc.table(head(st_mrs_age$fact$mrs_age), split.table = Inf)

## -----------------------------------------------------------------------------
st_mrs_cause <- st_mrs_cause %>%
  modify_dimension_records(updates_st_mrs_age)

ct_mrs <- ct_mrs %>%
  modify_conformed_dimension_records(updates_st_mrs_age)

## -----------------------------------------------------------------------------
mrs_age_definition <- function(ft, dm, updates) {
  star_schema(ft, dm) %>%
    role_playing_dimension(
      dim_names = c("when", "when_available"),
      name = "When Common",
      attributes = c("date", "week", "year")
    ) %>%
    snake_case() %>%
    character_dimensions(NA_replacement_value = "Unknown",
                         length_integers = list(week = 2)) %>%
    modify_dimension_records(updates)
}

## -----------------------------------------------------------------------------
st_mrs_age_w10 <-
  mrs_age_definition(mrs_age_w10, dm_mrs_age, updates_st_mrs_age)

st_mrs_age_w11 <-
  mrs_age_definition(mrs_age_w11, dm_mrs_age, updates_st_mrs_age)

## -----------------------------------------------------------------------------
st_mrs_age <- st_mrs_age %>%
  incremental_refresh_star_schema(st_mrs_age_w10, existing = "replace") %>%
  incremental_refresh_star_schema(st_mrs_age_w11, existing = "replace")

## -----------------------------------------------------------------------------
ct_mrs <- ct_mrs %>%
  incremental_refresh_constellation(st_mrs_age_w10, existing = "replace") %>%
  incremental_refresh_constellation(st_mrs_age_w11, existing = "replace")

## -----------------------------------------------------------------------------
mrs_cause_definition <- function(ft, dm, updates) {
  star_schema(ft, dm) %>%
    snake_case() %>%
    character_dimensions(
      NA_replacement_value = "Unknown",
      length_integers = list(
        week = 2,
        data_availability_week = 2,
        reception_week = 2
      )
    ) %>%
    role_playing_dimension(
      dim_names = c("when", "when_received", "when_available"),
      name = "when_common",
      attributes = c("date", "week", "year")
    ) %>%
    modify_dimension_records(updates)
}

st_mrs_cause_w10 <-
  mrs_cause_definition(mrs_cause_w10, dm_mrs_cause, updates_st_mrs_age)

st_mrs_cause_w11 <-
  mrs_cause_definition(mrs_cause_w11, dm_mrs_cause, updates_st_mrs_age)

st_mrs_cause <- st_mrs_cause %>%
  incremental_refresh_star_schema(st_mrs_cause_w10, existing = "group") %>%
  incremental_refresh_star_schema(st_mrs_cause_w11, existing = "group")

ct_mrs <- ct_mrs %>%
  incremental_refresh_constellation(st_mrs_cause_w10, existing = "group") %>%
  incremental_refresh_constellation(st_mrs_cause_w11, existing = "group")

## -----------------------------------------------------------------------------
tl <- st_mrs_age %>%
  star_schema_as_tibble_list()

## -----------------------------------------------------------------------------
ms <- ct_mrs %>%
  constellation_as_multistar()

## -----------------------------------------------------------------------------
dm <- dimensional_model()

## -----------------------------------------------------------------------------
dm <- dimensional_model() %>%
  define_dimension(name = "When",
                   attributes = c("Week Ending Date",
                                  "WEEK",
                                  "Year"))

## -----------------------------------------------------------------------------
dm <- dimensional_model() %>%
  define_fact(
    name = "mrs_age",
    measures = c("Deaths"),
    agg_functions = c("SUM"),
    nrow_agg = "nrow_agg"
  )

dm <- dimensional_model() %>%
  define_fact(name = "Factless fact")

## -----------------------------------------------------------------------------
st <- star_schema(mrs_age, dm_mrs_age)

## -----------------------------------------------------------------------------
st <- star_schema(mrs_age, dm_mrs_age) %>%
  role_playing_dimension(
    dim_names = c("when", "when_available"),
    name = "When Common",
    attributes = c("Date", "Week", "Year")
  )

## -----------------------------------------------------------------------------
st <- star_schema(mrs_age, dm_mrs_age) %>%
  snake_case()

## -----------------------------------------------------------------------------
st <- star_schema(mrs_age, dm_mrs_age) %>%
  character_dimensions()

## -----------------------------------------------------------------------------
ct <- constellation(list(st_mrs_age, st_mrs_cause), name = "mrs")

## -----------------------------------------------------------------------------
dn <- st_mrs_age %>%
  get_dimension_names()

## -----------------------------------------------------------------------------
where <- st_mrs_age %>%
  get_dimension("where")

## -----------------------------------------------------------------------------
dn <- ct_mrs %>%
  get_conformed_dimension_names()

## -----------------------------------------------------------------------------
when <- ct_mrs %>%
  get_conformed_dimension("when")

## -----------------------------------------------------------------------------
updates <- record_update_set()

## -----------------------------------------------------------------------------
updates <- record_update_set() %>%
  match_records(dimension = where,
                old = 1,
                new = 2)

## -----------------------------------------------------------------------------
updates <- record_update_set() %>%
  update_record(
    dimension = where,
    old = 1,
    values = c("1", "CT", "Bridgeport")
  )

## -----------------------------------------------------------------------------
updates <- record_update_set() %>%
  update_selection(
    dimension = where,
    columns = c("city"),
    old_values = c("Bridgepor"),
    new_values = c("Bridgeport")
  )

## -----------------------------------------------------------------------------
updates <- record_update_set() %>%
  update_selection_general(
    dimension = where,
    columns_old = c("state", "city"),
    old_values = c("CT", "Bridgepor"),
    columns_new = c("city"),
    new_values = c("Bridgeport")
  )

## -----------------------------------------------------------------------------
st <- st_mrs_age %>%
  modify_dimension_records(updates_st_mrs_age)

## -----------------------------------------------------------------------------
ct <- ct_mrs %>%
  modify_conformed_dimension_records(updates_st_mrs_age)

## -----------------------------------------------------------------------------
st <- st_mrs_age %>%
  incremental_refresh_star_schema(st_mrs_age_w10, existing = "replace")

## -----------------------------------------------------------------------------
ct <- ct_mrs %>%
  incremental_refresh_constellation(st_mrs_age_w10, existing = "replace")

## -----------------------------------------------------------------------------
ft <- st_mrs_age %>%
  star_schema_as_flat_table()

## -----------------------------------------------------------------------------
ms <- st_mrs_age %>%
  star_schema_as_multistar()

## -----------------------------------------------------------------------------
tl <- st_mrs_age %>%
  star_schema_as_tibble_list(include_role_playing = TRUE)

## -----------------------------------------------------------------------------
ms <- ct_mrs %>%
  constellation_as_multistar()

## -----------------------------------------------------------------------------
tl <- ct_mrs %>%
  constellation_as_tibble_list(include_role_playing = TRUE)

