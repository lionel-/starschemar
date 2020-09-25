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
st_mrs_age <-
  st_mrs_age %>% rename_dimension_attributes(
    name = "when",
    attributes = c("week_ending_date", "week", "year"),
    new_names = c(
      "when_happened_date",
      "when_happened_week",
      "when_happened_year"
    )
  ) %>%
  rename_dimension_attributes(
    name = "where",
    attributes = c("region"),
    new_names = c("division")
  )

## -----------------------------------------------------------------------------
st_mrs_cause <-
  st_mrs_cause %>% rename_dimension_attributes(
    name = "when",
    attributes = c("week_ending_date", "week", "year"),
    new_names = c(
      "when_happened_date",
      "when_happened_week",
      "when_happened_year"
    )
  ) %>%
  rename_dimension_attributes(
    name = "where",
    attributes = c("region"),
    new_names = c("division")
  )

## -----------------------------------------------------------------------------
st_mrs_age <-
  st_mrs_age %>% rename_measures(measures = c("deaths"),
                                 new_names = c("n_deaths"))

## -----------------------------------------------------------------------------
dim_names <- st_mrs_age %>%
    get_dimension_names()

where <- st_mrs_age %>%
  get_dimension("where")

# View(where)
# where[where$where_key %in% c(1, 2, 62), ]

when <- st_mrs_age %>%
  get_dimension("when")

# View(when)
# when[when$when_key %in% c(36, 37, 73), ]

who <- st_mrs_age %>%
  get_dimension("who")

# View(who)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(where[where$where_key %in% c(1, 2, 62), ], split.table = Inf)

## -----------------------------------------------------------------------------
updates_st_mrs_age <- record_update_set() %>%
  match_records(dimension = where,
                old = 1,
                new = 2) 

## -----------------------------------------------------------------------------
updates_st_mrs_age <- updates_st_mrs_age %>%
  update_selection_general(
    dimension = where,
    columns_old = c("state", "city"),
    old_values = c("DE", "Wilimington"),
    columns_new = c("city"),
    new_values = c("Wilmington")
  ) 

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(when[when$when_key %in% c(36, 37, 73), ], split.table = Inf)

## -----------------------------------------------------------------------------
updates_st_mrs_age <- updates_st_mrs_age %>%
  match_records(dimension = when,
                old = 37,
                new = 36) %>%
  update_record(
    dimension = when,
    old = 73,
    values = c("1962-02-17", "07", "1962")
  )

## -----------------------------------------------------------------------------
updates_st_mrs_age <- updates_st_mrs_age %>%
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

## -----------------------------------------------------------------------------
tb_who <-
  enrich_dimension_export(st_mrs_age,
                          name = "who",
                          attributes = c("age_range"))

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(tb_who, split.table = Inf)

## -----------------------------------------------------------------------------
v <-
  c("0-24 years", "0-24 years", "25+ years", "25+ years", "25+ years")
tb_who <-
  tibble::add_column(tb_who,
                     wide_age_range = v)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(tb_who, split.table = Inf)

## -----------------------------------------------------------------------------
st_mrs_age <-
  st_mrs_age %>%
  enrich_dimension_import(name = "who", tb_who)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(st_mrs_age$dimension$who, split.table = Inf)

## -----------------------------------------------------------------------------
tb_where <-
  enrich_dimension_export(st_mrs_age,
                          name = "where",
                          attributes = c("division"))

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(tb_where, split.table = Inf)

## -----------------------------------------------------------------------------
tb_where <-
  tibble::add_column(
    tb_where,
    division_name = c(
      "New England",
      "Middle Atlantic",
      "East North Central",
      "West North Central",
      "South Atlantic",
      "East South Central",
      "West South Central",
      "Mountain",
      "Pacific"
    ),
    region = c('1',
               '1',
               '2',
               '2',
               '3',
               '3',
               '3',
               '4',
               '4'),
    region_name = c(
      "Northeast",
      "Northeast",
      "Midwest",
      "Midwest",
      "South",
      "South",
      "South",
      "West",
      "West"
    )
  )

st_mrs_age <-
  st_mrs_age %>%
  enrich_dimension_import(name = "where", tb_where)

st_mrs_cause <-
  st_mrs_cause %>%
  enrich_dimension_import(name = "where", tb_where)

## -----------------------------------------------------------------------------
tb_missing <-
  st_mrs_age %>%
  enrich_dimension_import_test(name = "where", ft_usa_states)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(tb_missing, split.table = Inf)

## -----------------------------------------------------------------------------
tb_where_state <- ft_usa_states %>%
  tibble::add_row(state = "Unknown", state_name = "Unknown")

st_mrs_age <-
  st_mrs_age %>%
  enrich_dimension_import(name = "where", tb_where_state)

st_mrs_cause <-
  st_mrs_cause %>%
  enrich_dimension_import(name = "where", tb_where_state)

## -----------------------------------------------------------------------------
tb_where_county <- ft_usa_city_county %>%
  tibble::add_row(city = "Unknown",
                  state = "Unknown",
                  county = "Unknown")

st_mrs_age <-
  st_mrs_age %>%
  enrich_dimension_import(name = "where", tb_where_county)

st_mrs_cause <-
  st_mrs_cause %>%
  enrich_dimension_import(name = "where", tb_where_county)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(st_mrs_age$dimension$where, 10), split.table = Inf)

## -----------------------------------------------------------------------------
ct_mrs <- constellation(list(st_mrs_age, st_mrs_cause), name = "mrs")

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ct_mrs$dimension$when), split.table = Inf)
pander::pandoc.table(head(ct_mrs$dimension$when_available), split.table = Inf)
pander::pandoc.table(head(ct_mrs$dimension$where), split.table = Inf)

## -----------------------------------------------------------------------------
mrs_age_definition <-
  function(ft,
           dm,
           updates,
           tb_who,
           tb_where,
           tb_where_state,
           tb_where_county) {
    star_schema(ft, dm) %>%
      role_playing_dimension(
        dim_names = c("when", "when_available"),
        name = "When Common",
        attributes = c("date", "week", "year")
      ) %>%
      snake_case() %>%
      character_dimensions(NA_replacement_value = "Unknown",
                           length_integers = list(week = 2)) %>%
      rename_dimension_attributes(
        name = "when",
        attributes = c("week_ending_date", "week", "year"),
        new_names = c(
          "when_happened_date",
          "when_happened_week",
          "when_happened_year"
        )
      ) %>%
      rename_dimension_attributes(
        name = "where",
        attributes = c("region"),
        new_names = c("division")
      ) %>%
      rename_measures(measures = c("deaths"),
                      new_names = c("n_deaths")) %>%
      modify_dimension_records(updates) %>%
      enrich_dimension_import(name = "who", tb_who) %>%
      enrich_dimension_import(name = "where", tb_where) %>%
      enrich_dimension_import(name = "where", tb_where_state) %>%
      enrich_dimension_import(name = "where", tb_where_county)
  }

## -----------------------------------------------------------------------------
st_mrs_age_w10 <-
  mrs_age_definition(
    mrs_age_w10,
    dm_mrs_age,
    updates_st_mrs_age,
    tb_who,
    tb_where,
    tb_where_state,
    tb_where_county
  )

st_mrs_age_w11 <-
  mrs_age_definition(
    mrs_age_w11,
    dm_mrs_age,
    updates_st_mrs_age,
    tb_who,
    tb_where,
    tb_where_state,
    tb_where_county
  )

## -----------------------------------------------------------------------------
st_mrs_age <- st_mrs_age %>%
  incremental_refresh_star_schema(st_mrs_age_w10, existing = "replace") %>%
  incremental_refresh_star_schema(st_mrs_age_w11, existing = "replace")

## -----------------------------------------------------------------------------
ct_mrs <- ct_mrs %>%
  incremental_refresh_constellation(st_mrs_age_w10, existing = "replace") %>%
  incremental_refresh_constellation(st_mrs_age_w11, existing = "replace")


## -----------------------------------------------------------------------------
mrs_cause_definition <-
  function(ft,
           dm,
           updates,
           tb_where,
           tb_where_state,
           tb_where_county) {
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
      rename_dimension_attributes(
        name = "when",
        attributes = c("week_ending_date", "week", "year"),
        new_names = c(
          "when_happened_date",
          "when_happened_week",
          "when_happened_year"
        )
      ) %>%
      rename_dimension_attributes(
        name = "where",
        attributes = c("region"),
        new_names = c("division")
      ) %>%
      modify_dimension_records(updates) %>%
      enrich_dimension_import(name = "where", tb_where) %>%
      enrich_dimension_import(name = "where", tb_where_state) %>%
      enrich_dimension_import(name = "where", tb_where_county)
  }

st_mrs_cause_w10 <-
  mrs_cause_definition(
    mrs_cause_w10,
    dm_mrs_cause,
    updates_st_mrs_age,
    tb_where,
    tb_where_state,
    tb_where_county
  )

st_mrs_cause_w11 <-
  mrs_cause_definition(
    mrs_cause_w11,
    dm_mrs_cause,
    updates_st_mrs_age,
    tb_where,
    tb_where_state,
    tb_where_county
  )

st_mrs_cause <- st_mrs_cause %>%
  incremental_refresh_star_schema(st_mrs_cause_w10, existing = "group") %>%
  incremental_refresh_star_schema(st_mrs_cause_w11, existing = "group")

ct_mrs <- ct_mrs %>%
  incremental_refresh_constellation(st_mrs_cause_w10, existing = "group") %>%
  incremental_refresh_constellation(st_mrs_cause_w11, existing = "group")

## -----------------------------------------------------------------------------
st1 <- ct_mrs %>%
  get_star_schema("mrs_age") %>%
  filter_fact_rows(name = "where", city == "Boston")

st2 <- ct_mrs %>%
  get_star_schema("mrs_cause") %>%
  filter_fact_rows(name = "where", city == "Boston")

## -----------------------------------------------------------------------------
ct_tmp <- ct_mrs %>%
  incremental_refresh_constellation(st1, existing = "delete") %>%
  incremental_refresh_constellation(st2, existing = "delete")

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ct_tmp$dimension$where), split.table = Inf)

## -----------------------------------------------------------------------------
ct_tmp <- ct_tmp %>%
  purge_dimensions_constellation()

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ct_tmp$dimension$where), split.table = Inf)

## -----------------------------------------------------------------------------
tl <- st_mrs_age %>%
  star_schema_as_tibble_list()

## -----------------------------------------------------------------------------
ms_mrs <- ct_mrs %>%
  constellation_as_multistar()

## -----------------------------------------------------------------------------
ft <- ms_mrs %>%
  multistar_as_flat_table(fact = "mrs_age")

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ft), split.table = Inf)

## -----------------------------------------------------------------------------
ms_mrs <- ct_mrs %>%
  constellation_as_multistar()

## -----------------------------------------------------------------------------
ms <- dimensional_query(ms_mrs) %>%
  select_dimension(name = "where",
                   attributes = c("city", "state")) %>%
  select_dimension(name = "when",
                   attributes = c("when_happened_year")) %>%
  select_fact(name = "mrs_age",
              measures = c("n_deaths")) %>%
  select_fact(
    name = "mrs_cause",
    measures = c("pneumonia_and_influenza_deaths", "other_deaths")
  ) %>%
  filter_dimension(name = "when", when_happened_week <= "03") %>%
  filter_dimension(name = "where", city == "Bridgeport") %>%
  run_query()

## -----------------------------------------------------------------------------
ft <- ms %>%
  multistar_as_flat_table()

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(head(ft), split.table = Inf)

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
st <- st_mrs_age %>%
  rename_dimension(name = "when", new_name = "when_happened")

## -----------------------------------------------------------------------------
attribute_names <- 
  st_mrs_age %>% get_dimension_attribute_names("when")

## -----------------------------------------------------------------------------
st <-
  st_mrs_age %>% rename_dimension_attributes(
    name = "when",
    attributes = c("when_happened_week", "when_happened_year"),
    new_names = c("week", "year")
  )

## -----------------------------------------------------------------------------
st <- st_mrs_age %>% rename_fact("age") 

## -----------------------------------------------------------------------------
measure_names <- 
  st_mrs_age %>% get_measure_names()

## -----------------------------------------------------------------------------
st <-
  st_mrs_age %>% rename_measures(measures = c("n_deaths"),
                                 new_names = c("num_deaths"))

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
stn <- ct_mrs %>%
  get_star_schema_names()

## -----------------------------------------------------------------------------
age <- ct_mrs %>%
  get_star_schema("mrs_age")

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
    dimension = who,
    old = 1,
    values = c("1: <1 year")
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
tb <-
  enrich_dimension_export(st_mrs_age,
                          name = "when_common",
                          attributes = c("week", "year"))

## -----------------------------------------------------------------------------
tb <-
  enrich_dimension_export(st_mrs_age,
                          name = "when_common",
                          attributes = c("week", "year"))

# Add new columns with meaningful data (these are not), possibly exporting
# data to a file, populating it and importing it.
tb <- tibble::add_column(tb, x = "x", y = "y", z = "z")

st <- enrich_dimension_import(st_mrs_age, name = "when_common", tb)

## -----------------------------------------------------------------------------
tb <-
  enrich_dimension_export(st_mrs_age,
                          name = "when_common",
                          attributes = c("week", "year"))

# Add new columns with meaningful data (these are not), possibly exporting
# data to a file, populating it and importing it.
tb <- tibble::add_column(tb, x = "x", y = "y", z = "z")[-1, ]

tb2 <- enrich_dimension_import_test(st_mrs_age, name = "when_common", tb)

## -----------------------------------------------------------------------------
st <- st_mrs_age %>%
  incremental_refresh_star_schema(st_mrs_age_w10, existing = "replace")

## -----------------------------------------------------------------------------
st <- st_mrs_age %>%
  filter_fact_rows(name = "when", when_happened_week <= "03") %>%
  filter_fact_rows(name = "where", city == "Bridgeport")

st2 <- st_mrs_age %>%
  incremental_refresh_star_schema(st, existing = "delete")

## -----------------------------------------------------------------------------
st3 <- st2 %>%
  purge_dimensions_star_schema()

## -----------------------------------------------------------------------------
ct <- ct_mrs %>%
  incremental_refresh_constellation(st_mrs_age_w10, existing = "replace")

## -----------------------------------------------------------------------------
ct <- ct_mrs %>%
  purge_dimensions_constellation()

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

## -----------------------------------------------------------------------------
ft <- ms_mrs %>%
  multistar_as_flat_table(fact = "mrs_age")

## -----------------------------------------------------------------------------
ms_mrs <- ct_mrs %>%
  constellation_as_multistar()

dq <- dimensional_query(ms_mrs)

## -----------------------------------------------------------------------------
dq <- dimensional_query(ms_mrs) %>%
  select_fact(
    name = "mrs_age",
    measures = c("n_deaths"),
    agg_functions = c("MAX")
  )

dq <- dimensional_query(ms_mrs) %>%
  select_fact(name = "mrs_age",
              measures = c("n_deaths"))

dq <- dimensional_query(ms_mrs) %>%
  select_fact(name = "mrs_age")

## -----------------------------------------------------------------------------
dq <- dimensional_query(ms_mrs) %>%
  select_dimension(name = "where",
                   attributes = c("city", "state")) %>%
  select_dimension(name = "when")

## -----------------------------------------------------------------------------
dq <- dimensional_query(ms_mrs) %>%
  filter_dimension(name = "when", when_happened_week <= "03") %>%
  filter_dimension(name = "where", city == "Boston")

## -----------------------------------------------------------------------------
ms <- dimensional_query(ms_mrs) %>%
  select_dimension(name = "where",
                   attributes = c("city", "state")) %>%
  select_dimension(name = "when",
                   attributes = c("when_happened_year")) %>%
  select_fact(
    name = "mrs_age",
    measures = c("n_deaths")
  ) %>%
  select_fact(
    name = "mrs_cause",
    measures = c("pneumonia_and_influenza_deaths", "other_deaths")
  ) %>%
  filter_dimension(name = "when", when_happened_week <= "03") %>%
  filter_dimension(name = "where", city == "Boston") %>%
  run_query()

