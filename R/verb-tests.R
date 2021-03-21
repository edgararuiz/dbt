#' @import yaml
#' @import rlang
#' @import dplyr
#' @export
dbt_test_mutate <- function(test_expression = fld_double + 1,
                            target_table = testdata,
                            source_table = testdata) {

  sm <- mutate({{source_table}}, x = {{test_expression}})
  sr <- pull(sm, x)

  tm <- NULL
  try(
    tm <- mutate({{target_table}}, x = {{test_expression}}),
    silent = TRUE
  )
  if(!is.null(tm)) {
    tr <- pull(tm, x)
    all(sr == tr)
  } else {
    NULL
  }
}

#' @export
dbt_test_filter <- function(test_expression = fld_double > 2,
                            test_field = fld_double,
                            target_table = testdata,
                            source_table = testdata) {
  sm <- filter({{source_table}}, {{test_expression}})
  sr <- pull(sm, {{test_field}})

  tm <- NULL
  try(
    tm <- filter({{source_table}}, {{test_expression}}),
    silent = TRUE
  )
  if(!is.null(tm)) {
    tr <- pull(tm, {{test_field}})
    all(sr == tr)
  } else {
    NULL
  }
}
#' @export
dbt_test_arrange<- function(test_expression = fld_double + 1,
                            test_field = fld_double,
                            target_table = testdata,
                            source_table = testdata) {

  sm <- arrange({{source_table}}, {{test_expression}})
  sr <- pull(sm, {{test_field}})

  tm <- NULL
  try(
    tm <- arrange({{target_table}}, {{test_expression}}),
    silent = TRUE
  )
  if(!is.null(tm)) {
    tr <- pull(tm, {{test_field}})
    all(sr == tr)
  } else {
    NULL
  }
}

#' @export
dbt_test_summarise <- function(test_expression = sum(fld_double + 1),
                               target_table = testdata,
                               source_table = testdata) {

  sm <- summarise({{source_table}}, x = {{test_expression}})
  sr <- pull(sm, x)

  tm <- NULL
  try(
    tm <- summarise({{target_table}}, x = {{test_expression}}),
    silent = TRUE
  )
  if(!is.null(tm)) {
    tr <- pull(tm, x)
    all(sr == tr)
  } else {
    NULL
  }
}

#' @export
dbt_test_group_by <- function(test_expression = fld_double + 1,
                              target_table = testdata,
                              source_table = testdata) {

  sm <- group_by({{source_table}}, {{test_expression}})
  ss <- summarise(sm, x = n())
  sr <- pull(ss, x)

  tm <- NULL
  try({
    tm <- group_by({{target_table}}, {{test_expression}})
    ts <- summarise(tm, x = n())
    },
    silent = TRUE
  )
  if(!is.null(tm)) {
    tr <- pull(ts, x)
    all(sr == tr)
  } else {
    NULL
  }
}

#' @export
dbt_test_group_by_summarise <- function(group_by_expression = fld_double + 1,
                                        summarise_expression = sum(fld_double),
                                        target_table = testdata,
                                        source_table = testdata) {

  sm <- group_by({{source_table}}, {{group_by_expression}})
  ss <- summarise(sm, x = {{summarise_expression}})
  sr <- pull(ss, x)

  tm <- NULL
  try({
    tm <- group_by({{target_table}}, {{group_by_expression}})
    ts <- summarise(tm, x = {{summarise_expression}})
  },
  silent = TRUE
  )
  if(!is.null(tm)) {
    tr <- pull(ts, x)
    all(sr == tr)
  } else {
    NULL
  }
}
