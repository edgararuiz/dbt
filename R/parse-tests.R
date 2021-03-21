#' @import yaml
#' @import rlang
#' @import dplyr
#' @export
dbt_test_mutate <- function(test_expression = fld_double + 1,
                            target_table = testdata,
                            source_table = testdata) {

  sm <- mutate({{source_table}}, x = {{test_expression}})
  sr <- pull(sm, x)

  tm <- mutate({{target_table}}, x = {{test_expression}})
  tr <- pull(tm, x)

  all(sr == tr)
}

#' @export
dbt_test_filter <- function(test_expression = fld_double > 2,
                            test_field = fld_double,
                            target_table = testdata,
                            source_table = testdata) {
  sm <- filter({{source_table}}, {{test_expression}})
  sr <- pull(sm, {{test_field}})

  tm <- filter({{source_table}}, {{test_expression}})
  tr <- pull(tm, {{test_field}})

  all(sr == tr)
}
