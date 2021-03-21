#' @export
dbt_log_result <- function(test = "mutate()",
                       tested_expression = fld_double + 2,
                       source_table_result = NULL,
                       target_table_result = NULL,
                       status = "SUCCESS") {
  structure(
    list(
     test = test,
     tested_expression = enexpr(tested_expression),
     source_table_result = source_table_result,
     target_table_result = target_table_result,
     status = status
     ),
    class = "dbt_result"
  )
}

setOldClass("dbt_result")








