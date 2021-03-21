#' @import crayon
#' @export
dbt_log_result <- function(test = "mutate()",
                       tested_expression = NULL,
                       source_table_result = NULL,
                       target_table_result = NULL,
                       status = "SUCCESS") {
  structure(
    list(
     test = test,
     tested_expression = tested_expression,
     source_table_result = source_table_result,
     target_table_result = target_table_result,
     status = status
     ),
    class = "dbt_result"
  )
}

setOldClass("dbt_result")

#' @export
print.dbt_result <- function(x, ...) {
  out <- NULL
  if(x$status == "SUCCESS") out <- green(x$status)
  out <- c(out, bold(x$test), cyan(x$tested_expression))
  cat(paste(out, collapse = " | "))
  invisible(x)
}

