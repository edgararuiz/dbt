#' @importFrom crayon red green cyan bold yellow
#' @export
dbt_log_result <- function(dplyr_verb = "mutate()",
                           test = "add",
                           result = "Explanation of result",
                           tested_expression = NULL,
                           source_table_result = NULL,
                           target_table_result = NULL,
                           status = "SUCCESS") {
  structure(
    list(
      dplyr_verb = dplyr_verb,
      result = result,
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
  if(x$status == "ERROR") out <- red(x$status)
  if(x$status == "WARNING") out <- yellow(x$status)
  out <- c(
    out,
    bold(x$dplyr_verb),
    x$test,
    x$result,
    cyan(x$tested_expression)
    )
  cat(paste(out, collapse = " | "))
  invisible(x)
}

