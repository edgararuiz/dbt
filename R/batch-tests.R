#' @export
dbt_log_result_set <- function(test_results = list(),
                               source_table_class = NULL,
                               target_table_class = NULL
                               ) {
  statuses <- map_chr(test_results, ~ .x$status)
  success1 <- length(statuses[statuses == "SUCCESS"])
  error1 <- length(statuses[statuses == "ERROR"])
  warning1 <- length(statuses[statuses == "WARNING"])
  structure(
    list(
      test_results = test_results,
      source_table_class = source_table_class,
      target_table_class = target_table_class,
      result_total = length(statuses),
      result_success = success1,
      result_warning = warning1,
      result_error = error1
    ),
    class = "dbt_result_set"
  )
}

setOldClass("dbt_result_set")

#' @export
as_tibble.dbt_result_set <- function(x, ...) {
  imap_dfr(
    x$test_results,
    ~{
      tibble(
        test_number = as.integer(.y),
        dplyr_verb = .x$dplyr_verb,
        test = .x$test,
        result = ifelse(is.null(.x$result), "", .x$result),
        expression = .x$tested_expression,
        status = .x$status,
        target_result = paste0(.x$target_table_result, collapse = ", "),
        source_result = paste0(.x$source_table_result, collapse = ", ")
      )
    })
}

#' @export
print.dbt_result_set <- function(x, ...) {
  msg <- paste0(
    bold("Source table class: "), x$source_table_class, "\n",
    bold("Target table class: "), x$target_table_class, "\n",
    green("Succesful tests: "), x$result_success, "\n",
    yellow("Test with errors: "), x$result_warning, "\n",
    red("Failed tests: "), x$result_error, "\n",
    bold("Total number of tests: "), bold(x$result_total), "\n"
  )
  cat(paste(msg, "\n"))
}

#' @export
dbt_read_run_folder <- function(folder_path = system.file("tests", package = "dbt"),
                                source_table = dbt_test_data,
                                target_table = dbt_test_data,
                                silent = FALSE) {

  file_list <- list.files(folder_path)

  file_paths <- paste0(folder_path, "/", file_list)

  run_tests <- map(
    file_paths,
    ~{
      dbt_read_run_script(
        file_path = .x,
        source_table = source_table,
        target_table = target_table,
        silent = silent
      )$test_results
    }
  )
  run_tests <- flatten(run_tests)
  run_tests <- set_names(run_tests, 1:length(run_tests))
  dbt_log_result_set(
    test_results = run_tests,
    target_table_class = class(target_table),
    source_table_class = class(source_table)
    )
}

#' @export
dbt_read_run_script <- function(file_path = system.file("tests/math-trigonometry.yml", package = "dbt"),
                                source_table = dbt_test_data,
                                target_table = dbt_test_data,
                                silent = FALSE) {
  test_script <- read_yaml(file_path)
  dbt_run_script(
    script_list = test_script,
    source_table = source_table,
    target_table = target_table,
    silent = silent
  )
}

#' @export
dbt_run_script <- function(script_list,
                           source_table = dbt_test_data,
                           target_table = dbt_test_data,
                           silent = FALSE) {
  raw_results <- script_list %>%
    map(~ {
      x <- .x
      imap(
        x,
        ~ {
          test <- .y
          verb_tests <- .x
          imap(
            verb_tests, ~ {
              test_expression <- parse_expr(.x)
              dplyr_verb <- .y
              res <- NULL
              if (dplyr_verb == "mutate") {
                res <- dbt_test_mutate(
                  test_expression = !!test_expression,
                  source_table = source_table,
                  target_table = target_table,
                  test = test
                )
              }
              if (dplyr_verb == "summarise") {
                res <- dbt_test_summarise(
                  test_expression = !!test_expression,
                  source_table = source_table,
                  target_table = target_table,
                  test = test
                )
              }
              if (dplyr_verb == "summarize") {
                res <- dbt_test_summarise(
                  test_expression = !!test_expression,
                  source_table = source_table,
                  target_table = target_table,
                  test = test
                )
              }
              if (dplyr_verb == "arrange") {
                res <- dbt_test_arrange(
                  test_expression = !!test_expression,
                  source_table = source_table,
                  target_table = target_table,
                  test = test
                )
              }
              if (dplyr_verb == "group_by") {
                res <- dbt_test_group_by(
                  test_expression = !!test_expression,
                  source_table = source_table,
                  target_table = target_table,
                  test = test
                )
              }
              if (dplyr_verb == "filter") {
                res <- dbt_test_filter(
                  test_expression = !!test_expression,
                  source_table = source_table,
                  target_table = target_table,
                  test = test
                )
              }
              if (!silent) print(res)
              res
            }
          )
        }
      )
    })
  test_results <- flatten(flatten(raw_results))
  test_results <- set_names(test_results, 1:length(test_results))
  dbt_log_result_set(test_results)
}
