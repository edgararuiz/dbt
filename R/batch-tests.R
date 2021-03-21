#' @importFrom purrr map imap
#' @importFrom yaml read_yaml


#' @export
dbt_read_run_script <- function(file_path = system.file("tests/math-trigonometry.yml", package = "dbt"),
                                source_table = testdata,
                                target_table = testdata,
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
                           source_table = testdata,
                           target_table = testdata,
                           silent = FALSE) {
  test_results <- script_list %>%
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
              if (dplyr_verb == "mutate") res <- dbt_test_mutate(test_expression = !!test_expression,
                                                                 source_table = source_table,
                                                                 target_table = target_table,
                                                                 test = test)
              if (dplyr_verb == "summarise") res <- dbt_test_summarise(test_expression = !!test_expression,
                                                                       source_table = source_table,
                                                                       target_table = target_table,
                                                                       test = test)
              if (dplyr_verb == "summarize") res <- dbt_test_summarise(test_expression = !!test_expression,
                                                                       source_table = source_table,
                                                                       target_table = target_table,
                                                                       test = test)
              if (dplyr_verb == "arrange") res <- dbt_test_arrange(test_expression = !!test_expression,
                                                                   source_table = source_table,
                                                                   target_table = target_table,
                                                                   test = test)
              if (dplyr_verb == "group_by") res <- dbt_test_group_by(test_expression = !!test_expression,
                                                                     source_table = source_table,
                                                                     target_table = target_table,
                                                                     test = test)
              if (dplyr_verb == "filter") res <- dbt_test_filter(test_expression = !!test_expression,
                                                                 source_table = source_table,
                                                                 target_table = target_table,
                                                                 test = test)
              if(!silent) print(res)
              res
            }
          )
        }
      )
    })

}
