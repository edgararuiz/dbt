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
summary.dbt_result_set <- function(object, failed_only = TRUE) {
  tr <- object$test_results

  trt <- map(tr, ~.x$test)
  char_trt <- as.character(trt)
  unique_trt <- unique(char_trt)

  find_trt <- map(
    unique_trt,
    ~{
      ct <- .x
      map_lgl(tr, ~.x$test == ct)
    })

  find_trt <- set_names(find_trt, unique_trt)

  table_trt <- function(verb_name, sub_tr, test_name) {
    verb_location <- map_lgl(sub_tr, ~.x$dplyr_verb == paste0(verb_name, "()"))
    if(sum(verb_location) > 0) {
      sts <- sub_tr[verb_location][[1]]$status
      if(sts == "SUCCESS") res <- "S"
      if(sts == "WARNING") res <- "W"
      if(sts == "ERROR") res <- "E"
    } else {
      res <- "NT"
    }
    res
  }

  m_trt <- imap_dfr(
    find_trt,
    ~{
      sub_tr <- tr[.x]
      mutate_val <- table_trt("mutate", sub_tr, .y)
      filter_val <- table_trt("filter", sub_tr, .y)
      group_by_val <- table_trt("group_by", sub_tr, .y)
      summarise_val <- table_trt("summarise", sub_tr, .y)
      arrange_val <- table_trt("arrange", sub_tr, .y)

      all_success <- "Yes"
      if(mutate_val != "S" & mutate_val != "NT") all_success <- "No"
      if(filter_val != "S" & filter_val != "NT") all_success <- "No"
      if(group_by_val != "S" & group_by_val != "NT") all_success <- "No"
      if(summarise_val != "S" & summarise_val != "NT") all_success <- "No"
      if(arrange_val != "S" & arrange_val != "NT") all_success <- "No"

      tibble(
        test = .y,
        mutate = mutate_val,
        filter = filter_val,
        group_by = group_by_val,
        summarise = summarise_val,
        arrange = arrange_val,
        all_successful = all_success
      )
    }
  )

  if(!failed_only) {
    m_trt
  } else {
    sr <- m_trt$all_successful == "No"
    m_trt[sr, ]
  }
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
