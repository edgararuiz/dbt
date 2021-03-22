#' @import yaml
#' @import rlang
#' @import dplyr
#' @export
#'
dbt_test_mutate <- function(test_expression = fld_double + 1,
                            target_table = dbt_test_data,
                            source_table = dbt_test_data,
                            test = NULL) {
  sm <- mutate({{ source_table }}, x = {{ test_expression }})
  sr1 <- pull(sm, x)
  sr <- sr1[order(sr1)]

  tm <- NULL
  tr <- NULL
  try(
    {
      tm <- mutate({{ target_table }}, x = {{ test_expression }})
    },
    silent = TRUE
  )

  if (!is.null(tm)) {
    tp <- pull(tm, x)
    tr <- tp[order(tp)]
    res <- length(sr) == length(tr)
    if (is.numeric(sr) & res) {
      res <- !any(abs(sr - tr) > 0.1)
    } else {
      res <- all(sr == tr)
    }
  } else {
    res <- NULL
  }

  dbt_verb_result(
    dplyr_verb = "mutate()",
    test = test,
    tested_expression = as_label(enexpr(test_expression)),
    source_table_result = sr,
    target_table_result = tr,
    status = res
  )
}

#' @export
dbt_test_filter <- function(test_expression = fld_double > 2,
                            test_field = fld_double,
                            target_table = dbt_test_data,
                            source_table = dbt_test_data,
                            test = "add") {
  sm <- filter({{ source_table }}, {{ test_expression }})
  sr <- pull(sm, {{ test_field }})

  tm <- NULL
  tr <- NULL
  try(
    tm <- filter({{ source_table }}, {{ test_expression }}),
    silent = TRUE
  )
  if (!is.null(tm)) {
    tr <- pull(tm, {{ test_field }})
    res <- length(sr) == length(tr)
    if (res) res <- all(sr == tr)
  } else {
    res <- NULL
  }
  dbt_verb_result(
    dplyr_verb = "filter()",
    test = test,
    tested_expression = as_label(enexpr(test_expression)),
    source_table_result = sr,
    target_table_result = tr,
    status = res
  )
}
#' @export
dbt_test_arrange <- function(test_expression = fld_double + 1,
                             test_field = fld_double,
                             target_table = dbt_test_data,
                             source_table = dbt_test_data,
                             test = "add") {
  sm <- arrange({{ source_table }}, {{ test_expression }})
  sr <- pull(sm, {{ test_field }})

  tm <- NULL
  tr <- NULL
  try(
    tm <- arrange({{ target_table }}, {{ test_expression }}),
    silent = TRUE
  )

  if (!is.null(tm)) {
    tp <- pull(tm, {{ test_field }})
    res <- length(sr) == length(tr)
    if (is.numeric(sr) & res) {
      res <- !any(abs(sr - tr) > 0.1)
    } else {
      res <- all(sr == tr)
    }
  } else {
    res <- NULL
  }

  dbt_verb_result(
    dplyr_verb = "arrange()",
    test = test,
    tested_expression = as_label(enexpr(test_expression)),
    source_table_result = sr,
    target_table_result = tr,
    status = res
  )
}

#' @export
dbt_test_summarise <- function(test_expression = sum(fld_double + 1),
                               target_table = dbt_test_data,
                               source_table = dbt_test_data,
                               test = "add") {
  sm <- summarise({{ source_table }}, x = {{ test_expression }})
  sr <- pull(sm, x)

  tm <- NULL
  tr <- NULL
  try(
    tm <- summarise({{ target_table }}, x = {{ test_expression }}),
    silent = TRUE
  )

  if (!is.null(tm)) {
    tp <- pull(tm, x)
    tr <- tp[order(tp)]
    res <- length(sr) == length(tr)
    if (is.numeric(sr) & res) {
      res <- !any(abs(sr - tr) > 0.1)
    } else {
      res <- all(sr == tr)
    }
  } else {
    res <- NULL
  }

  dbt_verb_result(
    dplyr_verb = "summarise()",
    test = test,
    tested_expression = as_label(enexpr(test_expression)),
    source_table_result = sr,
    target_table_result = tr,
    status = res
  )
}

#' @export
dbt_test_group_by <- function(test_expression = fld_double + 1,
                              target_table = dbt_test_data,
                              source_table = dbt_test_data,
                              test = "add") {
  sm <- group_by({{ source_table }}, {{ test_expression }})
  ss <- summarise(sm, x = n())
  sp <- pull(ss, x)
  sr <- sp[order(sp)]

  tm <- NULL
  tr <- NULL
  try(
    {
      tm <- group_by({{ target_table }}, {{ test_expression }})
      ts <- summarise(tm, x = n())
    },
    silent = TRUE
  )
  if (!is.null(tm)) {
    tp <- pull(ts, x)
    tr <- tp[order(tp)]
    res <- length(sr) == length(tr)
    if (res) res <- all(sr == tr)
  } else {
    res <- NULL
  }
  dbt_verb_result(
    dplyr_verb = "group_by()",
    test = test,
    tested_expression = as_label(enexpr(test_expression)),
    source_table_result = sr,
    target_table_result = tr,
    status = res
  )
}

#' @export
dbt_test_group_by_summarise <- function(group_by_expression = fld_double + 1,
                                        summarise_expression = sum(fld_double),
                                        target_table = dbt_test_data,
                                        source_table = dbt_test_data,
                                        test = "add") {
  sm <- group_by({{ source_table }}, {{ group_by_expression }})
  ss <- summarise(sm, x = {{ summarise_expression }})
  sr <- pull(ss, x)

  tm <- NULL
  tr <- NULL
  try(
    {
      tm <- group_by({{ target_table }}, {{ group_by_expression }})
      ts <- summarise(tm, x = {{ summarise_expression }})
    },
    silent = TRUE
  )
  if (!is.null(tm)) {
    tr <- pull(ts, x)
    res <- length(sr) == length(tr)
    if (res) res <- all(sr == tr)
  } else {
    res <- NULL
  }
  dbt_verb_result(
    dplyr_verb = "group_by() / summarise()",
    test = test,
    tested_expression = as_label(enexpr(summarise_expression)),
    source_table_result = sr,
    target_table_result = tr,
    status = res
  )
}

#' @export
dbt_verb_result <- function(dplyr_verb = "mutate()",
                            test = "add",
                            result = "dplyr result",
                            tested_expression = NULL,
                            source_table_result = NULL,
                            target_table_result = NULL,
                            status = TRUE) {
  if (is.null(status)) {
    log_status <- "ERROR"
    log_result <- "The operation failed to run in Target source"
  } else {
    if (status) {
      log_status <- "SUCCESS"
      log_result <- NULL
    } else {
      log_status <- "WARNING"
      log_result <- "Operation ran, but results from Source and Target differ"
    }
  }
  dbt_log_result(
    dplyr_verb = dplyr_verb,
    result = log_result,
    test = test,
    tested_expression = tested_expression,
    target_table_result = target_table_result,
    source_table_result = source_table_result,
    status = log_status
  )
}
