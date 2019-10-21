#' Select variables from the current group
#'
#' Select a tibble made of a tidy selection of columns
#' and the rows of the current group
#'
#' @param ... tidy selection of columns
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(sepal = head(pick(starts_with("Sepal")), 2L))
#'
#' @export
pick <- function(...) {
  mask <- peek_mask()
  vars <- vars_select(peek_vars(), ...)
  mask$pick(vars)
}

#' @importFrom tidyselect peek_vars vars_select
#' @export
by_column <- function(df, funs = identity, .unpack_to = "{fn}_{var}", .fn_in_vars = FALSE) {
  colwise(funs, .unpack_to = .unpack_to, .fn_in_vars = .fn_in_vars)(df)
}

#' Apply a set of functions to a set of columns
#'
#' Creates a data frame by applying a set of functions to a tidy
#' selection of columns in the current slice
#'
#' @param select tidy selection of columns, forwarded to [pick()]
#' @param funs Functions to apply to each of the selected columns. Possible
#'   values are:
#'
#'   - A single function
#'   - A single quosure style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'   - A named list of functions and/or lambdas
#'
#'  @return A tibble
#'
#' @export
accross <- function(select, funs = identity, .unpack_to = "{fn}_{var}", .fn_in_vars = FALSE) {
  by_column(pick({{select}}), funs, .unpack_to = .unpack_to, .fn_in_vars = .fn_in_vars)
}

#' @export
over <- accross

#' @export
current_key <- function() {
  peek_mask()$current_key()
}

#' @export
colwise <- function(funs = identity, .unpack_to = "{fn}_{var}", .fn_in_vars = FALSE) {
  single_function <- is.function(funs) || is_formula(funs)
  if (single_function) {
    funs <- list(fn = funs)
  } else {
    if (is.null(names(funs))) {
      abort("funs should be a single function, a single formula, or a named list of functions or formulas")
    }
  }
  funs <- map(funs, as_function)
  packing <- is.null(.unpack_to)

  function(df) {
    if (packing) {
      results <- if (.fn_in_vars) {
        map(df, function(column) {
          as_tibble(map(funs, function(f) f(column)))
        })
      } else {
        map(funs, function(f) {
          as_tibble(map(df, f))
        })
      }
      tibble(!!!results)
    } else {

      results <- if (.fn_in_vars) {
        map2(df, names(df), function(column, column_name) {
          out <- map(funs, function(f) f(column))
          names(out) <- glue::glue(.unpack_to, var = column_name, fn = names(out))
          out
        })
      } else {
        map2(funs, names(funs), function(f, name) {
          out <- map(df, f)
          names(out) <- glue::glue(.unpack_to, var = names(out), fn = name)
          out
        })
      }

      tibble(!!!flatten(unname(results)))
    }

  }
}
