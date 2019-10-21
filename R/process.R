#' @export
process <- function(.data, expr) {
  UseMethod("process")
}

#' @export
process.data.frame <- function(.data, expr) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)

  expr <- enquo(expr)
  chunks <- map(seq_along(rows), function(group) {
    mask$eval(expr, group)
  })

  chunks
}
