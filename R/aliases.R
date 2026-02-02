#' BrainMap
#'
#' @description
#' **Deprecated:** This function is deprecated. Use \code{\link{fit_BBM}} instead.
#'
#' @param ... See \code{\link{fit_BBM}}
#' @export
BrainMap <- function(...) {
  .Deprecated("fit_BBM")
  fit_BBM(...)
}

#' engagements
#'
#' @description
#' **Deprecated:** This function is deprecated. Use \code{\link{id_engagements}} instead.
#'
#' @param ... See \code{\link{id_engagements}}
#' @export
engagements <- function(...) {
  .Deprecated("id_engagements")
  id_engagements(...)
}
