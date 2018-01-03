#' Any NA? (computed in a vectorized manner)
#' @rdname any_na
#' @export
any_na <- function(...) {
    vars <- list(...)
    Reduce('|', lapply(vars, is.na))
}


#' All NA? (computed in a vectorized manner)
#' @rdname any_na
#' @export
all_na <- function(...) {
    vars <- list(...)
    Reduce('&', lapply(vars, is.na))
}


#' The first non-na element (computed in a vectorized manner)
#' @rdname any_na
#' @export
non_na <- function(...) {
    vars <- list(...)
    fix <- function(x, y) {
        idx <- is.na(x)
        if (!any(idx)) {
            return(x)
        }
        x[idx] <- y[idx]
        x 
    }
    Reduce(fix, vars)
}
