#' @export
write_clipboard <- function(x, row.names=FALSE, sep='\t', ...) {
    x[] <- lapply(x, function(z) {
        if (is.character(z)) {
            z <- replace(z, is.na(z), 'NA')
        } else if (is.factor(z) && is.character(levels(z))) {
            z <- replace(as.character(z), is.na(z), 'NA')
        }
        z
    })
    write.table(x, file='clipboard-4096', row.names=row.names, sep=sep, na='', ...)
}
