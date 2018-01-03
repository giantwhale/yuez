#' @export
write_clipboard <- function(x, row.names=FALSE, sep='\t', ...) {
    write.table(x, file='clipboard-4096', row.names=row.names, sep=sep)
}
