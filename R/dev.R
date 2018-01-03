# library(Rcpp)
# Rcpp.package.skeleton('yuez')

library(devtools)
library(roxygen2)

roxygenize('yuez')

clean_dll('yuez')
document('yuez')

dev_mode()
load_all('yuez')
unload('yuez')
dev_mode()
