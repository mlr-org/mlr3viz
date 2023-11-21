#' @import mlr3misc
#' @import checkmate
#' @import data.table
#' @import ggplot2
#' @importFrom utils head
#' @importFrom graphics plot
#' @importFrom stats sd as.dendrogram
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 2)

  utils::globalVariables(c("modname", "count"))
}
