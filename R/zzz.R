#' @import mlr3misc
#' @import checkmate
#' @import data.table
#' @import ggplot2
#' @importFrom utils head
#' @importFrom graphics plot
#' @importFrom stats sd as.dendrogram
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  utils::globalVariables(c("modname", "count"))
}
