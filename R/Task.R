#' @export
fortify.Task = function(model, data = NULL, ...) {
  as.data.table(model)
}
