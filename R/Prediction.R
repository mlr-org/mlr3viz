#' @export
#nolint next
fortify.Prediction = function(model, data, ...) {
  as.data.table(model)
}
