#' @export
fortify.Prediction = function(model, data, ...) {
  as.data.table(model)
}
