#' @export
fortify.Prediction = function(model, data, ...) { # nolint
  as.data.table(model)
}
