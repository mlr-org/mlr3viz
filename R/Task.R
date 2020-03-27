#' @export
fortify.Task = function(model, data = NULL, ...) { # nolint
  as.data.table(model)
}
