#' mlr-org ggplot2 theme
#'
#' @description
#'   The theme is heavily influenced and partly based on `ggpubr::theme_pubr()`.
#'   This theme is applied by default to all `autoplot()` methods in the mlr3
#'   ecosystem.
#'   If you do not like it and want to use the default ggplot2 theme, you can
#'   add `+ theme_gray()` to the `autoplot()` call.
#' @param base_size `[integer]`\cr
#'   Text font size.
#' @param base_family `[character]`\cr
#'   Font family.
#' @param border `[logical]`\cr
#'   If TRUE, adds a panel border.
#' @param margin `[logical]`\cr
#'   If FALSE, reduces the plot margin(s).
#' @param legend `[character]`\cr
#'  Specifies the legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top".
#' @param x.text.angle `[numeric]`\cr
#'   Rotation angle of x axis tick labels. Default value is 0.
#'   Use 90 for vertical text.
#' @examples
#' library("ggplot2")
#' p = ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point(aes(color = gear))
#'
#' # Default plot
#' p
#'
#' # theme_mlr3()
#' p + theme_mlr3()
#' @name theme_mlr3
#' @rdname theme_mlr3
#' @export
theme_mlr3 = function(base_size = 12, base_family = "", border = FALSE, margin = TRUE, legend = c("top", "bottom", "left", "right", "none"), x.text.angle = 0) {
  theme_minimal()
}
