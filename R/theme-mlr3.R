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
  half_line = base_size / 2
  if (!is.numeric(legend)) legend = match.arg(legend)
  if (x.text.angle > 5) xhjust = 1 else xhjust = NULL

  if (border) {
    panel.border = element_rect(fill = NA, colour = "black", size = 0.7)
    axis.line = element_blank()
  }
  else {
    panel.border = element_blank()
    axis.line = element_line(colour = "black", size = 0.5)
  }

  if (margin) {
    plot.margin = margin(half_line, half_line, half_line,
      half_line)
  } else {
    plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "mm")
  }

  .theme = theme_light(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.border = panel.border,
      axis.line = axis.line,
      axis.text = element_text(color = "black"),

      legend.key = element_blank(),
      # facets
      strip.background.x = element_rect(fill = NA, colour = "black", size = 1),
      strip.background.y = element_rect(fill = NA, colour = "black", size = 1),
      strip.text.x = element_text(color = "black", size = base_size, margin = margin(.15, 0, .15, 0, "cm")),
      strip.text.y = element_text(color = "black", size = base_size, margin = margin(.5, .15, .5, .15, "cm"),
        angle = 270),

      plot.margin = plot.margin,
      legend.position = legend,
      complete = TRUE)

  if (x.text.angle != 0) {
    .theme = .theme + theme(axis.text.x = element_text(angle = x.text.angle, hjust = xhjust))
  }

  .theme
}
