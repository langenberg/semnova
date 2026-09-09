#' Path diagram at a guaranteed physical size (retired)
#'
#' @description
#' The sizing [dag()] used to have, kept for reference and no longer
#' exported.
#'
#' Where [dag()] fixes the *shape* of the diagram and lets ggplot2 fit the
#' whole drawing to the figure, `dag_fixed()` pins the panel to a physical
#' size: with `x_scale`/`y_scale`, a unit of node spacing becomes a fixed
#' number of centimetres, so the spacing is guaranteed rather than
#' approximate. The price is that a figure smaller than
#' `attr(p, "fig_size")` shows a **cropped** diagram instead of a smaller
#' one, which is why it is not the default.
#'
#' Both behaviours come from the same [dag_build()], so this is the whole
#' difference between them.
#'
#' @param fit An [Lgc] object.
#' @param ... Passed to [dag_build()], `what`, `labels`, `digits`,
#'   `x_scale`, `y_scale`, `curve_scale`, `variance_scale`,
#'   `covariance_scale`, `node_size`, `text_size`, `edge_text_size`.
#'
#' @return A `ggplot` object. When scaled, its panel is pinned to a
#'   physical size and it carries a `fig_size` attribute: the width and
#'   height in inches it needs to be drawn without cropping.
#' @keywords internal
dag_fixed <- function(fit, ...) {
    dag_build(fit, ...)
}
