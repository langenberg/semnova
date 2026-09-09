#' @keywords internal
#' @noRd
#' Group heading anchored to the top of the diagram.
#'
#' A facet strip sits above the panel, and the gap between it and the diagram
#' is whatever the panel's expansion happens to be, proportional to the data
#' span, while the loops and labels that overhang the top of the panel are a
#' fixed number of millimetres. Scaling the node table changes the first and
#' not the second, so the heading drifts towards the diagram and eventually
#' collides with it.
#'
#' This draws the heading itself, at a fixed millimetre offset above the
#' topmost node, so the gap is the same at every scaling and figure size.
GeomDagHeading <- ggplot2::ggproto("GeomDagHeading", ggplot2::Geom,
    required_aes = c("x", "y", "label"),
    default_aes = ggplot2::aes(colour = "black", size = 3.5, alpha = NA),
    draw_key = ggplot2::draw_key_blank,

    draw_panel = function(data, panel_params, coord, offset = 14) {
        coords <- coord$transform(data, panel_params)
        grid::textGrob(
            label = coords$label,
            x = grid::unit(coords$x, "npc"),
            y = grid::unit(coords$y, "npc") + grid::unit(offset, "mm"),
            gp = grid::gpar(
                col = ggplot2::alpha(coords$colour, coords$alpha),
                fontsize = coords$size * ggplot2::.pt,
                fontface = "bold"
            )
        )
    }
)

#' @keywords internal
#' @noRd
geom_dag_heading <- function(mapping = NULL, data = NULL, ..., offset = 14,
                             inherit.aes = FALSE) {
    ggplot2::layer(
        geom = GeomDagHeading, mapping = mapping, data = data,
        stat = "identity", position = "identity",
        inherit.aes = inherit.aes,
        params = list(offset = offset, ...)
    )
}
