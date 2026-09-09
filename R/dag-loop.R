#' @keywords internal
#' @noRd
#' Self-loop geom drawn in absolute units.
#'
#' `ggraph::geom_edge_loop()` builds its loop in *data* units, while nodes are
#' drawn in millimetres. Resizing the figure changes how long a data unit is
#' physically, so the loops stretch and flatten while the nodes they belong to
#' stay put, at wide aspect ratios they balloon across the panel.
#'
#' This geom anchors each loop at the node's data position but builds it
#' entirely from millimetre offsets, so a loop keeps its size and shape at any
#' figure size, exactly as the node markers and text do.
GeomDagLoop <- ggplot2::ggproto("GeomDagLoop", ggplot2::Geom,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(
        colour = "black", linewidth = 0.4, linetype = 1, alpha = NA,
        label = "", label_size = 2.2, label_colour = "grey20"
    ),
    draw_key = ggplot2::draw_key_path,

    draw_panel = function(data, panel_params, coord,
                          node_radius = 4.5, loop_radius = 3.2,
                          direction = 90, gap = 70, arrow = NULL,
                          label_parse = TRUE) {
        coords <- coord$transform(data, panel_params)
        theta <- direction * pi / 180
        # centre of the loop, offset from the node so the two just touch
        offset <- node_radius + loop_radius * 0.75

        grobs <- lapply(seq_len(nrow(coords)), function(i) {
            angles <- seq(
                theta + pi + (gap / 2) * pi / 180,
                theta + pi + 2 * pi - (gap / 2) * pi / 180,
                length.out = 80
            )
            dx <- offset * cos(theta) + loop_radius * cos(angles)
            dy <- offset * sin(theta) + loop_radius * sin(angles)

            arc <- grid::polylineGrob(
                x = grid::unit(coords$x[i], "npc") + grid::unit(dx, "mm"),
                y = grid::unit(coords$y[i], "npc") + grid::unit(dy, "mm"),
                gp = grid::gpar(
                    col = ggplot2::alpha(coords$colour[i], coords$alpha[i]),
                    # a closed arrowhead takes its fill from gpar, not col --
                    # without this it draws hollow
                    fill = ggplot2::alpha(coords$colour[i], coords$alpha[i]),
                    lwd = coords$linewidth[i] * ggplot2::.pt,
                    lty = coords$linetype[i]
                ),
                arrow = arrow
            )

            label <- coords$label[i]
            if (is.null(label) || is.na(label) || !nzchar(label)) {
                return(arc)
            }
            text <- grid::textGrob(
                label = if (label_parse) tryCatch(parse(text = label), error = function(e) label) else label,
                x = grid::unit(coords$x[i], "npc") +
                    grid::unit(offset * cos(theta) + (loop_radius + 1.4) * cos(theta), "mm"),
                y = grid::unit(coords$y[i], "npc") +
                    grid::unit(offset * sin(theta) + (loop_radius + 1.4) * sin(theta), "mm"),
                gp = grid::gpar(
                    col = coords$label_colour[i],
                    fontsize = coords$label_size[i] * ggplot2::.pt
                )
            )
            grid::gTree(children = grid::gList(arc, text))
        })

        grid::gTree(children = do.call(grid::gList, grobs))
    }
)

#' @keywords internal
#' @noRd
geom_dag_loop <- function(mapping = NULL, data = NULL, ...,
                          node_radius = 4.5, loop_radius = 3.2,
                          direction = 90, gap = 70, arrow = NULL,
                          label_parse = TRUE, inherit.aes = FALSE) {
    ggplot2::layer(
        geom = GeomDagLoop, mapping = mapping, data = data,
        stat = "identity", position = "identity",
        inherit.aes = inherit.aes,
        params = list(
            node_radius = node_radius, loop_radius = loop_radius,
            direction = direction, gap = gap, arrow = arrow,
            label_parse = label_parse, ...
        )
    )
}
