#' @keywords internal
#' @noRd
#' Curved (co)variance edge drawn at a constant bend angle.
#'
#' `ggraph::geom_edge_arc()` sets its curvature as a fraction of the chord
#' measured in *data* units, so the bend changes whenever the data-to-figure
#' mapping does: scaling the node table in one direction (see [dag()]'s
#' `x_scale`/`y_scale`) flattens wide sweeping arcs into slivers, and resizing
#' the figure distorts them the same way.
#'
#' This geom takes the two endpoints, converts them to millimetres when the
#' plot is actually drawn, and builds a circular arc that leaves each node at a
#' fixed angle from the chord. The bend is then a property of the diagram,
#' not of the figure. [geom_dag_loop()] gives the self-loops the same
#' guarantee.
GeomDagArc <- ggplot2::ggproto("GeomDagArc", ggplot2::Geom,
    required_aes = c("x", "y", "xend", "yend"),
    default_aes = ggplot2::aes(
        colour = "black", linewidth = 0.4, linetype = 1, alpha = NA,
        label = "", label_size = 2.2, label_colour = "grey20", label_pos = 0.5,
        cap_hw_start = 0, cap_hh_start = 0, cap_rect_start = 0,
        cap_hw_end = 0, cap_hh_end = 0, cap_rect_end = 0, bend = 1
    ),
    draw_key = ggplot2::draw_key_path,

    draw_panel = function(data, panel_params, coord, angle = 45,
                          arrow = NULL, label_parse = TRUE) {
        coords <- coord$transform(data, panel_params)
        grid::gTree(
            coords = coords, angle = angle, arrow = arrow,
            label_parse = label_parse, cl = "dag_arc_grob"
        )
    }
)

#' @keywords internal
#' @noRd
#' Is a point inside a node's cap? Ellipse for the round markers, rectangle
#' for the manifest boxes, so the arc stops at the outline it actually meets.
dag_in_cap <- function(px, py, cx, cy, hw, hh, rect) {
    if (hw <= 0 || hh <= 0) {
        return(rep(FALSE, length(px)))
    }
    dx <- abs(px - cx)
    dy <- abs(py - cy)
    if (rect > 0.5) dx <= hw & dy <= hh else (dx / hw)^2 + (dy / hh)^2 <= 1
}

#' @exportS3Method grid::makeContent
makeContent.dag_arc_grob <- function(x) {
    co <- x$coords
    mm <- function(v, dim) {
        u <- grid::unit(v, "npc")
        if (dim == "x") grid::convertX(u, "mm", valueOnly = TRUE) else grid::convertY(u, "mm", valueOnly = TRUE)
    }
    # the conversion happens here, when the panel viewport is current, which is
    # what makes the bend independent of the figure's size and shape
    x0 <- mm(co$x, "x")
    y0 <- mm(co$y, "y")
    x1 <- mm(co$xend, "x")
    y1 <- mm(co$yend, "y")
    theta <- x$angle * pi / 180

    grobs <- lapply(seq_len(nrow(co)), function(i) {
        dx <- x1[i] - x0[i]
        dy <- y1[i] - y0[i]
        len <- sqrt(dx^2 + dy^2)
        if (!is.finite(len) || len < 1e-6) {
            return(grid::nullGrob())
        }

        # circular arc whose tangent leaves the chord at `theta` at both ends,
        # bulging along the normal (-dy, dx), or against it when `bend` is -1
        nx <- -dy / len * co$bend[i]
        ny <- dx / len * co$bend[i]
        radius <- (len / 2) / sin(theta)
        cx <- (x0[i] + x1[i]) / 2 - nx * radius * cos(theta)
        cy <- (y0[i] + y1[i]) / 2 - ny * radius * cos(theta)

        a0 <- atan2(y0[i] - cy, x0[i] - cx)
        # the arc spans 2 * theta, clockwise when it bulges to the left of the
        # chord. Set explicitly, since past 90 degrees it is the major arc and
        # taking the shorter way round would draw the wrong half.
        sweep <- -2 * theta * co$bend[i]
        angles <- seq(a0, a0 + sweep, length.out = 120)
        px <- cx + radius * cos(angles)
        py <- cy + radius * sin(angles)

        # trim what falls inside either node, so the arrowheads sit outside it
        keep <- !dag_in_cap(px, py, x0[i], y0[i], co$cap_hw_start[i] + 0.6,
            co$cap_hh_start[i] + 0.6, co$cap_rect_start[i]) &
            !dag_in_cap(px, py, x1[i], y1[i], co$cap_hw_end[i] + 0.6,
                co$cap_hh_end[i] + 0.6, co$cap_rect_end[i])
        if (sum(keep) < 2L) {
            return(grid::nullGrob())
        }
        px <- px[keep]
        py <- py[keep]

        gp <- grid::gpar(
            col = ggplot2::alpha(co$colour[i], co$alpha[i]),
            # a closed arrowhead takes its fill from gpar, not col
            fill = ggplot2::alpha(co$colour[i], co$alpha[i]),
            lwd = co$linewidth[i] * ggplot2::.pt,
            lty = co$linetype[i]
        )
        arc <- grid::polylineGrob(
            x = grid::unit(px, "mm"), y = grid::unit(py, "mm"),
            gp = gp, arrow = x$arrow
        )

        label <- co$label[i]
        if (is.null(label) || is.na(label) || !nzchar(label)) {
            return(arc)
        }
        # place it along the curve, turned to follow the tangent and nudged
        # clear of the line, the same treatment the straight edges get
        at <- max(2L, min(length(px) - 1L, round(co$label_pos[i] * length(px))))
        rot <- atan2(py[at + 1L] - py[at - 1L], px[at + 1L] - px[at - 1L]) * 180 / pi
        if (rot > 90) rot <- rot - 180
        if (rot < -90) rot <- rot + 180
        dodge <- 1.8
        text <- grid::textGrob(
            label = if (label_parse <- x$label_parse) {
                tryCatch(parse(text = label), error = function(e) label)
            } else {
                label
            },
            x = grid::unit(px[at] - sin(rot * pi / 180) * dodge, "mm"),
            y = grid::unit(py[at] + cos(rot * pi / 180) * dodge, "mm"),
            rot = rot,
            gp = grid::gpar(col = co$label_colour[i], fontsize = co$label_size[i] * ggplot2::.pt)
        )
        grid::gTree(children = grid::gList(arc, text))
    })

    grid::setChildren(x, do.call(grid::gList, grobs))
}

#' @keywords internal
#' @noRd
geom_dag_arc <- function(mapping = NULL, data = NULL, ...,
                         angle = 45, arrow = NULL, label_parse = TRUE,
                         inherit.aes = FALSE) {
    ggplot2::layer(
        geom = GeomDagArc, mapping = mapping, data = data,
        stat = "identity", position = "identity",
        inherit.aes = inherit.aes,
        params = list(angle = angle, arrow = arrow, label_parse = label_parse, ...)
    )
}
