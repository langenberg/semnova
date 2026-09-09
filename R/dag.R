# columns referenced inside ggplot2::aes()/ggraph filters; declared so R CMD
# check does not read them as undefined globals
utils::globalVariables(c(
    "x", "y", "shape", "node_label", "edge_label", "edge_type", "group_label",
    "arrow_ends", "from", "to", "start_cap", "end_cap", "label_pos",
    "xend", "yend", "cap_hw_start", "cap_hh_start", "cap_rect_start",
    "cap_hw_end", "cap_hh_end", "cap_rect_end", "label"
))

#' @keywords internal
#' @noRd
#' Node table for the path diagram, in the pre-rewrite layout.
#'
#' Columns, left to right: residual (epsilon) nodes, indicators, latent
#' constructs (etas), contrast variables (pis), covariates, covariate
#' indicators, their residual nodes; with the intercept triangles outside
#' each end. Each latent sits at the vertical centre of its own block of
#' indicators, and the eta and covariate halves are centred against each
#' other. This placement is deliberately identical to the original
#' implementation.
dag_node_table <- function(fit, residuals = TRUE, intercepts = TRUE) {
    etas <- fit$etas_mmodel
    eta_names <- names(etas)
    n_eta <- length(etas)
    n_ind_eta <- length(etas[[1L]])
    latent_dv <- n_ind_eta > 1L

    covariates <- fit$covariates_mmodel
    n_ind_cov <- if (length(covariates) == 0L) integer(0) else vapply(covariates, length, integer(1L))

    n_ind_eta_total <- n_eta * n_ind_eta
    n_ind_cov_total <- sum(n_ind_cov)
    start_eta <- 1 + max(0, (n_ind_cov_total - n_ind_eta_total) / 2)
    start_cov <- 1 + max(0, (n_ind_eta_total - n_ind_cov_total) / 2)

    add <- function(nodes, name, x, y, shape, label) {
        rbind(nodes, data.frame(
            name = name, x = x, y = y, shape = shape, node_label = label,
            stringsAsFactors = FALSE
        ))
    }
    nodes <- data.frame(
        name = character(), x = numeric(), y = numeric(),
        shape = character(), node_label = character(), stringsAsFactors = FALSE
    )

    # --- eta side ---------------------------------------------------------
    for (e in seq_len(n_eta)) {
        indicators <- etas[[e]]
        centre <- start_eta + (e - 1L) * n_ind_eta + (n_ind_eta - 1) / 2

        if (latent_dv) {
            nodes <- add(nodes, eta_names[e], 2, centre, "latent", sprintf("eta[%d]", e))
        }
        # pi indices are 0-based: pi0 carries the intercept contrast
        nodes <- add(nodes, fit$pis[e], 3, centre, "latent", sprintf("pi[%d]", e - 1L))

        for (k in seq_along(indicators)) {
            y_k <- start_eta + (e - 1L) * n_ind_eta + k - 1L
            nodes <- add(nodes, indicators[k], if (latent_dv) 1 else 2, y_k, "manifest", indicators[k])
            if (residuals) {
                nodes <- add(nodes, paste0(".eps_", indicators[k]), 0, y_k, "latent", "epsilon")
            }
        }
    }

    # --- covariate side ---------------------------------------------------
    if (length(covariates) > 0L) {
        for (j in seq_along(covariates)) {
            indicators <- covariates[[j]]
            inner <- start_cov + sum(n_ind_cov[seq_along(n_ind_cov) < j])

            if (length(indicators) > 1L) {
                nodes <- add(nodes, names(covariates)[j], 4, inner + (length(indicators) - 1) / 2,
                    "latent", names(covariates)[j])
                for (k in seq_along(indicators)) {
                    y_k <- inner + k - 1L
                    nodes <- add(nodes, indicators[k], 5, y_k, "manifest", indicators[k])
                    if (residuals) {
                        nodes <- add(nodes, paste0(".eps_", indicators[k]), 6, y_k, "latent", "epsilon")
                    }
                }
            } else {
                nodes <- add(nodes, names(covariates)[j], 4, inner, "manifest", names(covariates)[j])
            }
        }
    }

    # --- intercept triangles ----------------------------------------------
    # The original draws a left triangle only when the DV is latent (a
    # manifest DV has no indicator intercepts to collect), and always a right
    # one, which carries the pi intercepts and any covariate.
    if (intercepts) {
        if (latent_dv) {
            nodes <- add(nodes, ".int_left", -1,
                start_eta + (n_ind_eta_total - 1) / 2, "intercept", "1")
        }
        right_x <- if (n_ind_cov_total == 0L) 4 else if (max(n_ind_cov) == 1L) 5 else 7
        nodes <- add(nodes, ".int_right", right_x,
            start_cov + (n_ind_cov_total - 1) / 2, "intercept", "1")
    }

    nodes$y <- -nodes$y + max(nodes$y) # drawn top to bottom, as the original did
    nodes
}

#' @keywords internal
#' @noRd
#' Edge table for one group, derived from [model_parameters()].
dag_edge_table <- function(fit, nodes, group, residuals = TRUE, intercepts = TRUE, digits = 2) {
    pars <- model_parameters(fit)
    pars <- pars[!is.na(pars$group) & pars$group == group, , drop = FALSE]

    label_of <- function(est, free, label = NULL) {
        value <- formatC(est, digits = digits, format = "f")
        # the naming engine owns the label format, in both directions: it
        # writes them into the lavaan syntax and renders them back as maths
        math <- if (is.null(label)) rep("", length(est)) else pretty_label(label, format = "plotmath")
        # a fixed parameter shows a ringed value; a free one shows its
        # parameter name set as maths, then its estimate, as the original did
        ifelse(
            !free, paste0("ring(", value, ")"),
            ifelse(nzchar(math), paste0(math, " == ", value), value)
        )
    }

    edges <- data.frame(
        from = character(), to = character(), edge_type = character(),
        edge_label = character(), arrow_ends = character(), label_pos = numeric(),
        stringsAsFactors = FALSE
    )
    push <- function(edges, from, to, type, label, ends = "last") {
        keep <- from %in% nodes$name & to %in% nodes$name
        if (!any(keep)) {
            return(edges)
        }
        rbind(edges, data.frame(
            from = from[keep], to = to[keep], edge_type = type,
            edge_label = label[keep], arrow_ends = ends,
            # paths leaving a pi (pi =~ eta, or pi =~ indicator for a manifest
            # DV) span the whole diagram; their labels are placed a third of
            # the way along instead of at the midpoint, where they collide
            label_pos = ifelse(from[keep] %in% fit$pis, 0.3, 0.5),
            stringsAsFactors = FALSE
        ))
    }

    # measurement: the latent points at what it measures
    m <- pars[pars$type == "measurement", , drop = FALSE]
    edges <- push(edges, m$lhs, m$rhs, "measurement", label_of(m$est, m$free, m$label))

    # regression: the predictor points at the outcome
    r <- pars[pars$type == "regression", , drop = FALSE]
    edges <- push(edges, r$rhs, r$lhs, "regression", label_of(r$est, r$free, r$label))

    if (intercepts) {
        i <- pars[pars$type == "intercept" & pars$free, , drop = FALSE]
        # intercepts of the eta-side variables come from the left triangle,
        # covariate-side ones from the right
        # indicator intercepts hang off the left triangle; everything else
        # (the pis, any covariate) off the right one
        eta_indicators <- unlist(fit$etas_mmodel, use.names = FALSE)
        from <- ifelse(i$lhs %in% eta_indicators, ".int_left", ".int_right")
        edges <- push(edges, from, i$lhs, "intercept", label_of(i$est, i$free, i$label))
    }

    if (residuals) {
        # A (co)variance fixed to zero is not a relationship worth drawing --
        # the original filtered these out, and in a manifest model they are
        # numerous (every eta pair is fixed to 0), burying the diagram.
        drop_fixed <- function(d) d[d$free, , drop = FALSE]

        # latent variances as self-loops
        v <- drop_fixed(pars[pars$type == "variance" & pars$lhs == pars$rhs, , drop = FALSE])
        edges <- push(edges, v$lhs, v$lhs, "variance", label_of(v$est, v$free, v$label), ends = "both")

        # residual variance: an arrow from the indicator's epsilon node, plus
        # the epsilon's own variance as a self-loop, the original's style
        rv <- drop_fixed(pars[pars$type == "resid_var" & pars$lhs == pars$rhs, , drop = FALSE])
        eps <- paste0(".eps_", rv$lhs)
        edges <- push(edges, eps, rv$lhs, "resid_var", rep("", nrow(rv)))
        edges <- push(edges, eps, eps, "resid_var", label_of(rv$est, rv$free, rv$label), ends = "both")

        cv <- drop_fixed(pars[pars$type == "covariance" & pars$lhs != pars$rhs, , drop = FALSE])
        edges <- push(edges, cv$lhs, cv$rhs, "covariance", label_of(cv$est, cv$free, cv$label), ends = "both")

        rc <- drop_fixed(pars[pars$type == "resid_cov" & pars$lhs != pars$rhs, , drop = FALSE])
        edges <- push(edges, paste0(".eps_", rc$lhs), paste0(".eps_", rc$rhs),
            "resid_cov", label_of(rc$est, rc$free, rc$label), ends = "both")
    }

    edges
}

#' @keywords internal
#' @noRd
#' Physical size, in centimetres, of one panel of the diagram.
#'
#' Node spacing only means something in physical units once the panel stops
#' resizing itself to fit the data, so scaling the node table has to be paired
#' with a panel size derived from it. A unit of spacing is given the room its
#' contents need: a row must hold the taller of a node marker and a label box,
#' a column the widest variable name plus the arrow reaching it. The node
#' coordinates arrive already scaled, so the panel grows with them and the
#' centimetres per unit stay put, which is what makes the scaling physical.
dag_panel_size <- function(nodes, node_size, text_size) {
    # the multipliers mirror the panel expansion set in dag()
    span_x <- diff(range(nodes$x)) * 1.6
    span_y <- diff(range(nodes$y)) * 1.2

    row_mm <- max(node_size * 0.75, text_size * 3) + 7
    manifest <- nodes$node_label[nodes$shape == "manifest"]
    name_mm <- if (length(manifest) > 0L) max(nchar(manifest)) * text_size * 0.62 else 0
    # names overhang their column, as they already do, 0.8 of the width is
    # enough to keep neighbouring boxes apart
    col_mm <- max(name_mm * 0.8 + 8, 22)

    c(width = span_x * col_mm, height = span_y * row_mm) / 10
}

#' Build the path diagram
#'
#' @description
#' The shared machinery behind [dag()] and the retired [dag_fixed()]: it
#' draws the fitted model as a SEM path diagram, in semnova's column
#' layout: residual (epsilon) nodes, indicators, the latent constructs
#' (etas), the contrast variables (pis), then covariates and their
#' indicators, with intercept triangles at each end.
#'
#' Standard SEM conventions: single-headed arrows for directed effects
#' (loadings, regressions, intercepts), double-headed arrows for
#' (co)variances, self-loops for variances. Squares are manifest
#' variables, circles latent ones, triangles the constant. Fixed
#' parameters are marked with a ring accent over the estimate.
#'
#' Multigroup fits are drawn as facets, one per group, labelled with the
#' group name. Nodes stay perfectly circular at any figure size, see
#' [dag_semplot()] for a `semPlot`-rendered alternative.
#'
#' @param fit An [Lgc] object.
#' @param what Which parts of the model to draw. Any of `"measurement"`,
#'   `"regression"`, `"variance"`, `"covariance"`, `"resid_var"`,
#'   `"resid_cov"`, `"intercept"`, or `"all"` for everything. The default
#'   is the measurement and regression structure. It keeps the diagram
#'   readable, since a fully crossed design has a great many (co)variances.
#' @param labels Which edges to label: `"all"` (default) labels every
#'   parameter drawn, with fixed ones shown inside a ring accent so the
#'   design is distinguishable from the estimates at a glance; `"free"`
#'   labels only freely estimated parameters, which thins out a dense
#'   diagram, the contrast loadings between the pi and eta layers are
#'   fixed, numerous, and all converge on the same nodes; `"none"` labels
#'   nothing, leaving the structure alone.
#' @param digits Digits for edge labels.
#' @param x_scale,y_scale Stretch the node positions horizontally and
#'   vertically. Only their ratio matters. They are normalised so the
#'   smaller is 1 and the larger scales up from it, since stretching both
#'   equally is the same diagram. `x_scale = 1.5` puts the columns half
#'   again as far apart while the rows keep their spacing; `y_scale = 1.5`
#'   does the same for the rows. Both `1` (the default) leaves the diagram
#'   fitted to whatever figure it is given.
#' @param curve_scale How pronounced the curved (co)variance paths are.
#'   Self-loops and arcs are drawn at a fixed physical size and a fixed bend
#'   angle. `x_scale`/`y_scale` and the figure size leave them alone, so
#'   this is the only thing that changes them. `variance_scale` and
#'   `covariance_scale` default to it and can be set separately: the first
#'   scales the radius of the variance self-loops, the second the angle at
#'   which a covariance path leaves its node.
#' @param variance_scale,covariance_scale Per-kind overrides of
#'   `curve_scale`, which they follow unless given.
#' @param node_size,text_size,edge_text_size Sizes for the node markers and
#'   the two kinds of text, in millimetres.
#'
#' @details
#' **Spacing.** With no scaling the diagram fits itself to the figure:
#' widen the figure and the columns move apart, heighten it and the rows
#' do. That is usually all a crowded diagram needs, and it never clips.
#'
#' Scaling changes the deal. Since only one diagram can fit a given figure,
#' `x_scale`/`y_scale` pin the drawing to a physical size instead, each
#' unit of node spacing becomes a fixed number of centimetres, multiplied
#' by the scaling factor, and it then needs a figure big enough to hold
#' it, or it is cropped rather than squeezed. The size it wants, in inches,
#' comes back on the plot as `attr(p, "fig_size")`; pass those to
#' `fig.width`/`fig.height` or to [ggplot2::ggsave()].
#'
#' @return A `ggplot` object. When scaled, it carries a `fig_size`
#'   attribute: the width and height in inches the diagram needs, and the
#'   panel is pinned to that size.
#' @keywords internal
dag_build <- function(
    fit,
    what = c("measurement", "regression"),
    labels = c("all", "free", "none"),
    digits = 2,
    x_scale = 1,
    y_scale = 1,
    curve_scale = 1,
    variance_scale = curve_scale,
    covariance_scale = curve_scale,
    node_size = 9,
    text_size = 2.6,
    edge_text_size = 2.2
) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }
    for (pkg in c("ggplot2", "ggraph", "tidygraph")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop_semnova(
                "path diagrams require the '", pkg, "' package; install it with install.packages(\"", pkg, "\")"
            )
        }
    }
    labels <- match.arg(labels)
    check_positive(x_scale, "x_scale")
    check_positive(y_scale, "y_scale")
    check_positive(variance_scale, "variance_scale")
    check_positive(covariance_scale, "covariance_scale")
    check_positive(node_size, "node_size")
    check_positive(text_size, "text_size")
    check_positive(edge_text_size, "edge_text_size")

    components <- c(
        "measurement", "regression", "variance", "covariance",
        "resid_var", "resid_cov", "intercept"
    )
    check_character(what, "what")
    if (identical(what, "all")) {
        what <- components
    }
    unknown <- setdiff(what, components)
    if (length(unknown) > 0L) {
        stop_semnova(
            "`what` must be drawn from: ", paste(c(components, "all"), collapse = ", "),
            ". Unknown: ", paste(unknown, collapse = ", ")
        )
    }

    # the (co)variance block covers all four of these types, so asking for any
    # one of them must switch it on, gating it on resid_* alone silently
    # dropped `what = "variance"`
    variances <- any(c("variance", "covariance", "resid_var", "resid_cov") %in% what)
    residuals <- any(c("resid_var", "resid_cov") %in% what)
    intercepts <- "intercept" %in% what

    base_nodes <- dag_node_table(fit, residuals = residuals, intercepts = intercepts)
    # Stretching both directions equally is the same diagram, the panel only
    # ever holds one, so normalise to the ratio: the tighter direction keeps
    # its spacing at 1 and the other grows away from it.
    scale <- c(x_scale, y_scale) / min(x_scale, y_scale)
    base_nodes$x <- base_nodes$x * scale[1L]
    base_nodes$y <- base_nodes$y * scale[2L]

    # one copy of the graph per group, so facets show each group's own
    # estimates over an identical layout
    nodes <- do.call(rbind, lapply(fit$group_labels, function(g) {
        n <- base_nodes
        n$group_label <- g
        n$key <- paste0(g, "@@", n$name)
        n
    }))
    edges <- do.call(rbind, lapply(fit$group_labels, function(g) {
        e <- dag_edge_table(fit, base_nodes, g,
            residuals = variances, intercepts = intercepts, digits = digits
        )
        e <- e[e$edge_type %in% what, , drop = FALSE]
        if (nrow(e) == 0L) {
            return(NULL)
        }
        e$group_label <- g
        e$from <- paste0(g, "@@", e$from)
        e$to <- paste0(g, "@@", e$to)
        e
    }))
    if (is.null(edges) || nrow(edges) == 0L) {
        stop_semnova("nothing to draw: no parameters of type ", paste(what, collapse = "/"), " in this model")
    }

    # drop nodes nothing connects to, so unused epsilon/intercept nodes don't
    # leave empty markers floating in the diagram
    connected <- unique(c(edges$from, edges$to))
    nodes <- nodes[nodes$key %in% connected, , drop = FALSE]

    if (labels == "none") {
        edges$edge_label <- ""
    } else if (labels == "free") {
        edges$edge_label[grepl("^ring\\(", edges$edge_label)] <- ""
    }

    arrow_single <- ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed", ends = "last")
    arrow_double <- ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed", ends = "both")

    # Caps shorten each edge so its arrowhead lands *outside* the node rather
    # than under it: circles for the fixed-size latent markers, text-sized
    # rectangles for the manifest boxes, which are as wide as their names.
    node_cap <- function(keys) {
        idx <- match(keys, nodes$key)
        caps <- ggraph::circle(rep(node_size * 0.037, length(idx)), "cm")
        manifest <- nodes$shape[idx] == "manifest"
        if (any(manifest)) {
            caps[manifest] <- ggraph::label_rect(
                nodes$node_label[idx][manifest],
                fontsize = text_size * 2.9
            )
        }
        caps
    }
    edges$start_cap <- node_cap(edges$from)
    edges$end_cap <- node_cap(edges$to)

    # The same outlines as millimetre half-extents, for the curved (co)variance
    # paths, which are drawn by our own geom rather than by ggraph.
    node_cap_mm <- function(keys) {
        idx <- match(keys, nodes$key)
        manifest <- nodes$shape[idx] == "manifest"
        data.frame(
            hw = ifelse(manifest, nchar(nodes$node_label[idx]) * text_size * 0.31 + 1.5, node_size * 0.375),
            hh = ifelse(manifest, text_size * 1.1, node_size * 0.375),
            rect = as.numeric(manifest)
        )
    }

    graph <- tidygraph::tbl_graph(
        nodes = nodes[c("key", "name", "x", "y", "shape", "node_label", "group_label")],
        edges = edges[c(
            "from", "to", "edge_type", "edge_label", "arrow_ends",
            "group_label", "start_cap", "end_cap", "label_pos"
        )],
        node_key = "key", directed = TRUE
    )

    is_loop <- edges$from == edges$to
    p <- ggraph::ggraph(graph, layout = "manual", x = nodes$x, y = nodes$y)

    if (any(!is_loop & edges$arrow_ends == "last")) {
        p <- p + ggraph::geom_edge_link(
            ggplot2::aes(
                filter = arrow_ends == "last" & from != to, label = edge_label,
                start_cap = start_cap, end_cap = end_cap, label_pos = label_pos
            ),
            colour = "black", arrow = arrow_single,
            label_parse = TRUE, label_size = edge_text_size, angle_calc = "along",
            label_dodge = ggplot2::unit(2, "mm"), label_colour = "grey20"
        )
    }
    if (any(!is_loop & edges$arrow_ends == "both")) {
        # Covariances curve, and the curve is drawn by our own geom for the
        # same reason the loops are: ggraph bends an arc by a fraction of the
        # chord *in data units*, so the bend collapses as soon as x and y are
        # scaled differently. See R/dag-arc.R.
        arcs <- edges[!is_loop & edges$arrow_ends == "both", , drop = FALSE]
        from_i <- match(arcs$from, nodes$key)
        to_i <- match(arcs$to, nodes$key)
        arcs$x <- nodes$x[from_i]
        arcs$y <- nodes$y[from_i]
        arcs$xend <- nodes$x[to_i]
        arcs$yend <- nodes$y[to_i]
        start_cap <- node_cap_mm(arcs$from)
        end_cap <- node_cap_mm(arcs$to)
        arcs$cap_hw_start <- start_cap$hw
        arcs$cap_hh_start <- start_cap$hh
        arcs$cap_rect_start <- start_cap$rect
        arcs$cap_hw_end <- end_cap$hw
        arcs$cap_hh_end <- end_cap$hh
        arcs$cap_rect_end <- end_cap$rect

        p <- p + geom_dag_arc(
            data = arcs,
            mapping = ggplot2::aes(
                x = x, y = y, xend = xend, yend = yend,
                label = edge_label, label_pos = label_pos,
                cap_hw_start = cap_hw_start, cap_hh_start = cap_hh_start,
                cap_rect_start = cap_rect_start,
                cap_hw_end = cap_hw_end, cap_hh_end = cap_hh_end,
                cap_rect_end = cap_rect_end
            ),
            angle = 18 * covariance_scale,
            arrow = arrow_double, label_size = edge_text_size, colour = "black"
        )
    }
    if (any(is_loop)) {
        # Loops are drawn by our own geom, in millimetres anchored at the
        # node, rather than ggraph's data-unit loop. See R/dag-loop.R for
        # why (they otherwise flatten and balloon when the figure is resized).
        loops <- edges[is_loop, , drop = FALSE]
        loops$x <- nodes$x[match(loops$from, nodes$key)]
        loops$y <- nodes$y[match(loops$from, nodes$key)]

        p <- p + geom_dag_loop(
            data = loops,
            mapping = ggplot2::aes(x = x, y = y, label = edge_label),
            # a ggplot2 point of `size` has that many mm of *fontsize*, but the
            # drawn circle glyph is only ~0.75 of it, so the visible radius is
            # size * 0.375, assuming size/2 leaves the loop floating detached
            node_radius = node_size * 0.375,
            loop_radius = node_size * 0.34 * variance_scale,
            arrow = arrow_double,
            label_size = edge_text_size,
            colour = "black"
        )
    }

    p <- p +
        # latent variables and the constant: fixed-size markers, so a circle
        # stays a circle no matter how the figure is resized
        ggraph::geom_node_point(
            ggplot2::aes(filter = shape != "manifest", shape = shape),
            size = node_size, fill = "white", colour = "grey20", stroke = 0.6
        ) +
        ggraph::geom_node_text(
            ggplot2::aes(filter = shape != "manifest", label = node_label),
            parse = TRUE, size = text_size
        ) +
        # manifest variables: boxes that size themselves to the variable name,
        # rather than clipping it. Drawn with ggplot2 directly on the node
        # table (the ggraph layout shares its x/y), because
        # ggraph::geom_node_label() warns about an unknown `label.size`.
        ggplot2::geom_label(
            data = nodes[nodes$shape == "manifest", , drop = FALSE],
            mapping = ggplot2::aes(x = x, y = y, label = node_label),
            size = text_size, fill = "white", colour = "grey20",
            label.r = ggplot2::unit(0, "lines"),
            label.padding = ggplot2::unit(0.18, "lines")
        ) +
        ggplot2::scale_shape_manual(
            values = c(latent = 21, intercept = 24),
            guide = "none"
        ) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.3)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
        # loops and labels are drawn in millimetres beyond their node, so they
        # must be allowed to overhang the panel rather than being clipped
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_void() +
        ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", size = 10, margin = ggplot2::margin(4, 4, 4, 4)),
            plot.margin = ggplot2::margin(16, 8, 8, 8)
        )

    if (length(fit$group_labels) > 1L) {
        # The facet strip is kept for the split but not for the text: its
        # distance from the diagram is the panel's expansion, which is
        # proportional to the data span, while what overhangs the top of the
        # panel (loops, their labels) is a fixed number of millimetres. Scaling
        # therefore walks the heading into the diagram. Drawing it ourselves at
        # a millimetre offset above the topmost node fixes the gap for good.
        headings <- data.frame(
            x = mean(range(nodes$x)),
            y = max(nodes$y),
            label = fit$group_labels,
            group_label = fit$group_labels,
            stringsAsFactors = FALSE
        )
        loop_reach <- if (any(is_loop)) node_size * 0.34 * variance_scale * 1.75 + 4 else 0
        p <- p +
            ggplot2::facet_wrap(~group_label) +
            ggplot2::theme(strip.text = ggplot2::element_blank()) +
            geom_dag_heading(
                data = headings,
                mapping = ggplot2::aes(x = x, y = y, label = label),
                offset = node_size * 0.375 + loop_reach + 5
            )
    }

    if (!identical(scale, c(1, 1))) {
        # Scaled coordinates alone change nothing: each axis rescales to its own
        # data, so the nodes land back where they started. Pinning the panel to
        # a physical size is what turns the scaling into real distance.
        panel <- dag_panel_size(nodes, node_size, text_size)
        if (!"panel.widths" %in% names(formals(ggplot2::theme))) {
            stop_semnova(
                "`x_scale`/`y_scale` need ggplot2 >= 4.0.0 for absolute panel sizing; ",
                "with ", as.character(utils::packageVersion("ggplot2")),
                " installed, size the figure itself instead"
            )
        }
        p <- p + ggplot2::theme(
            panel.widths = ggplot2::unit(panel[["width"]], "cm"),
            panel.heights = ggplot2::unit(panel[["height"]], "cm")
        )

        # A panel bigger than the figure is cropped, not shrunk, so hand back
        # the figure size that fits it, margins, strips and panel spacing in.
        n_panels <- length(fit$group_labels)
        attr(p, "fig_size") <- round(c(
            width = n_panels * panel[["width"]] / 2.54 + (n_panels - 1) * 0.2 + 0.4,
            height = panel[["height"]] / 2.54 + 0.4 + if (n_panels > 1L) 0.4 else 0
        ), 1)
    }

    p
}

#' Plot a model's path diagram
#'
#' @description
#' Draws the fitted model as a SEM path diagram, in semnova's column
#' layout: residual (epsilon) nodes, indicators, the latent constructs
#' (etas), the contrast variables (pis), then covariates and their
#' indicators, with intercept triangles at each end.
#'
#' Standard SEM conventions: single-headed arrows for directed effects
#' (loadings, regressions, intercepts), double-headed arrows for
#' (co)variances, self-loops for variances. Squares are manifest
#' variables, circles latent ones, triangles the constant. Fixed
#' parameters are marked with a ring accent over the estimate.
#'
#' Multigroup fits are drawn one panel per group, each headed with the
#' group name. Nodes stay perfectly circular, and the curved paths keep
#' their bend, at any figure size.
#'
#' @inheritParams dag_build
#'
#' @details
#' **Spacing.** `x_scale` and `y_scale` stretch the node positions, and
#' only their ratio matters: they are normalised so the tighter direction
#' keeps its spacing at 1 and the other grows away from it, since
#' stretching both equally is the same diagram.
#'
#' The scaling fixes the *shape* of the diagram, how far apart the
#' columns sit relative to the rows, and the whole drawing is then fitted
#' to whatever figure it is given, shrinking uniformly rather than being
#' cropped. The spacing is exact at `attr(p, "fig_size")`, the width and
#' height in inches the diagram was built for; below that you get a
#' scaled-down copy, and since the markers and text keep their fixed
#' millimetre size, a small figure crowds again. So for a dense model:
#' scale for the shape, and give it a figure at least `fig_size` for the
#' room.
#'
#' @return A `ggplot` object, carrying a `fig_size` attribute: the width
#'   and height in inches at which the spacing is exact.
#' @export
dag <- function(
    fit,
    what = c("measurement", "regression"),
    labels = c("all", "free", "none"),
    digits = 2,
    x_scale = 1,
    y_scale = 1,
    curve_scale = 1,
    variance_scale = curve_scale,
    covariance_scale = curve_scale,
    node_size = 9,
    text_size = 2.6,
    edge_text_size = 2.2
) {
    p <- dag_build(
        fit,
        what = what, labels = labels, digits = digits,
        x_scale = x_scale, y_scale = y_scale,
        curve_scale = curve_scale, variance_scale = variance_scale,
        covariance_scale = covariance_scale,
        node_size = node_size, text_size = text_size,
        edge_text_size = edge_text_size
    )

    nodes <- p$data
    panel <- dag_panel_size(nodes, node_size, text_size)
    span_x <- diff(range(nodes$x)) * 1.6
    span_y <- diff(range(nodes$y)) * 1.2

    # Drop the absolute panel that dag_build() sets when scaled: it is what
    # crops a diagram too big for its figure. The spacing is then carried by
    # the coordinate system instead, which ggplot2 fits to any figure.
    # `%+replace%` rather than subsetting, which would hand back a bare list
    # and fail the plot's own validity check; called by name, since ggplot2 is
    # a conditional Suggests and nothing is imported into the namespace.
    p$theme <- ggplot2::`%+replace%`(
        p$theme,
        ggplot2::theme(panel.widths = NULL, panel.heights = NULL)
    )

    # coord_fixed's ratio is y over x, so this is centimetres per unit of
    # vertical spacing over centimetres per unit of horizontal spacing, the
    # same geometry the panel gave, expressed as a shape rather than a size.
    ratio <- (panel[["height"]] / span_y) / (panel[["width"]] / span_x)
    p <- suppressMessages(p + ggplot2::coord_fixed(ratio = ratio, clip = "off"))

    n_panels <- length(fit$group_labels)
    attr(p, "fig_size") <- round(c(
        width = n_panels * panel[["width"]] / 2.54 + (n_panels - 1) * 0.2 + 0.4,
        height = panel[["height"]] / 2.54 + 0.4 + if (n_panels > 1L) 0.4 else 0
    ), 1)
    p
}
