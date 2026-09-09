latent_fit <- function() {
    rl <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    suppressWarnings(semnova(
        data = rl, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"), covariates = "iq",
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
}

manifest_fit <- function() {
    suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
}

multigroup_fit <- function() {
    suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"), betweens = "sex",
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum", sex = "contr.treatment")
    ))
}

# ---- node placement -------------------------------------------------------

test_that("node placement uses the original column layout", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)

    col_of <- function(name) nodes$x[nodes$name == name]
    expect_equal(col_of(".eps_fix_count_grade1_sentence"), 0) # residuals
    expect_equal(col_of("fix_count_grade1_sentence"), 1) # indicators
    expect_equal(col_of(names(fit$etas_mmodel)[1]), 2) # etas
    expect_equal(col_of(fit$pis[1]), 3) # pis
    expect_equal(col_of("iq"), 4) # covariates
    expect_equal(col_of(".int_left"), -1) # intercept triangle
})

test_that("each eta and pi is centred on its own block of indicators", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = FALSE, intercepts = FALSE)

    for (e in seq_along(fit$etas_mmodel)) {
        ind_y <- nodes$y[nodes$name %in% fit$etas_mmodel[[e]]]
        expect_equal(nodes$y[nodes$name == names(fit$etas_mmodel)[e]], mean(ind_y), tolerance = 1e-8)
        expect_equal(nodes$y[nodes$name == fit$pis[e]], mean(ind_y), tolerance = 1e-8)
    }
})

test_that("a manifest DV has no separate eta column -- the indicator is the construct", {
    nodes <- semnova:::dag_node_table(manifest_fit(), residuals = FALSE, intercepts = FALSE)
    expect_equal(sort(unique(nodes$x)), c(2, 3))
})

test_that("node labels use greek with the original indexing (pi is 0-based)", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = FALSE, intercepts = FALSE)

    expect_true("eta[1]" %in% nodes$node_label)
    expect_true("pi[0]" %in% nodes$node_label) # pi0 carries the intercept contrast
    expect_true("pi[5]" %in% nodes$node_label)
    expect_false("pi[6]" %in% nodes$node_label)
})

test_that("shapes follow the original: squares manifest, circles latent, triangles the constant", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)

    expect_equal(nodes$shape[nodes$name == "fix_count_grade1_sentence"], "manifest")
    expect_equal(nodes$shape[nodes$name == names(fit$etas_mmodel)[1]], "latent")
    expect_equal(nodes$shape[nodes$name == ".int_left"], "intercept")
})

# ---- edges ----------------------------------------------------------------

test_that("edges point the way SEM conventions require, and mark fixed parameters with a ring", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)
    edges <- semnova:::dag_edge_table(fit, nodes, fit$group_labels[1])

    # measurement points from the latent to what it measures
    m <- edges[edges$edge_type == "measurement", ]
    expect_true(any(m$from %in% names(fit$etas_mmodel) & m$to %in% unlist(fit$etas_mmodel)))
    # regression points from the covariate to the pi it predicts
    r <- edges[edges$edge_type == "regression", ]
    expect_true(all(r$from == "iq"))
    expect_true(all(r$to %in% fit$pis))
    # (co)variances are double-headed
    expect_true(all(edges$arrow_ends[edges$edge_type %in% c("variance", "covariance")] == "both"))
    # fixed parameters carry a ring accent
    expect_true(any(grepl("^ring\\(", edges$edge_label)))
})

test_that("residual variance is drawn as an epsilon node feeding the indicator", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)
    edges <- semnova:::dag_edge_table(fit, nodes, fit$group_labels[1])

    rv <- edges[edges$edge_type == "resid_var", ]
    expect_true(any(grepl("^\\.eps_", rv$from) & rv$to %in% unlist(fit$etas_mmodel)))
    expect_true(any(rv$from == rv$to)) # the epsilon's own variance, as a self-loop
})

# ---- dag() ----------------------------------------------------------------

test_that("dag() returns a ggplot for latent, manifest and multigroup fits", {
    skip_if_not_installed("ggraph")
    for (fit in list(latent_fit(), manifest_fit(), multigroup_fit())) {
        expect_s3_class(dag(fit), "ggplot")
    }
})

test_that("dag() facets a multigroup fit, one panel per group", {
    skip_if_not_installed("ggraph")
    p <- dag(multigroup_fit())
    expect_s3_class(p$facet, "FacetWrap")
})

test_that("dag() defaults to the original's measurement + regression view", {
    skip_if_not_installed("ggraph")
    fit <- latent_fit()
    default_types <- unique(semnova:::dag_edge_table(
        fit, semnova:::dag_node_table(fit, FALSE, FALSE), fit$group_labels[1],
        residuals = FALSE, intercepts = FALSE
    )$edge_type)
    expect_setequal(default_types, c("measurement", "regression"))
})

test_that("dag(what = 'all') adds variances, residuals and intercepts", {
    skip_if_not_installed("ggraph")
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)
    types <- unique(semnova:::dag_edge_table(fit, nodes, fit$group_labels[1])$edge_type)

    expect_true(all(c("variance", "resid_var", "intercept") %in% types))
    expect_s3_class(dag(fit, what = "all"), "ggplot")
})

test_that("dag() validates `what` and its other arguments", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    expect_error(dag(fit, what = "nonsense"), "`what` must be drawn from")
    expect_error(dag(fit, labels = "sometimes"), "'arg' should be one of")
    expect_error(dag("not a fit"), "must be an Lgc")
})

test_that("dag() labels every parameter by default, fixed ones included", {
    skip_if_not_installed("ggraph")
    # the latent fit has free parameters in the default view (loadings, and the
    # covariate's regressions); a manifest one draws nothing but fixed contrast
    # loadings, so "free" and "none" would coincide there
    fit <- latent_fit()

    labelled <- function(...) {
        b <- ggplot2::ggplot_build(dag(fit, ...))
        sum(vapply(b$data, function(d) {
            if (!"label" %in% names(d)) return(0L)
            sum(nzchar(as.character(d$label)), na.rm = TRUE)
        }, integer(1)))
    }

    # the default is "all": the fixed contrast loadings are part of the model
    # the diagram claims to show, and the ring accent marks them as fixed
    expect_equal(eval(formals(dag)$labels)[1], "all")
    expect_equal(labelled(), labelled(labels = "all"))
    expect_gt(labelled(labels = "all"), labelled(labels = "free"))
    expect_gt(labelled(labels = "free"), labelled(labels = "none"))
})

test_that("dag() label modes control which estimates are shown", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    for (lb in c("free", "all", "none")) {
        expect_s3_class(dag(fit, labels = lb), "ggplot")
    }
})

# ---- dag_semplot() --------------------------------------------------------

test_that("the retired dag_semplot() still draws, and keeps its own layout helper", {
    skip_if_not_installed("semPlot")
    fit <- manifest_fit()
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)

    lay <- semnova:::dag_layout(fit)
    expect_equal(nrow(lay$coords), length(lay$names))
    expect_false(anyNA(lay$coords))

    png(tmp)
    out <- semnova:::dag_semplot(fit)
    dev.off()
    expect_true(!is.null(out))
})

# ---- rendering conventions (regressions on things that were wrong before) ---

test_that("directed paths are drawn straight, not as arcs", {
    skip_if_not_installed("ggraph")
    # the default view is measurement + regression, all directed, so the
    # plot should carry no arc layer at all
    p <- dag(manifest_fit())
    stat_classes <- vapply(p$layers, function(l) class(l$stat)[1], character(1))
    expect_true(any(grepl("Link", stat_classes)))
    expect_false(any(grepl("Arc", stat_classes)))
})

test_that("fixed (co)variances are not drawn, as in the original", {
    fit <- manifest_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)
    edges <- semnova:::dag_edge_table(fit, nodes, fit$group_labels[1])

    variance_like <- edges[edges$edge_type %in% c("variance", "covariance", "resid_var", "resid_cov"), ]
    # a zero-fixed (co)variance is not a relationship; in a manifest model
    # every eta pair is fixed to 0, which would otherwise bury the diagram
    expect_false(any(grepl("^ring\\(", variance_like$edge_label)))
})

test_that("a manifest DV gets no left intercept triangle, but does get the right one", {
    nodes <- semnova:::dag_node_table(manifest_fit(), residuals = FALSE, intercepts = TRUE)
    expect_false(".int_left" %in% nodes$name)
    expect_true(".int_right" %in% nodes$name)

    latent_nodes <- semnova:::dag_node_table(latent_fit(), residuals = FALSE, intercepts = TRUE)
    expect_true(".int_left" %in% latent_nodes$name)
})

test_that("indicator intercepts come from the left triangle, pi intercepts from the right", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = FALSE, intercepts = TRUE)
    edges <- semnova:::dag_edge_table(fit, nodes, fit$group_labels[1], residuals = FALSE)
    int <- edges[edges$edge_type == "intercept", ]

    indicators <- unlist(fit$etas_mmodel, use.names = FALSE)
    expect_true(all(int$from[int$to %in% indicators] == ".int_left"))
    expect_true(all(int$from[int$to %in% fit$pis] == ".int_right"))
})

test_that("edge labels are rendered by the naming engine, and are valid plotmath", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = TRUE, intercepts = TRUE)
    edges <- semnova:::dag_edge_table(fit, nodes, fit$group_labels[1])

    free <- edges$edge_label[!grepl("^ring\\(", edges$edge_label) & nzchar(edges$edge_label)]
    expect_true(any(grepl("^lambda\\[", free)))
    for (lab in unique(edges$edge_label[nzchar(edges$edge_label)])) {
        expect_silent(parse(text = lab))
    }
})

test_that("free parameters show name and estimate; fixed ones show a ringed value", {
    fit <- latent_fit()
    nodes <- semnova:::dag_node_table(fit, residuals = FALSE, intercepts = FALSE)
    edges <- semnova:::dag_edge_table(fit, nodes, fit$group_labels[1], residuals = FALSE, intercepts = FALSE)

    expect_true(any(grepl("==", edges$edge_label)))     # free: "lambda[..] == 1.05"
    expect_true(any(grepl("^ring\\(", edges$edge_label))) # fixed contrast loadings
    expect_silent(lapply(edges$edge_label[nzchar(edges$edge_label)], function(l) parse(text = l)))
})

test_that("variance loops keep their size and shape when the figure is resized", {
    # The loop is built from millimetre offsets around a single npc anchor, so
    # its *extent* comes entirely from those offsets and must be identical on
    # devices of different widths. ggraph's data-unit loop is not: it flattens
    # and balloons with the aspect ratio, which is what this guards against.
    df <- data.frame(
        x = 0.5, y = 0.5, colour = "grey35", linewidth = 0.4, linetype = 1,
        alpha = NA, label = "", label_size = 2.2, label_colour = "grey20",
        stringsAsFactors = FALSE
    )
    fake_coord <- list(transform = function(data, panel_params) data)
    grob <- semnova:::GeomDagLoop$draw_panel(df, NULL, fake_coord)
    arc <- grob$children[[1L]]

    extent_at <- function(width) {
        f <- tempfile(fileext = ".png")
        grDevices::png(f, width = width, height = 6, units = "in", res = 100)
        on.exit({
            grDevices::dev.off()
            unlink(f)
        }, add = TRUE)
        grid::grid.newpage()
        c(
            w = diff(range(grid::convertX(arc$x, "mm", valueOnly = TRUE))),
            h = diff(range(grid::convertY(arc$y, "mm", valueOnly = TRUE)))
        )
    }

    narrow <- extent_at(6)
    wide <- extent_at(14)

    expect_equal(unname(narrow[["w"]]), unname(wide[["w"]]), tolerance = 1e-6)
    expect_equal(unname(narrow[["h"]]), unname(wide[["h"]]), tolerance = 1e-6)
    # and it is a loop, not a flat line
    expect_gt(narrow[["w"]], 1)
    expect_gt(narrow[["h"]], 1)
})

test_that("dag() draws loops with the absolute-units geom, not ggraph's data-unit one", {
    skip_if_not_installed("ggraph")
    p <- dag(manifest_fit(), what = c("measurement", "variance"))
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true("GeomDagLoop" %in% geoms)
    expect_false(any(grepl("EdgeLoop", geoms)))
})

test_that("x_scale/y_scale move the nodes, and the retired dag_fixed() pins the panel", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    dag <- semnova:::dag_fixed # the sizing this test is about

    # plain columns: the layout tibble carries tidygraph attributes with pointers
    coords <- function(p) {
        d <- p$data[order(p$data$name), ]
        data.frame(x = as.numeric(d$x), y = as.numeric(d$y))
    }
    plain <- dag(fit)
    wide <- dag(fit, x_scale = 2)
    tall <- dag(fit, y_scale = 2)

    expect_equal(coords(wide)$x, coords(plain)$x * 2)
    expect_equal(coords(wide)$y, coords(plain)$y)
    expect_equal(coords(tall)$y, coords(plain)$y * 2)
    expect_equal(coords(tall)$x, coords(plain)$x)

    # Scaled coordinates alone are a no-op -- each axis rescales to its own data
    # -- so the scaling has to come with an absolute panel size to mean anything.
    expect_null(plain$theme$panel.widths)
    expect_s3_class(wide$theme$panel.widths, "unit")
    expect_s3_class(tall$theme$panel.heights, "unit")

    # and the figure size that panel needs comes back on the plot
    expect_null(attr(plain, "fig_size"))
    expect_named(attr(wide, "fig_size"), c("width", "height"))
    expect_gt(attr(wide, "fig_size")[["width"]], attr(plain2 <- dag(fit, x_scale = 1.5), "fig_size")[["width"]])
    expect_equal(attr(wide, "fig_size")[["height"]], attr(plain2, "fig_size")[["height"]])
})

test_that("only the ratio of the two scales matters", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    dag <- semnova:::dag_fixed # the panel assertion below is its behaviour
    # plain columns: the layout tibble carries tidygraph attributes with pointers
    coords <- function(p) {
        d <- p$data[order(p$data$name), ]
        data.frame(x = as.numeric(d$x), y = as.numeric(d$y))
    }

    # scaling both equally is the same diagram: the smaller is normalised to 1
    expect_equal(coords(dag(fit, x_scale = 3, y_scale = 3)), coords(dag(fit)))
    expect_null(dag(fit, x_scale = 3, y_scale = 3)$theme$panel.widths)

    expect_equal(coords(dag(fit, x_scale = 3, y_scale = 2)),
                 coords(dag(fit, x_scale = 1.5, y_scale = 1)))
})

test_that("dag() rejects sizes that are not single positive numbers", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    for (bad in list(0, -1, "2", c(1, 2), NA_real_, Inf)) {
        expect_error(dag(fit, x_scale = bad), "must be a single positive number")
        expect_error(dag(fit, y_scale = bad), "must be a single positive number")
        expect_error(dag(fit, node_size = bad), "must be a single positive number")
        expect_error(dag(fit, text_size = bad), "must be a single positive number")
        expect_error(dag(fit, edge_text_size = bad), "must be a single positive number")
    }
})

# ---- dag(): fits the figure, where dag_fixed() would crop -------------------

test_that("dag() carries the spacing in the coordinate system, not the panel", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()

    flex <- dag(fit, x_scale = 1.5)
    fixed <- semnova:::dag_fixed(fit, x_scale = 1.5)

    # same layout, same requested figure -- only the sizing mechanism differs
    expect_equal(as.numeric(flex$data$x), as.numeric(fixed$data$x))
    expect_equal(attr(flex, "fig_size"), attr(fixed, "fig_size"))

    # no absolute panel, so nothing can overflow the figure
    expect_null(flex$theme$panel.widths)
    expect_null(flex$theme$panel.heights)
    expect_s3_class(fixed$theme$panel.widths, "unit")

    # a fixed ratio is what holds the spacing instead (ggplot2 4.0 gives
    # coord_fixed() as a CoordCartesian carrying a ratio)
    expect_false(is.null(flex$coordinates$ratio))
    expect_gt(flex$coordinates$ratio, 0)
})

test_that("dag() puts the scaling in the coordinates and the shape in the ratio", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()

    # The ratio is centimetres per row over centimetres per column, which the
    # scaling does not touch: the stretch is carried by the node coordinates
    # moving apart, not by the units changing shape. That separation is the
    # whole point -- it is what lets the drawing shrink uniformly to fit.
    plain <- dag(fit)$coordinates$ratio
    expect_equal(dag(fit, x_scale = 2)$coordinates$ratio, plain)
    expect_equal(dag(fit, y_scale = 2)$coordinates$ratio, plain)

    # so a stretch shows up as a wider (or taller) figure being asked for
    expect_gt(attr(dag(fit, x_scale = 2), "fig_size")[["width"]],
              attr(dag(fit), "fig_size")[["width"]])
    expect_gt(attr(dag(fit, y_scale = 2), "fig_size")[["height"]],
              attr(dag(fit), "fig_size")[["height"]])
})

# ---- curved (co)variance paths and group headings --------------------------

geom_params_of <- function(p, cls) {
    i <- which(vapply(p$layers, function(l) class(l$geom)[1], character(1)) == cls)
    if (length(i) == 0L) NULL else p$layers[[i[1]]]$geom_params
}

test_that("(co)variance curvature is untouched by x_scale/y_scale", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    w <- c("measurement", "regression", "variance", "covariance")

    plain <- dag(fit, what = w)
    stretched <- dag(fit, what = w, x_scale = 2, y_scale = 1.3)

    # the whole point: the bend is a property of the diagram, not of the
    # data-to-figure mapping the scaling changes
    expect_equal(geom_params_of(stretched, "GeomDagArc")$angle,
                 geom_params_of(plain, "GeomDagArc")$angle)
    expect_equal(geom_params_of(stretched, "GeomDagLoop")$loop_radius,
                 geom_params_of(plain, "GeomDagLoop")$loop_radius)
})

test_that("curve_scale drives both kinds, and each can be overridden", {
    skip_if_not_installed("ggraph")
    fit <- manifest_fit()
    w <- c("measurement", "regression", "variance", "covariance")

    base_arc <- geom_params_of(dag(fit, what = w), "GeomDagArc")$angle
    base_loop <- geom_params_of(dag(fit, what = w), "GeomDagLoop")$loop_radius

    both <- dag(fit, what = w, curve_scale = 2)
    expect_equal(geom_params_of(both, "GeomDagArc")$angle, base_arc * 2)
    expect_equal(geom_params_of(both, "GeomDagLoop")$loop_radius, base_loop * 2)

    # variance_scale and covariance_scale follow curve_scale unless given
    mixed <- dag(fit, what = w, curve_scale = 2, covariance_scale = 3)
    expect_equal(geom_params_of(mixed, "GeomDagArc")$angle, base_arc * 3)
    expect_equal(geom_params_of(mixed, "GeomDagLoop")$loop_radius, base_loop * 2)

    only_var <- dag(fit, what = w, variance_scale = 0.5)
    expect_equal(geom_params_of(only_var, "GeomDagArc")$angle, base_arc)
    expect_equal(geom_params_of(only_var, "GeomDagLoop")$loop_radius, base_loop * 0.5)

    for (bad in list(0, -1, "2", NA_real_)) {
        expect_error(dag(fit, what = w, curve_scale = bad), "must be a single positive number")
        expect_error(dag(fit, what = w, variance_scale = bad), "must be a single positive number")
    }
})

test_that("arc caps follow the node outlines they meet", {
    # rectangles for the manifest boxes, ellipses for the round markers
    # the corner region belongs to the box but not to the circle inside it
    expect_true(semnova:::dag_in_cap(2.5, 2.5, 0, 0, 3, 3, rect = 1))
    expect_false(semnova:::dag_in_cap(2.5, 2.5, 0, 0, 3, 3, rect = 0))
    expect_true(semnova:::dag_in_cap(1, 0, 0, 0, 3, 3, rect = 0))
    expect_equal(semnova:::dag_in_cap(1, 1, 0, 0, 0, 0, rect = 0), FALSE)
})

test_that("group headings are anchored to the top of the diagram, not to the strip", {
    skip_if_not_installed("ggraph")
    fit <- multigroup_fit()
    p <- dag(fit, what = c("measurement", "regression", "variance"))

    i <- which(vapply(p$layers, function(l) class(l$geom)[1], character(1)) == "GeomDagHeading")
    expect_length(i, 1L)
    headings <- p$layers[[i]]$data
    expect_setequal(headings$label, fit$group_labels)
    expect_equal(unique(headings$y), max(p$data$y)) # max(dag) + a fixed offset
    expect_gt(p$layers[[i]]$geom_params$offset, 0)

    # the strip is kept for the split, but its text is ours to place
    expect_s3_class(p$theme$strip.text, "element_blank")

    # a single-group fit has no heading and no facet
    expect_null(geom_params_of(dag(manifest_fit()), "GeomDagHeading"))
})

# ---- dag_save() ------------------------------------------------------------

test_that("dag_save() crops the whitespace off a saved diagram", {
    skip_if_not_installed("ggraph")
    skip_if_not_installed("magick")
    p <- dag(manifest_fit())

    raw <- tempfile(fileext = ".png")
    cropped <- tempfile(fileext = ".png")
    on.exit(unlink(c(raw, cropped)), add = TRUE)

    dag_save(raw, p, width = 8, height = 5, dpi = 70, trim = FALSE)
    dag_save(cropped, p, width = 8, height = 5, dpi = 70)

    dims <- function(f) as.integer(magick::image_info(magick::image_read(f))[, c("width", "height")])
    expect_equal(dims(raw), c(560L, 350L)) # 8 x 5 inches at 70 dpi
    expect_true(all(dims(cropped) < dims(raw)))

    # padding is what is left around the ink, so less of it crops tighter
    tight <- tempfile(fileext = ".png")
    on.exit(unlink(tight), add = TRUE)
    dag_save(tight, p, width = 8, height = 5, dpi = 70, padding = 0)
    expect_true(all(dims(tight) < dims(cropped)))
})

test_that("dag_save() takes its size from the plot's fig_size", {
    skip_if_not_installed("ggraph")
    skip_if_not_installed("magick")
    p <- dag(manifest_fit(), x_scale = 1.5)
    size <- attr(p, "fig_size")
    expect_false(is.null(size))

    out <- tempfile(fileext = ".png")
    on.exit(unlink(out), add = TRUE)
    dag_save(out, p, dpi = 60, trim = FALSE)

    got <- as.integer(magick::image_info(magick::image_read(out))[, c("width", "height")])
    expect_equal(got, as.integer(round(size * 60)))
})

test_that("dag_save() still writes a file when it cannot crop, and says why", {
    skip_if_not_installed("ggraph")
    p <- dag(manifest_fit())
    out <- tempfile(fileext = ".pdf")
    on.exit(unlink(out), add = TRUE)

    expect_message(dag_save(out, p, width = 6, height = 4), "cannot crop a .pdf file")
    expect_true(file.exists(out))
})

test_that("dag_save() guards its arguments", {
    skip_if_not_installed("ggraph")
    p <- dag(manifest_fit())
    out <- tempfile(fileext = ".png")
    on.exit(unlink(out), add = TRUE)

    expect_error(dag_save(out, "not a plot"), "must be a ggplot object")
    expect_error(dag_save(1, p), "must be a single character string")
    expect_error(dag_save(out, p, dpi = 0), "must be a single positive number")
    expect_error(dag_save(out, p, width = -1), "must be a single positive number")
    expect_error(dag_save(out, p, padding = -1), "must be a single non-negative number")
    expect_error(dag_save(out, p, trim = "yes"), "must be TRUE or FALSE")
})

test_that("dag_save() writes an uncropped file when magick is not installed", {
    skip_if_not_installed("ggraph")
    # asked before the mock below, which would otherwise answer for it: the
    # mock intercepts requireNamespace(), and that is what skipping consults
    skip_if_not_installed("magick")
    p <- dag(manifest_fit())
    out <- tempfile(fileext = ".png")
    on.exit(unlink(out), add = TRUE)

    # pretend magick is not there: the fallback has to be exercised, not assumed
    testthat::local_mocked_bindings(
        requireNamespace = function(package, ...) {
            if (identical(package, "magick")) FALSE else TRUE
        },
        .package = "base"
    )

    expect_message(
        dag_save(out, p, width = 6, height = 4, dpi = 70),
        "needs the 'magick' package to crop"
    )
    expect_true(file.exists(out))
    expect_gt(file.size(out), 0)

    # and it really is the uncropped image: the full figure, 6 x 4 at 70 dpi.
    # magick is only mocked out of `requireNamespace`, so it can still be
    # called here to measure what the fallback produced.
    dims <- as.integer(magick::image_info(magick::image_read(out))[, c("width", "height")])
    expect_equal(dims, c(420L, 280L))
})
