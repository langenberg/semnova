#' Node placement for a model's path diagram
#'
#' @description
#' Computes where each variable sits in the path diagram, in the column
#' layout semnova models call for: indicators, latent constructs (etas),
#' the contrast variables (pis), then covariates and their indicators.
#' Each eta and pi is centred vertically on its own block of indicators,
#' and the covariate block is centred against the eta block.
#'
#' Returned in the node order [semPlot::semPaths()] expects, so it can be
#' handed straight to its `layout` argument. Split out from [dag()] so the
#' placement is inspectable and testable without rendering anything.
#'
#' @param fit An [Lgc] object.
#' @param intercepts Logical; reserve a position for the constant
#'   ("triangle") node that [semPlot::semPaths()] adds when intercepts are
#'   drawn. The node set grows by one in that case, and a layout matrix
#'   with too few rows is an error, so this must match what [dag()] passes
#'   to `semPaths()`.
#'
#' @return A list with `coords` (a two-column matrix of x/y positions),
#'   `labels` (display labels in the same order), and `names` (the
#'   underlying variable names).
#' @keywords internal
dag_layout <- function(fit, intercepts = FALSE) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }
    if (!requireNamespace("semPlot", quietly = TRUE)) {
        stop_semnova("path diagrams require the 'semPlot' package; install it with install.packages(\"semPlot\")")
    }

    vars <- semPlot::semPlotModel(fit$sem_obj)@Vars$name

    eta_names <- names(fit$etas_mmodel)
    n_eta <- length(eta_names)
    n_ind <- length(fit$etas_mmodel[[1L]])
    manifest_dv <- n_ind == 1L

    covariate_names <- names(fit$covariates_mmodel)
    n_ind_cov <- vapply(fit$covariates_mmodel, length, integer(1L))

    # centre the two halves of the diagram against each other, as the
    # pre-rewrite layout did
    n_ind_eta_total <- n_eta * n_ind
    n_ind_cov_total <- sum(n_ind_cov)
    start_eta <- max(0, (n_ind_cov_total - n_ind_eta_total) / 2)
    start_cov <- max(0, (n_ind_eta_total - n_ind_cov_total) / 2)

    coords <- data.frame(
        name = vars, x = NA_real_, y = NA_real_,
        label = vars, stringsAsFactors = FALSE
    )
    place <- function(name, x, y, label = NULL) {
        i <- match(name, coords$name)
        ok <- !is.na(i)
        coords$x[i[ok]] <<- x[ok]
        coords$y[i[ok]] <<- y[ok]
        if (!is.null(label)) coords$label[i[ok]] <<- label[ok]
    }

    # eta block: indicators, then the eta itself centred on them, then its pi
    for (e in seq_len(n_eta)) {
        indicators <- fit$etas_mmodel[[e]]
        ind_y <- start_eta + (e - 1L) * n_ind + seq_along(indicators) - 1L
        place(indicators, rep(if (manifest_dv) 2 else 1, length(indicators)), ind_y)

        centre <- start_eta + (e - 1L) * n_ind + (n_ind - 1) / 2
        if (!manifest_dv) {
            place(eta_names[e], 2, centre, sprintf("eta%d", e))
        }
        # pi indices are 0-based: pi0 is the intercept contrast
        place(fit$pis[e], 3, centre, sprintf("pi%d", e - 1L))
    }

    # covariate block, mirroring the eta block on the other side
    if (length(covariate_names) > 0L) {
        offset <- start_cov
        for (cv in seq_along(covariate_names)) {
            indicators <- fit$covariates_mmodel[[cv]]
            if (length(indicators) > 1L) {
                place(covariate_names[cv], 4, offset + (length(indicators) - 1) / 2)
                place(indicators, rep(5, length(indicators)), offset + seq_along(indicators) - 1L)
            } else {
                place(covariate_names[cv], 4, offset)
            }
            offset <- offset + max(1L, length(indicators))
        }
    }

    # anything the model contains that we didn't place (shouldn't happen, but
    # never hand semPaths an NA coordinate)
    unplaced <- is.na(coords$x)
    if (any(unplaced)) {
        coords$x[unplaced] <- 6
        coords$y[unplaced] <- seq_len(sum(unplaced)) - 1L
    }

    coords$y <- -coords$y + max(coords$y) # top-to-bottom, as the old layout drew it

    if (intercepts) {
        # semPaths adds one constant ("1") node *per variable with an
        # intercept*, not a single shared triangle, appended after the
        # model's variables in the order those intercept edges appear in the
        # parameter table. Place each just below the variable it belongs to.
        pars <- semPlot::semPlotModel(fit$sem_obj)@Pars
        targets <- unique(pars$rhs[pars$edge == "int"])
        targets <- targets[targets %in% coords$name]
        if (length(targets) > 0L) {
            at <- match(targets, coords$name)
            coords <- rbind(coords, data.frame(
                name = paste0(".const_", targets),
                x = coords$x[at],
                y = coords$y[at] - 0.45,
                label = "1",
                stringsAsFactors = FALSE
            ))
        }
    }

    list(
        coords = as.matrix(coords[, c("x", "y")]),
        labels = coords$label,
        names = coords$name
    )
}

#' @keywords internal
#' @noRd
#' Blank the labels of fixed paths, keeping semPaths' own for the rest.
#'
#' The plotted edge set is not the parameter table: semPaths splits and
#' filters edges, so a label vector built from `semPlotModel()@Pars` is the
#' wrong length and silently attaches labels to the wrong edges. Probing
#' with `DoNotPlot = TRUE` gives the labels already aligned to the edges
#' actually drawn, plus `lty` (2 = dashed = fixed) to select by.
dag_blank_fixed_labels <- function(args, labels) {
    probe <- do.call(semPlot::semPaths, c(args, list(DoNotPlot = TRUE)))
    edge_labels <- as.character(probe$graphAttributes$Edges$labels)

    if (labels == "none") {
        return(rep("", length(edge_labels)))
    }
    fixed <- probe$graphAttributes$Edges$lty != 1
    edge_labels[fixed] <- ""
    edge_labels
}

#' Plot a model's path diagram with semPlot
#'
#' @description
#' An alternative to [dag()] that renders through
#' [semPlot::semPaths()], laid out by [dag_layout()] in semnova's column
#' structure: indicators, etas, pis, covariates.
#'
#' [dag()] is the primary path-diagram function and is usually what you
#' want: it draws residuals as explicit epsilon nodes, labels multigroup
#' facets with their group names, and keeps nodes perfectly circular at any
#' figure size. `dag_semplot()` is **retired**: it is no longer exported,
#' kept because semPlot is the established
#' SEM-diagram package and its output is familiar, but note that qgraph
#' scales nodes to the device, so circles render as ellipses that change
#' shape when the figure is resized.
#'
#' Standard SEM conventions apply, courtesy of semPlot: single-headed
#' arrows for directed effects (loadings, regressions), double-headed
#' arrows for (co)variances, self-loops for variances and residual
#' variances, squares for manifest variables and ellipses for latent ones.
#' Fixed parameters are drawn with dashed lines and free ones solid, so
#' the fixed contrast loadings between etas and pis are visually distinct
#' from estimated paths.
#'
#' @param fit An [Lgc] object.
#' @param labels Which edges to label: `"free"` (default) labels only
#'   freely estimated parameters, with a fully crossed contrast layer
#'   there are many fixed loadings whose labels would otherwise collide;
#'   `"all"` labels everything; `"none"` labels nothing. Note that
#'   `"free"` cannot be honoured when a multigroup fit is drawn as panels
#'   (`panel_groups = TRUE`): each panel has its own estimates, but
#'   `semPaths()` takes only one label vector, so filtering would print one
#'   group's numbers on every panel. Panelled plots therefore label
#'   everything; fixed paths remain dashed, so the distinction is still
#'   visible.
#' @param residuals Logical; draw variances and residual variances.
#' @param intercepts Logical; draw intercepts.
#' @param digits Digits for edge labels.
#' @param panel_groups Logical; for a multigroup fit, draw one panel per
#'   group, in `fit$group_labels` order (left to right). Defaults to
#'   `TRUE` when the fit has more than one group.
#' @param ... Passed to [semPlot::semPaths()], overriding any default set
#'   here.
#'
#' @return The `qgraph` object, invisibly; called for the plot it draws.
#' @keywords internal
dag_semplot <- function(
    fit,
    labels = c("free", "all", "none"),
    residuals = TRUE,
    intercepts = FALSE,
    digits = 2,
    panel_groups = NULL,
    ...
) {
    if (!requireNamespace("semPlot", quietly = TRUE)) {
        stop_semnova("path diagrams require the 'semPlot' package; install it with install.packages(\"semPlot\")")
    }
    labels <- match.arg(labels)
    check_flag(residuals, "residuals")
    check_flag(intercepts, "intercepts")

    layout <- dag_layout(fit, intercepts = intercepts)
    multigroup <- length(fit$group_labels) > 1L
    if (is.null(panel_groups)) {
        panel_groups <- multigroup
    }
    check_flag(panel_groups, "panel_groups")

    defaults <- list(
        object = fit$sem_obj,
        what = "path",
        whatLabels = "est",
        nDigits = digits,
        layout = layout$coords,
        nodeLabels = layout$labels,
        residuals = residuals,
        intercepts = intercepts,
        shapeMan = "rectangle",
        shapeLat = "ellipse",
        shapeInt = "triangle",
        sizeMan = 11, sizeMan2 = 4,
        sizeLat = 9, sizeLat2 = 6,
        edge.label.cex = 0.5,
        label.cex = 1,
        # let semPaths shrink label text to fit its node rather than
        # overflowing the box, which long indicator names otherwise do
        label.scale = TRUE,
        nCharNodes = 0,
        edge.color = "grey30",
        curvePivot = TRUE,
        mar = c(4, 6, 4, 6)
    )
    if (panel_groups) {
        defaults$panelGroups <- TRUE
        # semPaths prints a bare panel count here rather than the group
        # names, which reads as a stray number; suppress it. Panels appear
        # in fit$group_labels order (documented on the `panel_groups` arg).
        defaults$title <- FALSE
    }

    args <- utils::modifyList(defaults, list(...))

    if (labels == "none") {
        args$whatLabels <- "hide"
    } else if (labels == "free" && !panel_groups && is.null(list(...)$edgeLabels)) {
        # `edgeLabels` is a single vector, so it cannot carry per-panel
        # values: applying it to a panelled multigroup plot would print the
        # first group's estimates on every panel. Fixed paths stay dashed
        # there regardless, so the distinction is still visible.
        args$edgeLabels <- dag_blank_fixed_labels(args, labels)
    }

    invisible(do.call(semPlot::semPaths, args))
}
