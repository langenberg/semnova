#' Lgc: a fitted latent growth components model
#'
#' @description
#' Pure data container for a fitted model: fields only, no behaviour.
#' Everything you do with a fit is a plain function taking it as an
#' argument:
#' [model_parameters()], [regression_coefficients()],
#' [contrast_estimates()], [test_hypothesis()]. Construct with [lgc()].
#'
#' @rdname Lgc-class
#' @export
Lgc <- R6::R6Class(
    "Lgc",
    public = list(
        #' @field sem_obj The fitted `lavaan` object.
        sem_obj = NULL,
        #' @field syntax Character(1). The generated lavaan syntax.
        syntax = NULL,
        #' @field etas_mmodel Named list, eta name -> indicator names.
        etas_mmodel = NULL,
        #' @field pis Character vector of pi (contrast) variable names.
        pis = NULL,
        #' @field c_matrix_within The within-subject contrast matrix.
        c_matrix_within = NULL,
        #' @field covariates_mmodel Named list, covariate name -> indicator
        #'   name(s).
        covariates_mmodel = NULL,
        #' @field group Character(1) or NULL. Grouping column in the data.
        group = NULL,
        #' @field group_labels Character vector of group labels.
        group_labels = NULL,
        #' @field hypotheses List of [Hypothesis] objects.
        hypotheses = NULL,
        #' @field data The data the model was fit to.
        data = NULL,

        #' @description Construct an Lgc container. Prefer [lgc()].
        #' @param ... Fields to set, by name.
        initialize = function(...) {
            fields <- list(...)
            for (nm in names(fields)) {
                self[[nm]] <- fields[[nm]]
            }
            invisible(self)
        }
    )
)

#' @keywords internal
#' @noRd
normalize_constructs <- function(x, arg) {
    if (is.null(x)) {
        return(list())
    }
    if (is.character(x)) {
        x <- list(x)
    }
    check_indicator_list(x, arg)

    n_per <- vapply(x, length, integer(1L))
    if (length(unique(n_per)) > 1L) {
        stop_semnova(
            "`", arg, "`: every construct must have the same number of indicators, got ",
            paste(n_per, collapse = ", ")
        )
    }

    if (is.null(names(x)) || any(!nzchar(names(x)))) {
        auto <- if (n_per[1L] == 1L) unlist(x, use.names = FALSE) else paste0(".", sub("s$", "", arg), seq_along(x))
        nms <- names(x)
        if (is.null(nms)) {
            nms <- rep("", length(x))
        }
        nms[!nzchar(nms)] <- auto[!nzchar(nms)]
        names(x) <- nms
    }

    x
}

#' @keywords internal
#' @noRd
fit_measurement_model <- function(
    data,
    etas_mmodel,
    c_matrix_within,
    covariates_mmodel = list(),
    group = NULL,
    group_labels = NULL,
    resid_cov = list(),
    equal_resid_cov = list(),
    equal_resid_var = list(),
    invariance_within = "strong",
    invariance_between = "strong",
    lv_scaling = "effect",
    sphericity = FALSE,
    compound_symmetry = FALSE,
    variance_homogeneity = FALSE,
    group_weights = "fixed",
    ...
) {
    # for a manifest (single-indicator) DV there is no latent eta: the pi
    # layer loads directly on the manifest columns themselves
    n_indicators <- length(etas_mmodel[[1L]])
    eta_names <- if (n_indicators == 1L) {
        vapply(etas_mmodel, function(x) x[[1L]], character(1L), USE.NAMES = FALSE)
    } else {
        names(etas_mmodel)
    }
    pis <- paste0(".pi", seq_len(ncol(c_matrix_within)))

    measurement <- build_measurement_syntax(
        etas_mmodel,
        group_labels = group_labels,
        invariance_within = invariance_within,
        invariance_between = invariance_between,
        lv_scaling = lv_scaling
    )
    # the pi layer owns the etas' mean/(co)variance structure; drop whatever
    # the measurement builder left free for them (see strip_construct_structure)
    measurement <- strip_construct_structure(measurement, eta_names)

    # Strict invariance already equates residual variances, across occasions
    # for invariance_within, across groups for invariance_between, by giving
    # them one shared label. A `resid_var` set writes a second label onto the
    # same parameters, and lavaan keeps the last one, which can *undo* the
    # invariance constraint rather than add to it (verified: a set covering one
    # occasion took the model from df 59 to 58). Invariance wins.
    strict <- c(invariance_within, invariance_between) == "strict"
    if (any(strict) && length(equal_resid_var) > 0L) {
        warning(
            "semnova: ", paste(c("invariance_within", "invariance_between")[strict], collapse = " and "),
            " = \"strict\" already constrains residual variances to be equal; ",
            "the `resid_var` set(s) would override that constraint rather than add to it, ",
            "and are ignored",
            call. = FALSE
        )
        equal_resid_var <- list()
    }

    pieces <- c(
        measurement,
        build_pi_layer_syntax(
            c_matrix_within, eta_names = eta_names, group_labels = group_labels,
            sphericity = sphericity, compound_symmetry = compound_symmetry,
            variance_homogeneity = variance_homogeneity
        ),
        build_covariate_syntax(
            covariates_mmodel,
            pis = pis,
            group_labels = group_labels,
            invariance_between = invariance_between,
            lv_scaling = lv_scaling,
            # covariate labels are numbered past the etas, so no covariate is
            # accidentally given an eta's label and constrained to it
            n_eta_constructs = length(etas_mmodel)
        ),
        build_resid_cov_syntax(resid_cov, equal_resid_cov, group_labels = group_labels),
        build_resid_var_syntax(equal_resid_var, group_labels = group_labels),
        build_group_weight_syntax(group_labels, group_weights)
    )
    syntax <- paste(pieces[nzchar(pieces)], collapse = "\n")

    sem_obj <- lavaan::sem(
        syntax,
        data = data,
        group = group,
        group.label = if (is.null(group)) NULL else group_labels,
        meanstructure = TRUE,
        # estimating the group sizes is a lavaan fitting option, not just
        # syntax: the `%` weights above do nothing without it
        group.w.free = identical(group_weights, "stochastic"),
        ...
    )

    list(syntax = syntax, sem_obj = sem_obj, pis = pis)
}

#' Fit a latent growth components model
#'
#' @description
#' The low-level entry point: you supply the latent constructs (`etas`) and
#' the within-subject contrast matrix directly, in the model's own
#' vocabulary. For an ANOVA-style interface that derives those from a
#' factorial design, use [semnova()].
#'
#' @param data A data.frame in wide format: one row per case, one column
#'   per indicator x occasion.
#' @param etas Named list, eta name -> character vector of indicator
#'   names, one element per measurement occasion; or a bare character
#'   vector for a single eta. Every eta must have the same number of
#'   indicators. Single-indicator (manifest) etas default to being named
#'   after their indicator.
#' @param c_matrix_within Numeric square contrast matrix, one row/column
#'   per occasion.
#' @param group Character(1) or NULL. Column in `data` identifying groups.
#' @param group_labels Character vector of group labels; defaults to the
#'   sorted unique values of `data[[group]]`.
#' @param covariates Named list (covariate name -> indicator name(s)) or a
#'   character vector of manifest covariate columns.
#' @param resid_cov List of character vectors of model variables; residual
#'   covariances within each set are freely estimated. (Documented
#'   separately from `equal_resid_cov` so that [semnova()], which takes
#'   [resid_set()] objects for the same job and has no `equal_resid_cov`,
#'   can inherit one without the other.)
#' @param equal_resid_cov List of character vectors of model variables;
#'   residual covariances within each set are constrained equal to each
#'   other.
#' @param equal_resid_var List of character vectors of model variables whose
#'   residual *variances* are constrained equal within each set. Sets may not
#'   overlap, and are ignored with a warning under `"strict"` invariance,
#'   which already equates those parameters. Use [semnova()]'s `resid_var` to
#'   describe these by indicator and condition instead of by variable.
#' @param invariance_within,invariance_between One of `"configural"`,
#'   `"weak"`, `"strong"`, `"strict"`.
#' @param lv_scaling One of `"effect"` (average loading 1, average
#'   intercept 0) or `"referent"` (first loading 1, first intercept 0).
#'   Ignored where [identification_scheme()] requires full tau-equivalence.
#' @param sphericity List of index vectors (or contrast-variable names).
#'   Each set is constrained to equal variances and zero covariances among
#'   its members, the covariance structure classical repeated-measures
#'   ANOVA assumes. Sets must not overlap. `TRUE` puts every contrast
#'   variable in one set; `FALSE` (the default) leaves the covariance
#'   matrix unstructured. [semnova()] additionally accepts model terms,
#'   which it translates to positions.
#' @param compound_symmetry Logical. Structure the eta layer instead: one
#'   variance for every occasion and one covariance for every pair, with the
#'   contrast variables' own (co)variances fixed to zero. Takes precedence
#'   over `sphericity`, which is then ignored with a warning.
#' @param variance_homogeneity Logical. Constrain those (co)variances to be
#'   equal across groups.
#' @param group_weights One of `"fixed"` (default, group sizes are what
#'   they are) or `"stochastic"` (group membership is random, so each
#'   group's frequency is estimated and reported as a `relfreq*` definition).
#' @param hypotheses A [Hypothesis] object or list of them, to attach to
#'   the fit.
#' @param ... Passed to [lavaan::sem()].
#'
#' @return An [Lgc] object.
#' @export
lgc <- function(
    data,
    etas,
    c_matrix_within,
    group = NULL,
    group_labels = NULL,
    covariates = NULL,
    resid_cov = list(),
    equal_resid_cov = list(),
    equal_resid_var = list(),
    invariance_within = "strong",
    invariance_between = "strong",
    lv_scaling = c("effect", "referent"),
    sphericity = FALSE,
    compound_symmetry = FALSE,
    variance_homogeneity = FALSE,
    group_weights = c("fixed", "stochastic"),
    hypotheses = NULL,
    ...
) {
    lv_scaling <- match.arg(lv_scaling)
    group_weights <- match.arg(group_weights)
    check_flag(compound_symmetry, "compound_symmetry")
    check_flag(variance_homogeneity, "variance_homogeneity")
    check_data(data)
    check_string(group, "group", null_ok = TRUE)
    check_invariance(invariance_within, "invariance_within")
    check_invariance(invariance_between, "invariance_between")
    check_hypotheses(hypotheses)

    etas_mmodel <- normalize_constructs(etas, "etas")
    covariates_mmodel <- normalize_constructs(covariates, "covariates")

    check_columns_exist(data, unlist(etas_mmodel, use.names = FALSE), "etas")
    if (length(covariates_mmodel) > 0L) {
        check_columns_exist(data, unlist(covariates_mmodel, use.names = FALSE), "covariates")
    }
    check_square_matrix(c_matrix_within, "c_matrix_within", n = length(etas_mmodel))

    if (is.null(group)) {
        group_labels <- "1"
    } else {
        check_columns_exist(data, group, "group")
        if (is.null(group_labels)) {
            group_labels <- sort(unique(as.character(data[[group]])))
        }
        check_character(group_labels, "group_labels")
    }

    if (inherits(hypotheses, "Hypothesis")) {
        hypotheses <- list(hypotheses)
    }

    fitted <- fit_measurement_model(
        data = data,
        etas_mmodel = etas_mmodel,
        c_matrix_within = c_matrix_within,
        covariates_mmodel = covariates_mmodel,
        group = group,
        group_labels = group_labels,
        resid_cov = resid_cov,
        equal_resid_cov = equal_resid_cov,
        equal_resid_var = equal_resid_var,
        invariance_within = invariance_within,
        invariance_between = invariance_between,
        lv_scaling = lv_scaling,
        sphericity = sphericity,
        compound_symmetry = compound_symmetry,
        variance_homogeneity = variance_homogeneity,
        group_weights = group_weights,
        ...
    )

    Lgc$new(
        sem_obj = fitted$sem_obj,
        syntax = fitted$syntax,
        etas_mmodel = etas_mmodel,
        pis = fitted$pis,
        c_matrix_within = c_matrix_within,
        covariates_mmodel = covariates_mmodel,
        group = group,
        group_labels = group_labels,
        hypotheses = hypotheses %||% list(),
        data = data
    )
}

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
