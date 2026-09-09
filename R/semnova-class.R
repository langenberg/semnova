#' Semnova: a fitted latent repeated-measures ANOVA
#'
#' @description
#' Pure data container, extending [Lgc] with the parsed factorial design.
#' Fields only, no behaviour, every operation on a fit is a plain
#' function taking it as an argument ([model_parameters()],
#' [contrast_estimates()], [test_hypothesis()], ...). Construct with
#' [semnova()].
#'
#' @rdname Semnova-class
#' @export
Semnova <- R6::R6Class(
    "Semnova",
    inherit = Lgc,
    public = list(
        #' @field within_design Design list from [parse_design()].
        within_design = NULL,
        #' @field between_design Design list from [parse_design()].
        between_design = NULL,
        #' @field id,dv,indicator Column names from the original long data.
        id = NULL,
        #' @field dv Dependent-variable column.
        dv = NULL,
        #' @field indicator Indicator column, or NULL for a manifest DV.
        indicator = NULL,
        #' @field withins,betweens The factor columns of each design.
        withins = NULL,
        #' @field betweens Between-subject factor columns.
        betweens = NULL,
        #' @field data_long The original long-format data.
        data_long = NULL
    )
)

#' Fit a latent repeated-measures ANOVA
#'
#' @description
#' The ANOVA-style entry point: describe the design in the vocabulary of
#' the experiment (a dependent variable, within- and between-subject
#' factors, optional indicators for a latent DV and covariates) and the
#' model is derived from it, contrast matrices from R's own contrast
#' machinery, and one hypothesis per model term (main effects and
#' interactions), each testable through [test_hypothesis()].
#'
#' @param data A data.frame in **long** format: one row per
#'   case x occasion x indicator.
#' @param id Character(1). Column identifying cases.
#' @param dv Character(1). Column holding the dependent variable.
#' @param indicator Character(1) or NULL. Column naming which indicator
#'   each row holds. `NULL` means a manifest (single-indicator) DV.
#' @param withins Character vector of within-subject factor columns.
#' @param betweens Character vector of between-subject factor columns.
#' @param covariates Character vector of covariate columns, numeric, or
#'   categorical and coded into contrast columns for you (a `k`-level factor
#'   becomes one `k - 1` degree-of-freedom term, coded with
#'   `contrasts_arg[[name]]` or `contr.treatment`), or a
#'   named list mapping a latent covariate to its indicator columns.
#' @param contrasts_arg Named list, factor name -> contrast function name
#'   (e.g. `list(grade = "contr.sum")`). Defaults: `contr.poly` for
#'   within factors, `contr.treatment` for between factors.
#' @param normalize_contrasts Logical; scale contrast columns to unit
#'   length.
#' @inheritParams lgc
#' @param resid_cov Residual covariances to add, described by
#'   [resid_set()]: which indicators, in which conditions, and whether the
#'   covariances in the set are equal to each other (`equal = TRUE`) or
#'   freely estimated (the default). Give one set, a list of them, or `TRUE`
#'   for one set covering everything. A bare character vector of indicator
#'   names is shorthand for `resid_set()` over all conditions. `FALSE` (the
#'   default) adds none.
#' @param resid_var Residual variances to constrain equal, described the
#'   same way, `resid_set("fix_count")` equates that indicator's residual
#'   variances across occasions, `resid_set("fix_count", sentence =
#'   "landolt")` only within landolt sentences, `TRUE` equates every
#'   residual variance in the model. `FALSE` (the default) leaves them all
#'   free. Ignored, with a warning, when `invariance_within` or
#'   `invariance_between` is `"strict"`: that already equates residual
#'   variances, and a set would override the invariance constraint rather
#'   than add to it. A set covering fewer
#'   than two residual terms (a single occasion, say) has nothing to
#'   constrain and is dropped with a warning.
#' @param sphericity Which model terms to constrain to sphericity, equal
#'   variances and zero covariances among the contrast variables that term
#'   spans, the structure classical repeated-measures ANOVA assumes.
#'   `TRUE` constrains every term that spans more than one contrast
#'   variable; otherwise give term names as they appear in the ANOVA table
#'   (`"grade"`, `"grade:sentence"`, or a list of them), or an interaction
#'   as its factors (`c("grade", "sentence")`). `FALSE`, the default,
#'   constrains nothing. Raw contrast-variable index sets, as [lgc()] takes
#'   them, are also accepted.
#' @param invariance_within,invariance_between One of `"configural"`,
#'   `"weak"`, `"strong"`, `"strict"`.
#' @param lv_scaling One of `"effect"` or `"referent"`.
#' @param ... Passed to [lavaan::sem()].
#'
#' @return A [Semnova] object.
#' @export
semnova <- function(
    data,
    id,
    dv,
    indicator = NULL,
    withins = NULL,
    betweens = NULL,
    covariates = NULL,
    contrasts_arg = list(),
    normalize_contrasts = FALSE,
    resid_cov = FALSE,
    resid_var = FALSE,
    invariance_within = "strong",
    invariance_between = "strong",
    sphericity = FALSE,
    compound_symmetry = FALSE,
    variance_homogeneity = FALSE,
    group_weights = c("fixed", "stochastic"),
    lv_scaling = c("effect", "referent"),
    ...
) {
    lv_scaling <- match.arg(lv_scaling)
    check_data(data)
    check_string(id, "id")
    check_string(dv, "dv")
    check_string(indicator, "indicator", null_ok = TRUE)
    check_character(withins, "withins", null_ok = TRUE)
    check_character(betweens, "betweens", null_ok = TRUE)
    check_invariance(invariance_within, "invariance_within")
    check_invariance(invariance_between, "invariance_between")
    check_flag(normalize_contrasts, "normalize_contrasts")
    group_weights <- match.arg(group_weights)
    check_flag(compound_symmetry, "compound_symmetry")
    check_flag(variance_homogeneity, "variance_homogeneity")
    if (!is.list(contrasts_arg)) {
        stop_semnova("`contrasts_arg` must be a named list, e.g. list(grade = \"contr.sum\")")
    }
    check_columns_exist(data, c(id, dv, indicator, withins, betweens), "data")

    # covariates: numeric manifest columns, or a named list of latent indicators
    covariate_columns <- NULL
    if (!is.null(covariates)) {
        if (is.character(covariates)) {
            covariate_columns <- covariates
        } else if (is.list(covariates)) {
            covariate_columns <- unlist(covariates, use.names = FALSE)
        } else {
            stop_semnova("`covariates` must be a character vector or a named list of indicator names")
        }
        check_columns_exist(data, covariate_columns, "covariates")
        if (is.list(covariates)) {
            # a latent covariate's indicators are measured variables, so there
            # is nothing sensible to code a factor into
            non_numeric <- covariate_columns[!vapply(data[covariate_columns], is.numeric, logical(1L))]
            if (length(non_numeric) > 0L) {
                stop_semnova(
                    "indicator column(s) of a latent covariate must be numeric: ",
                    paste(non_numeric, collapse = ", ")
                )
            }
        }
    }

    if (!is.null(betweens)) {
        check_constant_within(data, betweens, id, "betweens")
    }
    if (!is.null(covariate_columns)) {
        check_constant_within(data, covariate_columns, id, "covariates")
    }

    within_design <- parse_design(
        data, withins,
        contrasts_arg = contrasts_arg,
        default_contrast = "contr.poly",
        normalize_contrasts = normalize_contrasts
    )
    between_design <- parse_design(
        data, betweens,
        contrasts_arg = contrasts_arg,
        default_contrast = "contr.treatment",
        normalize_contrasts = normalize_contrasts
    )

    indicator_levels <- if (is.null(indicator)) dv else sort(unique(as.character(data[[indicator]])))
    etas_mmodel <- build_etas_mmodel(within_design$idata, indicator_levels)

    wide <- reshape_long_to_wide(
        data,
        id = id, dv = dv, indicator = indicator,
        withins = withins, betweens = betweens, covariates = covariate_columns
    )

    group <- NULL
    group_labels <- "1"
    if (!is.null(betweens)) {
        group <- ".group"
        wide[[group]] <- apply(wide[betweens], 1L, function(row) paste(as.character(row), collapse = "_"))
        group_labels <- cell_labels(between_design$idata)
        unseen <- setdiff(unique(wide[[group]]), group_labels)
        if (length(unseen) > 0L) {
            stop_semnova("between-subject cells present in the data but not in the design: ", paste(unseen, collapse = ", "))
        }
    }

    # Categorical covariates are coded into contrast columns here, once the
    # data is one row per case. The columns of one factor stay one term, so
    # its effect is tested jointly with k - 1 degrees of freedom.
    covariate_terms <- NULL
    if (is.character(covariates)) {
        expanded <- expand_categorical_covariates(wide, covariates, contrasts_arg)
        wide <- expanded$data
        # Each column is its own manifest covariate, hence its own construct.
        # Handing the bare vector on would make them the indicators of a single
        # latent covariate instead (normalize_constructs()' reading of a
        # character vector, which is what lgc() wants but semnova() does not).
        columns <- unlist(expanded$terms, use.names = FALSE)
        covariates <- stats::setNames(as.list(columns), columns)
        offsets <- cumsum(c(0L, vapply(expanded$terms, length, integer(1L))))
        covariate_terms <- stats::setNames(
            lapply(seq_along(expanded$terms), function(i) {
                offsets[i] + seq_along(expanded$terms[[i]])
            }),
            names(expanded$terms)
        )
    }

    sphericity <- resolve_sphericity_terms(sphericity, within_design)

    # With a manifest dependent variable there is no residual to structure: the
    # contrast layer loads straight onto the observed columns and fixes their
    # (co)variances to zero, so a residual constraint would be written and then
    # silently overwritten. Say so rather than appearing to do something.
    empty_spec <- function(x) {
        is.null(x) || (is.logical(x) && !isTRUE(x)) || (is.list(x) && length(x) == 0L)
    }
    if (length(etas_mmodel[[1L]]) == 1L && !(empty_spec(resid_cov) && empty_spec(resid_var))) {
        warning(
            "semnova: `resid_cov`/`resid_var` need a latent dependent variable, with a ",
            "manifest one the observed variables are the constructs, and have no residuals ",
            "of their own. Ignored.",
            call. = FALSE
        )
        resid_cov <- FALSE
        resid_var <- FALSE
    }

    # Residual structure is described by indicator and condition (resid_set()),
    # not by the wide column names semnova() itself generates. A covariance set
    # is free unless it says otherwise; a variance set exists only to equate,
    # so it defaults the other way.
    cov_sets <- resolve_resid_sets(
        resid_cov, etas_mmodel, indicator_levels, within_design$idata,
        arg = "resid_cov", default_equal = FALSE
    )
    var_sets <- resolve_resid_sets(
        resid_var, etas_mmodel, indicator_levels, within_design$idata,
        arg = "resid_var", default_equal = TRUE
    )
    equal <- vapply(cov_sets, function(s) isTRUE(s$equal), logical(1L))
    resid_cov <- lapply(cov_sets[!equal], function(s) s$variables)
    equal_resid_cov <- lapply(cov_sets[equal], function(s) s$variables)
    equal_resid_var <- lapply(var_sets, function(s) s$variables)

    covariates_mmodel <- normalize_constructs(covariates, "covariates")

    hypotheses <- generate_anova_hypotheses(
        within_design, between_design,
        n_covariates = length(covariates_mmodel),
        covariate_names = names(covariates_mmodel),
        covariate_terms = covariate_terms
    )

    fitted <- fit_measurement_model(
        data = wide,
        etas_mmodel = etas_mmodel,
        c_matrix_within = within_design$c_matrix,
        covariates_mmodel = covariates_mmodel,
        group = group,
        group_labels = group_labels,
        resid_cov = resid_cov,
        equal_resid_cov = equal_resid_cov,
        equal_resid_var = equal_resid_var,
        invariance_within = invariance_within,
        invariance_between = invariance_between,
        sphericity = sphericity,
        compound_symmetry = compound_symmetry,
        variance_homogeneity = variance_homogeneity,
        group_weights = group_weights,
        lv_scaling = lv_scaling,
        ...
    )

    Semnova$new(
        sem_obj = fitted$sem_obj,
        syntax = fitted$syntax,
        etas_mmodel = etas_mmodel,
        pis = fitted$pis,
        c_matrix_within = within_design$c_matrix,
        covariates_mmodel = covariates_mmodel,
        group = group,
        group_labels = group_labels,
        hypotheses = hypotheses,
        data = wide,
        within_design = within_design,
        between_design = between_design,
        id = id,
        dv = dv,
        indicator = indicator,
        withins = withins,
        betweens = betweens,
        data_long = data
    )
}
