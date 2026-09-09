#' @keywords internal
#' @noRd
design_levels <- function(data, factors, arg) {
    check_columns_exist(data, factors, arg)
    out <- lapply(factors, function(f) {
        v <- data[[f]]
        if (is.factor(v)) as.character(levels(droplevels(v))) else sort(unique(as.character(v)))
    })
    names(out) <- factors
    out
}

#' Parse a factorial design into contrast matrices
#'
#' @description
#' Turns a set of factor columns into the design objects the model needs:
#' the crossed level grid, the design formula, the model matrix (`b_matrix`)
#' built from R's own contrast machinery, and its inverse (`c_matrix`).
#' Used for the within-subject design, and, with different contrast
#' defaults, the between-subject design.
#'
#' @param data A data.frame.
#' @param factors Character vector of column names, or `NULL` for a design
#'   with no factors (a single implicit cell).
#' @param contrasts_arg Named list, factor name -> contrast function name
#'   (e.g. `"contr.sum"`).
#' @param default_contrast Contrast used for any factor not named in
#'   `contrasts_arg`.
#' @param normalize_contrasts Logical; scale each contrast column to unit
#'   length.
#'
#' @return A list with `idata`, `idesign`, `b_matrix`, `c_matrix`,
#'   `effect_names`, `effect_indices`, and `levels`.
#' @export
parse_design <- function(
    data,
    factors,
    contrasts_arg = list(),
    default_contrast = "contr.poly",
    normalize_contrasts = FALSE
) {
    check_flag(normalize_contrasts, "normalize_contrasts")

    if (is.null(factors) || length(factors) == 0L) {
        b_matrix <- matrix(1, nrow = 1L, ncol = 1L, dimnames = list(NULL, "(Intercept)"))
        attr(b_matrix, "assign") <- 0L
        return(list(
            idata = data.frame(.cell = "1", stringsAsFactors = FALSE),
            idesign = ~1,
            b_matrix = b_matrix,
            c_matrix = matrix(1, 1L, 1L),
            effect_names = "(Intercept)",
            effect_indices = 0L,
            levels = list()
        ))
    }

    check_character(factors, "factors")
    levels_list <- design_levels(data, factors, "factors")

    idata <- expand.grid(levels_list, stringsAsFactors = FALSE)
    idesign <- stats::as.formula(paste0("~", paste(factors, collapse = "*")))

    contrasts_use <- lapply(factors, function(f) {
        if (!is.null(contrasts_arg[[f]])) contrasts_arg[[f]] else default_contrast
    })
    names(contrasts_use) <- factors

    idata_f <- idata
    for (f in factors) {
        idata_f[[f]] <- factor(idata_f[[f]], levels = levels_list[[f]])
    }

    b_matrix <- stats::model.matrix(idesign, idata_f, contrasts.arg = contrasts_use)

    if (normalize_contrasts) {
        for (col in seq_len(ncol(b_matrix))) {
            len <- sqrt(sum(b_matrix[, col]^2))
            if (len > 0) b_matrix[, col] <- b_matrix[, col] / len
        }
    }

    if (nrow(b_matrix) != ncol(b_matrix)) {
        stop_semnova(
            "the design has ", nrow(b_matrix), " cells but ", ncol(b_matrix),
            " effects, a fully crossed design is required"
        )
    }
    c_matrix <- solve(b_matrix)

    list(
        idata = idata,
        idesign = idesign,
        b_matrix = b_matrix,
        c_matrix = c_matrix,
        effect_names = c("(Intercept)", attr(stats::terms(idesign), "term.labels")),
        effect_indices = attr(b_matrix, "assign"),
        levels = levels_list
    )
}

#' @keywords internal
#' @noRd
cell_labels <- function(idata) {
    apply(idata, 1L, function(row) paste(as.character(row), collapse = "_"))
}

#' @keywords internal
#' @noRd
build_etas_mmodel <- function(idata, indicator_levels) {
    cells <- cell_labels(idata)
    etas <- lapply(cells, function(cell) {
        make.names(paste0(indicator_levels, "_", cell))
    })
    names(etas) <- paste0(".eta", seq_along(cells))
    etas
}

#' @keywords internal
#' @noRd
reshape_long_to_wide <- function(data, id, dv, indicator = NULL, withins = NULL, betweens = NULL, covariates = NULL) {
    keep <- c(id, dv, indicator, withins, betweens, covariates)
    check_columns_exist(data, keep, "data")

    d <- as.data.frame(data)[keep]
    cell <- if (is.null(withins)) {
        rep("1", nrow(d))
    } else {
        apply(d[withins], 1L, function(row) paste(as.character(row), collapse = "_"))
    }
    indicator_part <- if (is.null(indicator)) dv else as.character(d[[indicator]])
    d$.column <- make.names(paste0(indicator_part, "_", cell))

    id_cols <- c(id, betweens, covariates)
    wide <- stats::reshape(
        d[c(id_cols, ".column", dv)],
        idvar = id_cols,
        timevar = ".column",
        direction = "wide",
        sep = "@@"
    )
    names(wide) <- sub(paste0("^", dv, "@@"), "", names(wide))
    rownames(wide) <- NULL
    wide
}

#' Expand categorical covariates into contrast columns
#'
#' @description
#' A covariate enters the model as a predictor of every contrast variable,
#' which requires a numeric column. A categorical covariate therefore has to
#' be coded first: `k` levels become `k - 1` contrast columns, exactly as
#' `lm()` would code the same factor.
#'
#' The columns belonging to one factor stay **one term**: they are named
#' together in the returned mapping so that
#' [generate_anova_hypotheses()] can test them jointly, giving a `k - 1`
#' degree-of-freedom effect rather than `k - 1` separate one-df effects.
#'
#' @param data Wide-format data frame, one row per case.
#' @param covariates Character vector of covariate column names.
#' @param contrasts_arg Named list of contrast functions, as passed to
#'   [semnova()]. A covariate not named here uses `default_contrast`.
#' @param default_contrast Contrast function name for unnamed factors.
#'
#' @return A list with `data` (the frame plus any new contrast columns) and
#'   `terms` (named list, covariate -> the column names representing it;
#'   length 1 for a covariate that was already numeric).
#' @export
expand_categorical_covariates <- function(data, covariates, contrasts_arg = list(),
                                          default_contrast = "contr.treatment") {
    terms <- stats::setNames(vector("list", length(covariates)), covariates)

    for (nm in covariates) {
        column <- data[[nm]]
        if (is.numeric(column)) {
            terms[[nm]] <- nm
            next
        }

        f <- as.factor(column)
        if (nlevels(f) < 2L) {
            stop_semnova("covariate `", nm, "` has only one level, so it cannot predict anything")
        }
        contrast_fun <- contrasts_arg[[nm]] %||% default_contrast
        cmat <- do.call(contrast_fun, list(levels(f)))

        suffix <- colnames(cmat) %||% as.character(seq_len(ncol(cmat)))
        new_names <- make.unique(c(names(data), paste(nm, suffix, sep = "_")))
        new_names <- utils::tail(new_names, ncol(cmat))

        coded <- cmat[as.integer(f), , drop = FALSE]
        for (j in seq_along(new_names)) {
            data[[new_names[j]]] <- as.numeric(coded[, j])
        }
        terms[[nm]] <- new_names
    }

    list(data = data, terms = terms)
}

#' @keywords internal
#' @noRd
#' Translate a sphericity specification from design terms into pi indices.
#'
#' [lgc()] is agnostic about factors. It knows contrast variables and their
#' positions, so its `sphericity` argument takes index sets. [semnova()]
#' speaks the design's own language, and its users think in model terms, so it
#' accepts those and translates here: sphericity holds *within* a term, among
#' the contrast variables that term spans.
#'
#' Accepts `TRUE` (every term that spans more than one contrast variable),
#' term names as they appear in the ANOVA table (`"grade"`,
#' `"grade:sentence"`), an interaction given as its factors
#' (`c("grade", "sentence")`), or (unchanged) the raw index sets [lgc()]
#' takes, for anything the term vocabulary cannot express.
resolve_sphericity_terms <- function(sphericity, within_design) {
    if (is.null(sphericity) || (is.logical(sphericity) && !isTRUE(sphericity))) {
        return(list())
    }

    effect_names <- within_design$effect_names
    effect_indices <- within_design$effect_indices
    columns_of <- function(j) which(effect_indices == j - 1L)
    factor_sets <- lapply(strsplit(effect_names, ":", fixed = TRUE), sort)

    if (isTRUE(sphericity)) {
        sets <- lapply(seq_along(effect_names), columns_of)
        return(sets[vapply(sets, length, integer(1L)) > 1L])
    }

    if (is.character(sphericity)) {
        sphericity <- as.list(sphericity)
    }
    if (!is.list(sphericity)) {
        stop_semnova(
            "`sphericity` must be TRUE, a model term, a list of them, or a list of ",
            "contrast-variable index vectors"
        )
    }

    sets <- lapply(sphericity, function(spec) {
        if (!is.character(spec)) {
            return(spec) # an index set: lgc()'s vocabulary, passed through
        }
        if (all(grepl("^\\.pi[0-9]+$", spec))) {
            return(spec) # contrast variables by name, likewise
        }

        hit <- match(spec, effect_names)
        if (length(spec) == 1L && !is.na(hit)) {
            return(columns_of(hit))
        }
        # an interaction may be given as its factors, in any order
        wanted <- sort(unlist(strsplit(spec, ":", fixed = TRUE)))
        hit <- which(vapply(factor_sets, identical, logical(1L), wanted))
        if (length(hit) == 1L) {
            return(columns_of(hit))
        }
        stop_semnova(
            "`sphericity`: no model term matches ", paste(spec, collapse = ":"),
            ". Available terms: ", paste(effect_names, collapse = ", ")
        )
    })

    # a term spanning a single contrast variable has no covariance to constrain
    sets[vapply(sets, length, integer(1L)) > 1L]
}

#' @keywords internal
#' @noRd
#' Resolve residual-term sets to the model variables they cover.
#'
#' [lgc()] takes the variables themselves, because it is handed a wide data
#' frame and knows nothing about what generated it. [semnova()] generates
#' those columns (one per indicator x design cell) so asking its users to
#' write them out is asking them to reconstruct names the function just made
#' up. They describe the set instead ([resid_set()]: which indicators, which
#' conditions) and it is resolved here, where the design is known.
#'
#' Accepted, in decreasing order of expressiveness: a [ResidSet]; a bare
#' character vector of indicator names (all conditions, equal by default for
#' variances and free for covariances, the shorthand that predates
#' `resid_set()`); or model variables named directly, for anything the design
#' vocabulary cannot express. `TRUE` means one set covering everything.
#'
#' A set covering fewer than two terms constrains nothing, most commonly
#' when there is a single occasion, so an indicator appears once, and is
#' dropped with a warning rather than silently doing nothing.
resolve_resid_sets <- function(sets, etas_mmodel, indicator_levels, idata, arg, default_equal) {
    if (is.null(sets) || (is.logical(sets) && !isTRUE(sets))) {
        return(list())
    }
    if (isTRUE(sets)) {
        sets <- list(resid_set()) # everything, in one set
    }
    if (inherits(sets, "ResidSet")) {
        sets <- list(sets)
    }
    if (is.character(sets)) {
        sets <- list(sets)
    }
    if (!is.list(sets)) {
        stop_semnova(
            "`", arg, "` must be TRUE, a resid_set(), or a list of them ",
            "(character vectors of indicator names are also accepted)"
        )
    }

    variables <- unlist(etas_mmodel, use.names = FALSE)

    resolved <- lapply(sets, function(spec) {
        if (!inherits(spec, "ResidSet")) {
            check_character(spec, arg)
            # the shorthand: indicators, or model variables, no conditions
            known <- spec %in% variables
            if (all(known)) {
                return(list(variables = spec, equal = default_equal))
            }
            spec <- resid_set(indicator = spec, equal = default_equal)
        }

        indicator <- spec$indicator
        if (is.null(indicator)) {
            indicator <- indicator_levels
        }
        unknown <- setdiff(indicator, indicator_levels)
        if (length(unknown) > 0L) {
            stop_semnova(
                "`", arg, "`: no indicator called ", paste(unknown, collapse = ", "),
                ". Indicators: ", paste(indicator_levels, collapse = ", ")
            )
        }

        # which occasions the conditions leave in
        keep <- rep(TRUE, nrow(idata))
        for (factor_name in names(spec$conditions)) {
            if (!factor_name %in% names(idata)) {
                stop_semnova(
                    "`", arg, "`: no within-subject factor called ", factor_name,
                    ". Factors: ", paste(names(idata), collapse = ", ")
                )
            }
            wanted <- spec$conditions[[factor_name]]
            available <- as.character(unique(idata[[factor_name]]))
            unknown_levels <- setdiff(wanted, available)
            if (length(unknown_levels) > 0L) {
                stop_semnova(
                    "`", arg, "`: ", factor_name, " has no level ",
                    paste(unknown_levels, collapse = ", "),
                    ". Levels: ", paste(available, collapse = ", ")
                )
            }
            keep <- keep & as.character(idata[[factor_name]]) %in% wanted
        }

        occasions <- which(keep)
        columns <- unlist(lapply(match(indicator, indicator_levels), function(k) {
            vapply(etas_mmodel[occasions], function(occasion) occasion[[k]], character(1L),
                USE.NAMES = FALSE
            )
        }), use.names = FALSE)

        list(variables = columns, equal = spec$equal %||% default_equal)
    })

    too_small <- vapply(resolved, function(s) length(s$variables) < 2L, logical(1L))
    if (any(too_small)) {
        warning(
            "semnova: ", sum(too_small), " `", arg, "` set(s) cover fewer than two residual terms ",
            "and are ignored, with a single occasion, or a single indicator in one condition, ",
            "there is nothing to constrain",
            call. = FALSE
        )
    }
    resolved[!too_small]
}
