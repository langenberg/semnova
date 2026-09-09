#' Identification scheme for a latent construct's measurement model
#'
#' @description
#' Decides how many identification constraints a latent construct needs,
#' based on how many occasions it's measured at and how many indicators it
#' has, **not** indicator count alone.
#'
#' A construct measured at a single occasion with exactly 2 indicators is
#' under-identified by referent-indicator or effects-coding partial fixing
#' (confirmed empirically: `semTools::measEq.syntax()` shares this gap,
#' 2026-09-03), it needs full tau-equivalence (both loadings fixed to 1,
#' both intercepts fixed to 0) regardless of the requested `lv_scaling`.
#' A construct measured at more than one occasion (or with a single
#' indicator, or with 3+ indicators at a single occasion) is fine with the
#' standard scheme.
#'
#' @param n_occasions Integer(1). How many occasions this construct is
#'   measured at (1 for any covariate, always).
#' @param n_indicators Integer(1). How many indicators this construct has.
#' @param lv_scaling One of `"effect"`, `"referent"`. Ignored when the
#'   degenerate case applies.
#'
#' @return A list with `method`, one of `"trivial"` (single indicator: no
#'   measurement model needed at all, the construct *is* its indicator),
#'   `"full_tau_equivalence"` (the degenerate case above), `"referent"`, or
#'   `"effect_coding"`.
#' @export
identification_scheme <- function(n_occasions, n_indicators, lv_scaling = c("effect", "referent")) {
    lv_scaling <- match.arg(lv_scaling)

    if (n_indicators == 1L) {
        return(list(method = "trivial"))
    }
    if (n_indicators == 2L && n_occasions == 1L) {
        return(list(method = "full_tau_equivalence"))
    }
    list(method = if (lv_scaling == "referent") "referent" else "effect_coding")
}

#' @keywords internal
#' @noRd
invariance_constraints <- function(level) {
    level <- match.arg(level, c("configural", "weak", "strong", "strict"))
    switch(level,
        configural = character(0),
        weak       = "loadings",
        strong     = c("loadings", "intercepts"),
        strict     = c("loadings", "intercepts", "residuals")
    )
}

#' Build the eta/indicator-layer measurement-model lavaan syntax
#'
#' @description
#' Builds the CFA + invariance syntax for the latent constructs measured by
#' `etas_mmodel`. Delegates to [semTools::measEq.syntax()] except for the
#' degenerate single-occasion, 2-indicator case, which
#' [identification_scheme()] routes around it (see its docs) since semTools
#' shares that gap.
#'
#' @param etas_mmodel Named list, construct name -> character vector of
#'   indicator names. All elements are one "occasion" of the same
#'   repeatedly-measured construct(s); `length(etas_mmodel)` is the
#'   occasion count.
#' @param group_labels Character vector of group labels (length 1 for a
#'   single group).
#' @param invariance_within,invariance_between One of `"configural"`,
#'   `"weak"`, `"strong"`, `"strict"`.
#' @param lv_scaling One of `"effect"`, `"referent"`.
#' @param construct_offset Integer added to every construct index in the
#'   generated labels. Constructs are numbered within a call, so the
#'   covariates (each built by its own call) need an offset past the
#'   etas, or two different constructs would be given the same label and
#'   silently constrained equal. See [relabel_measurement_syntax()].
#'
#' @return Character(1) of lavaan model syntax (possibly `""` if every
#'   construct has a single indicator, i.e. nothing needs measuring),
#'   labelled by [param_label()] throughout.
#' @export
build_measurement_syntax <- function(
    etas_mmodel,
    group_labels = "1",
    invariance_within = "strong",
    invariance_between = "strong",
    lv_scaling = c("effect", "referent"),
    construct_offset = 0L
) {
    lv_scaling <- match.arg(lv_scaling)
    eta_names <- names(etas_mmodel)
    n_occasions <- length(etas_mmodel)
    n_indicators <- length(etas_mmodel[[1L]])
    n_groups <- length(group_labels)

    scheme <- identification_scheme(n_occasions, n_indicators, lv_scaling)

    if (scheme$method == "trivial") {
        return("")
    }

    if (scheme$method == "full_tau_equivalence") {
        eta <- eta_names[1L]
        indicators <- etas_mmodel[[1L]]
        return(paste(c(
            sprintf("%s =~ %s", eta, paste0("1*", indicators, collapse = " + ")),
            sprintf("%s ~ 0*1", indicators),
            sprintf("%s ~ 1", eta)
        ), collapse = "\n"))
    }

    id_fac <- if (scheme$method == "referent") "marker" else "effects.code"

    configural <- paste(
        sprintf("%s =~ %s", eta_names, vapply(etas_mmodel, paste, character(1L), collapse = " + ")),
        collapse = "\n"
    )

    syn <- semTools::measEq.syntax(
        configural.model = configural,
        ID.fac = id_fac,
        meanstructure = TRUE,
        group = if (n_groups > 1L) ".group" else NULL,
        group.equal = if (n_groups > 1L) invariance_constraints(invariance_between) else character(0),
        longFacNames = if (n_occasions > 1L) list(.eta = eta_names) else list(),
        long.equal = if (n_occasions > 1L) invariance_constraints(invariance_within) else character(0),
        auto = 0,
        sample.nobs = rep(50L, n_groups)
    )

    relabel_measurement_syntax(
        as.character(syn), etas_mmodel,
        group_labels = group_labels, construct_offset = construct_offset
    )
}

#' Rewrite semTools' parameter labels into semnova's
#'
#' @description
#' `semTools::measEq.syntax()` decides *what is constrained*; semnova
#' decides *what things are called*. semTools labels in its own format
#' (`lambda.1_1`, `nu.1.g1`, no leading dot, occasion and group encoded by
#' name), which would otherwise leave a fitted model carrying two label
#' namespaces, only one of which [param_label()] can read.
#'
#' This substitutes every one of its labels with the engine's, and in doing
#' so makes the label state its own **equality class**. An invariance
#' constraint in semTools' output *is* a shared label, so the set of rows
#' carrying one label says exactly which dimensions are constrained equal:
#' a label spanning several constructs gets construct index `0`, one
#' spanning several groups gets group index `0`. `.lambda_0_1_0` therefore
#' reads "the first indicator's loading, one parameter across every occasion
#' and every group". Nothing is inferred from the requested invariance
#' level. It is read off the constraints actually generated, so partial
#' invariance would be described correctly too.
#'
#' Substitution is by token, so the constraint lines that reference labels
#' (effects-coding sums, for instance) follow automatically.
#'
#' @param syntax Character(1), lavaan syntax as generated by semTools.
#' @param etas_mmodel,group_labels As in [build_measurement_syntax()].
#' @param construct_offset Integer added to every construct index, so that
#'   constructs named in separate calls, the etas, then each covariate --
#'   do not collide and accidentally constrain each other.
#'
#' @return Character(1) of the same syntax, labelled by [param_label()].
#' @keywords internal
relabel_measurement_syntax <- function(syntax, etas_mmodel, group_labels = "1", construct_offset = 0L) {
    if (!nzchar(syntax)) {
        return(syntax)
    }
    eta_names <- names(etas_mmodel)
    indicators <- unlist(etas_mmodel, use.names = FALSE)

    pt <- lavaan::lavaanify(syntax, ngroups = length(group_labels), meanstructure = TRUE)
    pt <- pt[nzchar(pt$label), , drop = FALSE]
    if (nrow(pt) == 0L) {
        return(syntax)
    }

    construct_of <- function(indicator) {
        which(vapply(etas_mmodel, function(ind) indicator %in% ind, logical(1L)))
    }
    indicator_of <- function(indicator) {
        cn <- construct_of(indicator)
        if (length(cn) == 0L) return(integer(0))
        match(indicator, etas_mmodel[[cn]])
    }

    # the identity of the parameter a row describes, or NULL if it is not one
    # the engine names (in which case semTools' label is left alone)
    identity_of <- function(row) {
        if (row$op == "=~" && row$rhs %in% indicators) {
            return(list(type = "loading", construct = construct_of(row$rhs), indicator = indicator_of(row$rhs)))
        }
        if (row$op == "~1" && row$lhs %in% indicators) {
            return(list(type = "intercept", construct = construct_of(row$lhs), indicator = indicator_of(row$lhs)))
        }
        if (row$op == "~1" && row$lhs %in% eta_names) {
            return(list(type = "latent_mean", construct = match(row$lhs, eta_names)))
        }
        if (row$op == "~~" && row$lhs %in% indicators && row$rhs %in% indicators) {
            if (row$lhs == row$rhs) {
                return(list(type = "resid_var", construct = construct_of(row$lhs), indicator = indicator_of(row$lhs)))
            }
            return(list(
                type = "resid_cov", construct = construct_of(row$lhs),
                indicator = indicator_of(row$lhs), indicator2 = indicator_of(row$rhs)
            ))
        }
        if (row$op == "~~" && row$lhs %in% eta_names && row$rhs %in% eta_names) {
            if (row$lhs == row$rhs) {
                return(list(type = "latent_var", construct = match(row$lhs, eta_names)))
            }
            return(list(
                type = "latent_cov", construct = match(row$lhs, eta_names),
                construct2 = match(row$rhs, eta_names)
            ))
        }
        NULL
    }

    old <- unique(pt$label)
    new <- vapply(old, function(lab) {
        rows <- pt[pt$label == lab, , drop = FALSE]
        ids <- lapply(seq_len(nrow(rows)), function(i) identity_of(rows[i, ]))
        if (any(vapply(ids, is.null, logical(1L)))) {
            return(lab) # not a parameter this engine names
        }
        first <- ids[[1L]]
        spans <- function(field) length(unique(vapply(ids, function(id) id[[field]] %||% NA_integer_, integer(1L)))) > 1L

        # a dimension the constraint spans collapses to 0: "shared across all"
        construct <- if (spans("construct")) 0L else first$construct + construct_offset
        group <- if (length(unique(rows$group)) > 1L) 0L else rows$group[1L]

        args <- list(type = first$type, construct = construct, group = group)
        for (field in c("construct2", "indicator", "indicator2")) {
            if (!is.null(first[[field]])) {
                args[[field]] <- if (spans(field)) 0L else first[[field]]
            }
        }
        if (!is.null(first$construct2)) {
            args$construct2 <- args$construct2 + construct_offset
        }
        do.call(param_label, args)
    }, character(1L))

    # Distinct constraints must not end up sharing a name: that would silently
    # impose an equality the model was never given. Only partial invariance
    # could produce it, which is not offered yet, so this is a guard, not a
    # code path, if it ever trips, the label scheme needs another index.
    renamed <- new != old
    if (anyDuplicated(new[renamed])) {
        clash <- new[renamed][duplicated(new[renamed])]
        stop_semnova(
            "cannot relabel the measurement model: distinct constraints would share the label(s) ",
            paste(unique(clash), collapse = ", ")
        )
    }

    # longest first, so one label is never rewritten inside another (nu.1
    # inside nu.10)
    ord <- order(nchar(old), decreasing = TRUE)
    for (i in ord[renamed[ord]]) {
        syntax <- gsub(old[i], new[i], syntax, fixed = TRUE)
    }
    syntax
}

#' Strip a construct's own mean and (co)variance lines from measurement syntax
#'
#' @description
#' [build_measurement_syntax()] (via `semTools::measEq.syntax()`, or its
#' own hand-rolled branch) always leaves each construct's own latent mean
#' and (co)variance free, correct when the measurement syntax is used
#' standalone, but redundant once a pi layer is added: `pi =~ eta` with
#' fixed loadings is an invertible transform, so the pi intercepts and pi
#' (co)variances already carry the etas' entire mean and covariance
#' structure. Leaving both free produces a singular (non-positive-definite)
#' parameter covariance matrix.
#'
#' Removes lines where a construct appears on the left of `~` (its latent
#' mean) or `~~` (its (co)variances); measurement lines (`=~`) are kept.
#' [build_pi_layer_syntax()] emits the corresponding `eta ~ 0*1` and
#' `eta ~~ 0*eta` fixings, so the combined syntax has exactly one
#' specification per parameter and no "modifier ... overwritten" warnings.
#'
#' @param syntax Character(1). Lavaan syntax, e.g. from
#'   [build_measurement_syntax()].
#' @param construct_names Character vector of construct (eta) names whose
#'   own mean/(co)variance lines should be removed.
#' @return Character(1), `syntax` with matching lines removed.
#' @export
strip_construct_structure <- function(syntax, construct_names) {
    if (length(construct_names) == 0L || identical(syntax, "")) {
        return(syntax)
    }
    lines <- strsplit(syntax, "\n", fixed = TRUE)[[1L]]
    # `name ~ ...` (latent mean) and `name ~~ ...` ((co)variance), but not
    # `name =~ ...` (measurement), which has "=" before the "~"
    pattern <- paste0("^\\s*(", paste(construct_names, collapse = "|"), ")\\s*~")
    paste(lines[!grepl(pattern, lines)], collapse = "\n")
}

#' Build the pi-layer lavaan syntax
#'
#' @description
#' `pi =~ eta` with loadings fixed to the (inverse) within-subject contrast
#' matrix, plus free pi intercepts and (co)variances, and eta's own
#' (co)variance fixed to 0 (confirmed necessary via testing: `pi =~ eta` is
#' an invertible fixed-loading transform, so any additional free variance
#' on eta beyond what pi carries is redundant with it and leaves the vcov
#' non-positive-definite). semTools has no equivalent. This layer
#' (contrast-transformed latent variables) is semnova's own statistical
#' contribution, always hand-rolled.
#'
#' The covariance structure of the layer is what `sphericity`,
#' `compound_symmetry` and `variance_homogeneity` control:
#'
#' * By default every pi (co)variance is free and every eta (co)variance is
#'   fixed to 0, an unstructured covariance matrix, carried entirely by
#'   the pi layer.
#' * `sphericity` constrains a set of contrast variables to equal variances
#'   and zero covariances among themselves, the condition classical
#'   repeated-measures ANOVA assumes. Equality is imposed by giving the set
#'   one shared label, named after its lowest-numbered member.
#' * `compound_symmetry` moves the structure to the eta layer instead: one
#'   variance shared by every occasion, one covariance shared by every pair,
#'   with the pi (co)variances fixed to 0. The shared labels use index `0`,
#'   the engine's marker for "one parameter across this dimension".
#' * `variance_homogeneity` shares those parameters across groups as well,
#'   by setting the group index to `0`.
#'
#' @param c_matrix_within Numeric `q x q` contrast matrix.
#' @param eta_names Character vector naming the constructs the pi's are
#'   measured by, one per occasion. For a latent DV these are the eta
#'   names; for a manifest (single-indicator) DV they are the manifest
#'   columns themselves, since there is no separate latent variable.
#'   Passed explicitly rather than read off the contrast matrix's dimnames,
#'   which carry row numbers, not names.
#' @param group_labels Character vector of group labels.
#' @param sphericity List of integer vectors (or pi names), each naming a
#'   set of contrast variables to constrain to equal variances and zero
#'   covariances among themselves. Sets must not overlap. Ignored, with a
#'   warning, when `compound_symmetry` is `TRUE`.
#' @param compound_symmetry Logical. Give the etas a compound-symmetric
#'   covariance matrix (one variance, one covariance) and fix the pi
#'   (co)variances to zero.
#' @param variance_homogeneity Logical. Constrain the (co)variances to be
#'   equal across groups.
#' @return Character(1) of lavaan model syntax.
#' @export
build_pi_layer_syntax <- function(c_matrix_within, eta_names, group_labels = "1",
                                  sphericity = list(), compound_symmetry = FALSE,
                                  variance_homogeneity = FALSE) {
    b_matrix <- solve(c_matrix_within)
    n_pis <- ncol(b_matrix)
    n_etas <- nrow(b_matrix)
    n_groups <- length(group_labels)

    check_character(eta_names, "eta_names")
    if (length(eta_names) != n_etas) {
        stop_semnova(
            "`eta_names` must have one name per occasion (", n_etas, "), got ", length(eta_names)
        )
    }
    pi_names <- paste0(".pi", seq_len(n_pis))

    check_flag(compound_symmetry, "compound_symmetry")
    check_flag(variance_homogeneity, "variance_homogeneity")
    sphericity <- check_sphericity(sphericity, pi_names)
    if (length(sphericity) > 0L && compound_symmetry) {
        warning(
            "semnova: `compound_symmetry = TRUE` structures the eta layer, which leaves ",
            "the pi (co)variances fixed to zero; `sphericity` has nothing left to constrain ",
            "and is ignored",
            call. = FALSE
        )
        sphericity <- list()
    }
    # a parameter shared across groups is one parameter: index 0 says so
    group_index <- function(g) if (variance_homogeneity) 0L else g
    # the set a contrast variable is constrained within, if any
    set_of <- function(p) {
        hit <- which(vapply(sphericity, function(s) p %in% s, logical(1L)))
        if (length(hit) == 0L) NA_integer_ else hit
    }

    group_vec <- function(labels) {
        if (n_groups == 1L) paste0(labels[1L], "*") else sprintf("c(%s)*", paste(labels, collapse = ", "))
    }

    measurement <- vapply(seq_len(n_pis), function(p) {
        terms <- sprintf("%s*%s", format(b_matrix[, p], scientific = FALSE, trim = TRUE), eta_names)
        sprintf("%s =~ %s", pi_names[p], paste(terms, collapse = " + "))
    }, character(1L))

    intercepts <- vapply(seq_len(n_pis), function(p) {
        labels <- vapply(seq_len(n_groups), function(g) param_label("pi_intercept", pi = p, group = g), character(1L))
        sprintf("%s ~ %s1", pi_names[p], group_vec(labels))
    }, character(1L))

    variances <- character(0)
    for (p1 in seq_len(n_pis)) {
        for (p2 in p1:n_pis) {
            set1 <- set_of(p1)
            set2 <- set_of(p2)
            same_set <- !is.na(set1) && !is.na(set2) && set1 == set2

            if (compound_symmetry || (same_set && p1 != p2)) {
                # the eta layer carries the structure instead, or the two are
                # constrained uncorrelated by sphericity
                variances <- c(variances, sprintf("%s ~~ 0*%s", pi_names[p1], pi_names[p2]))
                next
            }

            labels <- vapply(seq_len(n_groups), function(g) {
                if (p1 != p2) {
                    return(param_label("pi_cov", pi = p1, pi2 = p2, group = group_index(g)))
                }
                # every variance in a sphericity set is one parameter, named
                # after the set's lowest-numbered member
                param_label("pi_var", pi = if (same_set) min(sphericity[[set1]]) else p1, group = group_index(g))
            }, character(1L))
            variances <- c(variances, sprintf("%s ~~ %s%s", pi_names[p1], group_vec(labels), pi_names[p2]))
        }
    }

    # eta's own mean is fixed to 0: pi =~ eta is an invertible fixed-loading
    # transform, so the pi intercepts already carry the etas' entire mean
    # structure, and leaving it free is redundant (confirmed via testing).
    eta_means <- sprintf("%s ~ 0*1", eta_names)

    # Same argument for the (co)variances, which is why they are fixed to 0 --
    # unless compound symmetry is asked for, in which case the structure lives
    # here instead and the pi (co)variances above are the ones fixed to 0.
    eta_variances <- character(0)
    for (e1 in seq_len(n_etas)) {
        for (e2 in e1:n_etas) {
            if (!compound_symmetry) {
                eta_variances <- c(eta_variances, sprintf("%s ~~ 0*%s", eta_names[e1], eta_names[e2]))
                next
            }
            labels <- vapply(seq_len(n_groups), function(g) {
                # construct index 0: one variance for every occasion, one
                # covariance for every pair, compound symmetry, as a name
                if (e1 == e2) {
                    param_label("latent_var", construct = 0, group = group_index(g))
                } else {
                    param_label("latent_cov", construct = 0, construct2 = 0, group = group_index(g))
                }
            }, character(1L))
            eta_variances <- c(eta_variances, sprintf("%s ~~ %s%s", eta_names[e1], group_vec(labels), eta_names[e2]))
        }
    }

    paste(c(measurement, intercepts, variances, eta_means, eta_variances), collapse = "\n")
}

#' Build the covariate measurement-model + pi-regression lavaan syntax
#'
#' @description
#' Each latent covariate is its own construct, always measured at a single
#' occasion (covariates are never repeated over occasions in this
#' framework. See [n_occasions()]), so its measurement syntax is built by
#' calling [build_measurement_syntax()] once per covariate with an
#' occasion count of 1. That is the same [identification_scheme()] branch
#' the etas use, reused rather than duplicated. Manifest (single-indicator)
#' covariates need no measurement syntax at all (the "construct" is just
#' the raw variable). Adds `pi ~ covariate` regressions for every
#' covariate regardless of manifest/latent.
#'
#' @param covariates_mmodel Named list, covariate name -> character vector
#'   of indicator names (length 1 for manifest).
#' @param pis Character vector of pi variable names (e.g. `.pi1`, `.pi2`,
#'   ...).
#' @param group_labels Character vector of group labels.
#' @param invariance_between One of `"configural"`, `"weak"`, `"strong"`,
#'   `"strict"`.
#' @param lv_scaling One of `"effect"`, `"referent"`.
#' @param n_eta_constructs Integer, how many constructs the eta side already
#'   used up, so covariate labels start after them rather than colliding
#'   with them.
#' @return Character(1) of lavaan model syntax (`""` if there are no
#'   covariates).
#' @export
build_covariate_syntax <- function(
    covariates_mmodel,
    pis,
    group_labels = "1",
    invariance_between = "strong",
    lv_scaling = c("effect", "referent"),
    n_eta_constructs = 0L
) {
    lv_scaling <- match.arg(lv_scaling)
    if (length(covariates_mmodel) == 0L) {
        return("")
    }

    n_pis <- length(pis)
    n_cov <- length(covariates_mmodel)
    n_groups <- length(group_labels)

    measurement <- vapply(seq_along(covariates_mmodel), function(cv) {
        cov_name <- names(covariates_mmodel)[cv]
        build_measurement_syntax(
            stats::setNames(covariates_mmodel[cov_name], cov_name),
            group_labels = group_labels,
            invariance_within = "strong",
            invariance_between = invariance_between,
            lv_scaling = lv_scaling,
            # past the etas, and past every covariate before this one
            construct_offset = n_eta_constructs + cv - 1L
        )
    }, character(1L))

    group_vec <- function(labels) {
        if (n_groups == 1L) paste0(labels[1L], "*") else sprintf("c(%s)*", paste(labels, collapse = ", "))
    }

    regressions <- vapply(seq_len(n_pis), function(p) {
        terms <- vapply(seq_len(n_cov), function(cv) {
            labels <- vapply(seq_len(n_groups), function(g) param_label("regression", pi = p, covariate = cv, group = g), character(1L))
            sprintf("%s%s", group_vec(labels), names(covariates_mmodel)[cv])
        }, character(1L))
        sprintf("%s ~ %s", pis[p], paste(terms, collapse = " + "))
    }, character(1L))

    paste(c(measurement[measurement != ""], regressions), collapse = "\n")
}

#' Build arbitrary residual-covariance lavaan syntax
#'
#' @description
#' `measEq.syntax()`'s `auto=` only supports blanket same-lag
#' autocorrelation; semnova allows arbitrary user-specified indicator sets
#' (equal or freely varying within a set), so this is always hand-rolled
#' and called with `auto = 0` on the corresponding `measEq.syntax()` calls.
#'
#' @param resid_cov List of character vectors; residual covariances within
#'   each set are freely estimated (each pair its own parameter).
#' @param equal_resid_cov List of character vectors; residual covariances
#'   within each set are constrained equal to each other, sharing one
#'   parameter per group.
#' @param group_labels Character vector of group labels.
#' @return Character(1) of lavaan model syntax (`""` if both are empty).
#' @export
build_resid_cov_syntax <- function(resid_cov = list(), equal_resid_cov = list(), group_labels = "1") {
    n_groups <- length(group_labels)
    group_vec <- function(labels) {
        if (n_groups == 1L) paste0(labels[1L], "*") else sprintf("c(%s)*", paste(labels, collapse = ", "))
    }
    pairs_of <- function(set) {
        if (length(set) < 2L) list() else utils::combn(set, 2L, simplify = FALSE)
    }

    free_lines <- unlist(lapply(resid_cov, function(set) {
        vapply(pairs_of(set), function(pr) sprintf("%s ~~ %s", pr[1L], pr[2L]), character(1L))
    }))

    # every covariance in a set is one parameter, named after the set
    equal_lines <- unlist(lapply(seq_along(equal_resid_cov), function(set_index) {
        labels <- vapply(seq_len(n_groups), function(g) {
            param_label("resid_cov_set", set = set_index, group = g)
        }, character(1L))
        vapply(pairs_of(equal_resid_cov[[set_index]]), function(pr) {
            sprintf("%s ~~ %s%s", pr[1L], group_vec(labels), pr[2L])
        }, character(1L))
    }))

    paste(c(free_lines, equal_lines), collapse = "\n")
}

#' Build the equal-residual-variance lavaan syntax
#'
#' @description
#' Residual variances are freely estimated by default. Each set here is
#' constrained instead: its members share one label, and therefore one
#' parameter, per group. What belongs in a set is a design question --
#' the same indicator across occasions, or one indicator within a condition
#', resolved by [semnova()] before it reaches here (see [resid_set()]).
#'
#' @param equal_resid_var List of character vectors of model variables; the
#'   residual variances within each vector are constrained equal.
#' @param group_labels Character vector of group labels.
#' @return Character(1) of lavaan model syntax (`""` if there is nothing to
#'   constrain).
#' @export
build_resid_var_syntax <- function(equal_resid_var = list(), group_labels = "1") {
    if (length(equal_resid_var) == 0L) {
        return("")
    }
    # Two sets covering one variable would write two labels onto its residual
    # variance, and lavaan keeps the last, so the first set would be silently
    # broken rather than combined with the second.
    variables <- unlist(equal_resid_var, use.names = FALSE)
    if (anyDuplicated(variables)) {
        stop_semnova(
            "`resid_var`: variable(s) appear in more than one set: ",
            paste(unique(variables[duplicated(variables)]), collapse = ", "),
            ". Put them in one set instead, separate sets would overwrite each other"
        )
    }
    n_groups <- length(group_labels)
    group_vec <- function(labels) {
        if (n_groups == 1L) paste0(labels[1L], "*") else sprintf("c(%s)*", paste(labels, collapse = ", "))
    }

    lines <- unlist(lapply(seq_along(equal_resid_var), function(set_index) {
        labels <- vapply(seq_len(n_groups), function(g) {
            param_label("resid_var_set", set = set_index, group = g)
        }, character(1L))
        vapply(equal_resid_var[[set_index]], function(variable) {
            sprintf("%s ~~ %s%s", variable, group_vec(labels), variable)
        }, character(1L), USE.NAMES = FALSE)
    }))

    paste(lines, collapse = "\n")
}

#' Build the group-weight lavaan syntax
#'
#' @description
#' By default a multigroup model treats the group sizes as fixed: they are
#' what they are, and nothing is estimated from them. `"stochastic"` instead
#' treats group membership as random and estimates each group's frequency,
#' which is what you want when the grouping is itself an outcome of sampling
#' rather than a design the study imposed. lavaan's `%` operator carries the
#' weights; the model must also be fitted with `group.w.free = TRUE`.
#'
#' Each group's estimated relative frequency is added as a `relfreq*`
#' definition, so it appears in the parameter estimates rather than having to
#' be worked out from the log weights by hand.
#'
#' @param group_labels Character vector of group labels.
#' @param group_weights One of `"fixed"` or `"stochastic"`.
#' @return Character(1) of lavaan model syntax (`""` for `"fixed"`, or for a
#'   single group, where there is nothing to weight).
#' @export
build_group_weight_syntax <- function(group_labels, group_weights = c("fixed", "stochastic")) {
    group_weights <- match.arg(group_weights)
    n_groups <- length(group_labels)
    if (group_weights == "fixed" || n_groups < 2L) {
        return("")
    }

    labels <- vapply(seq_len(n_groups), function(g) param_label("group_weight", group = g), character(1L))
    # `group` here is lavaan's keyword for the grouping itself, not the name of
    # the grouping column, using the column name is a model-parameter error
    weights <- sprintf("group %% c(%s)*w", paste(labels, collapse = ", "))
    total <- sprintf("N := %s", paste(sprintf("exp(%s)", labels), collapse = " + "))
    rel_freqs <- sprintf("relfreq%d := exp(%s) / N", seq_len(n_groups), labels)

    paste(c(weights, total, rel_freqs), collapse = "\n")
}
