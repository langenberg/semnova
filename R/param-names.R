#' Parameter naming engine
#'
#' @description
#' semnova identifies model parameters (loadings, intercepts, residual
#' (co)variances, pi-layer regression coefficients, ...) by fixed-format
#' lavaan labels. `param_label()` builds one from a parameter's identity when
#' writing lavaan syntax, in any of three renderings. `parse_param_label()`
#' reads a label's type and indices back off a fitted model.
#' `regression_labels()` enumerates the one family a caller needs as a
#' positioned array rather than one label at a time.
#'
#' Every label the package uses is produced or parsed here. Nothing else
#' should ever `paste0()` a parameter label.
#'
#' Deliberately not an object: every piece of information a label depends on
#' is already available wherever a label is needed, either as a function
#' parameter during model construction or as a field on a fitted `Lgc`/
#' `Semnova` object afterwards.
#'
#' Labels encode by *index*, not by name (`.lambda_3_2_1`, not
#' `.lambda_grade1_sentence_Y2_group1`), so that construct/indicator/group
#' names (which may themselves contain underscores) never have to be
#' embedded in, and re-extracted from, the label string.
#'
#' @name param-names
NULL

#' @describeIn param-names One label for one fully-specified parameter.
#' @param type Character. One of `"loading"`, `"intercept"`, `"resid_var"`,
#'   `"resid_cov"`, `"resid_var_set"`, `"resid_cov_set"`, `"latent_mean"`,
#'   `"latent_var"`, `"latent_cov"`, `"pi_intercept"`, `"pi_var"`,
#'   `"pi_cov"`, `"regression"`, `"group_weight"`.
#' @param set Integer index of a constrained set of residual terms (see
#'   [resid_set()]), whose members share one parameter.
#' @param construct,construct2 Integer index of a construct (eta or
#'   covariate).
#' @param indicator,indicator2 Integer index of an indicator within a
#'   construct.
#' @param pi,pi2 Integer index of a pi (contrast) variable.
#' @param covariate Integer index of a covariate.
#' @param group Integer index of a group.
#' @param format How to render the parameter. `"syntax"` (the default) is
#'   the lavaan label the model is written with; `"text"` and `"plotmath"`
#'   are the readable forms for printing and for plot annotations. The
#'   identity of a parameter is its type and indices, so all three come
#'   from the same arguments. No caller should have to build a label string
#'   and hand it back to be re-parsed.
#' @export
param_label <- function(
    type,
    construct = NULL,
    construct2 = NULL,
    indicator = NULL,
    indicator2 = NULL,
    pi = NULL,
    pi2 = NULL,
    covariate = NULL,
    group = NULL,
    set = NULL,
    format = c("syntax", "text", "plotmath")
) {
    format <- match.arg(format)
    s <- function(x) {
        if (is.null(x)) {
            stop("semnova: param_label() missing an index for type '", type, "'", call. = FALSE)
        }
        as.character(as.integer(x))
    }

    if (format != "syntax") {
        return(render_parts(param_parts(type,
            construct = construct, construct2 = construct2,
            indicator = indicator, indicator2 = indicator2,
            pi = pi, pi2 = pi2, covariate = covariate, set = set, group = group
        ), format))
    }

    switch(type,
        loading      = sprintf(".lambda_%s_%s_%s", s(construct), s(indicator), s(group)),
        intercept    = sprintf(".nu_%s_%s_%s", s(construct), s(indicator), s(group)),
        resid_var    = sprintf(".theta_%s_%s_%s", s(construct), s(indicator), s(group)),
        resid_cov    = sprintf(".theta_%s_%s_%s_%s", s(construct), s(indicator), s(indicator2), s(group)),
        # a set's members share one parameter, so the set is what names it
        resid_var_set = sprintf(".theta_set_%s_%s", s(set), s(group)),
        resid_cov_set = sprintf(".theta_cov_%s_%s", s(set), s(group)),
        latent_mean  = sprintf(".alpha_%s_%s", s(construct), s(group)),
        latent_var   = sprintf(".psi_%s_%s", s(construct), s(group)),
        latent_cov   = sprintf(".psi_%s_%s_%s", s(construct), s(construct2), s(group)),
        pi_intercept = sprintf(".alpha_pi_%s_%s", s(pi), s(group)),
        pi_var       = sprintf(".psi_pi_%s_%s", s(pi), s(group)),
        pi_cov       = sprintf(".psi_pi_%s_%s_%s", s(pi), s(pi2), s(group)),
        regression   = sprintf(".beta_%s_%s_%s", s(pi), s(covariate), s(group)),
        group_weight = sprintf(".gw_%s", s(group)),
        stop("semnova: unknown parameter type '", type, "'", call. = FALSE)
    )
}

#' @describeIn param-names The label array for the pi-layer regression
#'   coefficients: `(covariate x pi x group)`, with `"(Intercept)"` as the
#'   first covariate row. This is the one family that has to be enumerated
#'   rather than built one label at a time, because
#'   [regression_coefficients()] fills an array of estimates from it and the
#'   two must agree position for position. That array is the `B` of the
#'   general linear hypothesis (see `vignette("glh")`). It is stated on the
#'   pi layer, so it holds no loadings, intercepts or residuals.
#'   Everything else is named with [param_label()] at the point of use.
#' @param pis Character or integer vector naming/indexing the pi variables.
#' @param covariates Character vector of covariate names (used only for
#'   `dimnames`; indices are derived from position).
#' @param groups Character vector of group labels (used only for
#'   `dimnames`; indices are derived from position).
#' @export
regression_labels <- function(pis, covariates = character(), groups = NULL) {
    n_grp <- max(1L, length(groups))
    grp_labels <- if (is.null(groups) || length(groups) == 0L) "1" else groups
    n_pi <- length(pis)
    n_cov <- length(covariates)

    arr <- array("", dim = c(n_cov + 1L, n_pi, n_grp))
    for (g in seq_len(n_grp)) {
        for (p in seq_len(n_pi)) {
            arr[1L, p, g] <- param_label("pi_intercept", pi = p, group = g)
            for (cv in seq_len(n_cov)) {
                arr[cv + 1L, p, g] <- param_label("regression", pi = p, covariate = cv, group = g)
            }
        }
    }
    dimnames(arr) <- list(
        covariate = c("(Intercept)", covariates),
        pi = if (is.null(names(pis))) as.character(pis) else names(pis),
        group = grp_labels
    )
    arr
}

#' @describeIn param-names Inverse of [param_label()]: recover a label's
#'   type and indices. The label format is fixed, so parsing needs no design
#'   data. Returns `list(type = NA_character_)` if `label`
#'   was not produced by [param_label()] (e.g. it's a raw manifest variable
#'   name).
#' @param label Character(1). A label produced by [param_label()].
#' @export
parse_param_label <- function(label) {
    match1 <- function(pattern) {
        m <- regmatches(label, regexec(pattern, label))[[1]]
        if (length(m) == 0L) NULL else m
    }
    as_int <- function(x) as.integer(x)

    if (!is.null(v <- match1("^\\.lambda_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "loading", construct = as_int(v[2]), indicator = as_int(v[3]), group = as_int(v[4])))
    }
    if (!is.null(v <- match1("^\\.nu_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "intercept", construct = as_int(v[2]), indicator = as_int(v[3]), group = as_int(v[4])))
    }
    if (!is.null(v <- match1("^\\.theta_set_(\\d+)_(\\d+)$"))) {
        return(list(type = "resid_var_set", set = as_int(v[2]), group = as_int(v[3])))
    }
    if (!is.null(v <- match1("^\\.theta_cov_(\\d+)_(\\d+)$"))) {
        return(list(type = "resid_cov_set", set = as_int(v[2]), group = as_int(v[3])))
    }
    if (!is.null(v <- match1("^\\.theta_(\\d+)_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "resid_cov", construct = as_int(v[2]), indicator = as_int(v[3]), indicator2 = as_int(v[4]), group = as_int(v[5])))
    }
    if (!is.null(v <- match1("^\\.theta_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "resid_var", construct = as_int(v[2]), indicator = as_int(v[3]), group = as_int(v[4])))
    }
    if (!is.null(v <- match1("^\\.alpha_pi_(\\d+)_(\\d+)$"))) {
        return(list(type = "pi_intercept", pi = as_int(v[2]), group = as_int(v[3])))
    }
    if (!is.null(v <- match1("^\\.alpha_(\\d+)_(\\d+)$"))) {
        return(list(type = "latent_mean", construct = as_int(v[2]), group = as_int(v[3])))
    }
    if (!is.null(v <- match1("^\\.psi_pi_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "pi_cov", pi = as_int(v[2]), pi2 = as_int(v[3]), group = as_int(v[4])))
    }
    if (!is.null(v <- match1("^\\.psi_pi_(\\d+)_(\\d+)$"))) {
        return(list(type = "pi_var", pi = as_int(v[2]), group = as_int(v[3])))
    }
    if (!is.null(v <- match1("^\\.psi_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "latent_cov", construct = as_int(v[2]), construct2 = as_int(v[3]), group = as_int(v[4])))
    }
    if (!is.null(v <- match1("^\\.psi_(\\d+)_(\\d+)$"))) {
        return(list(type = "latent_var", construct = as_int(v[2]), group = as_int(v[3])))
    }
    if (!is.null(v <- match1("^\\.beta_(\\d+)_(\\d+)_(\\d+)$"))) {
        return(list(type = "regression", pi = as_int(v[2]), covariate = as_int(v[3]), group = as_int(v[4])))
    }
    if (!is.null(v <- match1("^\\.gw_(\\d+)$"))) {
        return(list(type = "group_weight", group = as_int(v[2])))
    }

    list(type = NA_character_)
}

#' @keywords internal
#' @noRd
#' The greek letter and subscripts a parameter is written with. This is the
#' one place that knows what a parameter looks like as *maths*, as the
#' `switch()` in [param_label()] is the one place that knows what it looks
#' like as lavaan syntax, both driven by the same identity.
param_parts <- function(type, ...) {
    greek <- c(
        loading = "lambda", intercept = "nu", resid_var = "theta", resid_cov = "theta",
        resid_var_set = "theta", resid_cov_set = "theta",
        latent_mean = "alpha", latent_var = "psi", latent_cov = "psi",
        pi_intercept = "alpha", pi_var = "psi", pi_cov = "psi",
        regression = "beta", group_weight = "w"
    )
    if (!type %in% names(greek)) {
        stop("semnova: unknown parameter type '", type, "'", call. = FALSE)
    }
    idx <- list(...)
    idx <- unlist(idx[!vapply(idx, is.null, logical(1))])
    # the pi layer shares alpha/psi with the eta layer, so it is marked
    pi_family <- type %in% c("pi_intercept", "pi_var", "pi_cov")
    # a set-shared residual parameter is marked, so it is not mistaken for one
    # indicator's own, "s" for the set it stands for
    set_family <- type %in% c("resid_var_set", "resid_cov_set")
    prefix <- if (pi_family) "pi" else if (set_family) "s" else NULL
    subs <- as.character(c(prefix, idx))
    # index 0 means "one parameter across this whole dimension", an equality
    # constraint from the invariance level. Indices are 1-based, so 0 is free
    # to carry that meaning, and it reads better as a star than as a zero.
    subs[subs == "0"] <- "*"
    list(greek = greek[[type]], subs = subs)
}

#' @keywords internal
#' @noRd
#' Greek letter plus subscripts, as text or as a plotmath expression string.
render_parts <- function(parts, format) {
    if (is.null(parts)) {
        return(if (format == "plotmath") "" else NA_character_)
    }
    if (format == "text") {
        return(sprintf("%s_{%s}", parts$greek, paste(parts$subs, collapse = ",")))
    }
    if (length(parts$subs) == 0L) {
        return(parts$greek)
    }
    # numbers stay bare; anything else is quoted so plotmath renders it
    subs <- ifelse(grepl("^[0-9]+$", parts$subs), parts$subs, sprintf("'%s'", parts$subs))
    sprintf("%s[%s]", parts$greek, paste(subs, collapse = "*','*"))
}

#' @keywords internal
#' @noRd
#' Greek parts of a label written by something other than this engine.
#'
#' The measurement layer is generated by `semTools::measEq.syntax()`, which
#' labels in its own format (`lambda.1_1`, no leading dot). Reading those is
#' the only reason a label string is ever parsed for display; everything
#' semnova writes itself is formatted from its identity instead.
foreign_parts <- function(label) {
    greek_names <- c(
        "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
        "iota", "kappa", "lambda", "mu", "nu", "xi", "pi", "rho", "sigma",
        "tau", "upsilon", "phi", "chi", "psi", "omega"
    )
    if (is.na(label) || !nzchar(label)) {
        return(NULL)
    }
    # fold semTools' "." separators into "_"; a semnova label reaching here
    # would keep its leading dot as an empty first token, so strip that too
    tokens <- strsplit(gsub(".", "_", sub("^\\.", "", label), fixed = TRUE), "_", fixed = TRUE)[[1L]]
    if (length(tokens) == 0L || !tokens[1L] %in% greek_names) {
        return(NULL)
    }
    subs <- tokens[-1L]
    subs[!nzchar(subs)] <- "_" # an empty subscript position, shown literally
    list(greek = tokens[1L], subs = subs)
}

#' @describeIn param-names Formatting for a label you already have as a
#'   string, such as one read off a fitted model. Prefer
#'   `param_label(format = )` wherever the parameter's identity is known --
#'   this exists because the measurement layer is generated by
#'   `semTools::measEq.syntax()` and carries labels in semTools' own format,
#'   which can only be read back from the string. semnova's own labels are
#'   routed through [parse_param_label()] and formatted from their identity,
#'   so the two agree. `format = "syntax"` is the identity function;
#'   `"plotmath"` gives `""` rather than the raw label for anything that is
#'   not a parameter, since a caller about to `parse()` it has no use for a
#'   variable name. Vectorised over `label`.
#' @param group_labels Character vector mapping a group index back to its
#'   name.
#' @param construct_labels Character vector mapping a construct index back
#'   to its name.
#' @export
pretty_label <- function(label, group_labels = NULL, construct_labels = NULL,
                         format = c("text", "plotmath", "syntax")) {
    format <- match.arg(format)
    if (format == "syntax") {
        return(label)
    }

    vapply(label, function(one) {
        parsed <- parse_param_label(one)
        if (!is.na(parsed$type)) {
            return(do.call(param_label, c(parsed, list(format = format))))
        }
        out <- render_parts(foreign_parts(one), format)
        if (is.na(out)) one else out
    }, character(1L), USE.NAMES = FALSE)
}

#' @describeIn param-names Occasion count for a named construct: `1` for
#'   any covariate (covariates are never repeated over occasions in this
#'   framework, a direct corollary of the identification-scheme guard,
#'   see `../decisions.md`); the shared within-design occasion count for any
#'   eta.
#' @param n_within_occasions Integer, total occasions in the within-design
#'   (e.g. `length(etas_mmodel)`).
#' @param covariate_names Character vector of covariate names.
#' @export
n_occasions <- function(construct, n_within_occasions, covariate_names = character()) {
    if (construct %in% covariate_names) {
        return(1L)
    }
    as.integer(n_within_occasions)
}
