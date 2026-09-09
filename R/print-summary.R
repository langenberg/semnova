#' @keywords internal
#' @noRd
#' Add each model's own fit indices, and their changes, to an LRT table.
#'
#' A chi-square difference test says whether a constraint costs significantly;
#' it says nothing about how well either model fits, and with any real sample
#' it will reject constraints that barely move the fit. Invariance testing is
#' therefore normally read off the descriptive indices and their changes as
#' well, so they belong in the same table.
#'
#' Three details matter here. `lavTestLRT()` already has an `RMSEA` column, but
#' that is RMSEA_D, the RMSEA *of the difference test*, a different quantity
#' from either model's own RMSEA, so it is renamed to say so. The table is
#' ordered by degrees of freedom, not by the order the models were passed, so
#' the indices are matched by row name. And the p-value column has to stay
#' last, since that is how `print.anova()` finds it.
add_fit_indices <- function(comparison, sem_objs, model_names, indices = c("rmsea", "cfi")) {
    if (is.null(indices) || length(indices) == 0L) {
        return(comparison)
    }
    check_character(indices, "indices")
    if (!is.data.frame(comparison) || is.null(rownames(comparison))) {
        return(comparison)
    }
    position <- match(rownames(comparison), model_names)
    if (anyNA(position)) {
        return(comparison) # rows we cannot attribute to a model: leave it alone
    }

    available <- names(lavaan::fitMeasures(sem_objs[[1L]]))
    unknown <- setdiff(indices, available)
    if (length(unknown) > 0L) {
        stop_semnova(
            "`indices`: no fit measure called ", paste(unknown, collapse = ", "),
            ". See lavaan::fitMeasures() for the ", length(available), " available"
        )
    }

    values <- vapply(sem_objs[position], function(object) {
        tryCatch(
            unname(lavaan::fitMeasures(object, indices)),
            error = function(e) rep(NA_real_, length(indices))
        )
    }, numeric(length(indices)))
    if (length(indices) == 1L) {
        values <- matrix(values, nrow = 1L)
    }

    names(comparison)[names(comparison) == "RMSEA"] <- "RMSEA_D"

    # each measure, then its change from the previous (less constrained) model,
    # in the order the caller asked for them
    for (i in seq_along(indices)) {
        column <- toupper(indices[i])
        comparison[[column]] <- values[i, ]
        comparison[[paste(column, "diff")]] <- c(NA_real_, diff(values[i, ]))
    }

    # The fit indices sit between the model-level information criteria and the
    # difference test. The p-value stays last because that is how
    # print.anova() finds it, and anything lavaan added that is not named here
    # (RMSEA_D, a scaling correction) keeps its place just before it.
    p_column <- names(comparison)[grep("^(P|Pr)\\(", names(comparison))]
    order <- c(
        "Df", "AIC", "BIC",
        as.vector(rbind(toupper(indices), paste(toupper(indices), "diff"))),
        "Chisq", "Chisq diff", "Df diff"
    )
    order <- intersect(order, names(comparison))
    rest <- setdiff(names(comparison), c(order, p_column))

    out <- comparison[c(order, rest, p_column)]
    structure(out, heading = attr(comparison, "heading"), class = class(comparison))
}

#' Fit measures for a fitted model
#'
#' @description
#' The usual global fit indices, pulled from the underlying `lavaan` fit.
#' Returns `NULL` for a saturated model (zero degrees of freedom), where
#' they carry no information.
#'
#' @param fit An [Lgc] object.
#' @return A named numeric vector, or `NULL` if the model is saturated.
#' @export
fit_measures <- function(fit) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }
    measures <- lavaan::fitmeasures(fit$sem_obj)
    if (measures[["df"]] <= 0) {
        return(NULL)
    }
    measures[c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")]
}

#' Test every hypothesis attached to a fit
#'
#' @description
#' Runs [test_hypothesis()] over `fit$hypotheses` and stacks the omnibus
#' tests into one table, the ANOVA table for a design fitted by
#' [semnova()]. Hypotheses a user attached by hand are treated no
#' differently from the ones derived from the design.
#'
#' @param fit An [Lgc] object.
#' @return A data.frame with one row per hypothesis, of class
#'   `c("anova", "data.frame")`.
#' @export
anova_table <- function(fit) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }
    if (length(fit$hypotheses) == 0L) {
        stop_semnova("this fit has no hypotheses attached, pass some to lgc(), or use semnova() to derive them from a design")
    }

    results <- lapply(fit$hypotheses, function(h) test_hypothesis(fit, h)$test)
    out <- data.frame(
        Chisq = vapply(results, function(r) r$stat, numeric(1L)),
        Df = vapply(results, function(r) r$df, numeric(1L)),
        `Pr(>Chisq)` = vapply(results, function(r) r$p.value, numeric(1L)),
        check.names = FALSE,
        row.names = vapply(fit$hypotheses, function(h) h$description, character(1L))
    )
    structure(out, heading = "Hypothesis tests:", class = c("anova", "data.frame"))
}

#' Per-contrast estimates for every hypothesis attached to a fit
#'
#' @description
#' Stacks the per-contrast tables from [contrast_estimates()] across all of
#' `fit$hypotheses`, the coefficient-level view that sits underneath
#' [anova_table()]'s omnibus tests.
#'
#' @param fit An [Lgc] object.
#' @param level Confidence level for the interval columns.
#' @return A coefficient table: a data.frame of class `"anova"`, so it prints
#'   with formatted p-values and significance codes. Rows are labelled
#'   `hypothesis: contrast`, and the statistic and p-value columns are named
#'   `z value` and `Pr(>|z|)`, the names [stats::printCoefmat()] recognises.
#'   Index those with backticks.
#' @export
contrast_table <- function(fit, level = 0.95) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }
    tables <- lapply(fit$hypotheses, function(h) {
        tbl <- contrast_estimates(fit, h, level = level)$table
        # the hypothesis a contrast belongs to goes into the row label, not a
        # column: a character column would be lost to printCoefmat(). A
        # contrast named after its own hypothesis (the intercept) is not
        # repeated.
        labels <- rownames(tbl)
        rownames(tbl) <- ifelse(
            labels == h$description, labels, paste0(h$description, ": ", labels)
        )
        tbl
    })

    out <- do.call(rbind, tables)
    structure(out, heading = "Contrast estimates:", class = c("anova", "data.frame"))
}

#' @rdname anova_table
#' @param object An [Lgc] object.
#' @param ... Further [Lgc] objects, to compare nested models instead.
#'
#' @param indices Fit measures to report for each model, as
#'   [lavaan::fitMeasures()] names them. Each appears as a column followed by
#'   its change from the previous, less constrained model, in the order given.
#'   `c("rmsea", "cfi")` by default; `NULL` reports none. Anything
#'   `fitMeasures()` knows works, `c("rmsea", "cfi", "srmr")`, say.
#'
#' @section Comparing models:
#' Given several fits, `anova()` runs a chi-square difference test through
#' [lavaan::lavTestLRT()] and adds each model's own fit indices, plus their
#' change from the previous, less constrained model. The difference test alone
#' answers only whether a constraint costs significantly, which in a large
#' sample it usually does; invariance is conventionally judged on those
#' descriptive changes as well.
#'
#' Note that lavaan's own `RMSEA` column is the RMSEA *of the difference
#' test*, a different quantity from either model's RMSEA, and is therefore
#' shown as `RMSEA_D`.
#' @export
anova.Lgc <- function(object, ..., indices = c("rmsea", "cfi")) {
    others <- list(...)
    is_fit <- vapply(others, inherits, logical(1L), "Lgc")

    if (!any(is_fit)) {
        return(anova_table(object))
    }

    sem_objs <- lapply(c(list(object), others[is_fit]), function(f) f$sem_obj)

    # name the models after the expressions the caller passed, so the
    # comparison table reads "f_configural" rather than a deparsed lavaan
    # object. Both of these matter: `quote = TRUE` keeps do.call() from
    # substituting the S4 objects into the call it builds, and explicit
    # `model.names` keeps lavTestLRT from deparsing them for its heading --
    # either one alone still produces megabytes of output.
    call_args <- as.list(match.call(expand.dots = TRUE))[-1L]
    fit_positions <- c(TRUE, is_fit)
    model_names <- vapply(call_args[which(fit_positions)], function(e) paste(deparse(e), collapse = ""), character(1L))
    if (length(model_names) != length(sem_objs) || anyNA(model_names)) {
        model_names <- paste0("model", seq_along(sem_objs))
    }

    comparison <- do.call(
        lavaan::lavTestLRT,
        c(sem_objs, list(model.names = unname(model_names)), others[!is_fit]),
        quote = TRUE
    )

    add_fit_indices(comparison, sem_objs, unname(model_names), indices)
}

#' @export
print.Lgc <- function(x, ...) {
    n_occasions <- length(x$etas_mmodel)
    n_indicators <- length(x$etas_mmodel[[1L]])
    kind <- if (n_indicators == 1L) "manifest" else sprintf("latent (%d indicators)", n_indicators)

    cat(sprintf("%s fit\n", if (inherits(x, "Semnova")) "semnova" else "lgc"))
    cat(sprintf("  dependent variable : %s\n", kind))
    cat(sprintf("  occasions          : %d\n", n_occasions))
    if (length(x$covariates_mmodel) > 0L) {
        cat(sprintf("  covariates         : %s\n", paste(names(x$covariates_mmodel), collapse = ", ")))
    }
    if (length(x$group_labels) > 1L) {
        cat(sprintf("  groups             : %s\n", paste(x$group_labels, collapse = ", ")))
    }

    measures <- fit_measures(x)
    if (is.null(measures)) {
        cat("  fit                : saturated (df = 0)\n")
    } else {
        cat(sprintf(
            "  fit                : chisq(%d) = %.2f, p = %.3f; CFI = %.3f, RMSEA = %.3f\n",
            measures[["df"]], measures[["chisq"]], measures[["pvalue"]],
            measures[["cfi"]], measures[["rmsea"]]
        ))
    }

    if (length(x$hypotheses) > 0L) {
        cat("\n")
        print(anova_table(x))
    }

    invisible(x)
}

#' Summarize a fitted model
#'
#' @param object An [Lgc] object.
#' @param level Confidence level for the interval columns.
#' @param ... Unused.
#' @return An object of class `semnova_summary`.
#' @export
summary.Lgc <- function(object, level = 0.95, ...) {
    structure(
        list(
            fit = object,
            measures = fit_measures(object),
            anova = if (length(object$hypotheses) > 0L) anova_table(object) else NULL,
            contrasts = if (length(object$hypotheses) > 0L) contrast_table(object, level = level) else NULL
        ),
        class = "semnova_summary"
    )
}

#' @export
print.semnova_summary <- function(x, ...) {
    print(x$fit)
    if (!is.null(x$contrasts)) {
        # the table carries its own heading, being an anova-classed one
        cat("\n")
        print(x$contrasts, digits = 4)
    }
    invisible(x)
}
