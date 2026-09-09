# column names referenced inside ggplot2::aes(); declared so R CMD check
# doesn't read them as undefined globals. Using bare names (rather than the
# .data pronoun) keeps rlang out of Imports, ggplot2 itself is only a
# Suggests, used conditionally.
utils::globalVariables(c("contrast", "estimate", ".lower", ".upper"))

#' @keywords internal
#' @noRd
match_hypothesis <- function(fit, terms) {
    check_character(terms, "terms")
    wanted <- sort(terms)
    descriptions <- vapply(fit$hypotheses, function(h) h$description, character(1L))

    matches <- vapply(descriptions, function(d) {
        identical(sort(strsplit(d, ":", fixed = TRUE)[[1L]]), wanted)
    }, logical(1L))

    if (!any(matches)) {
        stop_semnova(
            "no hypothesis matches term(s) ", paste(terms, collapse = ":"),
            ". Available: ", paste(descriptions, collapse = ", ")
        )
    }
    if (sum(matches) > 1L) {
        stop_semnova("term(s) ", paste(terms, collapse = ":"), " match more than one hypothesis")
    }

    fit$hypotheses[[which(matches)]]
}

#' Plot the estimated contrasts of a hypothesis
#'
#' @description
#' Visualises one hypothesis' contrast estimates. Built directly on
#' [contrast_estimates()] (the same call [test_hypothesis()] makes) so
#' the plot and the test can never disagree about what was estimated.
#'
#' Give it either a `hypothesis` object or the `terms` of a model term
#' (e.g. `c("grade", "sentence")` for the grade-by-sentence interaction),
#' which is matched against the hypotheses derived from the design.
#'
#' @param x An [Lgc] object.
#' @param hypothesis A [Hypothesis] object. Optional if `terms` is given.
#' @param terms Character vector naming a model term, matched against the
#'   fit's hypotheses. Optional if `hypothesis` is given.
#' @param geom One or more of `"point"`, `"line"`, `"bar"`.
#' @param interval `"ci"` for confidence intervals, `"sd"` for
#'   between-person SD bands, `"none"` for no error bars.
#' @param level Confidence level, when `interval = "ci"`.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot.Lgc <- function(
    x,
    hypothesis = NULL,
    terms = NULL,
    geom = c("point", "line"),
    interval = c("ci", "sd", "none"),
    level = 0.95,
    ...
) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop_semnova("plotting requires the 'ggplot2' package; install it with install.packages(\"ggplot2\")")
    }
    geom <- match.arg(geom, choices = c("point", "line", "bar"), several.ok = TRUE)
    interval <- match.arg(interval)

    if (is.null(hypothesis)) {
        if (is.null(terms)) {
            stop_semnova("supply either `hypothesis` or `terms` (e.g. terms = c(\"grade\", \"sentence\"))")
        }
        hypothesis <- match_hypothesis(x, terms)
    }

    result <- contrast_estimates(x, hypothesis, level = level)
    dat <- result$table
    # the contrast names are the table's row labels, so the plot reads them
    # from there rather than from a column
    dat$contrast <- factor(rownames(dat), levels = rownames(dat))

    lower <- switch(interval,
        ci = dat$ci_lower,
        sd = dat$estimate - dat$sd_between,
        none = NULL
    )
    upper <- switch(interval,
        ci = dat$ci_upper,
        sd = dat$estimate + dat$sd_between,
        none = NULL
    )

    p <- ggplot2::ggplot(dat, ggplot2::aes(x = contrast, y = estimate, group = 1L))

    if ("bar" %in% geom) {
        p <- p + ggplot2::geom_col(fill = "grey70")
    }
    if ("line" %in% geom) {
        p <- p + ggplot2::geom_line()
    }
    if ("point" %in% geom) {
        p <- p + ggplot2::geom_point(size = 2)
    }
    if (!is.null(lower)) {
        dat$.lower <- lower
        dat$.upper <- upper
        p <- p + ggplot2::geom_errorbar(
            data = dat,
            mapping = ggplot2::aes(ymin = .lower, ymax = .upper),
            width = 0.15
        )
    }

    p +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
        ggplot2::labs(
            title = result$description,
            subtitle = switch(interval,
                ci = sprintf("%.0f%% confidence intervals", 100 * level),
                sd = "bands: between-person SD",
                none = NULL
            ),
            x = "contrast",
            y = "estimate"
        ) +
        ggplot2::theme_classic()
}
