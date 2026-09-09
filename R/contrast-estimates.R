#' Estimate and test a general linear hypothesis on a fitted model
#'
#' @description
#' Pipeline stage 3, and the single place the contrast math happens.
#' One function tests every effect the package reports. The default
#' coefficient table, each ANOVA term, a contrast plot and any hypothesis
#' you build by hand all call it with a different [Hypothesis]. There is no
#' separate code path for
#' "built-in" versus "custom" hypotheses.
#'
#' Returns both the per-contrast estimates (with standard errors,
#' confidence bounds, and between-person SD bands) and the omnibus Wald
#' test of the whole hypothesis.
#'
#' @param fit An [Lgc] object.
#' @param hypothesis A [Hypothesis] object (see [hypothesis()]).
#' @param level Confidence level for the interval columns.
#'
#' @return An object of class `semnova_contrast`: a list with `table`,
#'   `test` (list with `stat`, `df`, `p.value`), and `description`.
#'
#'   `table` is a coefficient table. It is a data.frame of class `"anova"`, whose
#'   rows are labelled with the contrast names and whose columns are
#'   `estimate`, `std_error`, `z value`, `ci_lower`, `ci_upper`, `sd_between`
#'   and `Pr(>|z|)`. The last two names are the ones
#'   [stats::printCoefmat()] looks for, so the p-values print formatted and
#'   with significance codes; index them with backticks.
#' @export
contrast_estimates <- function(fit, hypothesis, level = 0.95) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }
    if (!inherits(hypothesis, "Hypothesis")) {
        stop_semnova("`hypothesis` must be a Hypothesis object (see hypothesis())")
    }
    if (!is.numeric(level) || length(level) != 1L || is.na(level) || level <= 0 || level >= 1) {
        stop_semnova("`level` must be a single number strictly between 0 and 1")
    }

    validate_hypothesis(hypothesis, fit)
    coefs <- regression_coefficients(fit)

    R <- contrast_matrix(G = hypothesis$G, L = hypothesis$L, M = hypothesis$M)$R

    if (ncol(R) != length(coefs$B_vec)) {
        stop_semnova(
            "contrast matrix has ", ncol(R), " columns but the model has ",
            length(coefs$B_vec), " pi-layer coefficients, check the hypothesis' G/L/M dimensions"
        )
    }

    est <- as.vector(R %*% coefs$B_vec)
    v_est <- R %*% coefs$B_vec_vcov %*% t(R)
    se <- sqrt(diag(v_est))
    sd_between <- sqrt(pmax(diag(R %*% coefs$B_var_vec %*% t(R)), 0))

    z <- est / se
    crit <- stats::qnorm(1 - (1 - level) / 2)

    contrast_names <- hypothesis$contrast_names
    if (length(contrast_names) != length(est) || identical(contrast_names, "")) {
        contrast_names <- paste0("contrast", seq_along(est))
    }

    # Built as a coefficient table: the contrast names are row labels rather
    # than a column, and the statistic and p-value columns carry the names
    # printCoefmat() recognises, so it formats the p-values and adds
    # significance codes. A character column here would be converted to factor
    # codes by that formatting, taking the labels with it.
    table <- data.frame(
        estimate = est,
        std_error = se,
        `z value` = z,
        ci_lower = est - crit * se,
        ci_upper = est + crit * se,
        sd_between = sd_between,
        `Pr(>|z|)` = 2 * stats::pnorm(-abs(z)),
        row.names = contrast_names,
        check.names = FALSE
    )
    table <- structure(table, heading = "Contrast estimates:", class = c("anova", "data.frame"))

    # omnibus Wald test of the full hypothesis
    k <- as.vector(hypothesis$K)
    q <- est - k
    v_inv <- tryCatch(solve(v_est), error = function(e) MASS::ginv(v_est))
    stat <- as.numeric(t(q) %*% v_inv %*% q)
    df <- qr(R)$rank

    structure(
        list(
            table = table,
            test = list(stat = stat, df = df, p.value = stats::pchisq(stat, df = df, lower.tail = FALSE)),
            description = hypothesis$description
        ),
        class = "semnova_contrast"
    )
}

#' Test a hypothesis on a fitted model
#'
#' @description
#' Alias for [contrast_estimates()]. The same object is both "the
#' estimates for this contrast" and "the test of this hypothesis", so both
#' names are accurate and neither wraps extra logic.
#'
#' @inheritParams contrast_estimates
#' @return An object of class `semnova_contrast`.
#' @export
test_hypothesis <- function(fit, hypothesis, level = 0.95) {
    contrast_estimates(fit, hypothesis, level = level)
}

#' @export
print.semnova_contrast <- function(x, ...) {
    if (nzchar(x$description)) {
        cat("Hypothesis: ", x$description, "\n\n", sep = "")
    }
    print(x$table, digits = 4)
    cat(sprintf(
        "\nOmnibus Wald test: chisq(%d) = %.3f, p = %.4f\n",
        x$test$df, x$test$stat, x$test$p.value
    ))
    invisible(x)
}
