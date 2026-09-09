#' semnova: latent repeated-measures ANOVA
#'
#' @description
#' Fits repeated-measures ANOVA designs as structural equation models via
#' `lavaan`, so measurement occasions, dependent variables and covariates
#' can be genuinely latent (multi-indicator, measurement-error-corrected)
#' rather than manifest composites.
#'
#' Start with [semnova()] for an ANOVA-style design, or [lgc()] to specify
#' the latent constructs and contrast matrix directly. Results come from
#' [anova_table()]/[test_hypothesis()], and are visualised with [plot()]
#' and [dag()].
#'
#' @importFrom R6 R6Class
#' @keywords internal
"_PACKAGE"
