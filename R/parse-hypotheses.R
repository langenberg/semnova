#' Generate one hypothesis per model term
#'
#' @description
#' Builds the ANOVA-style hypothesis set for a factorial design: one
#' [Hypothesis] per combination of a within-subject term, a between-subject
#' term, and a covariate term (including each design's intercept, which
#' stands for "no term from this design"). Each hypothesis' `M`/`G`/`L`
#' selects exactly the contrast columns/rows belonging to those terms, so
#' testing it is testing that main effect or interaction.
#'
#' The resulting hypotheses are ordinary [Hypothesis] objects, handled by
#' [contrast_estimates()] through exactly the same path as one a user
#' builds by hand.
#'
#' @param within_design,between_design Design lists from [parse_design()].
#' @param n_covariates Integer, number of covariate columns in the model.
#' @param covariate_names Character vector of covariate column names.
#' @param covariate_terms Named list mapping a term name to the covariate
#'   column indices that represent it, for covariates that take more than
#'   one column, a categorical covariate coded into `k - 1` contrast
#'   columns is one term with `k - 1` degrees of freedom, not `k - 1` terms.
#'   `NULL` (the default) gives one term per column.
#'
#' @return A list of [Hypothesis] objects.
#' @export
generate_anova_hypotheses <- function(within_design, between_design, n_covariates = 0L,
                                      covariate_names = character(), covariate_terms = NULL) {
    n_pis <- ncol(within_design$b_matrix)

    # M: selects the pi columns belonging to each within-subject term
    within_indices <- within_design$effect_indices
    Ms <- lapply(unique(within_indices), function(effect) {
        cols <- which(within_indices == effect)
        m <- matrix(0, nrow = n_pis, ncol = length(cols))
        for (i in seq_along(cols)) m[cols[i], i] <- 1
        colnames(m) <- colnames(within_design$b_matrix)[cols]
        m
    })
    names(Ms) <- within_design$effect_names

    # G: the between-subject contrast rows belonging to each between term
    between_indices <- between_design$effect_indices
    Gs <- lapply(unique(between_indices), function(effect) {
        between_design$c_matrix[which(between_indices == effect), , drop = FALSE]
    })
    names(Gs) <- between_design$effect_names

    # L: selects the intercept, or the columns of one covariate term, out of
    # the (1 + n_cov) columns. A term spanning several columns gets one row
    # each, so the hypothesis tests them jointly.
    n_cols_L <- n_covariates + 1L
    if (is.null(covariate_terms)) {
        covariate_terms <- stats::setNames(as.list(seq_len(n_covariates)), covariate_names)
    }
    select_columns <- function(cols) {
        l <- matrix(0, nrow = length(cols), ncol = n_cols_L)
        for (i in seq_along(cols)) l[i, cols[i]] <- 1
        rownames(l) <- colnames(l)[cols] %||% covariate_names[cols - 1L]
        l
    }
    Ls <- c(
        list("(Intercept)" = select_columns(1L)),
        lapply(covariate_terms, function(cols) select_columns(as.integer(cols) + 1L))
    )

    hypotheses <- list()
    for (l_name in names(Ls)) {
        for (g_name in names(Gs)) {
            for (m_name in names(Ms)) {
                parts <- c(m_name, g_name, l_name)
                parts <- parts[parts != "(Intercept)"]
                description <- if (length(parts) == 0L) "(Intercept)" else paste(parts, collapse = ":")

                M <- Ms[[m_name]]
                G <- Gs[[g_name]]
                L <- Ls[[l_name]]

                # R has d * h * r rows, ordered group-outermost, then M
                # column, then L row (see contrast_matrix()); expand.grid
                # varies its first argument fastest, so this matches.
                grid <- expand.grid(
                    l = rownames(L) %||% seq_len(nrow(L)),
                    m = colnames(M) %||% seq_len(ncol(M)),
                    g = seq_len(nrow(G)),
                    stringsAsFactors = FALSE
                )
                contrast_names <- paste0(
                    grid$m,
                    if (nrow(L) > 1L) paste0(":", grid$l) else "",
                    if (nrow(G) > 1L) paste0(":g", grid$g) else ""
                )

                hypotheses[[length(hypotheses) + 1L]] <- hypothesis(
                    M = M, G = G, L = L,
                    description = description,
                    contrast_names = contrast_names
                )
            }
        }
    }

    hypotheses
}
