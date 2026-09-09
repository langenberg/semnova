#' Hypothesis: a general linear hypothesis contrast specification
#'
#' @description
#' Holds the four matrices of a general linear hypothesis
#' `H0: G . (L . B_g . M) = K` (see `vignettes/glh.Rmd`). A pure data
#' container, all field validation/defaulting logic lives in
#' [validate_hypothesis()], not in this class; construct with [hypothesis()]
#' rather than `Hypothesis$new()` directly.
#'
#' @rdname Hypothesis-class
#' @export
Hypothesis <- R6::R6Class(
    "Hypothesis",
    public = list(
        #' @field M Numeric matrix, `q x r`. The within-subject contrast
        #'   matrix.
        M = NULL,
        #' @field G Numeric matrix, `d x n_groups`. The group contrast
        #'   matrix.
        G = NULL,
        #' @field L Numeric matrix, `h x (p+1)`. The predictor contrast
        #'   matrix.
        L = NULL,
        #' @field K Numeric array, `d x h x r`. The target (usually zero).
        K = NULL,
        #' @field description Character. Human-readable hypothesis label.
        description = "",
        #' @field contrast_names Character vector. One name per row of the
        #'   resulting contrast.
        contrast_names = "",

        #' @description Construct a Hypothesis. Prefer [hypothesis()].
        #' @param M,G,L,K See field docs. May be `NULL` and filled in later
        #'   by [validate_hypothesis()] when omittable (single eta/group/no
        #'   covariates).
        #' @param description,contrast_names See field docs.
        initialize = function(M = NULL, G = NULL, L = NULL, K = NULL, description = "", contrast_names = "") {
            self$M <- M
            self$G <- G
            self$L <- L
            self$K <- K
            self$description <- description
            self$contrast_names <- contrast_names
            invisible(self)
        }
    )
)

#' Construct a general linear hypothesis
#'
#' @description
#' Constructor-wrapper front door for the [Hypothesis] R6 class. Prefer
#' this over `Hypothesis$new()`.
#'
#' @param M Numeric matrix, `q x r`. The within-subject contrast matrix.
#'   May be omitted (`NULL`) if there is only one eta.
#' @param G Numeric matrix, `d x n_groups`. The group contrast matrix. May
#'   be omitted if there is only one group.
#' @param L Numeric matrix, `h x (p+1)`. The predictor contrast matrix. May
#'   be omitted if there are no covariates.
#' @param K Numeric array, `d x h x r`. The target (usually zero). May be
#'   omitted (defaults to all zeros).
#' @param description Character. Human-readable hypothesis label.
#' @param contrast_names Character vector. One name per row of the
#'   resulting contrast.
#' @return A [Hypothesis] object.
#' @export
hypothesis <- function(M = NULL, G = NULL, L = NULL, K = NULL, description = "", contrast_names = "") {
    Hypothesis$new(M = M, G = G, L = L, K = K, description = description, contrast_names = contrast_names)
}

#' Validate and default-fill a Hypothesis against a fit
#'
#' @description
#' Fills in default `M`/`G`/`L`/`K` when they're omittable (a single eta, a
#' single group, or no covariates respectively) and checks every supplied
#' matrix's dimensions against `fit`'s public fields
#' (`fit$etas_mmodel`/`fit$group_labels`/`fit$covariates_mmodel`) --
#' errors with an informative message otherwise. Mutates `hypothesis` in
#' place (R6 objects are reference-semantics) and returns it, so repeated
#' validation of an already-filled hypothesis is a cheap no-op dimension
#' check.
#'
#' @param hypothesis A [Hypothesis] object.
#' @param fit An object exposing `etas_mmodel`, `group_labels`,
#'   `covariates_mmodel` as public fields (an `Lgc`/`Semnova` fit, or
#'   anything with the same shape, e.g. a plain list, for testing).
#' @return `hypothesis`, invisibly mutated.
#' @export
validate_hypothesis <- function(hypothesis, fit) {
    if (!inherits(hypothesis, "Hypothesis")) {
        stop("semnova: `hypothesis` must be a Hypothesis object (see hypothesis())", call. = FALSE)
    }

    n_etas <- length(fit$etas_mmodel)
    n_groups <- length(fit$group_labels)
    n_covariates <- length(fit$covariates_mmodel)

    if (is.null(hypothesis$M)) {
        if (n_etas != 1L) {
            stop("semnova: hypothesis$M must be provided when there is more than one eta (", n_etas, ")", call. = FALSE)
        }
        hypothesis$M <- matrix(1, ncol = 1, nrow = 1)
    } else if (nrow(hypothesis$M) != n_etas) {
        stop("semnova: nrow(hypothesis$M) must equal the number of etas (", n_etas, "), got ", nrow(hypothesis$M), call. = FALSE)
    }

    if (is.null(hypothesis$G)) {
        if (n_groups != 1L) {
            stop("semnova: hypothesis$G must be provided when there is more than one group (", n_groups, ")", call. = FALSE)
        }
        hypothesis$G <- matrix(1, ncol = 1, nrow = 1)
    } else if (ncol(hypothesis$G) != n_groups) {
        stop("semnova: ncol(hypothesis$G) must equal the number of groups (", n_groups, "), got ", ncol(hypothesis$G), call. = FALSE)
    }

    if (is.null(hypothesis$L)) {
        if (n_covariates != 0L) {
            stop("semnova: hypothesis$L must be provided when there are covariates (", n_covariates, ")", call. = FALSE)
        }
        hypothesis$L <- matrix(1, ncol = 1, nrow = 1)
    } else if (ncol(hypothesis$L) != (n_covariates + 1L)) {
        stop("semnova: ncol(hypothesis$L) must equal the number of covariates + 1 (", n_covariates + 1L, "), got ", ncol(hypothesis$L), call. = FALSE)
    }

    dim_K <- c(nrow(hypothesis$G), nrow(hypothesis$L), ncol(hypothesis$M))
    if (is.null(hypothesis$K)) {
        hypothesis$K <- array(0, dim = dim_K)
    } else if (!identical(as.integer(dim(hypothesis$K)), as.integer(dim_K))) {
        stop(
            "semnova: dim(hypothesis$K) must be (", paste(dim_K, collapse = ", "),
            "), got (", paste(dim(hypothesis$K), collapse = ", "), ")", call. = FALSE
        )
    }

    invisible(hypothesis)
}
