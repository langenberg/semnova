#' ResidSet: a set of residual terms to constrain
#'
#' @description
#' Pure data container: fields only, no behaviour. Describes *which*
#' residual terms a constraint applies to, in the design's own vocabulary:
#' indicators, and the conditions they were measured in. Construct with
#' [resid_set()], which validates; resolving the set to actual model
#' variables happens in [semnova()], the only place that knows the design.
#'
#' @rdname ResidSet-class
#' @export
ResidSet <- R6::R6Class(
    "ResidSet",
    public = list(
        #' @field indicator Character vector of indicator names, or NULL
        #'   for every indicator.
        indicator = NULL,
        #' @field conditions Named list, within-factor -> the levels the set
        #'   is restricted to. Empty for every condition.
        conditions = NULL,
        #' @field equal Logical. Constrain the set's terms to be equal.
        equal = NULL,

        #' @description Construct a ResidSet. Prefer [resid_set()].
        #' @param ... Fields to set, by name.
        initialize = function(...) {
            fields <- list(...)
            for (nm in names(fields)) {
                self[[nm]] <- fields[[nm]]
            }
            invisible(self)
        }
    )
)

#' Select residual terms to constrain
#'
#' @description
#' Names a set of residual variances or covariances for [semnova()] to
#' constrain, by indicator and by condition. Both are optional: omit
#' `indicator` and the set covers every indicator, omit the conditions and
#' it covers every occasion.
#'
#' The set is the unit of constraint. For `resid_var` its members' residual
#' variances are equated; for `resid_cov` residual covariances are added
#' among all of its members, equal to each other or freely estimated
#' according to `equal`.
#'
#' @param indicator Character vector of indicator names, or `NULL` (the
#'   default) for every indicator. Indicators are the levels of the
#'   `indicator` column given to [semnova()]; for a manifest dependent
#'   variable there is only one, the dv itself.
#' @param ... Conditions restricting the set, named by within-subject
#'   factor: `sentence = "landolt"`, `grade = c("grade1", "grade2")`.
#'   Omitted factors are unrestricted.
#' @param equal Logical, or `NULL` (the default) to let the argument the
#'   set is passed to decide. `resid_var` sets are equated, which is the
#'   only reason to name one. `resid_cov` sets are freely estimated.
#'   Set it explicitly to override, most usefully `equal = TRUE` on a
#'   covariance set.
#'
#' @return A [ResidSet] object.
#'
#' @examples
#' # one indicator's residual variances, equal across every occasion
#' resid_set("fix_count")
#'
#' # ... restricted to landolt sentences
#' resid_set("fix_count", sentence = "landolt")
#'
#' # every indicator, at one occasion
#' resid_set(grade = "grade1")
#' @export
resid_set <- function(indicator = NULL, ..., equal = NULL) {
    check_character(indicator, "indicator", null_ok = TRUE)
    if (!is.null(equal)) {
        check_flag(equal, "equal")
    }

    conditions <- list(...)
    if (length(conditions) > 0L) {
        nms <- names(conditions)
        if (is.null(nms) || any(!nzchar(nms))) {
            stop_semnova(
                "conditions in resid_set() must be named by the factor they restrict, ",
                "e.g. resid_set(\"fix_count\", sentence = \"landolt\")"
            )
        }
        if (anyDuplicated(nms)) {
            stop_semnova(
                "resid_set(): factor(s) named more than once: ",
                paste(unique(nms[duplicated(nms)]), collapse = ", "),
                ". Give several levels as a vector instead, e.g. grade = c(\"grade1\", \"grade2\")"
            )
        }
        conditions <- lapply(conditions, function(levels) as.character(levels))
        empty <- nms[vapply(conditions, function(l) length(l) == 0L || anyNA(l), logical(1L))]
        if (length(empty) > 0L) {
            stop_semnova("resid_set(): no levels given for ", paste(empty, collapse = ", "))
        }
    }

    # `equal = NULL` is a real state ("whichever the argument means") so it
    # is stored as such rather than resolved to a default here, where the
    # argument this set will be passed to is not yet known
    set <- ResidSet$new(indicator = indicator, conditions = conditions)
    set$equal <- equal
    set
}

#' @description Print a [ResidSet].
#' @param x A [ResidSet].
#' @param ... Ignored.
#' @rdname resid_set
#' @export
print.ResidSet <- function(x, ...) {
    parts <- c(
        if (is.null(x$indicator)) "every indicator" else paste(x$indicator, collapse = ", "),
        if (length(x$conditions) > 0L) {
            paste(
                mapply(
                    function(nm, levels) paste0(nm, " = ", paste(levels, collapse = ", ")),
                    names(x$conditions), x$conditions
                ),
                collapse = " | "
            )
        },
        if (is.null(x$equal)) "equal or free, by argument" else if (x$equal) "equal" else "free"
    )
    cat("<resid_set>", paste(parts, collapse = " | "), "\n")
    invisible(x)
}
