#' Strict argument guards for exported functions
#'
#' @description
#' Every exported function validates its arguments' types/classes/shapes up
#' front and fails fast with an informative error, no silent coercion, no
#' discovering a bad argument three calls deep inside a cryptic lavaan
#' error. These helpers keep those checks short at the call site and identical
#' across entry points.
#'
#' @name guards
#' @keywords internal
NULL

#' @noRd
stop_semnova <- function(...) {
    stop("semnova: ", ..., call. = FALSE)
}

#' @noRd
check_data <- function(data, arg = "data") {
    if (!is.data.frame(data)) {
        stop_semnova("`", arg, "` must be a data.frame or tibble, not ", class(data)[1L])
    }
    if (nrow(data) == 0L) {
        stop_semnova("`", arg, "` has no rows")
    }
    invisible(TRUE)
}

#' @noRd
check_columns_exist <- function(data, columns, arg) {
    missing <- setdiff(columns, names(data))
    if (length(missing) > 0L) {
        stop_semnova(
            "`", arg, "` refers to column(s) not present in the data: ",
            paste(missing, collapse = ", ")
        )
    }
    invisible(TRUE)
}

#' @noRd
check_string <- function(x, arg, null_ok = FALSE) {
    if (is.null(x)) {
        if (null_ok) {
            return(invisible(TRUE))
        }
        stop_semnova("`", arg, "` must not be NULL")
    }
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop_semnova("`", arg, "` must be a single character string")
    }
    invisible(TRUE)
}

#' @noRd
check_character <- function(x, arg, null_ok = FALSE) {
    if (is.null(x)) {
        if (null_ok) {
            return(invisible(TRUE))
        }
        stop_semnova("`", arg, "` must not be NULL")
    }
    if (!is.character(x) || length(x) == 0L || anyNA(x)) {
        stop_semnova("`", arg, "` must be a character vector with no missing values")
    }
    invisible(TRUE)
}

#' @noRd
check_flag <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        stop_semnova("`", arg, "` must be TRUE or FALSE")
    }
    invisible(TRUE)
}

#' @noRd
check_positive <- function(x, arg) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
        stop_semnova("`", arg, "` must be a single positive number")
    }
    invisible(TRUE)
}

#' @noRd
#' A between-subject factor and a covariate are properties of the case, not
#' of the occasion, so they must hold one value per case. Reshaping a column
#' that varies within a case yields missing cells, which surfaces much later
#' as a lavaan complaint about listwise deletion, so it is caught here.
check_constant_within <- function(data, columns, id, arg) {
    varying <- columns[vapply(columns, function(col) {
        any(vapply(split(data[[col]], data[[id]]), function(v) length(unique(v)) > 1L, logical(1L)))
    }, logical(1L))]
    if (length(varying) > 0L) {
        stop_semnova(
            "`", arg, "`: column(s) ", paste(varying, collapse = ", "),
            " take more than one value within a case (`", id,
            "`), so they cannot be a property of the case. A column that varies ",
            "over occasions belongs in `withins`, not in `", arg, "`"
        )
    }
    invisible(TRUE)
}

#' @noRd
check_invariance <- function(x, arg) {
    check_string(x, arg)
    levels <- c("configural", "weak", "strong", "strict")
    if (!x %in% levels) {
        stop_semnova("`", arg, "` must be one of: ", paste(levels, collapse = ", "), ", not \"", x, "\"")
    }
    invisible(TRUE)
}

#' @noRd
check_square_matrix <- function(x, arg, n = NULL) {
    if (!is.matrix(x) || !is.numeric(x)) {
        stop_semnova("`", arg, "` must be a numeric matrix")
    }
    if (nrow(x) != ncol(x)) {
        stop_semnova("`", arg, "` must be square, got ", nrow(x), " x ", ncol(x))
    }
    if (!is.null(n) && nrow(x) != n) {
        stop_semnova("`", arg, "` must be ", n, " x ", n, " to match the design, got ", nrow(x), " x ", ncol(x))
    }
    if (inherits(try(solve(x), silent = TRUE), "try-error")) {
        stop_semnova("`", arg, "` is singular and cannot be inverted")
    }
    invisible(TRUE)
}

#' @noRd
check_hypotheses <- function(x, arg = "hypotheses") {
    if (is.null(x)) {
        return(invisible(TRUE))
    }
    if (inherits(x, "Hypothesis")) {
        return(invisible(TRUE))
    }
    if (!is.list(x) || !all(vapply(x, inherits, logical(1L), "Hypothesis"))) {
        stop_semnova("`", arg, "` must be a Hypothesis object or a list of them (see hypothesis())")
    }
    invisible(TRUE)
}

#' @noRd
check_indicator_list <- function(x, arg) {
    if (!is.list(x) || length(x) == 0L) {
        stop_semnova("`", arg, "` must be a non-empty list")
    }
    ok <- vapply(x, function(el) is.character(el) && length(el) > 0L && !anyNA(el), logical(1L))
    if (!all(ok)) {
        stop_semnova("`", arg, "` must contain only non-empty character vectors of indicator names")
    }
    invisible(TRUE)
}

#' @noRd
#' Sphericity sets: which contrast variables are constrained together.
#' Accepts positions or pi names, and returns integer positions.
check_sphericity <- function(x, pi_names, arg = "sphericity") {
    if (is.null(x) || length(x) == 0L || (is.logical(x) && !isTRUE(x))) {
        return(list())
    }
    if (isTRUE(x)) {
        # every contrast variable in one set: the only reading available here,
        # where there are no model terms to group them by
        return(list(seq_along(pi_names)))
    }
    if (!is.list(x)) {
        stop_semnova("`", arg, "` must be a list of index vectors, e.g. list(c(2, 3, 4))")
    }

    sets <- lapply(x, function(set) {
        if (is.character(set)) {
            hit <- match(set, pi_names)
            if (anyNA(hit)) {
                stop_semnova(
                    "`", arg, "` names contrast variable(s) not in the model: ",
                    paste(set[is.na(hit)], collapse = ", ")
                )
            }
            return(as.integer(hit))
        }
        if (!is.numeric(set) || length(set) == 0L || anyNA(set)) {
            stop_semnova("`", arg, "` must contain non-empty numeric index vectors or contrast-variable names")
        }
        set <- as.integer(set)
        out_of_range <- set[set < 1L | set > length(pi_names)]
        if (length(out_of_range) > 0L) {
            stop_semnova(
                "`", arg, "` refers to contrast variable(s) outside the design (1-",
                length(pi_names), "): ", paste(out_of_range, collapse = ", ")
            )
        }
        set
    })

    sets <- sets[vapply(sets, length, integer(1L)) > 1L] # a set of one constrains nothing
    all_indices <- unlist(sets, use.names = FALSE)
    if (anyDuplicated(all_indices)) {
        stop_semnova(
            "`", arg, "`: contrast variable(s) appear in more than one set: ",
            paste(unique(all_indices[duplicated(all_indices)]), collapse = ", ")
        )
    }
    sets
}
