#' @keywords internal
new_par_table <- function() {
    par_table <- new("par_table",
                     id    = numeric(),
                     lhs   = character(),
                     op    = character(),
                     rhs   = character(),
                     free  = numeric(),
                     value = character(),
                     group = numeric(),
                     na    = numeric())
}

#' @keywords internal
is_emtpy_par_table <- function(par_table) {
    return(length(par_table@id) == 0L)
}

#' @keywords internal
add_parameter <- function(par_table, lhs, op, rhs, free, value, group=1, na=0) {
    if (length(par_table@id) == 0) {
        par_table@id    <- c(par_table@id, 1)
    } else {
        par_table@id    <- c(par_table@id, max(par_table@id)+1)
    }
    par_table@lhs   <- c(par_table@lhs,   lhs)
    par_table@op    <- c(par_table@op,    op)
    par_table@rhs   <- c(par_table@rhs,   rhs)
    par_table@free  <- c(par_table@free,  free)
    par_table@value <- c(par_table@value, value)
    par_table@group <- c(par_table@group, group)
    par_table@na    <- c(par_table@na,    na)
    return(par_table)
}

#' @keywords internal
remove_parameter <- function(par_table, id) {
    if (length(par_table@id) != 0) {
        par_table@lhs   <- par_table@lhs[par_table@id   != id]
        par_table@op    <- par_table@op[par_table@id    != id]
        par_table@rhs   <- par_table@rhs[par_table@id   != id]
        par_table@free  <- par_table@free[par_table@id  != id]
        par_table@value <- par_table@value[par_table@id != id]
        par_table@group <- par_table@group[par_table@id != id]
        par_table@na    <- par_table@na[par_table@id    != id]
        par_table@id    <- par_table@id[par_table@id    != id]
    }
    return(par_table)
}

#' @keywords internal
#' @importFrom methods slotNames
get_par_table_internal <- function(par_table) {
    if (is_emtpy_par_table(par_table)) {
        return(NULL)
    }
    new_par_table <- data.frame(par_table@id,
                                par_table@lhs,
                                par_table@op,
                                par_table@rhs,
                                par_table@free,
                                par_table@value,
                                par_table@group,
                                par_table@na)
    new_par_table <- as.data.frame(new_par_table, stringAsFactors=FALSE)
    names(new_par_table) <- slotNames(par_table)
    return(new_par_table)
}
