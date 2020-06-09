#' @keywords internal
get_model_string <- function(mod) {
    mod@model_string <- paste0("\n  # loadings",    get_model_string_internal(mod@mmodel),
                               "\n  # intercepts",  get_model_string_internal(mod@intercepts),
                               "\n  # variances",   get_model_string_internal(mod@variances),
                               "\n  # struc_coeff", get_model_string_internal(mod@struc_coeff),
                               "\n  # regressions", get_model_string_internal(mod@regressions),
                               "\n  # constraints", get_model_string_internal(mod@constraints))
    if (!is.null(mod@append)) {
        mod@model_string <- paste0(mod@model_string,
                                   "\n  # user defined syntax\n", mod@append)
    }


    return(mod)
}

#' @keywords internal
get_model_string_internal <- function(par_table) {
    if (is_emtpy_par_table(par_table)) {
        return("")
    }

    par_table <- get_par_table_internal(par_table)

    lhss <- as.character(unique(par_table$lhs))
    ops <- as.character(unique(par_table$op))

    model_string <- ""

    for (lhs in lhss) {
        for (op in ops) {
            par_table_sub <- par_table[par_table$lhs == lhs & par_table$op == op, ]

            if (nrow(par_table_sub) > 0) {
                model_string <- paste0(model_string,
                                       "\n    ",
                                       lhs,
                                       " ",
                                       op)

                for (rhs in 1:nrow(par_table_sub)) {
                    not_first <- if (rhs!=1) " +" else ""

                    if (par_table_sub[rhs, "free"] == 1) {
                        model_string <- paste0(model_string,
                                               not_first,
                                               " ",
                                               par_table_sub[rhs, "rhs"])
                    } else {
                        model_string <- paste0(model_string,
                                               not_first,
                                               " ",
                                               par_table_sub[rhs, "value"],
                                               "*",
                                               par_table_sub[rhs, "rhs"])
                    }
                    if (par_table_sub[rhs, "na"] == 1) {
                        model_string <- paste0(model_string,
                                               not_first,
                                               " + NA*",
                                               par_table_sub[rhs, "rhs"])
                    }

                }
            }
        }
    }
    return(model_string)
}

