#' @keywords internal
create_schedule <- function(ncores, njobs) {
    replicate(ncores, {njobs%/%ncores}) + (1:ncores + ncores*(njobs%/%ncores) <= njobs)
}

#' @keywords internal
#' @importFrom stats terms
extract_dep_var <- function(form) {
    if (attr(terms(form), "response") == 0L) {
        return(NULL)
    }

    response <- attr(terms(form), "variables")[[2]]

    if(is.call(response)) {
        ## multivariate
        response <- as.list(response)
        if (response[1] != "cbind") {
            return(NULL)
        }
        response <- response[-1]
        response <- sapply(response, as.character)
        return(response)
    } else {
        ## univariate
        return(as.character(response))
    }
}

#' @keywords internal
intercept_only <- function(form) {
    if (attr(terms(form), "intercept") == 0L) {
        return(FALSE)
    }
    if (length(attr(terms(form), "factors")) > 0L) {
        return(FALSE)
    }
    return(TRUE)
}
