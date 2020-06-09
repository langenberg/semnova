#' @keywords internal
#' @importFrom stats model.matrix contrasts<-
contrast_names <- function(idata, idesign) {
    ## this is necessary because model.matrix returns weird colnames when using
    ## "contr.poly". Therefore, procedure has to be rerun with "contr.sum" in
    ## order to return correct colnames
    icontrasts <- c("contr.sum", "contr.sum")
    imatrix <- model.matrix(idesign, idata)
    # idesign <- as.formula(paste0("~", paste0(names(idata),collapse="*")))
    for (i in 1:length(idata)) {
        contrasts(idata[,i]) <- if (is.ordered(idata[,i])) icontrasts[2] else icontrasts[1]
    }
    imatrix <- model.matrix(idesign, idata)
    return(colnames(imatrix))
}

#' @keywords internal
depvar_names <- function(idata) {
    return(apply(idata, 1, paste0, collapse="."))
}

#' @keywords internal
#' @importFrom stats as.formula
idesign_by_idata <- function(idata) {
    return(as.formula(paste0("~", paste0(names(idata),collapse="*"))))
}

#' @keywords internal
get_term_names <- function(idesign = NULL, idata = NULL) {
    if (!is.null(idesign)) {
        return(c("(Intercept)", labels(terms(idesign))))
    } else if (!is.null(idata)) {
        return(idesign_by_idata(idata))
    } else {
        return(NULL)
    }
}

#' @keywords internal
get_contrast_matrix <- function(idata, idesign, icontrasts) {
    ## set contrast type
    for (i in 1:length(idata)) {
        contrasts(idata[,i]) <- if (is.ordered(idata[,i])) icontrasts[2] else icontrasts[1]
    }

    ## create imatrix
    imatrix <- model.matrix(idesign, idata)
    rownames(imatrix) <- depvar_names(idata)
    colnames(imatrix) <- contrast_names(idata, idesign)

    ## scale columns to the length of 1
    if (icontrasts[1] == "contr.poly") {
        for (i in 1:ncol(imatrix)) {
            imatrix[,i] <- imatrix[,i] / sqrt(sum(imatrix[,i]^2))
        }
    }

    # idesign does not include all possible terms.
    # Thus, contrast matrix is not a square matrix.
    # -> fill contrast matrix will orthogonal linearly independent columns
    if (nrow(imatrix) != ncol(imatrix)) {
        new_imatrix <- fill_C_matrix_cols(imatrix)
        attr(new_imatrix, "assign") <- attr(imatrix, "assign")
        attr(new_imatrix, "contrasts") <- attr(imatrix, "contrasts")
        dimnames(new_imatrix) <- list(rownames(imatrix),
                                      c(colnames(imatrix),
                                        paste0("C", seq(ncol(imatrix)+1, nrow(imatrix)))))
        imatrix <- new_imatrix
    }

    return(t(imatrix))
}

#' @keywords internal
get_hypotheses <- function(idesign, imatrix) {
    ## create hypotheses
    term_names <- get_term_names(idesign)
    ## step1: select imatrix indices
    hypo_inds <- lapply(unique(attr(imatrix, "assign")), function(x) which(attr(imatrix, "assign") == x))
    ## merge hypotheses into list
    hypo_list <- lapply(1:length(hypo_inds),
                        function(x) list(term=term_names[x],
                                         indices=hypo_inds[[x]]))

    # hypo_list <- lapply(1:length(hypo_inds),
    #                     function(x) list(term=term_names[x],
    #                                      indices=hypo_inds[[x]],
    #                                      imatrix=t(imatrix[hypo_inds[[x]],,drop=FALSE])))

    names(hypo_list) <- get_term_names(idesign)

    return(hypo_list)
}

