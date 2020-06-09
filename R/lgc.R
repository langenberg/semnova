#' Printing the summary for an LGC object.
#' @export
#' @param object \code{lgc} object. The object to get a summary about.
#' @param ... Additional arguments. Currently none supported.
setMethod("summary", signature(object = "lgc"),
          function(object, ...) {
              if(is.null(object@hypotheses)) {
                  warning("No hypotheses supplied. Returning lavaan summary.")
                  summary(object@sem_obj)
              } else {
                  cat("------------------------------------------\n\n")
                  for (hypothesis in object@hypotheses) {

                      cat(paste0("term: ", hypothesis$term,"\n"))

                      if (!is.null(hypothesis$multiv_tests)) {
                          cat("\n")
                          print(hypothesis$multiv_tests$imatrix)
                          cat("\n")
                          print(hypothesis$multiv_tests$tests)
                      }

                      if (!is.null(hypothesis$univ_tests)) {
                          cat("\n")
                          print(hypothesis$univ_tests)
                      }

                      if (!is.null(hypothesis$random_tests)) {
                          cat("\n")
                          print(hypothesis$random_tests)
                      }

                      cat("\n------------------------------------------\n\n")
                  }
              }
          }
)

#' @keywords internal
get_par_table <- function(mod) {
    return(rbind(get_par_table_internal(mod@mmodel),
                 get_par_table_internal(mod@intercepts),
                 get_par_table_internal(mod@variances),
                 get_par_table_internal(mod@struc_coeff),
                 get_par_table_internal(mod@regressions),
                 get_par_table_internal(mod@constraints)))
}

#' @keywords internal
get_mmodel <- function(mod) {
    return(get_par_table_internal(mod@mmodel))
}

#' @keywords internal
get_intercepts <- function(mod) {
    return(get_par_table_internal(mod@intercepts))
}

#' @keywords internal
get_variances <- function(mod) {
    return(get_par_table_internal(mod@variances))
}

#' @keywords internal
get_struc_coeff <- function(mod) {
    return(get_par_table_internal(mod@struc_coeff))
}

#' @keywords internal
get_regressions <- function(mod) {
    return(get_par_table_internal(mod@regressions))
}

#' @keywords internal
get_constraints <- function(mod) {
    return(get_par_table_internal(mod@constraints))
}

#' @keywords internal
#' @importFrom methods new
new_lgc <- function() {
    lgc <- new("lgc",
               dvs                = character(),
               lvs                = character(),
               indicators         = character(),
               groups             = character(),
               lv_labels          = character(),
               mmodel             = new_par_table(),
               intercepts         = new_par_table(),
               variances          = new_par_table(),
               struc_coeff        = new_par_table(),
               regressions        = new_par_table(),
               constraints        = new_par_table(),
               C_matrix           = matrix(),    # contrast matrix, dimension
               append             = NULL,
               sem_obj            = NULL,    # sem object
               model_string       = character(), # sem model string
               hypotheses         = NULL,
               opts               = init_options(),
               N                  = numeric())
}

#' General function to specify a general latent growth components model.
#' @export
#' @importFrom methods is
#' @param data Dataframe. Data object to be passed to lavaan.
#' @param mmodel Object of class \code{mmodel}. If not provided, manifest variables from the formula object will be used.
#' Otherwise, use \code{create_mmodel()} to specify measurement model.
#' @param C_matrix Contrast matrix. Must be invertible.
#' @param verbose Boolean. Print details during procedure.
#' @param compound_symmetry Boolean. When set to TRUE, compound symmetry is assumed.
#' @param sphericity Boolean or formula. When set to TRUE, sphericity is assumed for all effects.
#' @param multiv_tests Character vector. Multivariate test statistics that are to be computed.
#' Possible statistics are: \code{c("wilks", "wald")}. Default is \code{multiv_tests = c("wilks", "wald")}.
#' @param univ_tests Character vector. Univariate test statistics that are to be computed.
#' Possible statistics are: \code{c("F")}. Default is \code{univ_tests = NULL}.
#' @param append Character. Syntax that is to be appended to lavaan syntax.
#' @param hypotheses List of numeric vectors. Each list element represents a hypothesis.
#' For each hypothesis, the contrasts indicated by the elements of the vectors are tested against zero.
#' @param ... Additional arguments to be passed to lavaan.
#' @param covariates Not implemented yet.
#' @param groups Not implemented yet.
#' @param randomization Not yet supported.
#' @return Function returns an lgc object. Use \code{summary(object)} to print
#' hypotheses. Otherwise use \code{object@sem_obj} to get access to the underlying
#' lavaan object.
#' @examples
#'
#' set.seed(323412431)
#'
#' data <- get_test_data()
#'
#' mmodel <- create_mmodel(
#'     A1B1 = "var1",
#'     A2B1 = "var2",
#'     A3B1 = "var3",
#'     A1B2 = "var4",
#'     A2B2 = "var5",
#'     A3B2 = "var6",
#'     lv_scaling = "referent"
#' )
#'
#' hypotheses <- list(
#'     Intercept = c(1),
#'     A        = c(2, 3),
#'     B        = c(4),
#'     AB       = c(5, 6)
#' )
#'
#' C_matrix <- matrix(
#'     c(1, 1, 0, 1, 1, 0,
#'       1, 0, 1, 1, 0, 1,
#'       1,-1,-1, 1,-1,-1,
#'       1, 1, 0,-1,-1, 0,
#'       1, 0, 1,-1, 0,-1,
#'       1,-1,-1,-1, 1, 1),
#'     nrow=6
#' )
#'
#' fit_lgc <- lgc(data, mmodel, C_matrix, hypotheses)
#' summary(fit_lgc)
#'
lgc <- function(
    data,
    mmodel,
    C_matrix,
    hypotheses        = NULL,
    covariates        = NULL,
    groups            = NULL,
    append            = NULL,
    verbose           = FALSE,
    compound_symmetry = FALSE,
    sphericity        = FALSE,
    multiv_tests      = c("wilks", "wald"),
    univ_tests        = NULL,
    randomization     = list(ncores = 1, nsamples = 1e3),
    ...
) {

    ###################################
    ### 1. some parameter checking  ###
    ###################################

    if (!is.null(hypotheses)) {
        if (!is.list(hypotheses)) {
            hypotheses <- list(hypotheses)
        }
        if (is.null(names(hypotheses))) {
            names(hypotheses) <- paste0("hypothesis", seq(1, length(hypotheses)))
        }
        hypotheses <- lapply(1:length(hypotheses),
                             function(x) list(term   = names(hypotheses)[x],
                                              indices= hypotheses[[x]]))
    }

    if (ncol(C_matrix) != nrow(C_matrix)) {
        C_matrix <- fill_C_matrix_rows(C_matrix)
    }

    if (det(C_matrix) == 0) {
        stop("C_matrix is not invertible.")
    }

    if (is.null(mmodel)) {
        stop("Please provide a measurement model.")
    } else if (is(mmodel, "mmodel")) {
        if (is.null(names(mmodel$list))) {
            dvs <- paste0(".", unlist(mmodel$list))
            names(mmodel$list) <- dvs
        } else if (any(names(mmodel$list) == "")) {
            dvs <- names(mmodel$list)
            has_name <- (names(mmodel$list) != "")
            dvs[!has_name] <- paste0(".", unlist(mmodel$list[[!has_name]]))
            names(mmodel$list) <- dvs
        }
    } else if (!is(mmodel, "mmodel")) {
        stop("Don't know how to deal with this mmodel. Maybe, have another look at the documentation.")
    }

    ###############################
    ## 2. pass parameters to lgc ##
    ###############################

    return(lgc_internal(data              = data,
                        mmodel            = mmodel,
                        C_matrix          = C_matrix,
                        dvs               = names(mmodel$list),
                        hypotheses        = hypotheses,
                        covariates        = covariates,
                        groups            = groups,
                        append            = NULL,
                        verbose           = verbose,
                        compound_symmetry = compound_symmetry,
                        sphericity        = sphericity,
                        multiv_tests      = multiv_tests,
                        univ_tests        = univ_tests,
                        randomization     = randomization,
                        ...))
}

#' @import lavaan
#' @keywords internal
#' @importFrom methods is
lgc_internal <- function(data,
                         mmodel,
                         C_matrix,
                         dvs,
                         hypotheses        = NULL,
                         covariates        = NULL,
                         groups            = NULL,
                         append            = NULL,
                         verbose           = FALSE,
                         compound_symmetry = FALSE,
                         sphericity        = FALSE,
                         multiv_tests      = c("wilks", "wald"),
                         univ_tests        = c("F"),
                         randomization     = list(ncores = 1, nsamples = 1e3),
                         ...) {

    ###################################
    ### 1. some parameter checking  ###
    ###################################

    if (!is.null(covariates)) {
        stop("Covariates are not supported in this version.")
    }
    if (!is.null(groups)) {
        stop("Multiple groups are not supported in this version.")
    }
    if(any(grepl("^\\.",names(data)))) {
        stop("Please don't use dots [.] at the beginning of variable names.")
    }
    if (is.list(groups)) {
        groups <- unlist(groups)
    }
    for (group in groups) {
        if(any(grepl("\\.",data[,group]))) {
            stop("Please don't use dots [.] in your group names.")
        }
    }
    if (is.character(mmodel) && length(mmodel) == 1L) {
        stop("Passing a character string as measurement model is not yet supported.")
    } else if (!is(mmodel, "mmodel")) {
        stop("Don't know how to deal with this mmodel. Maybe, have another look at the documentation.")
    }
    if (compound_symmetry & length(sphericity) > 0L) {
        compound_symmetry <- FALSE
        warning("Both sphericity and compound symmetry have been specified. Neglecting compound symmetry.")
    }

    #########################
    ### 2. specify model  ###
    #########################

    mod <- new_lgc()
    mod@C_matrix <- C_matrix
    mod@hypotheses <- hypotheses
    mod@append <- append
    mod@dvs <- dvs

    ## didnt have the effort yet to go with a more elegant solution (match.call)
    mod <- set_options(
        mod,
        compound_symmetry = compound_symmetry,
        sphericity        = sphericity,
        verbose           = verbose,
        multiv_tests      = multiv_tests,
        univ_tests        = univ_tests,
        randomization     = randomization
    )

    mod <- specify_measurement_model(mod, mmodel)
    mod <- specify_intercepts_indicator(mod, mmodel)
    mod <- specify_intercepts_dv(mod)
    mod <- specify_structural_coefficients(mod)
    mod <- specify_regressions(mod)
    mod <- specify_variances(mod)

    if (verbose == "detailed") {
        message("\n[verbose] parameter table:\n",
                get_par_table(mod),
                "\n\n")
    }

    #######################
    ### 3. specify SEM  ###
    #######################

    mod <- get_model_string(mod)

    if (verbose == "detailed") {
        message("\n[verbose] model string:\n",
                mod@model_string,
                "\n\n")
    }

    mod@N <- nrow(data)

    return(lgc_fit(mod, data, ...))

}

#' @keywords internal
lgc_fit <- function(mod, data, ...) {

    ###################
    ### 4. run SEM  ###
    ###################

    mod@sem_obj <- lavaan::sem(mod@model_string, data, ...)

    ###########################
    ### 5. test hypotheses  ###
    ###########################

    mod <- test_hypotheses(mod, data)

    return(mod)
}

