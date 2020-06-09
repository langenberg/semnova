#' Latent repeated-measures ANOVA using the LGC approach
#' @export
#' @description Function specifies an LGC model. The idata object is used to create
#' the contrast matrix that is passed to the \code{lgc()} function. Typical
#' hypotheses are specified as well.
#' @param formula Formula.
#' @param data Dataframe. Data object to be passed to lavaan.
#' @param mmodel Object of class \code{mmodel}. If not provided, manifest variables from the formula object will be used.
#' Otherwise, use \code{create_mmodel()} to specify measurement model.
#' @param idata Dataframe. The dataframe contains the factorial design.
#' @param idesign Formula. Within-subjects design formula.
#' @param verbose Boolean. Print details during procedure.
#' @param icontrasts Character vector. Use this argument to select the type of contrasts to be used.
#' Default is \code{c("contr.sum", "contr.poly")} (not ordered, ordered).
#' @param compound_symmetry Boolean. When set to TRUE, compound symmetry is assumed among
#' dependent variables.
#' @param ... Additional arguments to be passed to lavaan.
#' @param sphericity Boolean or formula. When set to TRUE, sphericity is assumed for all effects.
#' @param multiv_tests Character vector. Multivariate test statistics that are to be computed.
#' Possible statistics are: \code{c("wilks", "wald")}. Default is \code{multiv_tests = c("wilks", "wald")}.
#' @param univ_tests Character vector. Univariate test statistics that are to be computed.
#' Possible statistics are: \code{c("F")}. Default is \code{univ_tests = NULL}.
#' @param randomization Not yet supported.
#' @param covariates Not implemented yet.
#' @param groups Not implemented yet.
#' @param append Character vector. Syntax that is to be appended to lavaan syntax.
#' @return Function returns an lgc object. Use \code{summary(object)} to print
#' hypotheses. Otherwise use \code{object@sem_obj} to get access to the underlying
#' lavaan object.
#' @examples
#'
#' set.seed(323412431)
#'
#' data <- get_test_data()
#'
#' idata  <- expand.grid(A = c("A1", "A2", "A3"), B = c("B1", "B2"))
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
#' fit_semnova <-
#'     semnova(
#'         formula = cbind(A1B1, A2B1, A3B1, A1B2, A2B2, A3B2) ~ 1,
#'         data = data,
#'         idata = idata,
#'         idesign = ~ A * B,
#'         mmodel = mmodel
#'     )
#'
#' summary(fit_semnova)
#'
semnova <- function(
    formula,
    idesign,
    idata,
    data,
    mmodel            = NULL,
    covariates        = NULL,
    groups            = NULL,
    append            = NULL,
    icontrasts        = c("contr.poly", "contr.sum"),
    verbose           = FALSE,
    compound_symmetry = FALSE,
    sphericity        = FALSE,
    multiv_tests      = c("wilks", "wald"),
    univ_tests        = c("F"),
    randomization     = list(ncores = 1, nsamples = 1e3),
    ...
) {

    ###################################
    ### 0. some parameter checking  ###
    ###################################

    if (!is.null(covariates) | !intercept_only(formula) | !is.null(groups)) {
        stop("Covariates and multiple groups are not supported in this version.")
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

    if (is.null(mmodel)) {
        dvs <- extract_dep_var(formula)
        mmodel <- as.list(extract_dep_var(formula))
        dvs <- paste0(".", dvs)
        names(mmodel) <- dvs
        mmodel <- create_mmodel(list = mmodel)
    } else if (is.character(mmodel) && length(mmodel) == 1L) {
        dvs <- extract_dep_var(formula)
        mmodel <- create_mmodel(mmodel)
    } else if (is(mmodel, "mmodel")) {
        dvs <- extract_dep_var(formula)
        is_included <- sapply(dvs, function(dv) dv %in% names(mmodel$list))
        if (!all(is_included)) {
            to_be_included <- list(dvs[!is_included])
            names(to_be_included) <- paste0(".", to_be_included)
            dvs[!is_included] <- paste0(".", dvs[!is_included])
            mmodel$list <- c(mmodel$list, to_be_included)
        }
    } else if (!is(mmodel, "mmodel")) {
        stop("Don't know how to deal with this mmodel. Maybe, have another look at the documentation.")
    }

    #################################
    ## 1. create contrast matrix C ##
    #################################

    C_matrix <- get_contrast_matrix(idata, idesign, icontrasts)

    if (verbose == "detailed") {
        message("\n[verbose] contrast matrix:\n",
                C_matrix,
                "\n")
    }

    ###############################
    ## 2. get list of hypotheses ##
    ###############################

    hypotheses <- get_hypotheses(idesign, C_matrix)

    if (verbose == "detailed") {
        message("\n[verbose] hypotheses:\n",
                hypotheses,
                "\n")
    }

    ##################################
    ## 3. parse sphericity formualt ##
    ##################################

    sphericity <- parse_sphericity_formula(sphericity, idesign)

    ###############################
    ## 4. pass parameters to lgc ##
    ###############################

    if (verbose == "detailed") {
        message("\n[verbose] passing parameters to lgc function\n")
    }

    return(lgc_internal(data              = data,
                        mmodel            = mmodel,
                        C_matrix          = C_matrix,
                        dvs               = dvs,
                        hypotheses        = hypotheses,
                        covariates        = covariates,
                        groups            = groups,
                        verbose           = verbose,
                        compound_symmetry = compound_symmetry,
                        sphericity        = sphericity,
                        multiv_tests      = multiv_tests,
                        univ_tests        = univ_tests,
                        randomization     = randomization,
                        ...))
}
