#' @keywords internal
#' @importFrom stats na.omit
test_hypotheses <- function(mod, data) {
    if (is.null(mod@hypotheses)) {
        return(mod)
    }

    if ("randomization" %in% (get_option(mod, "multiv_tests"))) {
        null_dist <- create_random_null_dist(mod, data)
    }

    mod@hypotheses <- lapply(1:length(mod@hypotheses), function(hypo_index) {

        hypothesis <- mod@hypotheses[[hypo_index]]

        multiv_tests <- NULL
        univ_tests <- NULL
        random_tests <- NULL

        if (!is.null(get_option(mod, "multiv_tests"))) {
            multiv_tests <- matrix(NA, nrow = 2, ncol = 6)

            if ("wald" %in% get_option(mod, "multiv_tests")) {
                multiv_tests[1, ] <- wald(mod, hypothesis)
            }
            if ("wilks" %in% get_option(mod, "multiv_tests")) {
                multiv_tests[2, ] <- wilks(mod, hypothesis)
            }

            colnames(multiv_tests) <- c("Df", "test stat",  "approx F",
                                        "num Df", "den Df", "Pr(>F)")
            rownames(multiv_tests) <- c("Wald", "Wilks")
            multiv_tests <- na.omit(multiv_tests)
            multiv_tests <- as.data.frame(multiv_tests)
            class(multiv_tests) <- c("anova", "data.frame")
            multiv_tests <- structure(
                multiv_tests,
                heading = "multiv. tests:",
                class = c("anova", "data.frame")
            )

            indices <- hypothesis$indices
            imatrix <- t(mod@C_matrix[indices, , drop = FALSE])
            imatrix <- structure(
                as.data.frame(imatrix),
                heading = "Response transformation matrix:",
                class   = c("anova", "data.frame")
            )

            multiv_tests <- list(imatrix = imatrix,
                                 tests = multiv_tests)
        }

        if (!is.null(get_option(mod, "univ_tests"))) {
            if (!is_orthogonal_mat(t(mod@C_matrix))) {
                warning("Contrast matrix is not orthogonal. Univariate F-statistic cannot be computed.",
                        call. = FALSE)
            } else{
                univ_tests <- matrix(NA, nrow = 1, ncol = 6)

                if ("f" %in% get_option(mod, "univ_tests")) {
                    univ_tests[1, ] <- univ_F_test(mod, hypothesis)
                }

                colnames(univ_tests) <- c("Sum Sq", "num Df", "Error SS",
                                          "den Df", "F value", "Pr(>F)")
                rownames(univ_tests) <- c("F-test")
                univ_tests <- na.omit(univ_tests)
                univ_tests <- as.data.frame(univ_tests)
                class(univ_tests) <- c("anova", "data.frame")
                univ_tests <- structure(
                    univ_tests,
                    heading = "univ. test:",
                    class = c("anova", "data.frame")
                )
            }
        }

        if ("randomization" %in% (get_option(mod, "multiv_tests"))) {

            random_tests <- matrix(NA, nrow = 3, ncol = 1)
            random_multiv_null <- null_dist[[hypo_index]]$multiv_tests
            random_univ_null <- null_dist[[hypo_index]]$univ_tests

            random_wald_null <- random_multiv_null[,colnames(random_multiv_null) == "wald"]
            random_wilks_null <- random_multiv_null[,colnames(random_multiv_null) == "wilks"]
            random_f_null <- random_univ_null[,colnames(random_univ_null) == "f"]

            if ("wald" %in% get_option(mod, "multiv_tests")) {
                emp_wald <- multiv_tests$tests[which(rownames(multiv_tests$tests) == "Wald"), "test stat"]
                random_tests[1, ] <- random_wald(emp_wald,
                                                 random_wald_null)
            }
            if ("wilks" %in% get_option(mod, "multiv_tests")) {
                emp_wilks <- multiv_tests$tests[which(rownames(multiv_tests$tests) == "Wilks"), "test stat"]
                random_tests[2, ] <- random_wilks(emp_wilks,
                                                  random_wilks_null)
            }
            if ("f" %in% get_option(mod, "univ_tests")) {
                emp_univ_f <- univ_tests[1, "Pr(>F)"]
                random_tests[3,] <- random_univ_F_test(emp_univ_f,
                                                       random_f_null)
            }

            colnames(random_tests) <- c("Pr(>F)")
            rownames(random_tests) <- c("Wald", "Wilks", "univ. F-test")
            random_tests <- na.omit(random_tests)
            random_tests <- as.data.frame(random_tests)
            class(random_tests) <- c("anova", "data.frame")
            random_tests <- structure(
                random_tests,
                heading = "randomization tests:",
                class = c("anova", "data.frame")
            )

        }

        hypothesis$multiv_tests <- multiv_tests
        hypothesis$univ_tests <- univ_tests
        hypothesis$random_tests <- random_tests

        hypothesis
    })

    return(mod)
}

#' @keywords internal
random_wald <- function(emp, null_dist) {
    return(1 - mean(null_dist <= emp))
}

#' @keywords internal
random_wilks <- function(emp, null_dist) {
    return(1 - mean(null_dist >= emp))
}

#' @keywords internal
random_univ_F_test <- function(emp, null_dist) {
    return(1 - mean(null_dist >= emp))
}

#' @keywords internal
create_random_null_dist <- function(mod, data) {
    mmodel <- NULL # needed for C-RAN, remove on revision!
    stop("create_random_null_dist: Revision needed.")
    if (is.null(get_option(mod, "randomization"))) {
        return(NULL)
    }

    ## modify options to not do the randomization test again
    ## otherwise, we end up in an endless loop
    mod_random <- mod
    mod_random <- set_options(
        mod_random,
        multiv_tests = setdiff(get_option(mod_random, "multiv_tests"),
                               "randomization"),
        univ_tests   = setdiff(get_option(mod_random, "univ_tests"),
                               "randomization")
    )

    ## extract options for randomization from original model
    random_opts <- get_option(mod, "randomization")

    ## we have to maintain the measurement model!
    ## first, get indices of columns of the measurement model
    ## then, randomize latent variables but not indicators within latent variables!
    indices <- lapply(mmodel, sapply, function(x) which(x == names(data)))

    ## do the randomization test using parallel::mclapply
    results <- parallel::mclapply(
        create_schedule(random_opts$ncores, random_opts$nsamples),
        function(nsims) {
            replicate(nsims, {

                ## create randomized order of latent variables for each row
                indices_subj <- replicate(nrow(data), sample(indices), simplify = FALSE)

                ## go through data set and shuffle latent variables
                for (myrow in 1:nrow(data)) {
                    for (depvar in 1:length(indices)) {
                        data[myrow, indices[[depvar]]] <- data[myrow, indices_subj[[myrow]][[depvar]]]
                    }
                }

                ## fit model and extract hypotheses tests
                tryCatch({
                    hypotheses <- lgc_fit(mod_random, data)@hypotheses
                    lapply(hypotheses, function(hypothesis) {
                        list(multiv_tests = hypothesis$multiv_tests$tests[, "test stat"],
                             univ_tests   = hypothesis$univ_tests[, "F value"])
                    })
                }, error = function(e) {
                    NA
                })
            }, simplify = FALSE)
        },
        mc.cores = random_opts$ncores
    )

    ## glue together the simulations from the ncores
    results <- do.call(c, results)

    ## reorganize by hypotheses and multiv/univ tests
    ## new format: $A
    ##             $A$multiv_tests
    ##                         [,1]   [,2]
    ##             [1,]        "wald" "wilks"
    ##             [2,]        "wald" "wilks"
    ##             [3,]        "wald" "wilks"
    ##             ...
    ##             [nsamples,] "wald" "wilks"
    ##
    ##             $A$univ_tests
    ##                         [,1]
    ##             [1,]        "F"
    ##             [2,]        "F"
    ##             [3,]        "F"
    ##             ...
    ##             [nsamples,] "F"
    ##
    ##
    ##             $B
    ##             $B$multiv_tests
    ##                         [,1]   [,2]
    ##             [1,]        "wald" "wilks"
    ##             [2,]        "wald" "wilks"
    ##             [3,]        "wald" "wilks"
    ##             ...
    ##             [nsamples,] "wald" "wilks"
    ##
    ##             $B$univ_tests
    ##                         [,1]
    ##             [1,]        "F"
    ##             [2,]        "F"
    ##             [3,]        "F"
    ##             ...
    ##             [nsamples,] "F"

    results <- lapply(1:length(mod@hypotheses), function(hypothesis) {
        multiv_tests <- do.call(rbind, lapply(results, function(sim) sim[[hypothesis]]$multiv_tests))
        colnames(multiv_tests) <- get_option(mod_random, "multiv_tests")
        univ_tests <- do.call(rbind, lapply(results, function(sim) sim[[hypothesis]]$univ_tests))
        colnames(univ_tests) <- get_option(mod_random, "univ_tests")
        list(multiv_tests = multiv_tests,
             univ_tests   = univ_tests)
    })

    ## give the first level of the result (i.e., the hypotheses) a name
    names(results) <- names(mod@hypotheses)

    return(results)
}

#'@keywords internal
wald <- function(mod, hypothesis) {
    N <- mod@N
    lv_labels <- mod@lv_labels
    sem_obj <- mod@sem_obj

    indices <- hypothesis$indices
    nmeasures <- length(indices)

    wald_string <-
        paste0(paste0(lv_labels[indices], "==0"), collapse = "\n")
    wald <- lavaan::lavTestWald(sem_obj, wald_string)

    return(c(
        df = wald[[2]],
        stat = wald[[1]],
        F_value(wald, N, nmeasures)
    ))
}

#' @keywords internal
#' @importFrom stats pf
F_value <- function(wald, N, nmeasures) {
    F_value <- wald$stat / wald$df
    df1 <- wald$df
    df2 <- (N - 1) * nmeasures
    F_p_value <- 1 - pf(F_value, df1, df2)
    return(c(
        F_value = F_value,
        df1 = df1,
        df2 = df2,
        p_value = F_p_value
    ))
}

#' @keywords internal
#' @importFrom stats pf
wilks <- function(mod, hypothesis) {
    ## variables that we will need
    indices <- hypothesis$indices
    nmeasures <- length(indices)
    variables <- mod@lvs[indices]
    lvs <- mod@lvs

    ## extract relevant data from the lavaan object
    if (get_option(mod, "compound_symmetry")) {
        C_matrix <- mod@C_matrix
        dvs <- mod@dvs

        alpha <- lavInspect(mod@sem_obj, "est")$alpha
        B <- alpha[variables, 1, drop = F]

        # get lv (co)variances
        psi <- lavInspect(mod@sem_obj, "est")$psi
        psi_dvs <- psi[dvs, dvs]
        psi_lvs <- C_matrix %*% psi_dvs %*% t(C_matrix)

        psi[, ] <- 0
        psi[lvs, lvs] <- psi_lvs

        # psi <- solve(B) %*% psi %*% t(solve(B))
    } else {
        alpha <- lavInspect(mod@sem_obj, "est")$alpha
        psi <- lavInspect(mod@sem_obj, "est")$psi
        B <- alpha[variables, 1, drop = F]
    }

    ## calculate H-SSCP and E-SSCP
    HSSCP <- B %*% t(B)
    ESSCP <- psi[variables, variables]
    eig <- Re(eigen(solve(ESSCP) %*% HSSCP)$values)

    ## perform test using wilks stat
    #TODO:DFmustbechecked!!!
    wilks <- wilks_car(eig, 1, mod@N - 1)
    names(wilks) <- c("stat", "F_value", "df1", "df2")

    return(c(df = 1, wilks, 1 - pf(wilks[2], wilks[3], wilks[4])))
}

##Thisfunctionisborrowedfromthecarpackage,therefore_car
#'@keywords internal
wilks_car <- function(eig, q, df.res) {
    test <- prod(1 / (1 + eig))
    p <- length(eig)
    tmp1 <- df.res - 0.5 * (p - q + 1)
    tmp2 <- (p * q - 2) / 4
    tmp3 <- p ^ 2 + q ^ 2 - 5
    tmp3 <- if (tmp3 > 0) sqrt(((p * q) ^ 2 - 4) / tmp3) else 1
    c(
        test,
        ((test ^ (-1 / tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2)) / p / q,
        p * q,
        tmp1 * tmp3 - 2 * tmp2
    )
}

#'@keywords internal
univ_F_test <- function(mod, hypothesis) {
    indices <- hypothesis$indices
    nmeasures <- length(indices)
    variables <- mod@lvs[indices]
    lvs <- mod@lvs

    if (get_option(mod, "compound_symmetry")) {
        C_matrix <- mod@C_matrix
        dvs <- mod@dvs

        alpha <- lavInspect(mod@sem_obj, "est")$alpha
        B <- alpha[variables, 1, drop = F]

        # get lv (co)variances
        psi <- lavInspect(mod@sem_obj, "est")$psi
        psi_dvs <- psi[dvs, dvs]
        psi_lvs <- C_matrix %*% psi_dvs %*% t(C_matrix)

        psi[,] <- 0
        psi[lvs, lvs] <- psi_lvs

        # psi <- solve(B) %*% psi %*% t(solve(B))
    } else {
        alpha <- lavInspect(mod@sem_obj, "est")$alpha
        psi <- lavInspect(mod@sem_obj, "est")$psi
        B <- alpha[variables, 1, drop = F]
    }

    SSH <- t(B) %*% B * mod@N
    SSE <- sum(diag(psi[variables, variables, drop = F])) * mod@N

    df1 <- nmeasures
    df2 <- nmeasures * (mod@N - 1)
    F_value <- (SSH / df1) / (SSE / df2)
    p_value <- 1 - pf(F_value, df1, df2)

    #
    #SSA<-var(mod@C_matrix%*%alpha)
    #SSE<-sum(diag(psi)[-1])*N

    return(c(
        SSH = SSH,
        df1 = df1,
        SSE = SSE,
        df2 = df2,
        F_value = F_value,
        p_value = p_value
    ))

    #psi<-lavaan::lavInspect(mod@sem_obj,"est")$psi
    #psi<-psi[mod@lvs,mod@lvs]
    #alpha<-lavaan::lavInspect(mod@sem_obj,"est")$alpha
    #alpha<-alpha[which(rownames(alpha)%in%mod@lvs)]
    #
    #sum(diag(psi)[c(2,3)])*N/2
    #sum(diag(psi)[c(4)])*N/3
    #sum(diag(psi)[c(5,6)])*N
    #
    #var((C_matrix%*%alpha)[c(1,2,3)])*N*(3-1)
    #var(alpha[c(1,2,3)])*N*(3-1)
    #var(alpha[c(1,4)])*N*(2-1)
    #var(alpha[c(1,5,6)])*N*(3-1)
    #
    #SSA<-var(C_matrix%*%alpha)
    #SSE<-sum(diag(psi)[-1])*N


    #SSA<-0
    #SSE<-0
    #df1<-nmeasures
    #df2<-(N-1)*nmeasures
    #F.value<-(SSA/df1)/(SSE/df2)
    #p.value<-1-pf(Fvalue,df1,df2)

    #exact_F_stat<-list(SSA=SSA,
    #SSE=SSE,
    #F.value=F.value,
    #p.value=p.value,
    #df1=df1,
    #df2=df2)
}


#mod@hypotheses[[hyp_index]]$imatrix<-t(mod@C_matrix[mod@hypotheses[[hyp_index]]$indices,,drop=FALSE])
#mod@hypotheses[[hyp_index]]$wald_string<-paste0(paste0(mod@lv_labels[mod@hypotheses[[hyp_index]]$indices],"==0"),collapse="\n")
#mod@hypotheses[[hyp_index]]$wald_stat<-data.frame(lavaan::lavTestWald(mod@sem_obj,mod@hypotheses[[hyp_index]]$wald_string))
#mod@hypotheses[[hyp_index]]$approx_F_stat<-data.frame(F_value(mod@hypotheses[[hyp_index]]$wald_stat,N,nmeasures))


##Maybe,Iwilladdsquaresumsinthefuture
#psi<-lavaan::lavInspect(mod@sem_obj,"est")$psi
#psi<-psi[mod@lvs,mod@lvs]
#alpha<-lavaan::lavInspect(mod@sem_obj,"est")$alpha
#alpha<-alpha[which(rownames(alpha)%in%mod@lvs)]
#
#sum(diag(psi)[c(2,3)])*N/2
#sum(diag(psi)[c(4)])*N/3
#sum(diag(psi)[c(5,6)])*N
#
#var((C_matrix%*%alpha)[c(1,2,3)])*N*(3-1)
#var(alpha[c(1,2,3)])*N*(3-1)
#var(alpha[c(1,4)])*N*(2-1)
#var(alpha[c(1,5,6)])*N*(3-1)
#
#SSA<-var(C_matrix%*%alpha)
#SSE<-sum(diag(psi)[-1])*N
#
#
#SSA<-0
#SSE<-0
#df1<-nmeasures
#df2<-(N-1)*nmeasures
#F.value<-(SSA/df1)/(SSE/df2)
#p.value<-1-pf(Fvalue,df1,df2)
#
#exact_F_stat<-list(SSA=SSA,
#SSE=SSE,
#F.value=F.value,
#p.value=p.value,
#df1=df1,
#df2=df2)
#
#mod@hypotheses[[hyp_index]]$exact_F_stat<-lst
