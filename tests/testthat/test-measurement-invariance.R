# Uses the package's own bundled `reading_latent` data (real data, no
# simulation).
# No oracle exists yet for expected numeric values, so these check
# structural sanity (converges, correct df, monotonic nesting), not exact
# estimates.

reading_cell <- function(grades, sentences, indicators = c("fix_count", "ini_land_pos")) {
    d <- reading_latent[
        reading_latent$indicator %in% indicators &
            reading_latent$grade %in% grades &
            reading_latent$sentence %in% sentences &
            !is.na(reading_latent$dv),
    ]
    d$cell <- paste0(d$indicator, "_", as.character(d$grade), "_", as.character(d$sentence))
    wide <- tidyr::pivot_wider(
        as.data.frame(d)[c("id", "cell", "dv")],
        names_from = "cell", values_from = "dv"
    )
    wide <- as.data.frame(wide)
    wide[stats::complete.cases(wide), ]
}

fit_quietly <- function(syntax, data) {
    withCallingHandlers(
        lavaan::sem(syntax, data = data, meanstructure = TRUE),
        warning = function(w) invokeRestart("muffleWarning")
    )
}

test_that("scenario 6b: single-occasion, 2-indicator latent construct identifies cleanly with no manual workaround", {
    # this is the direct regression test for the identification bug --
    # see decisions.md and target/test-scenarios.md scenario 6b. The old
    # code (and semTools's measEq.syntax(), confirmed empirically) produces
    # df = -1 (non-identified) here; the fix must produce df >= 0.
    d <- reading_cell(grades = "grade1", sentences = "sentence")
    etas <- list(eta1 = c("fix_count_grade1_sentence", "ini_land_pos_grade1_sentence"))

    syn <- build_measurement_syntax(etas)
    expect_match(syn, "1\\*fix_count_grade1_sentence")
    expect_match(syn, "~ 0\\*1")

    fit <- lavaan::sem(syn, data = d, meanstructure = TRUE)
    expect_true(lavaan::lavInspect(fit, "converged"))
    expect_equal(unname(lavaan::fitmeasures(fit)["df"]), 1)
})

test_that("a single-occasion construct with 3+ indicators does NOT need the tau-equivalence patch (standard identification suffices)", {
    d <- reading_cell(grades = "grade1", sentences = "sentence", indicators = c("fix_count", "ini_land_pos", "re_fix_time"))
    etas <- list(eta1 = c("fix_count_grade1_sentence", "ini_land_pos_grade1_sentence", "re_fix_time_grade1_sentence"))

    syn <- build_measurement_syntax(etas)
    fit <- fit_quietly(syn, d)
    expect_true(lavaan::lavInspect(fit, "converged"))
    expect_equal(unname(lavaan::fitmeasures(fit)["df"]), 0)
})

test_that("scenario 6: multi-occasion (6 cells), 2-indicator constructs identify without a manual append workaround", {
    d <- reading_cell(grades = c("grade1", "grade2", "grade4"), sentences = c("sentence", "landolt"))
    etas <- list(
        eta1 = c("fix_count_grade1_sentence", "ini_land_pos_grade1_sentence"),
        eta2 = c("fix_count_grade2_sentence", "ini_land_pos_grade2_sentence"),
        eta3 = c("fix_count_grade4_sentence", "ini_land_pos_grade4_sentence"),
        eta4 = c("fix_count_grade1_landolt", "ini_land_pos_grade1_landolt"),
        eta5 = c("fix_count_grade2_landolt", "ini_land_pos_grade2_landolt"),
        eta6 = c("fix_count_grade4_landolt", "ini_land_pos_grade4_landolt")
    )

    syn <- build_measurement_syntax(etas, invariance_within = "strong")
    fit <- fit_quietly(syn, d)
    expect_true(lavaan::lavInspect(fit, "converged"))
    expect_equal(unname(lavaan::fitmeasures(fit)["df"]), 49)
})

test_that("scenario 7: invariance levels nest monotonically (configural <= weak <= strong <= strict in df)", {
    d <- reading_cell(grades = c("grade1", "grade2", "grade4"), sentences = c("sentence", "landolt"))
    etas <- list(
        eta1 = c("fix_count_grade1_sentence", "ini_land_pos_grade1_sentence"),
        eta2 = c("fix_count_grade2_sentence", "ini_land_pos_grade2_sentence"),
        eta3 = c("fix_count_grade4_sentence", "ini_land_pos_grade4_sentence"),
        eta4 = c("fix_count_grade1_landolt", "ini_land_pos_grade1_landolt"),
        eta5 = c("fix_count_grade2_landolt", "ini_land_pos_grade2_landolt"),
        eta6 = c("fix_count_grade4_landolt", "ini_land_pos_grade4_landolt")
    )

    dfs <- vapply(c("configural", "weak", "strong", "strict"), function(level) {
        fit <- fit_quietly(build_measurement_syntax(etas, invariance_within = level), d)
        expect_true(lavaan::lavInspect(fit, "converged"), info = level)
        unname(lavaan::fitmeasures(fit)["df"])
    }, numeric(1))

    expect_true(all(diff(dfs) > 0))
})

test_that("a single-indicator (manifest) construct needs no measurement syntax", {
    expect_equal(build_measurement_syntax(list(Y1 = "Y1")), "")
})

test_that("build_pi_layer_syntax() produces fixed loadings and zeroes out the etas' own mean/(co)variance", {
    c_within <- matrix(c(1, 1, 1, -1), nrow = 2)
    syn <- build_pi_layer_syntax(c_within, eta_names = c("e1", "e2"))
    expect_match(syn, "\\.pi1 =~")
    expect_match(syn, "\\.pi2 =~")
    expect_match(syn, "e1 ~ 0\\*1")      # latent mean fixed: pi carries it
    expect_match(syn, "e1 ~~ 0\\*e1")    # variance fixed: pi carries it
})

test_that("build_pi_layer_syntax() requires one eta name per occasion", {
    c_within <- matrix(c(1, 1, 1, -1), nrow = 2)
    expect_error(build_pi_layer_syntax(c_within, eta_names = "only_one"), "one name per occasion")
})

test_that("strip_construct_structure() removes mean/(co)variance lines but keeps measurement lines", {
    syntax <- paste(
        "eta1 =~ NA*Y1 + lambda.1_1*Y1",
        "eta1 ~ NA*1 + alpha.1*1",
        "eta1 ~~ NA*eta1 + psi.1_1*eta1",
        "Y1 ~~ NA*Y1 + theta.1_1*Y1",
        sep = "\n"
    )
    out <- strip_construct_structure(syntax, "eta1")
    expect_match(out, "eta1 =~")        # measurement kept
    expect_false(grepl("eta1 ~ NA", out))   # latent mean removed
    expect_false(grepl("eta1 ~~", out))     # variance removed
    expect_match(out, "Y1 ~~")          # other variables untouched
})

test_that("build_resid_cov_syntax() free vs. equal sets produce distinct syntax shapes", {
    free <- build_resid_cov_syntax(resid_cov = list(c("Y1", "Y2")))
    expect_equal(free, "Y1 ~~ Y2")

    equal <- build_resid_cov_syntax(equal_resid_cov = list(c("Y1", "Y2", "Y3")))
    expect_match(equal, "Y1 ~~ \\.theta_cov_1_1\\*Y2")
    expect_match(equal, "Y1 ~~ \\.theta_cov_1_1\\*Y3")
    expect_match(equal, "Y2 ~~ \\.theta_cov_1_1\\*Y3")
})

test_that("build_resid_cov_syntax() with nothing specified returns an empty string", {
    expect_equal(build_resid_cov_syntax(), "")
})

test_that("full eta+pi integration: strip_construct_structure() combines cleanly with no duplicate-modifier warnings, correct end-to-end df", {
    d <- reading_cell(grades = c("grade1", "grade2", "grade4"), sentences = c("sentence", "landolt"))
    eta_names <- paste0(".eta", 1:6)
    cells <- expand.grid(sentence = c("sentence", "landolt"), grade = c("grade1", "grade2", "grade4"))
    etas <- stats::setNames(
        lapply(seq_len(6), function(i) {
            c(paste0("fix_count_", cells$grade[i], "_", cells$sentence[i]), paste0("ini_land_pos_", cells$grade[i], "_", cells$sentence[i]))
        }),
        eta_names
    )

    c_within <- solve(cbind(1, stats::contr.poly(6)))
    dimnames(c_within) <- list(eta_names, NULL)

    meas_syn <- strip_construct_structure(build_measurement_syntax(etas, invariance_within = "strong"), eta_names)
    pi_syn <- build_pi_layer_syntax(c_within, eta_names = eta_names)

    msgs <- character(0)
    fit <- withCallingHandlers(
        lavaan::sem(paste(meas_syn, pi_syn, sep = "\n"), data = d, meanstructure = TRUE),
        warning = function(w) {
            msgs <<- c(msgs, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    expect_false(any(grepl("overwritten", msgs)))
    # the redundancy warning must be gone too: it's the signature of the
    # eta mean/variance being free alongside the pi layer's
    expect_false(any(grepl("vcov.*not.*positive definite|not identified", msgs)))
    expect_true(lavaan::lavInspect(fit, "converged"))
    expect_equal(unname(lavaan::fitmeasures(fit)["df"]), 49)

    pe <- lavaan::parameterEstimates(fit)
    expect_equal(sum(grepl("^\\.alpha_pi_", pe$label)), 6L)
})

# ---- label ownership -------------------------------------------------------

test_that("the measurement layer comes back labelled by the engine, not by semTools", {
    etas <- list(.eta1 = c("a1", "a2"), .eta2 = c("b1", "b2"), .eta3 = c("c1", "c2"))
    syn <- suppressWarnings(build_measurement_syntax(
        etas, group_labels = c("g1", "g2"),
        invariance_within = "strong", invariance_between = "weak"
    ))
    labels <- unique(lavaan::lavaanify(syn, ngroups = 2, meanstructure = TRUE)$label)
    labels <- labels[nzchar(labels)]

    expect_true(all(grepl("^\\.", labels))) # one namespace, semnova's
    expect_true(all(!is.na(vapply(labels, function(l) parse_param_label(l)$type, character(1)))))
})

test_that("a label states its own equality class", {
    etas <- list(.eta1 = c("a1", "a2"), .eta2 = c("b1", "b2"), .eta3 = c("c1", "c2"))
    syn <- suppressWarnings(build_measurement_syntax(
        etas, group_labels = c("g1", "g2"),
        invariance_within = "strong", invariance_between = "weak"
    ))
    pt <- lavaan::lavaanify(syn, ngroups = 2, meanstructure = TRUE)

    # loadings: equal across occasions (strong within) and groups (weak
    # between), so both dimensions collapse to 0 -- and one label covers all
    # six rows, which is what imposes the constraint
    loadings <- pt[pt$op == "=~" & nzchar(pt$label), ]
    expect_equal(sort(unique(loadings$label)), c(".lambda_0_1_0", ".lambda_0_2_0"))
    expect_equal(sum(loadings$label == ".lambda_0_1_0"), 6L) # 3 occasions x 2 groups

    # intercepts: equal across occasions but not groups, so the group index
    # survives while the construct index collapses
    intercepts <- pt[pt$op == "~1" & grepl("^\\.nu_", pt$label), ]
    expect_setequal(unique(intercepts$label), c(".nu_0_1_1", ".nu_0_1_2", ".nu_0_2_1", ".nu_0_2_2"))

    # configural constrains nothing, so nothing collapses
    free <- suppressWarnings(build_measurement_syntax(etas, invariance_within = "configural"))
    free_loadings <- lavaan::lavaanify(free, meanstructure = TRUE)
    free_loadings <- free_loadings[free_loadings$op == "=~" & nzchar(free_loadings$label), ]
    expect_false(any(grepl("_0_", free_loadings$label)))
})

test_that("relabelling preserves the constraint structure it renames", {
    etas <- list(.eta1 = c("a1", "a2"), .eta2 = c("b1", "b2"), .eta3 = c("c1", "c2"))
    configural <- paste(sprintf("%s =~ %s", names(etas),
        vapply(etas, paste, character(1), collapse = " + ")), collapse = "\n")
    raw <- as.character(suppressWarnings(semTools::measEq.syntax(
        configural.model = configural, ID.fac = "effects.code", meanstructure = TRUE,
        longFacNames = list(.eta = names(etas)), long.equal = c("loadings", "intercepts"),
        auto = 0, sample.nobs = 50L
    )))
    relabelled <- semnova:::relabel_measurement_syntax(raw, etas)

    count_free <- function(s) {
        pt <- lavaan::lavaanify(s, meanstructure = TRUE)
        c(rows = nrow(pt), free = max(pt$free), labels = length(unique(pt$label[nzchar(pt$label)])))
    }
    expect_equal(count_free(relabelled), count_free(raw))
    expect_false(identical(relabelled, raw)) # it really did rename them
})

test_that("covariate constructs are numbered past the etas, not on top of them", {
    covs <- list(iq = c("iq1", "iq2", "iq3"))
    syn <- build_covariate_syntax(covs, pis = c(".pi1", ".pi2"), n_eta_constructs = 6L)
    labels <- unique(lavaan::lavaanify(syn, meanstructure = TRUE)$label)
    loadings <- grep("^\\.lambda_", labels, value = TRUE)

    # an eta would have used constructs 1..6, so the covariate must start at 7
    expect_true(all(vapply(loadings, function(l) parse_param_label(l)$construct, integer(1)) == 7L))
})

# ---- covariance structure of the contrast layer ----------------------------

pi_syntax <- function(...) {
    build_pi_layer_syntax(diag(4), eta_names = paste0("y", 1:4), ...)
}

test_that("by default the pi layer is unstructured and the etas carry nothing", {
    lines <- strsplit(pi_syntax(), "\n")[[1]]
    covariances <- grep("^\\.pi[0-9]+ ~~", lines, value = TRUE)

    expect_true(all(grepl("\\.psi_pi_", covariances))) # every one freely labelled
    expect_false(any(grepl("0\\*", covariances)))
    expect_true(all(grepl("~~ 0\\*", grep("^y[0-9]+ ~~", lines, value = TRUE))))
})

test_that("sphericity equates a set's variances and zeroes its covariances", {
    lines <- strsplit(pi_syntax(sphericity = list(2:4)), "\n")[[1]]

    # one shared variance for the set, named after its lowest member
    variances <- grep("^\\.pi([234]) ~~ .*\\.pi\\1$", lines, value = TRUE)
    expect_length(variances, 3L)
    expect_true(all(grepl("\\.psi_pi_2_1\\*", variances)))

    # zero covariances inside the set, free ones to the unconstrained pi1
    expect_true(any(grepl("^\\.pi2 ~~ 0\\*\\.pi3$", lines)))
    expect_true(any(grepl("^\\.pi3 ~~ 0\\*\\.pi4$", lines)))
    expect_true(any(grepl("^\\.pi1 ~~ \\.psi_pi_1_2_1\\*\\.pi2$", lines)))
    expect_true(any(grepl("^\\.pi1 ~~ \\.psi_pi_1_1\\*\\.pi1$", lines)))
})

test_that("compound symmetry moves the structure onto the etas", {
    lines <- strsplit(pi_syntax(compound_symmetry = TRUE), "\n")[[1]]

    # one variance for every occasion, one covariance for every pair: index 0
    expect_true(any(grepl("^y1 ~~ \\.psi_0_1\\*y1$", lines)))
    expect_true(any(grepl("^y4 ~~ \\.psi_0_1\\*y4$", lines)))
    expect_true(any(grepl("^y1 ~~ \\.psi_0_0_1\\*y2$", lines)))
    expect_true(any(grepl("^y3 ~~ \\.psi_0_0_1\\*y4$", lines)))

    # and the contrast layer's own (co)variances are fixed to zero instead
    expect_true(all(grepl("~~ 0\\*", grep("^\\.pi[0-9]+ ~~", lines, value = TRUE))))
})

test_that("compound symmetry takes precedence over sphericity, and says so", {
    expect_warning(
        lines <- strsplit(pi_syntax(sphericity = list(2:4), compound_symmetry = TRUE), "\n")[[1]],
        "sphericity.*ignored"
    )
    expect_true(all(grepl("~~ 0\\*", grep("^\\.pi[0-9]+ ~~", lines, value = TRUE))))
})

test_that("variance homogeneity shares the parameters across groups", {
    lines <- strsplit(pi_syntax(group_labels = c("a", "b"), variance_homogeneity = TRUE), "\n")[[1]]
    variances <- grep("^\\.pi1 ~~ .*\\.pi1$", lines, value = TRUE)

    # one label repeated across the groups is what imposes the equality, and
    # group index 0 is how the label says it is shared
    expect_match(variances, "c\\(\\.psi_pi_1_0, \\.psi_pi_1_0\\)")

    free <- strsplit(pi_syntax(group_labels = c("a", "b")), "\n")[[1]]
    expect_match(grep("^\\.pi1 ~~ .*\\.pi1$", free, value = TRUE), "c\\(\\.psi_pi_1_1, \\.psi_pi_1_2\\)")
})

test_that("stochastic group weights are syntax plus definitions; fixed is nothing", {
    expect_equal(build_group_weight_syntax(c("a", "b"), "fixed"), "")
    expect_equal(build_group_weight_syntax("a", "stochastic"), "") # nothing to weight

    syn <- build_group_weight_syntax(c("a", "b"), "stochastic")
    expect_match(syn, "^group % c\\(\\.gw_1, \\.gw_2\\)\\*w")
    expect_match(syn, "N := exp\\(\\.gw_1\\) \\+ exp\\(\\.gw_2\\)")
    expect_match(syn, "relfreq2 := exp\\(\\.gw_2\\) / N")
})

test_that("sphericity sets are validated strictly", {
    pis <- paste0(".pi", 1:4)
    expect_error(semnova:::check_sphericity(c(2, 3), pis), "must be a list")
    expect_error(semnova:::check_sphericity(list(c(2, 9)), pis), "outside the design")
    expect_error(semnova:::check_sphericity(list(c(".pi2", ".nope")), pis), "not in the model")
    expect_error(semnova:::check_sphericity(list(2:3, 3:4), pis), "more than one set")
    expect_error(semnova:::check_sphericity(list(list(2)), pis), "numeric index vectors")

    expect_equal(semnova:::check_sphericity(list(c(".pi2", ".pi3")), pis), list(2:3))
    expect_equal(semnova:::check_sphericity(list(2), pis), list()) # a set of one constrains nothing
})
