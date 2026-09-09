test_that("hypothesis() constructs a Hypothesis with the given fields", {
    h <- hypothesis(M = matrix(1), G = matrix(1), L = matrix(1), description = "test")
    expect_s3_class(h, "Hypothesis")
    expect_equal(h$M, matrix(1))
    expect_equal(h$description, "test")
    expect_null(h$K)
})

test_that("validate_hypothesis() requires a Hypothesis object", {
    expect_error(validate_hypothesis(list(), fit = list()), "must be a Hypothesis")
})

# a minimal fit-like object (plain list) is enough, since validate_hypothesis()
# only reads public fields -- exactly the point of dropping the naming object
# and the R6-expose pattern
fake_fit <- function(n_etas = 1, n_groups = 1, n_covariates = 0) {
    list(
        etas_mmodel = setNames(as.list(rep("Y", n_etas)), paste0("eta", seq_len(n_etas))),
        group_labels = paste0("g", seq_len(n_groups)),
        covariates_mmodel = if (n_covariates == 0) list() else setNames(as.list(rep("cov", n_covariates)), paste0("cov", seq_len(n_covariates)))
    )
}

test_that("validate_hypothesis() fills in M/G/L defaults for the trivial single-eta/group/no-covariate case", {
    h <- hypothesis()
    validate_hypothesis(h, fake_fit())
    expect_equal(h$M, matrix(1))
    expect_equal(h$G, matrix(1))
    expect_equal(h$L, matrix(1))
    expect_equal(h$K, array(0, dim = c(1, 1, 1)))
})

test_that("validate_hypothesis() errors when M is required but missing (multiple etas)", {
    h <- hypothesis()
    expect_error(validate_hypothesis(h, fake_fit(n_etas = 2)), "hypothesis\\$M must be provided")
})

test_that("validate_hypothesis() errors when G is required but missing (multiple groups)", {
    h <- hypothesis()
    expect_error(validate_hypothesis(h, fake_fit(n_groups = 2)), "hypothesis\\$G must be provided")
})

test_that("validate_hypothesis() errors when L is required but missing (covariates present)", {
    h <- hypothesis()
    expect_error(validate_hypothesis(h, fake_fit(n_covariates = 1)), "hypothesis\\$L must be provided")
})

test_that("validate_hypothesis() checks supplied matrix dimensions", {
    h <- hypothesis(M = matrix(1, nrow = 2))
    expect_error(validate_hypothesis(h, fake_fit(n_etas = 1)), "nrow\\(hypothesis\\$M\\)")

    h2 <- hypothesis(M = matrix(1), G = matrix(1, ncol = 2))
    expect_error(validate_hypothesis(h2, fake_fit(n_groups = 1)), "ncol\\(hypothesis\\$G\\)")
})

test_that("validate_hypothesis() checks K's dimensions against G/L/M once filled", {
    h <- hypothesis(K = array(0, dim = c(2, 1, 1)))
    expect_error(validate_hypothesis(h, fake_fit()), "dim\\(hypothesis\\$K\\)")
})

test_that("validate_hypothesis() is idempotent on an already-filled hypothesis", {
    h <- hypothesis()
    validate_hypothesis(h, fake_fit())
    expect_silent(validate_hypothesis(h, fake_fit()))
})

test_that("validate_hypothesis() works for a full multigroup/multi-eta/covariate hypothesis, matching test-scenarios.md scenario 8", {
    # mirrors target/test-scenarios.md scenario 8's custom low-level workflow:
    # 6 within-conditions (single-indicator etas), 2 groups, no covariates
    G <- matrix(c(-1, 1), nrow = 1)
    L <- matrix(1, nrow = 1)
    M <- matrix(c(
        0, 0,
        0, 0,
        0, 0,
        0, 0,
        1, 0,
        0, 1
    ), nrow = 6, byrow = TRUE)
    h <- hypothesis(L = L, G = G, M = M)
    validate_hypothesis(h, fake_fit(n_etas = 6, n_groups = 2))
    expect_equal(dim(h$K), c(1L, 1L, 2L))
})
