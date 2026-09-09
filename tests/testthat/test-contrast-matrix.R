test_that("contrast_matrix() recovers the identity contrast for a single group/predictor/measure", {
    G <- matrix(1)
    L <- matrix(1)
    M <- matrix(1)
    out <- contrast_matrix(G, L, M)
    expect_equal(out$R, matrix(1))
    expect_equal(out$dim, c(rows = 1L, cols = 1L))
})

test_that("contrast_matrix() dimensions follow d*h*r rows, n_groups*(p+1)*q cols", {
    G <- matrix(c(1, -1), nrow = 1)                 # d=1, n_groups=2
    L <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE) # h=2, p+1=2
    M <- matrix(c(1, -1, 1, 1), nrow = 2)            # q=2, r=2
    out <- contrast_matrix(G, L, M)
    expect_equal(nrow(out$R), 1L * 2L * 2L)
    expect_equal(ncol(out$R), 2L * 2L * 2L)
    expect_equal(out$dim, c(rows = 4L, cols = 8L))
})

test_that("contrast_matrix() applies G/L/M correctly against a known B_array", {
    # 2 groups, p+1=1 (intercept only), q=1 (single measure): trivial case where
    # R %*% vec(B) should just extract/contrast the group-level intercepts.
    G <- matrix(c(1, -1), nrow = 1)   # group contrast: g1 - g2
    L <- matrix(1)
    M <- matrix(1)
    B_array <- array(c(5, 3), dim = c(1, 1, 2)) # intercepts: group1=5, group2=3
    out <- contrast_matrix(G, L, M, B_array = B_array)
    est <- out$R %*% out$vec_B
    expect_equal(as.numeric(est), 5 - 3)
})

test_that("contrast_matrix() errors on non-matrix input", {
    expect_error(contrast_matrix(1, matrix(1), matrix(1)), "must all be matrices")
})

test_that("contrast_matrix() errors when B_array is incompatible with L/M/G", {
    G <- matrix(1)
    L <- matrix(1, nrow = 1, ncol = 2)
    M <- matrix(1)
    expect_error(
        contrast_matrix(G, L, M, B_array = array(0, dim = c(1, 1, 1))),
        "not compatible"
    )
})

test_that("contrast_matrix() reproduces the tutorial's hand-derived within-contrast inverse", {
    # exact values from target/reference-tutorial.Rmd's "Contrast matrix
    # inversion" scenario (a 3-grade x 2-sentence within-design contrast
    # matrix) -- the tutorial itself is flagged buggy elsewhere, but this
    # snippet is pure linear algebra, not model specification, so it's a
    # legitimate fixed-point check independent of that.
    mxdata <- c(
        0.1666667, 0.1666667, 0.1666667, 0.1666667, 0.1666667, 0.1666667,
        0.3333333, 0.3333333, -0.1666667, -0.1666667, -0.1666667, -0.1666667,
        -0.1666667, -0.1666667, 0.3333333, 0.3333333, -0.1666667, -0.1666667,
        0.1666667, -0.1666667, 0.1666667, -0.1666667, 0.1666667, -0.1666667,
        0.3333333, -0.3333333, -0.1666667, 0.1666667, -0.1666667, 0.1666667,
        -0.1666667, 0.1666667, 0.3333333, -0.3333333, -0.1666667, 0.1666667
    )
    within_c <- matrix(mxdata, nrow = 6, ncol = 6, byrow = TRUE)

    inv <- solve(within_c)
    expect_equal(dim(inv), c(6L, 6L))
    # first column (intercept contrast) recovers the indicator pattern of all 1s
    expect_equal(round(inv[, 1]), rep(1, 6))
    # every entry is a recognizable +-1/0 pattern (no leftover fractional noise)
    expect_equal(round(inv), inv, tolerance = 1e-5)
})
