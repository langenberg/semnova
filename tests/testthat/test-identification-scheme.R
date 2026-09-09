test_that("identification_scheme() is trivial for a single indicator, regardless of occasions", {
    expect_equal(identification_scheme(1, 1, "effect")$method, "trivial")
    expect_equal(identification_scheme(6, 1, "referent")$method, "trivial")
})

test_that("identification_scheme() requires full tau-equivalence for the degenerate 2-indicator single-occasion case, regardless of lv_scaling", {
    expect_equal(identification_scheme(1, 2, "effect")$method, "full_tau_equivalence")
    expect_equal(identification_scheme(1, 2, "referent")$method, "full_tau_equivalence")
})

test_that("identification_scheme() uses the standard scheme once there is more than one occasion", {
    expect_equal(identification_scheme(2, 2, "effect")$method, "effect_coding")
    expect_equal(identification_scheme(3, 2, "referent")$method, "referent")
    expect_equal(identification_scheme(6, 2, "effect")$method, "effect_coding")
})

test_that("identification_scheme() uses the standard scheme for 3+ indicators even at a single occasion", {
    expect_equal(identification_scheme(1, 3, "effect")$method, "effect_coding")
    expect_equal(identification_scheme(1, 4, "referent")$method, "referent")
})

test_that("identification_scheme() defaults lv_scaling to effect coding", {
    expect_equal(identification_scheme(2, 2)$method, "effect_coding")
})
