# Run with Rscript test-model.R from this directory.
source("patch-model.R")

truth <- 0.26 / (1 - patch_settings$coverage)
baseline <- simulate_patches(truth)
stopifnot(
    all(baseline$susceptible >= 0),
    all(baseline$infectious >= 0),
    all(baseline$recovered >= 0),
    all(baseline$cases >= 0),
    all(baseline$cases <=
        patch_settings$population * (1 - patch_settings$coverage))
)

zero <- simulate_patches(rep(0, 3))
stopifnot(
    max(abs(zero$cases)) == 0,
    max(abs(zero$infectious -
        patch_settings$initial_infectious *
        exp(-patch_settings$gamma * patch_settings$days))) < 1e-6
)

isolated <- patch_settings
isolated$mixing <- 0
a <- simulate_patches(truth, isolated)
b <- simulate_patches(truth * c(1.1, 1, 1), isolated)
stopifnot(
    b$cases[1, 1] > a$cases[1, 1],
    identical(a$cases[1, 2:3], b$cases[1, 2:3])
)

fit <- calibrate_patches()
check <- fit$beta[seq_len(50), ]
stopifnot(
    max(abs(simulate_patches(check)$cases -
        simulate_patches(check, dt = 0.125)$cases)) < 1e-5,
    all(abs(fit$prediction - fit$target) <= fit$tolerance),
    identical(fit$beta, calibrate_patches()$beta),
    nrow(fit$beta) > 1000
)
cat("Model checks passed.\n")
cat("Accepted:", nrow(fit$beta), "of", fit$attempted, "\n")
