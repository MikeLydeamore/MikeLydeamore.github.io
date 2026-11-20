# Deterministic, frequency-dependent SIR model with perfect, pre-existing
# vaccine protection. Rows are parameter sets; columns are patches.
patch_settings <- list(
    population = rep(100000, 3),
    coverage = c(0.20, 0.45, 0.70),
    initial_infectious = rep(10, 3),
    gamma = 1 / 5,
    mixing = 0.02,
    days = 7
)

simulate_patches <- function(beta, settings = patch_settings, dt = 0.25) {
    beta <- matrix(beta, ncol = 3)
    stopifnot(
        all(is.finite(beta)), all(beta >= 0),
        dt > 0, settings$mixing >= 0, settings$mixing <= 1,
        abs(settings$days / dt - round(settings$days / dt)) < 1e-8
    )
    n <- nrow(beta)
    population <- matrix(settings$population, n, 3, byrow = TRUE)
    susceptible <- matrix(
        settings$population * (1 - settings$coverage) -
            settings$initial_infectious,
        n, 3, byrow = TRUE
    )
    infectious <- matrix(settings$initial_infectious, n, 3, byrow = TRUE)
    initial_susceptible <- susceptible
    mixing <- matrix(settings$mixing / 2, 3, 3)
    diag(mixing) <- 1 - settings$mixing

    derivative <- function(s, i) {
        force <- beta * ((i / population) %*% t(mixing))
        incidence <- s * force
        list(s = -incidence, i = incidence - settings$gamma * i)
    }

    for (step in seq_len(round(settings$days / dt))) {
        k1 <- derivative(susceptible, infectious)
        k2 <- derivative(
            susceptible + dt * k1$s / 2,
            infectious + dt * k1$i / 2
        )
        k3 <- derivative(
            susceptible + dt * k2$s / 2,
            infectious + dt * k2$i / 2
        )
        k4 <- derivative(susceptible + dt * k3$s, infectious + dt * k3$i)
        susceptible <- susceptible + dt / 6 *
            (k1$s + 2 * k2$s + 2 * k3$s + k4$s)
        infectious <- infectious + dt / 6 *
            (k1$i + 2 * k2$i + 2 * k3$i + k4$i)
    }
    recovered <- population - susceptible - infectious
    list(
        cases = initial_susceptible - susceptible,
        susceptible = susceptible,
        infectious = infectious,
        recovered = recovered
    )
}

calibrate_patches <- function(n = 60000, seed = 2026, tolerance = 0.5) {
    set.seed(seed)
    # A search box, not an assertion about plausible epidemiological priors:
    # all three initial within-patch reproduction numbers range from 1.1 to 1.5.
    lower <- 0.22 / (1 - patch_settings$coverage)
    upper <- 0.30 / (1 - patch_settings$coverage)
    beta <- sweep(matrix(runif(n * 3), ncol = 3), 2, upper - lower, "*")
    beta <- sweep(beta, 2, lower, "+")
    colnames(beta) <- paste0("beta", 1:3)
    truth <- 0.26 / (1 - patch_settings$coverage)
    target <- sum(simulate_patches(truth)$cases)
    prediction <- rowSums(simulate_patches(beta)$cases)
    keep <- abs(prediction - target) <= tolerance
    list(
        beta = beta[keep, , drop = FALSE],
        prediction = prediction[keep],
        target = target,
        truth = truth,
        lower = lower,
        upper = upper,
        tolerance = tolerance,
        attempted = n
    )
}
