# From the project root (so R activates the project's renv library):
# Rscript -e 'setwd("blog/seeing-correlations-with-tours"); source("test-tour.R")'
source("patch-model.R")
fit <- calibrate_patches()

# Execute the actual widget chunk so the test stays tied to the blog example.
post <- readLines("index.qmd")
start <- which(post == "#| label: fig-tour")
end <- start + which(post[seq.int(start + 1, length(post))] == "```")[1]
widget <- eval(parse(text = post[seq.int(start + 1, end - 1)]))
stopifnot(
    inherits(widget, "htmlwidget"),
    isTRUE(all.equal(attr(parameter_tour, "dataset"), fit$beta)),
    widget$x$config$paused,
    identical(widget$x$config$axisLabels, colnames(fit$beta))
)

# Display scaling must use one common multiplier, retaining the raw geometry.
centred <- scale(fit$beta, center = TRUE, scale = FALSE)
expected <- centred / max(sqrt(rowSums(centred^2)))
stopifnot(max(abs(widget$x$dataset - expected)) < 1e-12)

# Every frame must project onto two perpendicular unit directions.
bases <- widget$x$projectionMatrices
errors <- vapply(bases, function(b) max(abs(crossprod(b) - diag(2))), 0)
stopifnot(max(errors) < 1e-8)

# The unguided tour should encounter a view that reveals the thin sheet.
covariance <- cov(fit$beta)
ratios <- vapply(bases, function(b) {
    projected <- crossprod(b, covariance %*% b)
    variances <- eigen(projected, symmetric = TRUE, only.values = TRUE)$values
    sqrt(min(variances) / max(variances))
}, 0)
stopifnot(min(ratios) < 0.1)
cat("Tour checks passed:", length(bases), "frames.\n")
cat("Narrowest projected width/length ratio:", min(ratios), "\n")
