<h1 align="center">parSim</h1>

<p align="center">
    <a href="https://www.r-pkg.org/pkg/parSim"><img src="https://www.r-pkg.org/badges/version/parSim" alt="CRAN version"/></a>
    <a href="https://cran.r-project.org/web/checks/check_results_parSim.html"><img src="https://badges.cranchecks.info/worst/parSim.svg" alt="CRAN checks"/></a>
    <a href="https://github.com/SachaEpskamp/parSim/actions"><img src="https://github.com/SachaEpskamp/parSim/workflows/R-CMD-check/badge.svg" alt="R-CMD-check" /></a>
</p>

## Description.

`parSim` is an `R` package that provides convenient functions to perform
flexible simulations in parallel.

## Installation

- to install from CRAN run `install.packages("parSim")`
- to install the latest version from GitHub run `remotes::install_github("SachaEpskamp/parSim")`

## Example

```r
# Determine a function to evaluate for each simulation condition.
bias <- function(x, y) {
    # Perform some computation.
    result <- abs(x - y)

    # Return the result.
    return(result)
}

# Run the simulation.
results <- parSim(
    # The simulation conditions.
    sample_size = c(50, 100, 250),
    beta = c(0, 0.5, 1),
    sigma = c(0.25, 0.5, 1),

    # The expression to evaluate for each simulation condition.
    expression = {
        # Generate the data.
        x <- rnorm(sample_size)
        y <- beta * x + rnorm(sample_size, sigma)

        # Fit the model.
        fit <- lm(y ~ x)

        # Compute the relevant quantities.
        beta_estimate <- coef(fit)[2]
        r_squared <- summary(fit)$r.squared
        bias <- bias(beta, beta_estimate)

        # Return in a compatible format.
        list(
            beta_estimate = beta_estimate,
            r_squared = r_squared,
            bias = bias
        )
    },

    # The number of replications.
    replications = 100,

    # The conditions to exclude.
    exclude = sample_size == 50 | beta <= 0.5,

    # The variables to export.
    exports = c("bias"),

    # No packages are required for export.
    packages = NULL,

    # Do not save the results.
    save = FALSE,

    # Execute the simulation on a single core.
    cores = 1,

    # Show the progress bar.
    progress = TRUE
)

# Print the head of the results.
head(results)

# Configure the progress bar.
configure_bar(
    type = "modern",
    format = "[:bar] [:percent] [:elapsed]",
    show_after = 0.15
)

# Run the simulation again with more cores and the updated progress bar.
results <- parSim(
    # The simulation conditions.
    sample_size = c(50, 100, 250),
    beta = c(0, 0.5, 1),
    sigma = c(0.25, 0.5, 1),

    # The expression to evaluate for each simulation condition.
    expression = {
        # Generate the data.
        x <- rnorm(sample_size)
        y <- beta * x + rnorm(sample_size, sigma)

        # Fit the model.
        fit <- lm(y ~ x)

        # Compute the relevant quantities.
        beta_estimate <- coef(fit)[2]
        r_squared <- summary(fit)$r.squared
        bias <- bias(beta, beta_estimate)

        # Return in a compatible format.
        list(
            beta_estimate = beta_estimate,
            r_squared = r_squared,
            bias = bias
        )
    },

    # The number of replications.
    replications = 1000,

    # The conditions to exclude.
    exclude = sample_size == 50 | beta <= 0.5,

    # The variables to export.
    exports = c("bias"),

    # No packages are required for export.
    packages = NULL,

    # Save the results to a temporary file.
    save = TRUE,

    # Execute the simulation in parallel.
    cores = 2,

    # Show the progress bar.
    progress = TRUE
)

# Print the tail of the results.
tail(results)
```
