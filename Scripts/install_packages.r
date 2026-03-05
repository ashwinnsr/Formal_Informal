# Create local library directory if it doesn't exist
lib_dir <- "R_libs"
if (!dir.exists(lib_dir)) {
    dir.create(lib_dir)
}

# Add to library path
.libPaths(c(lib_dir, .libPaths()))

# Function to install if missing
install_if_missing <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, lib = lib_dir, repos = "http://cran.us.r-project.org")
    }
}

# Core analysis
install_if_missing("tidyverse")
install_if_missing("haven")
install_if_missing("fixest")
install_if_missing("ggplot2")
install_if_missing("patchwork")

# Diagnostics & robustness
install_if_missing("car") # VIF (multicollinearity)
install_if_missing("lmtest") # Breusch-Pagan, Durbin-Watson
install_if_missing("sensemakr") # Oster-style sensitivity bounds
install_if_missing("boot") # Bootstrap confidence intervals
install_if_missing("plm") # Panel unit root tests

# Legacy (kept for compatibility)
install_if_missing("stargazer")
install_if_missing("DescTools")
