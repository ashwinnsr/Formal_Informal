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

install_if_missing("fixest")
install_if_missing("stargazer")
install_if_missing("DescTools")
