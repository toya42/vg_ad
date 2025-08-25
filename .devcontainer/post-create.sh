#!/usr/bin/env bash
# Post-create provisioning for GitHub Codespaces (rocker/verse base)
# - Tames yaml build on Ubuntu by relaxing -Werror=format-security
# - Disables renv staged installs so the correct deps (yaml >= 2.3.10) are
#   loaded from the project library while building packages like 'quarto'
# - Restores the renv environment and prints useful diagnostics

set -euo pipefail

echo ">>> Configure compiler flags to avoid format-security errors (yaml)"
mkdir -p "${HOME}/.R"
{
  echo "CFLAGS += -Wno-error=format-security"
  echo "CXXFLAGS += -Wno-error=format-security"
} >> "${HOME}/.R/Makevars"

# If you pin CRAN in .Rprofile, renv will use it. Nothing to do here.

echo ">>> Ensure renv is available"
R -q -e 'if (!requireNamespace("renv", quietly=TRUE)) install.packages("renv")'

# Critical: avoid staged install so build-time dependency resolution does not
# pull an older yaml from the site library when installing 'quarto'.
export RENV_CONFIG_INSTALL_STAGED=FALSE

echo ">>> Restoring renv library"
R -q -e 'renv::restore(prompt = FALSE)'

echo ">>> Sanity checks"
# Show yaml version (quarto requires >= 2.3.10)
R -q -e 'cat("yaml version: ", as.character(utils::packageVersion("yaml")), "\n", sep = "")'

# Show quarto R package version if present
R -q -e 'if (requireNamespace("quarto", quietly = TRUE)) cat("quarto (R pkg): ", as.character(utils::packageVersion("quarto")), "\n", sep = "") else cat("quarto (R pkg): not installed\n")'

# Show Quarto CLI path (may be absent in some images)
R -q -e 'if (requireNamespace("quarto", quietly = TRUE)) { p <- try(quarto::quarto_path(), silent = TRUE); cat("Quarto CLI: ", if (inherits(p, "try-error")) "not found" else p, "\n", sep = "") } else cat("Quarto CLI: n/a (R pkg missing)\n")'

# Session info to help debugging
R -q -e 'sessionInfo()'

echo ">>> Post-create completed"
